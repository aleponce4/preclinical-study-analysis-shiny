#!/usr/bin/env Rscript

parse_cli_args <- function(args) {
  out <- list(
    now = FALSE,
    study_id = NULL,
    dry_run_email = FALSE,
    verbose = FALSE,
    config_dir = NULL
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (identical(arg, "--now")) {
      out$now <- TRUE
    } else if (identical(arg, "--dry-run-email")) {
      out$dry_run_email <- TRUE
    } else if (identical(arg, "--verbose")) {
      out$verbose <- TRUE
    } else if (identical(arg, "--study-id")) {
      i <- i + 1L
      if (i > length(args)) stop("--study-id requires a value", call. = FALSE)
      out$study_id <- args[[i]]
    } else if (identical(arg, "--config-dir")) {
      i <- i + 1L
      if (i > length(args)) stop("--config-dir requires a value", call. = FALSE)
      out$config_dir <- args[[i]]
    } else {
      stop(sprintf("Unknown argument: %s", arg), call. = FALSE)
    }

    i <- i + 1L
  }

  out
}

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- args_all[grepl("^--file=", args_all)]
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[[1]])
} else {
  file.path(getwd(), "automation", "run_reports.R")
}

script_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
repo_root_guess <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(repo_root_guess, "automation", "R", "bootstrap.R"), chdir = TRUE)

main <- function() {
  cli <- parse_cli_args(commandArgs(trailingOnly = TRUE))

  ctx_info <- automation_load_context(repo_root_guess)
  root <- ctx_info$root
  ctx <- automation_create_run_context(root, verbose = cli$verbose)

  automation_log(ctx, "INFO", "Automation run started.")
  automation_log(
    ctx,
    "INFO",
    sprintf(
      "Flags: now=%s, study_id=%s, dry_run_email=%s",
      cli$now,
      cli$study_id %||% "<all>",
      cli$dry_run_email
    )
  )

  config_dir <- if (is.null(cli$config_dir)) automation_path(root, "config") else cli$config_dir
  config <- automation_load_config(root = root, config_dir = config_dir)

  state <- automation_load_state(root)
  due_info <- automation_select_due_studies(
    studies = config$studies,
    schedules = config$schedules,
    state = state,
    now = Sys.time(),
    force_now = cli$now,
    study_filter = cli$study_id
  )

  if (!is.null(cli$study_id) && nzchar(cli$study_id)) {
    filter_info <- due_info$filter_info %||% list()
    if (identical(filter_info$match_count %||% 0L, 0L)) {
      stop(sprintf("--study-id '%s' was not found in configured studies.", cli$study_id), call. = FALSE)
    }
    if (identical(filter_info$enabled_match_count %||% 0L, 0L)) {
      stop(sprintf("--study-id '%s' exists but is disabled (enabled: false).", cli$study_id), call. = FALSE)
    }
  }

  automation_log(
    ctx,
    "INFO",
    sprintf("Due studies: %d | Skipped: %d", length(due_info$due), length(due_info$skipped))
  )

  if (!length(due_info$due)) {
    deleted <- c(
      automation_prune_files(automation_path(root, "output"), older_than_days = config$retention_days),
      automation_prune_files(automation_path(root, "logs"), older_than_days = config$retention_days, pattern = "\\.log$"),
      automation_prune_files(automation_path(root, "cache"), older_than_days = config$retention_days)
    )
    if (length(deleted)) {
      automation_log(ctx, "INFO", sprintf("Pruned %d old artifacts.", length(deleted)))
    }
    automation_log(ctx, "INFO", "No studies due. Exiting.")
    return(invisible(list(success = TRUE, failures = list())))
  }

  smtp_config <- if (isTRUE(cli$dry_run_email)) {
    list(
      smtp_server = "smtp://dry-run.local",
      username = "",
      password = "",
      from = Sys.getenv("LABAPP_EMAIL_FROM", unset = "dry-run@localhost"),
      reply_to = "",
      subject_prefix = Sys.getenv("LABAPP_EMAIL_SUBJECT_PREFIX", unset = "[DRY-RUN]"),
      use_ssl = "try"
    )
  } else {
    automation_smtp_config_from_env()
  }

  failures <- list()
  successes <- list()

  for (due_item in due_info$due) {
    study <- due_item$study
    automation_log(ctx, "INFO", sprintf("Processing study '%s'", study$study_id))

    result <- tryCatch(
      {
        workbook_path <- automation_download_workbook(study, ctx)
        style <- config$styles[[study$style_id]]

        analysis <- automation_prepare_study_analysis(
          study = study,
          style = style,
          workbook_path = workbook_path,
          generated_at = Sys.time()
        )

        report_path <- automation_render_study_report(
          root = root,
          analysis = analysis,
          scheduled_at = due_item$scheduled_at,
          verbose = cli$verbose
        )

        email_result <- automation_send_study_report_email(
          study = study,
          analysis = analysis,
          report_path = report_path,
          smtp_config = smtp_config,
          dry_run = cli$dry_run_email,
          verbose = cli$verbose
        )

        state <<- automation_mark_state(
          state = state,
          study_id = study$study_id,
          scheduled_at = due_item$scheduled_at,
          status = "success",
          message = report_path
        )

        list(ok = TRUE, report_path = report_path, email_result = email_result)
      },
      error = function(e) {
        state <<- automation_mark_state(
          state = state,
          study_id = study$study_id,
          scheduled_at = due_item$scheduled_at,
          status = "failed",
          message = conditionMessage(e)
        )

        list(ok = FALSE, error = conditionMessage(e))
      }
    )

    if (isTRUE(result$ok)) {
      automation_log(
        ctx,
        "INFO",
        sprintf("Study '%s' complete. Report: %s", study$study_id, result$report_path)
      )
      successes[[length(successes) + 1L]] <- list(
        study_id = study$study_id,
        report_path = result$report_path
      )
    } else {
      automation_log(
        ctx,
        "ERROR",
        sprintf("Study '%s' failed: %s", study$study_id, result$error)
      )
      failures[[length(failures) + 1L]] <- list(
        study_id = study$study_id,
        error = result$error
      )
    }
  }

  automation_save_state(root, state)

  if (length(failures)) {
    tryCatch(
      {
        digest_result <- automation_send_failure_digest(
          failures = failures,
          admin_email = config$admin_email,
          smtp_config = smtp_config,
          dry_run = cli$dry_run_email,
          verbose = cli$verbose
        )

        if (!is.null(digest_result)) {
          automation_log(ctx, "WARN", "Failure digest sent to admin recipients.")
        }
      },
      error = function(e) {
        automation_log(ctx, "ERROR", sprintf("Failed to send failure digest: %s", conditionMessage(e)))
      }
    )
  }

  deleted <- c(
    automation_prune_files(automation_path(root, "output"), older_than_days = config$retention_days),
    automation_prune_files(automation_path(root, "logs"), older_than_days = config$retention_days, pattern = "\\.log$"),
    automation_prune_files(automation_path(root, "cache"), older_than_days = config$retention_days)
  )
  if (length(deleted)) {
    automation_log(ctx, "INFO", sprintf("Pruned %d old artifacts.", length(deleted)))
  }

  automation_log(
    ctx,
    "INFO",
    sprintf("Automation run finished. Successes=%d Failures=%d", length(successes), length(failures))
  )

  invisible(list(success = !length(failures), failures = failures, successes = successes))
}

result <- main()
if (!isTRUE(result$success)) {
  q(save = "no", status = 1L)
}
