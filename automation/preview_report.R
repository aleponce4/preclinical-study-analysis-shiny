#!/usr/bin/env Rscript

parse_preview_args <- function(args) {
  out <- list(
    input = NULL,
    title = "Example Report Preview",
    format = "preview",
    verbose = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (identical(arg, "--input")) {
      i <- i + 1L
      if (i > length(args)) stop("--input requires a value", call. = FALSE)
      out$input <- args[[i]]
    } else if (identical(arg, "--title")) {
      i <- i + 1L
      if (i > length(args)) stop("--title requires a value", call. = FALSE)
      out$title <- args[[i]]
    } else if (identical(arg, "--format")) {
      i <- i + 1L
      if (i > length(args)) stop("--format requires a value", call. = FALSE)
      out$format <- tolower(args[[i]])
    } else if (identical(arg, "--verbose")) {
      out$verbose <- TRUE
    } else {
      stop(sprintf("Unknown argument: %s", arg), call. = FALSE)
    }

    i <- i + 1L
  }

  if (!out$format %in% c("preview", "pdf")) {
    stop("--format must be one of: preview, pdf", call. = FALSE)
  }

  out
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "") || all(is.na(x))) y else x
}

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- args_all[grepl("^--file=", args_all)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else file.path(getwd(), "automation", "preview_report.R")
script_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
repo_root_guess <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(repo_root_guess, "automation", "R", "bootstrap.R"), chdir = TRUE)

main <- function() {
  cli <- parse_preview_args(commandArgs(trailingOnly = TRUE))
  ctx_info <- automation_load_context(repo_root_guess)
  root <- ctx_info$root
  ctx <- automation_create_run_context(root, verbose = cli$verbose)

  input_path <- cli$input %||% project_path("inst", "templates", "example_weights.csv")
  resolved_input <- automation_resolve_source_reference(input_path)
  if (!file.exists(input_path) && !automation_is_http_url(resolved_input)) {
    stop(sprintf("Input file not found: %s", input_path), call. = FALSE)
  }

  style <- automation_load_styles(file.path(root, "automation", "config", "styles.yml"))$default
  source_format <- automation_detect_source_format(input_path)

  study <- list(
    study_id = "preview_report",
    enabled = TRUE,
    source_url = input_path,
    source_format = source_format,
    style_id = "default",
    schedule_id = "daily_0900",
    report_title = cli$title,
    weights_sheet = 1,
    email_to = c("preview@localhost"),
    email_cc = character()
  )

  workbook_path <- if (file.exists(input_path) && !automation_is_http_url(resolved_input)) {
    input_path
  } else {
    automation_download_workbook(study, ctx)
  }

  analysis <- automation_prepare_study_analysis(
    study = study,
    style = style,
    workbook_path = workbook_path,
    generated_at = Sys.time()
  )

  out_path <- if (identical(cli$format, "pdf")) {
    if (!rmarkdown::pandoc_available()) {
      stop("PDF preview requires pandoc to be installed.", call. = FALSE)
    }
    if (!requireNamespace("tinytex", quietly = TRUE) || !tinytex::is_tinytex()) {
      stop("PDF preview requires TinyTeX/LaTeX to be installed.", call. = FALSE)
    }
    automation_render_study_report(
      root = root,
      analysis = analysis,
      scheduled_at = Sys.time(),
      verbose = cli$verbose
    )
  } else {
    automation_render_preview_report(
      root = root,
      analysis = analysis,
      scheduled_at = Sys.time(),
      verbose = cli$verbose
    )
  }

  cat(out_path, "\n", sep = "")
}

main()
