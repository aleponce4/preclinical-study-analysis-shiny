automation_timestamp <- function(time = Sys.time(), tz = "UTC") {
  format(as.POSIXct(time, tz = tz), "%Y-%m-%d %H:%M:%S %Z", tz = tz)
}

automation_ensure_runtime_dirs <- function(root) {
  dirs <- list(
    state = ensure_dir(automation_path(root, "state")),
    output = ensure_dir(automation_path(root, "output")),
    logs = ensure_dir(automation_path(root, "logs")),
    cache = ensure_dir(automation_path(root, "cache"))
  )

  dirs
}

automation_create_run_context <- function(root, verbose = FALSE) {
  dirs <- automation_ensure_runtime_dirs(root)
  run_id <- format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")
  log_file <- file.path(dirs$logs, sprintf("run-%s.log", format(Sys.time(), "%Y%m%d", tz = "UTC")))

  list(
    run_id = run_id,
    verbose = isTRUE(verbose),
    started_at = Sys.time(),
    dirs = dirs,
    log_file = log_file,
    root = root
  )
}

automation_log <- function(ctx, level = "INFO", message, force_stdout = FALSE) {
  timestamp <- automation_timestamp(Sys.time(), tz = "UTC")
  line <- sprintf("[%s] [%s] [%s] %s", timestamp, toupper(level), ctx$run_id, message)

  cat(line, "\n", sep = "", file = ctx$log_file, append = TRUE)
  if (isTRUE(ctx$verbose) || isTRUE(force_stdout) || !identical(toupper(level), "DEBUG")) {
    cat(line, "\n", sep = "")
  }

  invisible(line)
}

automation_prune_files <- function(path, older_than_days = 30L, pattern = NULL) {
  older_than_days <- as.integer(older_than_days)
  if (is.na(older_than_days) || older_than_days < 1L || !dir.exists(path)) {
    return(invisible(character()))
  }

  entries <- list.files(path, all.files = TRUE, full.names = TRUE, recursive = TRUE, no.. = TRUE)
  if (!length(entries)) {
    return(invisible(character()))
  }

  # Never prune dotfiles such as .gitkeep placeholder files.
  entries <- entries[!grepl("^\\.", basename(entries))]
  if (!length(entries)) {
    return(invisible(character()))
  }

  file_info <- file.info(entries)
  keep <- !is.na(file_info$mtime)
  entries <- entries[keep]
  file_info <- file_info[keep, , drop = FALSE]

  if (!is.null(pattern) && nzchar(pattern)) {
    keep_pattern <- grepl(pattern, basename(entries), perl = TRUE)
    entries <- entries[keep_pattern]
    file_info <- file_info[keep_pattern, , drop = FALSE]
  }

  if (!length(entries)) {
    return(invisible(character()))
  }

  cutoff <- Sys.time() - as.difftime(older_than_days, units = "days")
  stale <- entries[file_info$mtime < cutoff]
  stale <- stale[file.exists(stale)]

  if (!length(stale)) {
    return(invisible(character()))
  }

  stale <- stale[order(nchar(stale), decreasing = TRUE)]
  deleted <- character()
  for (target in stale) {
    ok <- if (dir.exists(target)) unlink(target, recursive = TRUE, force = FALSE) == 0 else file.remove(target)
    if (isTRUE(ok)) {
      deleted <- c(deleted, target)
    }
  }

  invisible(deleted)
}
