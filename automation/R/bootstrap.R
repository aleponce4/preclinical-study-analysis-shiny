automation_find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)

  repeat {
    if (file.exists(file.path(current, "VERSION")) && dir.exists(file.path(current, "app", "R"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Project root not found.", call. = FALSE)
    }
    current <- parent
  }
}

automation_path <- function(root, ...) {
  file.path(root, "automation", ...)
}

automation_source_app_modules <- function(root) {
  options(labweight.app_root = root)
  source_files <- c(
    "utils.R",
    "runtime_config.R",
    "palettes.R",
    "import_csv.R",
    "mapping.R",
    "settings.R",
    "direct_entry.R",
    "transform_weights.R",
    "transform_scores.R",
    "transform_survival.R",
    "validate.R",
    "plots_weights.R",
    "plots_scores.R",
    "plots_survival.R",
    "downloads.R"
  )

  invisible(lapply(source_files, function(file) {
    source(file.path(root, "app", "R", file), chdir = TRUE)
  }))
}

automation_source_modules <- function(root) {
  module_files <- c(
    "logging.R",
    "config.R",
    "source_sharepoint.R",
    "schedule.R",
    "pipeline.R",
    "report_render.R",
    "email_smtp.R"
  )

  invisible(lapply(module_files, function(file) {
    source(file.path(root, "automation", "R", file), chdir = TRUE)
  }))
}

automation_load_context <- function(start = getwd()) {
  root <- automation_find_repo_root(start)
  automation_source_app_modules(root)
  automation_source_modules(root)

  list(
    root = root,
    automation_root = file.path(root, "automation")
  )
}
