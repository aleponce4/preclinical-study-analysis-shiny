#!/usr/bin/env Rscript

parse_bootstrap_args <- function(args) {
  out <- list(
    check_only = FALSE,
    skip_renv = FALSE,
    skip_tinytex = FALSE,
    verbose = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (identical(arg, "--check-only")) {
      out$check_only <- TRUE
    } else if (identical(arg, "--skip-renv")) {
      out$skip_renv <- TRUE
    } else if (identical(arg, "--skip-tinytex")) {
      out$skip_tinytex <- TRUE
    } else if (identical(arg, "--verbose")) {
      out$verbose <- TRUE
    } else if (identical(arg, "--help") || identical(arg, "-h")) {
      cat(
        paste(
          "Usage:",
          "Rscript --vanilla automation/bootstrap_reports.R [--check-only] [--skip-renv] [--skip-tinytex] [--verbose]"
        ),
        sep = "\n"
      )
      quit(save = "no", status = 0L)
    } else {
      stop(sprintf("Unknown argument: %s", arg), call. = FALSE)
    }

    i <- i + 1L
  }

  out
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "") || all(is.na(x))) y else x
}

bootstrap_log <- function(..., verbose_only = FALSE) {
  if (verbose_only && !isTRUE(cli$verbose)) {
    return(invisible(NULL))
  }
  cat(sprintf(...), "\n", sep = "")
}

ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

restore_project_packages <- function(project_root) {
  ensure_package("renv")
  bootstrap_log("Restoring project packages from renv.lock ...")
  renv::restore(project = project_root, prompt = FALSE)
}

ensure_tinytex <- function() {
  ensure_package("tinytex")
  if (!tinytex::is_tinytex()) {
    bootstrap_log("Installing TinyTeX ...")
    tinytex::install_tinytex()
  } else {
    bootstrap_log("TinyTeX already installed.", verbose_only = TRUE)
  }
}

collect_status <- function() {
  list(
    pandoc = requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available(),
    tinytex = requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex(),
    xelatex = nzchar(Sys.which("xelatex")),
    pdflatex = nzchar(Sys.which("pdflatex"))
  )
}

print_status <- function(status) {
  bootstrap_log("Dependency status:")
  bootstrap_log("  pandoc available: %s", if (isTRUE(status$pandoc)) "yes" else "no")
  bootstrap_log("  TinyTeX installed: %s", if (isTRUE(status$tinytex)) "yes" else "no")
  bootstrap_log("  xelatex on PATH:   %s", if (isTRUE(status$xelatex)) "yes" else "no")
  bootstrap_log("  pdflatex on PATH:  %s", if (isTRUE(status$pdflatex)) "yes" else "no")
}

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- args_all[grepl("^--file=", args_all)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else file.path(getwd(), "automation", "bootstrap_reports.R")
script_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
cli <- parse_bootstrap_args(commandArgs(trailingOnly = TRUE))

main <- function() {
  if (!isTRUE(cli$check_only) && !isTRUE(cli$skip_renv)) {
    restore_project_packages(project_root)
  }

  if (!isTRUE(cli$check_only) && !isTRUE(cli$skip_tinytex)) {
    ensure_tinytex()
  }

  status <- collect_status()
  print_status(status)

  if (!isTRUE(status$pandoc)) {
    stop("pandoc is not available. Install it with automation/scripts/install_system_deps.sh.", call. = FALSE)
  }

  if (!isTRUE(status$tinytex) && !isTRUE(status$xelatex) && !isTRUE(status$pdflatex)) {
    stop("No LaTeX engine found. Install TinyTeX or system TeX Live before rendering PDFs.", call. = FALSE)
  }

  bootstrap_log("Report bootstrap checks passed.")
}

main()
