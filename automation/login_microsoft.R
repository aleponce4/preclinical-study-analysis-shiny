#!/usr/bin/env Rscript

parse_login_args <- function(args) {
  out <- list(
    force = FALSE,
    verbose = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (identical(arg, "--force")) {
      out$force <- TRUE
    } else if (identical(arg, "--verbose")) {
      out$verbose <- TRUE
    } else if (identical(arg, "--help") || identical(arg, "-h")) {
      cat(
        paste(
          "Usage:",
          "Rscript --vanilla automation/login_microsoft.R [--force] [--verbose]"
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

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- args_all[grepl("^--file=", args_all)]
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[[1]])
} else {
  file.path(getwd(), "automation", "login_microsoft.R")
}

script_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
repo_root_guess <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
source(file.path(repo_root_guess, "automation", "R", "bootstrap.R"), chdir = TRUE)

main <- function() {
  cli <- parse_login_args(commandArgs(trailingOnly = TRUE))
  ctx_info <- automation_load_context(repo_root_guess)
  root <- ctx_info$root
  config <- automation_graph_config_from_env(root)

  if (!automation_graph_enabled(config)) {
    stop(
      paste(
        "Microsoft login is not configured.",
        "Set LABAPP_SOURCE_GRAPH_CLIENT_ID first."
      ),
      call. = FALSE
    )
  }

  token <- automation_graph_device_login(
    config = config,
    force = cli$force,
    quiet = FALSE
  )

  if (isTRUE(cli$verbose)) {
    cat(sprintf("Tenant: %s\n", config$tenant %||% "organizations"))
    cat(sprintf("Scopes: %s\n", config$scopes %||% ""))
  }

  cat(sprintf("Token cache updated: %s\n", config$token_cache))
  cat(sprintf("Token expires at: %s\n", token$expires_at %||% "<unknown>"))
}

main()
