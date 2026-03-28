args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)[1]
script_dir <- if (!is.na(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("scripts", winslash = "/", mustWork = TRUE)
}
source(file.path(script_dir, "build_common.R"))

require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf(
        "Missing required package '%s'. Install with install.packages('%s') and retry.",
        pkg,
        pkg
      ),
      call. = FALSE
    )
  }
}

required_env <- function(name) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    stop(sprintf("Missing required environment variable: %s", name), call. = FALSE)
  }
  value
}

set_account_if_supplied <- function(account, server) {
  token <- trimws(Sys.getenv("SHINYAPPS_TOKEN", unset = ""))
  secret <- trimws(Sys.getenv("SHINYAPPS_SECRET", unset = ""))

  if (xor(nzchar(token), nzchar(secret))) {
    stop("Set both SHINYAPPS_TOKEN and SHINYAPPS_SECRET, or neither.", call. = FALSE)
  }

  if (nzchar(token) && nzchar(secret)) {
    rsconnect::setAccountInfo(
      name = account,
      token = token,
      secret = secret,
      server = server
    )
    message("Configured rsconnect account credentials from environment.")
  } else {
    message("Using existing rsconnect account credentials on this machine.")
  }
}

build_env_map <- function() {
  env_map <- c(
    LABAPP_CLOUD_MODE = trimws(Sys.getenv("LABAPP_CLOUD_MODE", unset = "true")),
    LABAPP_AUTH_MODE = trimws(Sys.getenv("LABAPP_AUTH_MODE", unset = "shared_password")),
    LABAPP_STORAGE_MODE = trimws(Sys.getenv("LABAPP_STORAGE_MODE", unset = "session")),
    LABAPP_SHARED_USER = trimws(Sys.getenv("LABAPP_SHARED_USER", unset = "")),
    LABAPP_SHARED_PASSWORD_HASH = trimws(Sys.getenv("LABAPP_SHARED_PASSWORD_HASH", unset = "")),
    LABAPP_SHARED_PASSWORD = trimws(Sys.getenv("LABAPP_SHARED_PASSWORD", unset = ""))
  )

  has_user <- nzchar(env_map[["LABAPP_SHARED_USER"]])
  has_hash <- nzchar(env_map[["LABAPP_SHARED_PASSWORD_HASH"]])
  has_plain <- nzchar(env_map[["LABAPP_SHARED_PASSWORD"]])
  if (!has_user || (!has_hash && !has_plain)) {
    message(
      paste(
        "No LABAPP_SHARED_USER/PASSWORD found; deploying without auth.",
        "Set LABAPP_SHARED_USER and LABAPP_SHARED_PASSWORD_HASH to enable the login screen."
      )
    )
    return(NULL)
  }

  env_map[nzchar(env_map)]
}

require_pkg("rsconnect")
require_pkg("shinymanager")
require_pkg("digest")
require_pkg("rhandsontable")

sync_templates_for_deploy <- function(root, app_dir) {
  templates_src <- file.path(root, "inst", "templates")
  templates_dst <- file.path(app_dir, "inst", "templates")

  if (!dir.exists(templates_src)) {
    stop("Template source directory not found: inst/templates", call. = FALSE)
  }

  dir.create(templates_dst, recursive = TRUE, showWarnings = FALSE)

  template_files <- list.files(templates_src, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  if (!length(template_files)) {
    stop("No template files found in inst/templates.", call. = FALSE)
  }

  copied <- file.copy(template_files, templates_dst, overwrite = TRUE)
  if (any(!copied)) {
    failed <- basename(template_files[!copied])
    stop(
      sprintf("Failed to copy template file(s) into app/inst/templates: %s", paste(failed, collapse = ", ")),
      call. = FALSE
    )
  }

  required_files <- c("example_weights.csv", "example_survival.csv")
  mismatches <- vapply(required_files, function(fname) {
    src <- file.path(templates_src, fname)
    dst <- file.path(templates_dst, fname)

    if (!file.exists(src) || !file.exists(dst)) {
      return(TRUE)
    }

    !isTRUE(unname(tools::md5sum(src) == tools::md5sum(dst)))
  }, logical(1))

  if (any(mismatches)) {
    stop(
      sprintf(
        "Template parity check failed for: %s",
        paste(required_files[mismatches], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  message(
    sprintf(
      "Synced %d template file(s) from inst/templates to app/inst/templates and verified example parity.",
      length(template_files)
    )
  )
}

root <- find_root()
app_dir <- file.path(root, "app")
if (!file.exists(file.path(app_dir, "app.R"))) {
  stop("App entrypoint not found at app/app.R.", call. = FALSE)
}

account <- required_env("SHINYAPPS_ACCOUNT")
app_name <- required_env("SHINYAPPS_APP_NAME")
server <- trimws(Sys.getenv("SHINYAPPS_SERVER", unset = "shinyapps.io"))

set_account_if_supplied(account, server)

env_map <- build_env_map()
sync_templates_for_deploy(root, app_dir)

rsconnect::deployApp(
  appDir = app_dir,
  appName = app_name,
  account = account,
  server = server,
  launch.browser = FALSE,
  forceUpdate = TRUE
)

message("Deployment complete.")

if (!is.null(env_map)) {
  message(
    "\nReview the deployed app environment and confirm these variables are set:\n",
    paste(sprintf("  - %s", names(env_map)), collapse = "\n")
  )
}
