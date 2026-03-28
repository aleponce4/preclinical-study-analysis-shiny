args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)[1]
script_dir <- if (!is.na(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("scripts", winslash = "/", mustWork = TRUE)
}
source(file.path(script_dir, "build_common.R"))

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Missing required package 'rsconnect'. Install with install.packages('rsconnect').", call. = FALSE)
}

required_env <- function(name) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    stop(sprintf("Missing required environment variable: %s", name), call. = FALSE)
  }
  value
}

mask_secret <- function(x) {
  if (!nzchar(x %||% "")) {
    return("")
  }
  if (nchar(x) <= 6) {
    return(paste0(substr(x, 1, 1), "***"))
  }
  paste0(substr(x, 1, 2), strrep("*", max(0, nchar(x) - 4)), substr(x, nchar(x) - 1, nchar(x)))
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

configure_env <- function(account, app_name, server, env_map) {
  cfg <- get("configureApp", envir = asNamespace("rsconnect"))
  cfg_formals <- names(formals(cfg))
  args <- list(appName = app_name, account = account, server = server)
  if ("envVars" %in% cfg_formals) {
    args$envVars <- env_map
  } else if ("vars" %in% cfg_formals) {
    args$vars <- env_map
  } else {
    stop("rsconnect::configureApp does not expose envVars/vars arguments in this version.", call. = FALSE)
  }

  do.call(cfg, args)
}

account <- required_env("SHINYAPPS_ACCOUNT")
app_name <- required_env("SHINYAPPS_APP_NAME")
server <- trimws(Sys.getenv("SHINYAPPS_SERVER", unset = "shinyapps.io"))

shared_user <- required_env("LABAPP_SHARED_USER")
shared_hash <- trimws(Sys.getenv("LABAPP_SHARED_PASSWORD_HASH", unset = ""))
shared_plain <- trimws(Sys.getenv("LABAPP_SHARED_PASSWORD", unset = ""))
if (!nzchar(shared_hash) && !nzchar(shared_plain)) {
  stop("Set LABAPP_SHARED_PASSWORD_HASH (preferred) or LABAPP_SHARED_PASSWORD.", call. = FALSE)
}

env_map <- c(
  LABAPP_CLOUD_MODE = trimws(Sys.getenv("LABAPP_CLOUD_MODE", unset = "true")),
  LABAPP_AUTH_MODE = trimws(Sys.getenv("LABAPP_AUTH_MODE", unset = "shared_password")),
  LABAPP_STORAGE_MODE = trimws(Sys.getenv("LABAPP_STORAGE_MODE", unset = "session")),
  LABAPP_SHARED_USER = shared_user,
  LABAPP_SHARED_PASSWORD_HASH = shared_hash,
  LABAPP_SHARED_PASSWORD = shared_plain
)
env_map <- env_map[nzchar(env_map)]

set_account_if_supplied(account, server)
configure_env(account, app_name, server, env_map)

message("Updated shinyapps.io app environment variables:")
print(stats::setNames(
  vapply(names(env_map), function(key) {
    if (grepl("PASSWORD", key)) mask_secret(env_map[[key]]) else env_map[[key]]
  }, character(1)),
  names(env_map)
))
