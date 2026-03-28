env_value <- function(name, default = "") {
  value <- Sys.getenv(name, unset = default)
  if (is.null(value) || !length(value)) {
    return(default)
  }
  value[[1]]
}

env_flag <- function(name, default = FALSE) {
  raw <- tolower(trimws(env_value(name, if (isTRUE(default)) "true" else "false")))
  raw %in% c("1", "true", "t", "yes", "y", "on")
}

normalize_auth_mode <- function(value = "none") {
  mode <- tolower(trimws(as.character(value %||% "none")))
  if (!mode %in% c("none", "shared_password")) {
    "none"
  } else {
    mode
  }
}

normalize_storage_mode <- function(value = "local_disk", cloud_mode = FALSE) {
  candidate <- tolower(trimws(as.character(value %||% "")))
  if (!nzchar(candidate)) {
    return(if (isTRUE(cloud_mode)) "session" else "local_disk")
  }

  if (!candidate %in% c("session", "local_disk")) {
    return(if (isTRUE(cloud_mode)) "session" else "local_disk")
  }

  candidate
}

get_runtime_config <- function() {
  cloud_mode <- env_flag("LABAPP_CLOUD_MODE", default = FALSE)

  list(
    cloud_mode = cloud_mode,
    auth_mode = normalize_auth_mode(env_value("LABAPP_AUTH_MODE", "none")),
    shared_user = trimws(env_value("LABAPP_SHARED_USER", "")),
    shared_password = env_value("LABAPP_SHARED_PASSWORD", ""),
    shared_password_hash = tolower(trimws(env_value("LABAPP_SHARED_PASSWORD_HASH", ""))),
    storage_mode = normalize_storage_mode(
      env_value("LABAPP_STORAGE_MODE", ""),
      cloud_mode = cloud_mode
    )
  )
}

runtime_auth_enabled <- function(config = get_runtime_config()) {
  identical(config$auth_mode, "shared_password")
}

runtime_session_storage <- function(config = get_runtime_config()) {
  identical(config$storage_mode, "session")
}

validate_runtime_config <- function(config = get_runtime_config()) {
  if (!runtime_auth_enabled(config)) {
    return(invisible(TRUE))
  }

  if (!nzchar(config$shared_user %||% "")) {
    stop("LABAPP_SHARED_USER is required when LABAPP_AUTH_MODE=shared_password.", call. = FALSE)
  }

  has_hash <- nzchar(config$shared_password_hash %||% "")
  has_plain <- nzchar(config$shared_password %||% "")
  if (!has_hash && !has_plain) {
    stop(
      paste(
        "Set LABAPP_SHARED_PASSWORD_HASH (preferred) or LABAPP_SHARED_PASSWORD",
        "when LABAPP_AUTH_MODE=shared_password."
      ),
      call. = FALSE
    )
  }

  if (has_hash && !requireNamespace("digest", quietly = TRUE)) {
    stop("digest package is required to verify LABAPP_SHARED_PASSWORD_HASH.", call. = FALSE)
  }

  invisible(TRUE)
}

make_shared_password_checker <- function(config = get_runtime_config()) {
  validate_runtime_config(config)

  expected_user <- config$shared_user
  expected_hash <- config$shared_password_hash %||% ""
  expected_plain <- config$shared_password %||% ""

  function(user, password) {
    user <- as.character(user %||% "")
    password <- as.character(password %||% "")
    user_ok <- identical(user, expected_user)

    password_ok <- FALSE
    if (nzchar(expected_hash)) {
      password_ok <- identical(
        tolower(digest::digest(password, algo = "sha256", serialize = FALSE)),
        expected_hash
      )
    } else if (nzchar(expected_plain)) {
      password_ok <- identical(password, expected_plain)
    }

    if (isTRUE(user_ok) && isTRUE(password_ok)) {
      list(result = TRUE, user_info = list(user = user))
    } else {
      list(result = FALSE)
    }
  }
}
