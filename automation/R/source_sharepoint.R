automation_is_http_url <- function(x) {
  grepl("^https?://", as.character(x), ignore.case = TRUE)
}

automation_is_url_shortcut <- function(path) {
  nzchar(path %||% "") && grepl("\\.url$", path, ignore.case = TRUE)
}

automation_read_url_shortcut <- function(path) {
  if (!file.exists(path) || !automation_is_url_shortcut(path)) {
    return("")
  }

  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  url_line <- lines[grepl("^URL=", lines, ignore.case = TRUE)]
  if (!length(url_line)) {
    return("")
  }

  trimws(sub("^URL=", "", url_line[[1]], ignore.case = TRUE))
}

automation_resolve_source_reference <- function(source) {
  source <- trimws(as.character(source %||% ""))
  if (!nzchar(source)) {
    return(source)
  }

  if (file.exists(source) && automation_is_url_shortcut(source)) {
    target_url <- automation_read_url_shortcut(source)
    if (nzchar(target_url)) {
      return(target_url)
    }
  }

  source
}

automation_is_sharepoint_url <- function(url) {
  grepl("https?://[^/]+\\.(sharepoint|onedrive)\\.", as.character(url), ignore.case = TRUE)
}

automation_file_ext_from_url <- function(url, default = "xlsx") {
  clean <- strsplit(url, "[?#]", perl = TRUE)[[1]][1]
  ext <- tolower(tools::file_ext(clean))
  if (!nzchar(ext)) default else ext
}

automation_source_extension <- function(source, default = "xlsx") {
  resolved <- automation_resolve_source_reference(source)

  if (!automation_is_http_url(resolved) && file.exists(resolved)) {
    ext <- tolower(tools::file_ext(resolved))
    return(if (!nzchar(ext)) default else ext)
  }

  automation_file_ext_from_url(resolved, default = default)
}

automation_detect_source_format <- function(source, default = "excel") {
  ext <- automation_source_extension(source, default = if (identical(default, "csv")) "csv" else "xlsx")
  if (ext %in% c("csv", "txt")) "csv" else "excel"
}

automation_auth_headers_from_env <- function() {
  token <- trimws(Sys.getenv("LABAPP_SOURCE_BEARER_TOKEN", unset = ""))
  auth_header <- trimws(Sys.getenv("LABAPP_SOURCE_AUTH_HEADER", unset = ""))
  cookie <- trimws(Sys.getenv("LABAPP_SOURCE_COOKIE", unset = ""))

  headers <- list()

  if (nzchar(token)) {
    headers$Authorization <- sprintf("Bearer %s", token)
  } else if (nzchar(auth_header)) {
    if (grepl(":", auth_header, fixed = TRUE)) {
      parts <- strsplit(auth_header, ":", fixed = TRUE)[[1]]
      key <- trimws(parts[[1]])
      value <- trimws(paste(parts[-1], collapse = ":"))
      if (nzchar(key) && nzchar(value)) {
        headers[[key]] <- value
      }
    } else {
      headers$Authorization <- auth_header
    }
  }

  if (nzchar(cookie)) {
    headers$Cookie <- cookie
  }

  headers
}

automation_graph_default_token_cache <- function(root = getOption("labweight.app_root", getwd())) {
  file.path(root, "automation", "cache", "ms_graph_token.json")
}

automation_graph_scopes <- function(default = "Files.ReadWrite offline_access openid profile") {
  trimws(Sys.getenv("LABAPP_SOURCE_GRAPH_SCOPES", unset = default))
}

automation_graph_config_from_env <- function(root = getOption("labweight.app_root", getwd())) {
  list(
    client_id = trimws(Sys.getenv("LABAPP_SOURCE_GRAPH_CLIENT_ID", unset = "")),
    tenant = trimws(Sys.getenv("LABAPP_SOURCE_GRAPH_TENANT", unset = "organizations")),
    scopes = automation_graph_scopes(),
    token_cache = trimws(Sys.getenv("LABAPP_SOURCE_GRAPH_TOKEN_CACHE", unset = automation_graph_default_token_cache(root)))
  )
}

automation_graph_enabled <- function(config = automation_graph_config_from_env()) {
  nzchar(config$client_id %||% "")
}

automation_graph_cache_dir <- function(config) {
  cache_path <- trimws(config$token_cache %||% "")
  if (!nzchar(cache_path)) {
    return("")
  }
  dirname(cache_path)
}

automation_graph_read_token_cache <- function(config) {
  cache_path <- trimws(config$token_cache %||% "")
  if (!nzchar(cache_path) || !file.exists(cache_path)) {
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(cache_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

automation_graph_write_token_cache <- function(config, token) {
  cache_dir <- automation_graph_cache_dir(config)
  if (nzchar(cache_dir)) {
    ensure_dir(cache_dir)
  }

  payload <- list(
    access_token = automation_chr(token$access_token, default = ""),
    refresh_token = automation_chr(token$refresh_token, default = ""),
    token_type = automation_chr(token$token_type, default = "Bearer"),
    scope = automation_chr(token$scope, default = config$scopes),
    expires_at = automation_chr(token$expires_at, default = ""),
    obtained_at = automation_chr(token$obtained_at, default = ""),
    tenant = automation_chr(config$tenant, default = "organizations"),
    client_id = automation_chr(config$client_id, default = "")
  )

  writeLines(
    jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    config$token_cache
  )

  invisible(payload)
}

automation_graph_token_expired <- function(token, buffer_seconds = 300L) {
  expires_at <- as.POSIXct(token$expires_at %||% "", tz = "UTC")
  if (is.na(expires_at)) {
    return(TRUE)
  }

  expires_at <= (Sys.time() + as.difftime(buffer_seconds, units = "secs"))
}

automation_graph_token_endpoint <- function(config) {
  sprintf(
    "https://login.microsoftonline.com/%s/oauth2/v2.0/token",
    utils::URLencode(config$tenant %||% "organizations", reserved = TRUE)
  )
}

automation_graph_devicecode_endpoint <- function(config) {
  sprintf(
    "https://login.microsoftonline.com/%s/oauth2/v2.0/devicecode",
    utils::URLencode(config$tenant %||% "organizations", reserved = TRUE)
  )
}

automation_http_form_post <- function(url, fields, headers = list(), expected_status = 200L) {
  form_body <- paste(
    sprintf(
      "%s=%s",
      utils::URLencode(names(fields), reserved = TRUE),
      utils::URLencode(as.character(unlist(fields, use.names = FALSE)), reserved = TRUE)
    ),
    collapse = "&"
  )

  handle <- curl::new_handle(failonerror = FALSE, followlocation = TRUE)
  all_headers <- c(list(
    "Content-Type" = "application/x-www-form-urlencoded"
  ), headers)
  do.call(curl::handle_setheaders, c(list(handle), all_headers))
  curl::handle_setopt(handle, post = TRUE, postfields = form_body)

  response <- curl::curl_fetch_memory(url, handle = handle)
  body <- rawToChar(response$content)
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (!is.null(expected_status) && !response$status_code %in% expected_status) {
    message <- parsed$error_description %||%
      parsed$error$message %||%
      parsed$error %||%
      body %||%
      sprintf("HTTP %s", response$status_code)
    stop(message, call. = FALSE)
  }

  list(
    status_code = response$status_code,
    body = body,
    json = parsed
  )
}

automation_http_json_get <- function(url, headers = list(), expected_status = 200L) {
  handle <- curl::new_handle(failonerror = FALSE, followlocation = TRUE)
  if (length(headers)) {
    do.call(curl::handle_setheaders, c(list(handle), headers))
  }

  response <- curl::curl_fetch_memory(url, handle = handle)
  body <- rawToChar(response$content)
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (!is.null(expected_status) && !response$status_code %in% expected_status) {
    message <- parsed$error$message %||%
      parsed$error_description %||%
      parsed$error %||%
      body %||%
      sprintf("HTTP %s", response$status_code)
    stop(message, call. = FALSE)
  }

  list(
    status_code = response$status_code,
    body = body,
    json = parsed
  )
}

automation_graph_normalize_token <- function(token, config, fallback_refresh_token = "") {
  expires_in <- suppressWarnings(as.integer(token$expires_in %||% 0L))
  if (is.na(expires_in) || expires_in < 60L) {
    expires_in <- 3600L
  }

  list(
    access_token = automation_chr(token$access_token, default = ""),
    refresh_token = automation_chr(token$refresh_token, default = fallback_refresh_token),
    token_type = automation_chr(token$token_type, default = "Bearer"),
    scope = automation_chr(token$scope, default = config$scopes),
    expires_at = format(
      Sys.time() + as.difftime(max(expires_in - 120L, 60L), units = "secs"),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    obtained_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

automation_graph_refresh_token <- function(config, cached_token) {
  refresh_token <- automation_chr(cached_token$refresh_token, default = "")
  if (!nzchar(refresh_token)) {
    return(NULL)
  }

  response <- automation_http_form_post(
    url = automation_graph_token_endpoint(config),
    fields = list(
      client_id = config$client_id,
      grant_type = "refresh_token",
      refresh_token = refresh_token,
      scope = config$scopes
    ),
    expected_status = c(200L, 400L, 401L)
  )

  if (!identical(response$status_code, 200L)) {
    return(NULL)
  }

  token <- automation_graph_normalize_token(
    token = response$json,
    config = config,
    fallback_refresh_token = refresh_token
  )
  automation_graph_write_token_cache(config, token)
  token
}

automation_graph_device_login <- function(config = automation_graph_config_from_env(),
                                          force = FALSE,
                                          quiet = FALSE) {
  if (!automation_graph_enabled(config)) {
    stop(
      "Microsoft Graph login requires LABAPP_SOURCE_GRAPH_CLIENT_ID to be set.",
      call. = FALSE
    )
  }

  cached_token <- automation_graph_read_token_cache(config)
  if (!isTRUE(force) && !is.null(cached_token) && !automation_graph_token_expired(cached_token)) {
    return(cached_token)
  }

  device <- automation_http_form_post(
    url = automation_graph_devicecode_endpoint(config),
    fields = list(
      client_id = config$client_id,
      scope = config$scopes
    )
  )$json

  if (!isTRUE(quiet)) {
    cat(automation_chr(device$message, default = "Open the Microsoft device login page and enter the code shown."), "\n", sep = "")
  }

  interval <- suppressWarnings(as.integer(device$interval %||% 5L))
  if (is.na(interval) || interval < 1L) {
    interval <- 5L
  }

  started_at <- Sys.time()
  expires_in <- suppressWarnings(as.integer(device$expires_in %||% 900L))
  if (is.na(expires_in) || expires_in < 60L) {
    expires_in <- 900L
  }

  repeat {
    if (Sys.time() > started_at + as.difftime(expires_in, units = "secs")) {
      stop("Microsoft device login timed out before authorization completed.", call. = FALSE)
    }

    Sys.sleep(interval)

    response <- automation_http_form_post(
      url = automation_graph_token_endpoint(config),
      fields = list(
        grant_type = "urn:ietf:params:oauth:grant-type:device_code",
        client_id = config$client_id,
        device_code = device$device_code
      ),
      expected_status = c(200L, 400L)
    )

    if (identical(response$status_code, 200L)) {
      token <- automation_graph_normalize_token(response$json, config)
      automation_graph_write_token_cache(config, token)
      return(token)
    }

    error_code <- automation_chr(response$json$error, default = "")
    if (identical(error_code, "authorization_pending")) {
      next
    }
    if (identical(error_code, "slow_down")) {
      interval <- interval + 5L
      next
    }

    error_message <- automation_chr(
      response$json$error_description,
      default = response$body %||% "Microsoft device login failed."
    )
    stop(error_message, call. = FALSE)
  }
}

automation_graph_access_token <- function(config = automation_graph_config_from_env(),
                                          interactive = FALSE) {
  if (!automation_graph_enabled(config)) {
    return("")
  }

  cached_token <- automation_graph_read_token_cache(config)
  if (!is.null(cached_token) && !automation_graph_token_expired(cached_token)) {
    return(automation_chr(cached_token$access_token, default = ""))
  }

  refreshed_token <- if (!is.null(cached_token)) automation_graph_refresh_token(config, cached_token) else NULL
  if (!is.null(refreshed_token) && !automation_graph_token_expired(refreshed_token)) {
    return(automation_chr(refreshed_token$access_token, default = ""))
  }

  if (!isTRUE(interactive)) {
    stop(
      paste(
        "Microsoft Graph token is missing or expired.",
        "Run 'Rscript --vanilla automation/login_microsoft.R' to sign in first."
      ),
      call. = FALSE
    )
  }

  token <- automation_graph_device_login(config = config, force = TRUE, quiet = FALSE)
  automation_chr(token$access_token, default = "")
}

automation_graph_share_id_from_url <- function(url) {
  encoded <- jsonlite::base64_enc(charToRaw(enc2utf8(url)))
  encoded <- gsub("=+$", "", encoded)
  paste0("u!", chartr("+/", "-_", encoded))
}

automation_graph_drive_item_metadata <- function(source_url, config = automation_graph_config_from_env()) {
  token <- automation_graph_access_token(config, interactive = FALSE)
  share_id <- automation_graph_share_id_from_url(source_url)
  endpoint <- sprintf(
    "https://graph.microsoft.com/v1.0/shares/%s/driveItem",
    utils::URLencode(share_id, reserved = TRUE)
  )

  automation_http_json_get(
    url = endpoint,
    headers = list(Authorization = sprintf("Bearer %s", token))
  )$json
}

automation_graph_download_shared_file <- function(source_url, destfile, config = automation_graph_config_from_env()) {
  item <- automation_graph_drive_item_metadata(source_url, config = config)
  download_url <- automation_chr(item[["@microsoft.graph.downloadUrl"]], default = "")

  if (nzchar(download_url)) {
    curl::curl_download(
      download_url,
      destfile = destfile,
      mode = "wb",
      handle = curl::new_handle(failonerror = TRUE, followlocation = TRUE),
      quiet = TRUE
    )
    return(invisible(destfile))
  }

  share_id <- automation_graph_share_id_from_url(source_url)
  token <- automation_graph_access_token(config, interactive = FALSE)
  endpoint <- sprintf(
    "https://graph.microsoft.com/v1.0/shares/%s/driveItem/content",
    utils::URLencode(share_id, reserved = TRUE)
  )
  handle <- curl::new_handle(failonerror = TRUE, followlocation = TRUE)
  curl::handle_setheaders(handle, Authorization = sprintf("Bearer %s", token))

  curl::curl_download(endpoint, destfile = destfile, mode = "wb", handle = handle, quiet = TRUE)
  invisible(destfile)
}

automation_download_workbook <- function(study, ctx) {
  stopifnot(is.list(study), !is.null(study$study_id), !is.null(study$source_url))

  source_url <- automation_resolve_source_reference(study$source_url)
  if (!automation_is_http_url(source_url) && file.exists(source_url)) {
    return(normalizePath(source_url, winslash = "/", mustWork = TRUE))
  }

  download_dir <- ensure_dir(file.path(ctx$dirs$cache, "downloads"))
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")
  extension <- automation_source_extension(study$source_url)
  dest <- file.path(download_dir, sprintf("%s-%s.%s", study$study_id, timestamp, extension))

  if (!automation_is_http_url(source_url)) {
    stop(
      sprintf("Study '%s' source_url is neither an existing local file nor an http(s) URL: %s", study$study_id, source_url),
      call. = FALSE
    )
  }

  graph_config <- automation_graph_config_from_env(ctx$root %||% getOption("labweight.app_root", getwd()))
  if (automation_is_sharepoint_url(source_url) && automation_graph_enabled(graph_config)) {
    tryCatch(
      {
        automation_graph_download_shared_file(source_url, destfile = dest, config = graph_config)
      },
      error = function(e) {
        stop(
          sprintf("Study '%s' Microsoft Graph download failed from '%s': %s", study$study_id, source_url, conditionMessage(e)),
          call. = FALSE
        )
      }
    )
  } else {
  handle <- curl::new_handle(failonerror = TRUE, followlocation = TRUE)

  headers <- automation_auth_headers_from_env()
  if (length(headers)) {
    do.call(curl::handle_setheaders, c(list(handle), headers))
  }

  basic_user <- trimws(Sys.getenv("LABAPP_SOURCE_BASIC_USER", unset = ""))
  basic_pass <- Sys.getenv("LABAPP_SOURCE_BASIC_PASSWORD", unset = "")
  if (nzchar(basic_user)) {
    curl::handle_setopt(handle, username = basic_user, password = basic_pass)
  }

  tryCatch(
    curl::curl_download(source_url, destfile = dest, mode = "wb", handle = handle, quiet = TRUE),
    error = function(e) {
      stop(
        sprintf("Study '%s' download failed from '%s': %s", study$study_id, source_url, conditionMessage(e)),
        call. = FALSE
      )
    }
  )
  }

  if (!file.exists(dest) || file.size(dest) == 0L) {
    stop(
      sprintf("Study '%s' download produced an empty file from '%s'.", study$study_id, source_url),
      call. = FALSE
    )
  }

  dest
}
