automation_read_yaml <- function(path, default = list()) {
  if (!file.exists(path)) {
    stop(sprintf("Config file not found: %s", path), call. = FALSE)
  }

  out <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      stop(sprintf("Failed to parse YAML '%s': %s", path, conditionMessage(e)), call. = FALSE)
    }
  )

  if (is.null(out)) {
    default
  } else {
    out
  }
}

automation_chr <- function(x, default = "") {
  if (is.null(x) || !length(x) || all(is.na(x))) {
    return(default)
  }
  as.character(x[[1]])
}

automation_chr_vec <- function(x) {
  if (is.null(x) || !length(x)) {
    return(character())
  }
  unique(trimws(as.character(unlist(x, use.names = FALSE))))
}

automation_bool <- function(x, default = FALSE) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) {
    return(isTRUE(default))
  }

  raw <- tolower(trimws(as.character(x[[1]])))
  raw %in% c("1", "true", "t", "yes", "y", "on")
}

automation_normalize_scheduled_sampling_overrides <- function(x, study_id) {
  if (is.null(x) || !length(x)) {
    return(empty_scheduled_sampling_cages())
  }

  raw_overrides <- coerce_scheduled_sampling_cages_input(x)
  if (!nrow(raw_overrides)) {
    return(empty_scheduled_sampling_cages())
  }

  if (!"cage_card" %in% names(raw_overrides)) {
    return(empty_scheduled_sampling_cages())
  }

  if (!"role" %in% names(raw_overrides)) {
    raw_overrides$role <- "survival"
  }

  invalid_role <- raw_overrides |>
    dplyr::mutate(
      normalized_role = vapply(.data$role, scheduled_sampling_role_value, character(1), strict = TRUE)
    ) |>
    dplyr::filter(is.na(.data$normalized_role), nzchar(trimws(as.character(.data$role))))

  if (nrow(invalid_role)) {
    stop(
      sprintf(
        "Study '%s' has invalid scheduled_sampling_overrides role value(s): %s",
        study_id,
        paste(unique(trimws(as.character(invalid_role$role))), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  overrides <- normalize_scheduled_sampling_cages(raw_overrides)
  if (!nrow(overrides)) {
    return(empty_scheduled_sampling_cages())
  }

  missing_censor <- overrides |>
    dplyr::filter(.data$role == "scheduled_sampling", is.na(.data$censor_day))

  if (nrow(missing_censor)) {
    stop(
      sprintf(
        "Study '%s' has scheduled_sampling_overrides entries missing censor_day for cage(s): %s",
        study_id,
        paste(missing_censor$cage_card, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  overrides
}

automation_validate_email_list <- function(values, field_name) {
  values <- automation_chr_vec(values)
  if (!length(values)) {
    return(values)
  }

  pattern <- "^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$"
  invalid <- values[!grepl(pattern, values, perl = TRUE)]
  if (length(invalid)) {
    stop(
      sprintf("Invalid email(s) in %s: %s", field_name, paste(invalid, collapse = ", ")),
      call. = FALSE
    )
  }

  values
}

automation_normalize_style <- function(style) {
  style <- style %||% list()

  list(
    shared_style = normalize_shared_style_settings(style$shared_style %||% list()),
    stats = normalize_stats_settings(style$stats %||% list()),
    weight_plot = normalize_weight_plot_settings(style$weight_plot %||% list()),
    score_plot = normalize_score_plot_settings(style$score_plot %||% list()),
    survival_plot = normalize_survival_plot_settings(style$survival_plot %||% list())
  )
}

automation_load_styles <- function(path) {
  raw <- automation_read_yaml(path)
  styles_raw <- raw$styles %||% list()

  if (!length(styles_raw)) {
    stop("styles.yml must define at least one style under 'styles:'.", call. = FALSE)
  }

  styles <- lapply(styles_raw, automation_normalize_style)
  names(styles) <- names(styles_raw)

  if (!"default" %in% names(styles)) {
    stop("styles.yml must contain a 'default' style.", call. = FALSE)
  }

  styles
}

automation_normalize_schedule <- function(schedule, schedule_id, timezone_default = "America/Chicago") {
  schedule <- schedule %||% list()
  type <- tolower(trimws(automation_chr(schedule$type, default = "daily")))
  if (!type %in% c("daily", "weekdays", "weekly", "weekly_days")) {
    stop(sprintf("Schedule '%s' has unsupported type '%s'.", schedule_id, type), call. = FALSE)
  }

  time_str <- trimws(automation_chr(schedule$time, default = "09:00"))
  if (!grepl("^[0-2][0-9]:[0-5][0-9]$", time_str)) {
    stop(sprintf("Schedule '%s' has invalid time '%s' (expected HH:MM).", schedule_id, time_str), call. = FALSE)
  }

  hour <- as.integer(substr(time_str, 1, 2))
  minute <- as.integer(substr(time_str, 4, 5))
  if (is.na(hour) || is.na(minute) || hour > 23L) {
    stop(sprintf("Schedule '%s' has invalid time '%s'.", schedule_id, time_str), call. = FALSE)
  }

  timezone <- automation_chr(schedule$timezone, default = timezone_default)

  weekdays <- tolower(automation_chr_vec(schedule$weekdays %||% schedule$days))
  weekdays <- unique(gsub("[^a-z]", "", weekdays))

  weekday <- tolower(gsub("[^a-z]", "", automation_chr(schedule$weekday, default = "monday")))

  if (identical(type, "weekly") && !nzchar(weekday)) {
    stop(sprintf("Schedule '%s' requires 'weekday' for weekly schedules.", schedule_id), call. = FALSE)
  }

  if (identical(type, "weekly_days") && !length(weekdays)) {
    stop(sprintf("Schedule '%s' requires at least one weekday in 'weekdays'.", schedule_id), call. = FALSE)
  }

  list(
    id = schedule_id,
    type = type,
    time = time_str,
    hour = hour,
    minute = minute,
    timezone = timezone,
    weekday = weekday,
    weekdays = weekdays
  )
}

automation_load_schedules <- function(path) {
  raw <- automation_read_yaml(path)
  timezone <- automation_chr(raw$timezone, default = "America/Chicago")

  schedules_raw <- raw$schedules %||% list()
  if (!length(schedules_raw)) {
    stop("schedules.yml must define at least one schedule under 'schedules:'.", call. = FALSE)
  }

  schedules <- lapply(names(schedules_raw), function(id) {
    automation_normalize_schedule(schedules_raw[[id]], schedule_id = id, timezone_default = timezone)
  })
  names(schedules) <- names(schedules_raw)

  list(timezone = timezone, schedules = schedules)
}

automation_normalize_study <- function(study, index = 1L) {
  study <- study %||% list()
  study_id <- trimws(automation_chr(study$study_id, default = sprintf("study_%d", index)))

  if (!nzchar(study_id)) {
    stop(sprintf("Study entry %d is missing 'study_id'.", index), call. = FALSE)
  }

  source_url <- trimws(automation_chr(study$source_url, default = ""))
  if (!nzchar(source_url)) {
    stop(sprintf("Study '%s' is missing 'source_url'.", study_id), call. = FALSE)
  }

  source_format <- tolower(trimws(automation_chr(study$source_format, default = "excel")))
  if (!source_format %in% c("excel", "csv")) {
    stop(
      sprintf("Study '%s' has invalid source_format '%s'. Expected one of: excel, csv.", study_id, source_format),
      call. = FALSE
    )
  }

  list(
    study_id = study_id,
    enabled = automation_bool(study$enabled, default = TRUE),
    source_url = source_url,
    source_format = source_format,
    style_id = trimws(automation_chr(study$style_id, default = "default")),
    schedule_id = trimws(automation_chr(study$schedule_id, default = "daily_0900")),
    report_title = trimws(automation_chr(study$report_title, default = study_id)),
    weights_sheet = if (is.null(study$weights_sheet)) 1 else study$weights_sheet,
    scheduled_sampling_overrides = automation_normalize_scheduled_sampling_overrides(
      study$scheduled_sampling_overrides,
      study_id = study_id
    ),
    email_to = automation_validate_email_list(study$email_to, sprintf("study '%s' email_to", study_id)),
    email_cc = automation_validate_email_list(study$email_cc, sprintf("study '%s' email_cc", study_id))
  )
}

automation_load_studies <- function(path) {
  raw <- automation_read_yaml(path)
  retention_days <- suppressWarnings(as.integer(raw$retention_days %||% 30L))
  if (is.na(retention_days) || retention_days < 1L) {
    retention_days <- 30L
  }

  admin_to <- automation_validate_email_list(raw$admin_email$to %||% character(), "admin_email.to")
  admin_cc <- automation_validate_email_list(raw$admin_email$cc %||% character(), "admin_email.cc")

  studies_raw <- raw$studies %||% list()
  if (!length(studies_raw)) {
    stop("studies.yml must define at least one study under 'studies:'.", call. = FALSE)
  }

  studies <- lapply(seq_along(studies_raw), function(i) automation_normalize_study(studies_raw[[i]], i))

  study_ids <- vapply(studies, function(x) x$study_id, character(1))
  if (anyDuplicated(study_ids)) {
    dups <- unique(study_ids[duplicated(study_ids)])
    stop(sprintf("Duplicate study_id values in studies.yml: %s", paste(dups, collapse = ", ")), call. = FALSE)
  }

  list(
    retention_days = retention_days,
    admin_email = list(to = admin_to, cc = admin_cc),
    studies = studies
  )
}

automation_validate_study_refs <- function(studies, styles, schedules) {
  style_ids <- names(styles)
  schedule_ids <- names(schedules)

  for (study in studies) {
    if (!study$style_id %in% style_ids) {
      stop(
        sprintf(
          "Study '%s' references unknown style_id '%s'. Known styles: %s",
          study$study_id,
          study$style_id,
          paste(style_ids, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (!study$schedule_id %in% schedule_ids) {
      stop(
        sprintf(
          "Study '%s' references unknown schedule_id '%s'. Known schedules: %s",
          study$study_id,
          study$schedule_id,
          paste(schedule_ids, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (!length(study$email_to)) {
      stop(sprintf("Study '%s' must include at least one email recipient in email_to.", study$study_id), call. = FALSE)
    }
  }

  invisible(TRUE)
}

automation_load_config <- function(root, config_dir = automation_path(root, "config")) {
  studies_cfg <- automation_load_studies(file.path(config_dir, "studies.yml"))
  styles_cfg <- automation_load_styles(file.path(config_dir, "styles.yml"))
  schedules_cfg <- automation_load_schedules(file.path(config_dir, "schedules.yml"))

  automation_validate_study_refs(
    studies = studies_cfg$studies,
    styles = styles_cfg,
    schedules = schedules_cfg$schedules
  )

  list(
    config_dir = config_dir,
    timezone = schedules_cfg$timezone,
    retention_days = studies_cfg$retention_days,
    admin_email = studies_cfg$admin_email,
    studies = studies_cfg$studies,
    styles = styles_cfg,
    schedules = schedules_cfg$schedules
  )
}
