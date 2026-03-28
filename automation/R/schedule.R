automation_weekday_lookup <- function() {
  c(
    monday = 1L,
    mon = 1L,
    tuesday = 2L,
    tue = 2L,
    tues = 2L,
    wednesday = 3L,
    wed = 3L,
    thursday = 4L,
    thu = 4L,
    thurs = 4L,
    friday = 5L,
    fri = 5L,
    saturday = 6L,
    sat = 6L,
    sunday = 7L,
    sun = 7L
  )
}

automation_parse_weekday <- function(x) {
  key <- gsub("[^a-z]", "", tolower(as.character(x %||% "")))
  if (!nzchar(key)) {
    return(NA_integer_)
  }
  idx <- automation_weekday_lookup()[key]
  if (!length(idx) || is.na(idx)) NA_integer_ else as.integer(unname(idx[[1]]))
}

automation_schedule_stamp <- function(time) {
  format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

automation_time_for_date <- function(date_value, hour, minute, timezone) {
  as.POSIXct(
    sprintf("%s %02d:%02d:00", format(as.Date(date_value), "%Y-%m-%d"), as.integer(hour), as.integer(minute)),
    tz = timezone
  )
}

automation_is_weekday <- function(time_value) {
  wd <- lubridate::wday(as.Date(time_value), week_start = 1)
  wd >= 1L && wd <= 5L
}

automation_previous_date_matching <- function(start_date, predicate) {
  candidate <- as.Date(start_date)
  for (i in seq_len(14L)) {
    if (isTRUE(predicate(candidate))) {
      return(candidate)
    }
    candidate <- candidate - 1
  }
  as.Date(start_date)
}

automation_latest_schedule_time <- function(schedule, now = Sys.time()) {
  tz <- schedule$timezone
  now_tz <- lubridate::with_tz(as.POSIXct(now, tz = "UTC"), tzone = tz)

  candidate_for_date <- function(d) {
    automation_time_for_date(d, schedule$hour, schedule$minute, tz)
  }

  type <- schedule$type

  if (identical(type, "daily")) {
    candidate <- candidate_for_date(as.Date(now_tz))
    if (candidate > now_tz) candidate <- candidate_for_date(as.Date(now_tz) - 1)
    return(candidate)
  }

  if (identical(type, "weekdays")) {
    today_candidate <- candidate_for_date(as.Date(now_tz))
    base_date <- if (today_candidate <= now_tz) as.Date(now_tz) else as.Date(now_tz) - 1
    d <- automation_previous_date_matching(base_date, function(dd) {
      automation_is_weekday(dd)
    })
    return(candidate_for_date(d))
  }

  if (identical(type, "weekly")) {
    target_wd <- automation_parse_weekday(schedule$weekday)
    if (is.na(target_wd)) {
      stop(sprintf("Invalid schedule weekday '%s' for schedule '%s'.", schedule$weekday, schedule$id), call. = FALSE)
    }

    today <- as.Date(now_tz)
    today_wd <- lubridate::wday(now_tz, week_start = 1)
    diff_days <- today_wd - target_wd
    candidate_date <- today - diff_days
    candidate <- candidate_for_date(candidate_date)
    if (candidate > now_tz) {
      candidate <- candidate_for_date(candidate_date - 7)
    }
    return(candidate)
  }

  if (identical(type, "weekly_days")) {
    targets <- unique(stats::na.omit(vapply(schedule$weekdays, automation_parse_weekday, integer(1))))
    if (!length(targets)) {
      stop(sprintf("Schedule '%s' has no valid weekdays.", schedule$id), call. = FALSE)
    }

    today_candidate <- candidate_for_date(as.Date(now_tz))
    base_date <- if (today_candidate <= now_tz) as.Date(now_tz) else as.Date(now_tz) - 1

    d <- automation_previous_date_matching(base_date, function(dd) {
      wd <- lubridate::wday(as.Date(dd), week_start = 1)
      wd %in% targets
    })

    return(candidate_for_date(d))
  }

  stop(sprintf("Unsupported schedule type '%s'", type), call. = FALSE)
}

automation_state_path <- function(root) {
  automation_path(root, "state", "last_runs.json")
}

automation_load_state <- function(root) {
  path <- automation_state_path(root)
  if (!file.exists(path)) {
    return(list(studies = list()))
  }

  out <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) list(studies = list())
  )

  if (is.null(out$studies) || !is.list(out$studies)) {
    out$studies <- list()
  }

  out
}

automation_save_state <- function(root, state) {
  ensure_dir(dirname(automation_state_path(root)))
  jsonlite::write_json(state %||% list(studies = list()), automation_state_path(root), pretty = TRUE, auto_unbox = TRUE)
  invisible(state)
}

automation_mark_state <- function(state, study_id, scheduled_at, status, message = "") {
  studies <- state$studies %||% list()

  studies[[study_id]] <- list(
    last_scheduled_at = automation_schedule_stamp(scheduled_at),
    last_status = status,
    last_message = as.character(message %||% ""),
    last_run_at = automation_schedule_stamp(Sys.time())
  )

  state$studies <- studies
  state
}

automation_select_due_studies <- function(studies,
                                          schedules,
                                          state,
                                          now = Sys.time(),
                                          force_now = FALSE,
                                          study_filter = NULL) {
  selected <- studies
  filter_info <- list(applied = FALSE, match_count = 0L, enabled_match_count = 0L)
  if (!is.null(study_filter) && nzchar(study_filter)) {
    filter_info$applied <- TRUE
    matched <- Filter(function(x) identical(x$study_id, study_filter), selected)
    filter_info$match_count <- length(matched)
    filter_info$enabled_match_count <- length(Filter(function(x) isTRUE(x$enabled), matched))
    selected <- matched
  }

  selected <- Filter(function(x) isTRUE(x$enabled), selected)

  if (!length(selected)) {
    return(list(due = list(), skipped = list(), filter_info = filter_info))
  }

  if (isTRUE(force_now)) {
    due <- lapply(selected, function(study) {
      list(
        study = study,
        scheduled_at = as.POSIXct(now, tz = "UTC"),
        reason = "forced"
      )
    })
    return(list(due = due, skipped = list(), filter_info = filter_info))
  }

  due <- list()
  skipped <- list()

  for (study in selected) {
    schedule <- schedules[[study$schedule_id]]
    if (is.null(schedule)) {
      stop(sprintf("Missing schedule '%s' for study '%s'.", study$schedule_id, study$study_id), call. = FALSE)
    }

    scheduled_at <- automation_latest_schedule_time(schedule, now = now)
    stamp <- automation_schedule_stamp(scheduled_at)
    last_stamp <- state$studies[[study$study_id]]$last_scheduled_at %||% ""

    if (!identical(stamp, last_stamp)) {
      due[[length(due) + 1L]] <- list(
        study = study,
        scheduled_at = scheduled_at,
        reason = "scheduled"
      )
    } else {
      skipped[[length(skipped) + 1L]] <- list(
        study_id = study$study_id,
        reason = sprintf("already processed scheduled window %s", stamp)
      )
    }
  }

  list(due = due, skipped = skipped, filter_info = filter_info)
}
