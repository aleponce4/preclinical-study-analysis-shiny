test_that("daily schedule computes latest scheduled time and dedupes", {
  schedule <- automation_normalize_schedule(
    list(type = "daily", time = "09:00", timezone = "America/Chicago"),
    schedule_id = "daily_0900",
    timezone_default = "America/Chicago"
  )

  now <- as.POSIXct("2026-03-09 09:05:00", tz = "America/Chicago")
  scheduled_at <- automation_latest_schedule_time(schedule, now = now)
  expect_equal(format(scheduled_at, "%Y-%m-%d %H:%M", tz = "America/Chicago"), "2026-03-09 09:00")

  studies <- list(list(
    study_id = "s1",
    enabled = TRUE,
    schedule_id = "daily_0900",
    style_id = "default",
    source_url = "/tmp/s1.xlsx",
    source_format = "excel",
    report_title = "Study 1",
    weights_sheet = 1,
    email_to = c("x@example.org"),
    email_cc = character()
  ))

  schedules <- list(daily_0900 = schedule)

  due_info <- automation_select_due_studies(
    studies = studies,
    schedules = schedules,
    state = list(studies = list()),
    now = now,
    force_now = FALSE
  )
  expect_equal(length(due_info$due), 1)

  state <- automation_mark_state(list(studies = list()), "s1", scheduled_at, "success", "ok")
  due_after_mark <- automation_select_due_studies(
    studies = studies,
    schedules = schedules,
    state = state,
    now = now,
    force_now = FALSE
  )
  expect_equal(length(due_after_mark$due), 0)
  expect_equal(length(due_after_mark$skipped), 1)
})

test_that("weekdays schedule backs off weekend", {
  schedule <- automation_normalize_schedule(
    list(type = "weekdays", time = "09:00", timezone = "America/Chicago"),
    schedule_id = "weekdays_0900",
    timezone_default = "America/Chicago"
  )

  now <- as.POSIXct("2026-03-08 10:00:00", tz = "America/Chicago") # Sunday
  scheduled_at <- automation_latest_schedule_time(schedule, now = now)

  expect_equal(lubridate::wday(scheduled_at, week_start = 1), 5) # Friday
  expect_equal(format(scheduled_at, "%H:%M", tz = "America/Chicago"), "09:00")
})

test_that("study_filter returns metadata for unknown and disabled study handling", {
  schedule <- automation_normalize_schedule(
    list(type = "daily", time = "09:00", timezone = "America/Chicago"),
    schedule_id = "daily_0900",
    timezone_default = "America/Chicago"
  )
  schedules <- list(daily_0900 = schedule)
  now <- as.POSIXct("2026-03-09 09:05:00", tz = "America/Chicago")

  studies <- list(
    list(
      study_id = "enabled_s",
      enabled = TRUE,
      schedule_id = "daily_0900",
      style_id = "default",
      source_url = "/tmp/e.xlsx",
      source_format = "excel",
      report_title = "Enabled",
      weights_sheet = 1,
      email_to = c("x@example.org"),
      email_cc = character()
    ),
    list(
      study_id = "disabled_s",
      enabled = FALSE,
      schedule_id = "daily_0900",
      style_id = "default",
      source_url = "/tmp/d.xlsx",
      source_format = "excel",
      report_title = "Disabled",
      weights_sheet = 1,
      email_to = c("x@example.org"),
      email_cc = character()
    )
  )

  unknown <- automation_select_due_studies(
    studies = studies,
    schedules = schedules,
    state = list(studies = list()),
    now = now,
    study_filter = "missing"
  )
  expect_equal(unknown$filter_info$match_count, 0)
  expect_equal(unknown$filter_info$enabled_match_count, 0)

  disabled <- automation_select_due_studies(
    studies = studies,
    schedules = schedules,
    state = list(studies = list()),
    now = now,
    study_filter = "disabled_s"
  )
  expect_equal(disabled$filter_info$match_count, 1)
  expect_equal(disabled$filter_info$enabled_match_count, 0)
})

test_that("weekday parsing returns NA for malformed values instead of crashing", {
  expect_true(is.na(automation_parse_weekday("")))
  expect_true(is.na(automation_parse_weekday(NA)))
  expect_true(is.na(automation_parse_weekday(1)))
  expect_true(is.na(automation_parse_weekday("1")))
  expect_equal(automation_parse_weekday("Mon"), 1L)
})

test_that("schedule stamps are canonical UTC values", {
  t <- as.POSIXct("2026-03-09 09:05:00", tz = "America/Chicago")
  stamp <- automation_schedule_stamp(t)

  expect_equal(stamp, "2026-03-09T14:05:00Z")
  expect_match(stamp, "Z$")
})
