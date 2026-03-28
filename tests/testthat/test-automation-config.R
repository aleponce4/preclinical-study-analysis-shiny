test_that("automation config loads valid files", {
  cfg_dir <- tempfile("automation-config-")
  dir.create(cfg_dir, recursive = TRUE)

  writeLines(c(
    "retention_days: 30",
    "admin_email:",
    "  to: [admin@example.org]",
    "  cc: []",
    "studies:",
    "  - study_id: s1",
    "    enabled: true",
    "    source_url: /tmp/study1.xlsx",
    "    source_format: excel",
    "    style_id: default",
    "    schedule_id: daily_0900",
    "    report_title: Study 1",
    "    weights_sheet: 1",
    "    email_to: [a@example.org]",
    "    email_cc: [b@example.org]"
  ), file.path(cfg_dir, "studies.yml"))

  writeLines(c(
    "styles:",
    "  default:",
    "    stats:",
    "      p_adjust_method: bh"
  ), file.path(cfg_dir, "styles.yml"))

  writeLines(c(
    "timezone: America/Chicago",
    "schedules:",
    "  daily_0900:",
    "    type: daily",
    "    time: \"09:00\""
  ), file.path(cfg_dir, "schedules.yml"))

  cfg <- automation_load_config(getOption("labweight.app_root"), cfg_dir)
  expect_equal(length(cfg$studies), 1)
  expect_equal(cfg$studies[[1]]$study_id, "s1")
  expect_equal(cfg$studies[[1]]$style_id, "default")
  expect_equal(cfg$studies[[1]]$source_format, "excel")
  expect_equal(cfg$retention_days, 30)
})

test_that("automation config rejects invalid email", {
  cfg_dir <- tempfile("automation-config-")
  dir.create(cfg_dir, recursive = TRUE)

  writeLines(c(
    "retention_days: 30",
    "admin_email:",
    "  to: [admin@example.org]",
    "  cc: []",
    "studies:",
    "  - study_id: s1",
    "    enabled: true",
    "    source_url: /tmp/study1.xlsx",
    "    source_format: excel",
    "    style_id: default",
    "    schedule_id: daily_0900",
    "    report_title: Study 1",
    "    weights_sheet: 1",
    "    email_to: [not-an-email]",
    "    email_cc: []"
  ), file.path(cfg_dir, "studies.yml"))

  writeLines(c(
    "styles:",
    "  default:",
    "    stats:",
    "      p_adjust_method: bh"
  ), file.path(cfg_dir, "styles.yml"))

  writeLines(c(
    "timezone: America/Chicago",
    "schedules:",
    "  daily_0900:",
    "    type: daily",
    "    time: \"09:00\""
  ), file.path(cfg_dir, "schedules.yml"))

  expect_error(
    automation_load_config(getOption("labweight.app_root"), cfg_dir),
    "Invalid email"
  )
})

test_that("automation config rejects unknown style reference", {
  cfg_dir <- tempfile("automation-config-")
  dir.create(cfg_dir, recursive = TRUE)

  writeLines(c(
    "retention_days: 30",
    "admin_email:",
    "  to: [admin@example.org]",
    "  cc: []",
    "studies:",
    "  - study_id: s1",
    "    enabled: true",
    "    source_url: /tmp/study1.xlsx",
    "    source_format: excel",
    "    style_id: missing_style",
    "    schedule_id: daily_0900",
    "    report_title: Study 1",
    "    weights_sheet: 1",
    "    email_to: [a@example.org]",
    "    email_cc: []"
  ), file.path(cfg_dir, "studies.yml"))

  writeLines(c(
    "styles:",
    "  default:",
    "    stats:",
    "      p_adjust_method: bh"
  ), file.path(cfg_dir, "styles.yml"))

  writeLines(c(
    "timezone: America/Chicago",
    "schedules:",
    "  daily_0900:",
    "    type: daily",
    "    time: \"09:00\""
  ), file.path(cfg_dir, "schedules.yml"))

  expect_error(
    automation_load_config(getOption("labweight.app_root"), cfg_dir),
    "unknown style_id"
  )
})

test_that("automation config rejects invalid source_format", {
  cfg_dir <- tempfile("automation-config-")
  dir.create(cfg_dir, recursive = TRUE)

  writeLines(c(
    "retention_days: 30",
    "admin_email:",
    "  to: [admin@example.org]",
    "  cc: []",
    "studies:",
    "  - study_id: s1",
    "    enabled: true",
    "    source_url: /tmp/study1.xlsx",
    "    source_format: parquet",
    "    style_id: default",
    "    schedule_id: daily_0900",
    "    report_title: Study 1",
    "    weights_sheet: 1",
    "    email_to: [a@example.org]",
    "    email_cc: []"
  ), file.path(cfg_dir, "studies.yml"))

  writeLines(c(
    "styles:",
    "  default:",
    "    stats:",
    "      p_adjust_method: bh"
  ), file.path(cfg_dir, "styles.yml"))

  writeLines(c(
    "timezone: America/Chicago",
    "schedules:",
    "  daily_0900:",
    "    type: daily",
    "    time: \"09:00\""
  ), file.path(cfg_dir, "schedules.yml"))

  expect_error(
    automation_load_config(getOption("labweight.app_root"), cfg_dir),
    "invalid source_format"
  )
})

test_that("automation config loads scheduled_sampling_overrides from YAML row lists", {
  cfg_dir <- tempfile("automation-config-")
  dir.create(cfg_dir, recursive = TRUE)

  writeLines(c(
    "retention_days: 30",
    "admin_email:",
    "  to: [admin@example.org]",
    "  cc: []",
    "studies:",
    "  - study_id: s1",
    "    enabled: true",
    "    source_url: /tmp/study1.xlsx",
    "    source_format: excel",
    "    style_id: default",
    "    schedule_id: daily_0900",
    "    report_title: Study 1",
    "    weights_sheet: 1",
    "    scheduled_sampling_overrides:",
    "      - cage_card: \"449074\"",
    "        role: Scheduled sampling",
    "        censor_day: 6",
    "    email_to: [a@example.org]",
    "    email_cc: []"
  ), file.path(cfg_dir, "studies.yml"))

  writeLines(c(
    "styles:",
    "  default:",
    "    stats:",
    "      p_adjust_method: bh"
  ), file.path(cfg_dir, "styles.yml"))

  writeLines(c(
    "timezone: America/Chicago",
    "schedules:",
    "  daily_0900:",
    "    type: daily",
    "    time: \"09:00\""
  ), file.path(cfg_dir, "schedules.yml"))

  cfg <- automation_load_config(getOption("labweight.app_root"), cfg_dir)
  overrides <- cfg$studies[[1]]$scheduled_sampling_overrides

  expect_equal(overrides$cage_card[[1]], "449074")
  expect_equal(overrides$role[[1]], "scheduled_sampling")
  expect_equal(overrides$censor_day[[1]], 6L)
})

test_that("automation config rejects invalid scheduled_sampling_overrides role", {
  cfg_dir <- tempfile("automation-config-")
  dir.create(cfg_dir, recursive = TRUE)

  writeLines(c(
    "retention_days: 30",
    "admin_email:",
    "  to: [admin@example.org]",
    "  cc: []",
    "studies:",
    "  - study_id: s1",
    "    enabled: true",
    "    source_url: /tmp/study1.xlsx",
    "    source_format: excel",
    "    style_id: default",
    "    schedule_id: daily_0900",
    "    report_title: Study 1",
    "    weights_sheet: 1",
    "    scheduled_sampling_overrides:",
    "      - cage_card: \"449074\"",
    "        role: planned_necropsy",
    "        censor_day: 6",
    "    email_to: [a@example.org]",
    "    email_cc: []"
  ), file.path(cfg_dir, "studies.yml"))

  writeLines(c(
    "styles:",
    "  default:",
    "    stats:",
    "      p_adjust_method: bh"
  ), file.path(cfg_dir, "styles.yml"))

  writeLines(c(
    "timezone: America/Chicago",
    "schedules:",
    "  daily_0900:",
    "    type: daily",
    "    time: \"09:00\""
  ), file.path(cfg_dir, "schedules.yml"))

  expect_error(
    automation_load_config(getOption("labweight.app_root"), cfg_dir),
    "invalid scheduled_sampling_overrides role"
  )
})
