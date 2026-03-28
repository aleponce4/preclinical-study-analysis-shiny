test_that("runner fails for unknown --study-id", {
  root <- getOption("labweight.app_root")
  out <- withr::with_dir(root, {
    system2(
      "Rscript",
      c("--vanilla", "automation/run_reports.R", "--study-id", "does_not_exist", "--dry-run-email"),
      stdout = TRUE,
      stderr = TRUE,
      env = c("LABAPP_EMAIL_FROM=test@example.org")
    )
  })

  status <- attr(out, "status") %||% 0L
  expect_equal(status, 1L)
  expect_true(any(grepl("was not found", out, fixed = TRUE)))
})

test_that("runner fails for disabled --study-id", {
  root <- getOption("labweight.app_root")
  cfg_dir <- tempfile("automation-cli-")
  dir.create(cfg_dir, recursive = TRUE)

  writeLines(c(
    "retention_days: 30",
    "admin_email:",
    "  to: [admin@example.org]",
    "  cc: []",
    "studies:",
    "  - study_id: disabled_study",
    "    enabled: false",
    "    source_url: /tmp/disabled.xlsx",
    "    source_format: excel",
    "    style_id: default",
    "    schedule_id: daily_0900",
    "    report_title: Disabled",
    "    weights_sheet: 1",
    "    email_to: [d@example.org]",
    "    email_cc: []"
  ), file.path(cfg_dir, "studies.yml"))

  file.copy(file.path(root, "automation", "config", "styles.yml"), file.path(cfg_dir, "styles.yml"), overwrite = TRUE)
  file.copy(file.path(root, "automation", "config", "schedules.yml"), file.path(cfg_dir, "schedules.yml"), overwrite = TRUE)

  out <- withr::with_dir(root, {
    system2(
      "Rscript",
      c("--vanilla", "automation/run_reports.R", "--study-id", "disabled_study", "--dry-run-email", "--config-dir", cfg_dir),
      stdout = TRUE,
      stderr = TRUE,
      env = c("LABAPP_EMAIL_FROM=test@example.org")
    )
  })

  status <- attr(out, "status") %||% 0L
  expect_equal(status, 1L)
  expect_true(any(grepl("exists but is disabled", out, fixed = TRUE)))
})
