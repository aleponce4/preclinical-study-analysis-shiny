test_that("end-to-end dry-run processes due studies and isolates failures", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc required")
  skip_if_not_installed("tinytex")
  skip_if_not(tinytex::is_tinytex(), "TinyTeX/LaTeX required for PDF render")

  root <- getOption("labweight.app_root")
  cfg_dir <- tempfile("automation-e2e-")
  dir.create(cfg_dir, recursive = TRUE)

  valid_source <- project_path("inst", "templates", "example_weights.csv")
  invalid_source <- file.path(tempdir(), "missing-file.xlsx")

  writeLines(c(
    "retention_days: 30",
    "admin_email:",
    "  to: [admin@example.org]",
    "  cc: []",
    "studies:",
    "  - study_id: valid_study",
    "    enabled: true",
    sprintf("    source_url: %s", valid_source),
    "    style_id: default",
    "    schedule_id: daily_0900",
    "    report_title: Valid Study",
    "    email_to: [valid@example.org]",
    "    email_cc: []",
    "  - study_id: invalid_study",
    "    enabled: true",
    sprintf("    source_url: %s", invalid_source),
    "    style_id: default",
    "    schedule_id: daily_0900",
    "    report_title: Invalid Study",
    "    email_to: [invalid@example.org]",
    "    email_cc: []"
  ), file.path(cfg_dir, "studies.yml"))

  file.copy(file.path(root, "automation", "config", "styles.yml"), file.path(cfg_dir, "styles.yml"), overwrite = TRUE)
  file.copy(file.path(root, "automation", "config", "schedules.yml"), file.path(cfg_dir, "schedules.yml"), overwrite = TRUE)

  existing_reports <- list.files(file.path(root, "automation", "output"), pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)

  out <- system2(
    "Rscript",
    c("--vanilla", "automation/run_reports.R", "--now", "--dry-run-email", "--config-dir", cfg_dir),
    stdout = TRUE,
    stderr = TRUE,
    env = c("LABAPP_EMAIL_FROM=test@example.org"),
    cwd = root
  )

  status <- attr(out, "status") %||% 0L
  expect_equal(status, 1L)

  updated_reports <- list.files(file.path(root, "automation", "output"), pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
  expect_true(length(updated_reports) >= length(existing_reports) + 1L)

  state_path <- file.path(root, "automation", "state", "last_runs.json")
  expect_true(file.exists(state_path))

  state <- jsonlite::fromJSON(state_path)
  expect_true("valid_study" %in% names(state$studies))
  expect_true("invalid_study" %in% names(state$studies))
})
