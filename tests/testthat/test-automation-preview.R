test_that("preview renderer creates an html preview from example data", {
  root <- getOption("labweight.app_root")
  style <- automation_load_styles(file.path(root, "automation", "config", "styles.yml"))$default

  study <- list(
    study_id = "preview_example",
    enabled = TRUE,
    source_url = project_path("inst", "templates", "example_weights.csv"),
    source_format = "csv",
    style_id = "default",
    schedule_id = "daily_0900",
    report_title = "Preview Example",
    weights_sheet = 1,
    email_to = c("preview@example.org"),
    email_cc = character()
  )

  analysis <- automation_prepare_study_analysis(
    study = study,
    style = style,
    workbook_path = study$source_url,
    generated_at = Sys.time()
  )

  out_path <- automation_render_preview_report(
    root = root,
    analysis = analysis,
    scheduled_at = Sys.time(),
    verbose = FALSE
  )

  expect_true(file.exists(out_path))
  expect_match(out_path, "\\.html$")

  html <- paste(readLines(out_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("Study Monitoring Report", html, fixed = TRUE))
  expect_true(grepl("example_weights.csv", html, fixed = TRUE))
  expect_true(grepl("Weights", html, fixed = TRUE))
  expect_true(grepl("Survival", html, fixed = TRUE))
  expect_false(grepl("Weight Summary by DPI", html, fixed = TRUE))
  expect_false(grepl(study$source_url, html, fixed = TRUE))
})
