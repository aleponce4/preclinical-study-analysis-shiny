test_that("report rendering produces a PDF when toolchain is available", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is required for report rendering")
  skip_if_not_installed("tinytex")
  skip_if_not(tinytex::is_tinytex(), "TinyTeX/LaTeX is required for PDF rendering")

  root <- getOption("labweight.app_root")
  style <- automation_load_styles(file.path(root, "automation", "config", "styles.yml"))$default

  study <- list(
    study_id = "example_render",
    enabled = TRUE,
    source_url = project_path("inst", "templates", "example_weights.csv"),
    source_format = "csv",
    style_id = "default",
    schedule_id = "daily_0900",
    report_title = "Example Render",
    weights_sheet = 1,
    email_to = c("user@example.org"),
    email_cc = character()
  )

  analysis <- automation_prepare_study_analysis(
    study = study,
    style = style,
    workbook_path = study$source_url,
    generated_at = Sys.time()
  )

  report_path <- automation_render_study_report(
    root = root,
    analysis = analysis,
    scheduled_at = Sys.time(),
    verbose = FALSE
  )

  expect_true(file.exists(report_path))
  expect_match(report_path, "\\.pdf$")
})
