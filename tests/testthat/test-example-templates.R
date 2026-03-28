testthat::test_that("example template files are mirrored into app/inst/templates", {
  # app/inst/templates is created by the build script and is not present in CI.
  testthat::skip_on_ci()

  root_weights <- project_path("inst", "templates", "example_weights.csv")
  app_weights <- project_path("app", "inst", "templates", "example_weights.csv")
  root_survival <- project_path("inst", "templates", "example_survival.csv")
  app_survival <- project_path("app", "inst", "templates", "example_survival.csv")

  testthat::expect_true(file.exists(root_weights))
  testthat::expect_true(file.exists(app_weights))
  testthat::expect_true(file.exists(root_survival))
  testthat::expect_true(file.exists(app_survival))

  testthat::expect_identical(unname(tools::md5sum(root_weights)), unname(tools::md5sum(app_weights)))
  testthat::expect_identical(unname(tools::md5sum(root_survival)), unname(tools::md5sum(app_survival)))
})

testthat::test_that("load_example_import errors clearly when example file is missing", {
  old_dir <- getOption("labweight.app_dir")
  old_root <- getOption("labweight.app_root")
  tmp_root <- tempfile("labweight-missing-root-")
  tmp_app <- tempfile("labweight-missing-app-")
  dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_app, recursive = TRUE, showWarnings = FALSE)

  options(labweight.app_root = tmp_root, labweight.app_dir = tmp_app)
  on.exit({
    options(labweight.app_root = old_root, labweight.app_dir = old_dir)
    unlink(c(tmp_root, tmp_app), recursive = TRUE, force = TRUE)
  }, add = TRUE)

  testthat::expect_error(
    load_example_import("weights"),
    "was not found\\. Checked:"
  )
})

testthat::test_that("load_example_import errors clearly on malformed schema", {
  old_dir <- getOption("labweight.app_dir")
  old_root <- getOption("labweight.app_root")
  tmp_root <- tempfile("labweight-bad-root-")
  tmp_app <- tempfile("labweight-bad-app-")
  tmp_templates <- file.path(tmp_app, "inst", "templates")
  dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_templates, recursive = TRUE, showWarnings = FALSE)

  # Missing required columns for weights example schema.
  writeLines(c("foo,bar", "1,2"), file.path(tmp_templates, "example_weights.csv"))

  options(labweight.app_root = tmp_root, labweight.app_dir = tmp_app)
  on.exit({
    options(labweight.app_root = old_root, labweight.app_dir = old_dir)
    unlink(c(tmp_root, tmp_app), recursive = TRUE, force = TRUE)
  }, add = TRUE)

  testthat::expect_error(
    load_example_import("weights"),
    "failed schema checks"
  )
})

testthat::test_that("hosted-style app/inst example files validate without group conflicts", {
  old_dir <- getOption("labweight.app_dir")
  options(labweight.app_dir = project_path("app"))
  on.exit(options(labweight.app_dir = old_dir), add = TRUE)

  weights_import <- load_example_import("weights")
  survival_import <- load_example_import("survival")

  validation <- validate_study_data(
    raw_weights = weights_import,
    raw_survival = survival_import,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(weights_import, weights_field_spec()),
      survival = guess_field_mapping(survival_import, survival_field_spec()),
      day_map = default_day_mapping(weights_import),
      survival_day_map = default_survival_day_mapping(survival_import),
      score_day_map = default_score_day_mapping(weights_import)
    ),
    infer_survival = FALSE
  )

  conflict_warnings <- grep("group labels that conflict", validation$warnings, value = TRUE)
  testthat::expect_length(conflict_warnings, 0L)
})
