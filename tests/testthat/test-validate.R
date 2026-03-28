testthat::test_that("duplicate animal ids in weights block plotting", {
  weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  weights_import$data$animal_id[[2]] <- weights_import$data$animal_id[[1]]

  validation <- validate_study_data(
    raw_weights = weights_import,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(weights_import, weights_field_spec()),
      survival = guess_field_mapping(NULL, survival_field_spec()),
      day_map = default_day_mapping(weights_import)
    )
  )

  testthat::expect_true(any(grepl("Duplicate animal_id", validation$hard_errors)))
})

testthat::test_that("weights-only validation infers survival without requiring a separate file", {
  weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))

  validation <- validate_study_data(
    raw_weights = weights_import,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(weights_import, weights_field_spec()),
      survival = guess_field_mapping(NULL, survival_field_spec()),
      day_map = default_day_mapping(weights_import)
    )
  )

  testthat::expect_length(validation$hard_errors, 0)
  testthat::expect_gt(nrow(validation$clean_survival), 0)
  testthat::expect_true(any(grepl("Survival inferred from study data", validation$warnings)))
})

testthat::test_that("validation includes clean_scores when score columns are available", {
  weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))

  validation <- validate_study_data(
    raw_weights = weights_import,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(weights_import, weights_field_spec()),
      survival = guess_field_mapping(NULL, survival_field_spec()),
      day_map = default_day_mapping(weights_import),
      score_day_map = default_score_day_mapping(weights_import)
    ),
    infer_survival = TRUE
  )

  testthat::expect_true("clean_scores" %in% names(validation))
  testthat::expect_true(nrow(validation$clean_scores) > 0)
  testthat::expect_true(all(c("animal_id", "group_id", "day", "score") %in% names(validation$clean_scores)))
})

testthat::test_that("score columns with only blank or non-numeric values do not produce plot data", {
  weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  weights_import$data$score_d0 <- ""
  weights_import$data$score_d1 <- "n/a"
  weights_import$lookup <- dplyr::bind_rows(
    weights_import$lookup,
    tibble::tibble(
      original = c("score_d0", "score_d1"),
      clean = c("score_d0", "score_d1")
    )
  )

  validation <- validate_study_data(
    raw_weights = weights_import,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(weights_import, weights_field_spec()),
      survival = guess_field_mapping(NULL, survival_field_spec()),
      day_map = default_day_mapping(weights_import),
      score_day_map = tibble::tibble(
        source_label = c("score_d0 [score_d0]", "score_d1 [score_d1]"),
        source_column = c("score_d0", "score_d1"),
        day = c(0L, 1L),
        include = c(TRUE, TRUE)
      )
    )
  )

  testthat::expect_equal(nrow(validation$clean_scores), 0L)
  testthat::expect_true(any(grepl("did not contain any numeric score values", validation$warnings)))
})

testthat::test_that("normalized weights import does not emit spurious parse warnings", {
  weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))

  validation <- validate_study_data(
    raw_weights = weights_import,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(weights_import, weights_field_spec()),
      survival = guess_field_mapping(NULL, survival_field_spec()),
      day_map = default_day_mapping(weights_import)
    ),
    infer_survival = TRUE
  )

  parse_warnings <- grep("weight value\\(s\\) could not be parsed", validation$warnings, value = TRUE)
  testthat::expect_length(parse_warnings, 0)
})
