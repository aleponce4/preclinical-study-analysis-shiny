testthat::test_that("field aliases auto-detect expected columns", {
  imported <- list(
    data = tibble::tibble(
      id = 1:2,
      treatment = c("A", "B"),
      study = c("S1", "S1"),
      cage = c("1", "2")
    ),
    lookup = tibble::tibble(
      original = c("id", "treatment", "study", "cage"),
      clean = c("id", "treatment", "study", "cage")
    )
  )

  mapping <- guess_field_mapping(imported, weights_field_spec())
  testthat::expect_equal(mapping$animal_id, "id")
  testthat::expect_equal(mapping$group, "treatment")
  testthat::expect_equal(mapping$study_id, "study")
  testthat::expect_equal(mapping$cage_card, "cage")
})

testthat::test_that("manual day mapping can preserve edited values", {
  imported <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  default_map <- default_day_mapping(imported)
  saved_map <- tibble::tibble(
    source_label = default_map$source_label[1:2],
    source_column = default_map$source_column[1:2],
    day = c(10L, 11L),
    include = c(TRUE, TRUE)
  )

  merged <- merge_day_mapping(default_map, saved_map)
  testthat::expect_equal(merged$day[1:2], c(10L, 11L))
})

testthat::test_that("normalized real-template weights auto-map canonical fields", {
  imported <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  mapping <- guess_field_mapping(imported, weights_field_spec())
  day_map <- default_day_mapping(imported)

  testthat::expect_equal(mapping$animal_id, "animal_id")
  testthat::expect_equal(mapping$group, "group")
  testthat::expect_equal(mapping$study_id, "study_id")
  testthat::expect_equal(mapping$cage_card, "cage_card")
  testthat::expect_equal(day_map$source_column[day_map$include], paste0("d", 0:15))
})
