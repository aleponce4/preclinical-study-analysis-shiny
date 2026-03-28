write_offset_weights_xlsx <- function(path) {
  rows <- matrix(NA_character_, nrow = 14, ncol = 28)
  rows[1, 1] <- "example_study_1 natural history of Example virus X in example rats"
  rows[3, 2] <- "Protocol EXAMPLE-0000"
  rows[6, ] <- c(
    "Virus / Group", "Cage Card", "Rat ID",
    "D0 12/9/25", "", "", "", "Score",
    "D1 12/10/25", "", "", "", "Score",
    "D2 12/11/25", "", "", "", "Score",
    "D3 12/12/25", "", "", "", "Score",
    "D4 12/13/25", "", "", "", "Score"
  )
  rows[7, ] <- c("Group A Mock", "900001", "113", "46000.5", "113", "36.6C", "170.9 g", "", "46000.5", "113", "35.9C", "161.3 g", "", "46000.5", "113", "36.8C", "169.7g", "", "46000.5", "113", "36.2C", "173.1 g", "", "46000.5", "113", "36.5C", "178.2 g", "")
  rows[8, ] <- c("", "900002", "114", "46000.5", "114", "37.0C", "168.7 g", "", "46000.5", "114", "37.0C", "168.4 g", "", "46000.5", "114", "37.3C", "172.0 g", "", "46000.5", "114", "36.8C", "174.5 g", "", "46000.5", "114", "37.2C", "179.0 g", "")
  rows[9, ] <- c("", "900003", "115", "46000.5", "115", "36.8C", "151.2 g", "", "46000.5", "115", "37.4C", "157.2 g", "", "46000.5", "115", "36.9C", "162.5 g", "", "46000.5", "115", "37.1C", "165.4 g", "", "46000.5", "115", "37.0C", "169.1 g", "")
  rows[10, ] <- c("", "900004", "116", "46000.5", "116", "37.7C", "153.0 g", "", "46000.5", "116", "38.0C", "150.8 g", "", "46000.5", "116", "37.5C", "154.2 g", "", "46000.5", "116", "37.3C", "158.9 g", "", "46000.5", "116", "37.2C", "160.7 g", "")
  rows[11, ] <- c("Group B Challenge", "900007", "129", "46000.5", "129", "37.1C", "156.4 g", "", "46000.5", "129", "37.0C", "155.0 g", "", "46000.5", "129", "37.2C", "156.1 g", "", "46000.5", "129", "37.1C", "159.3 g", "", "46000.5", "129", "37.0C", "163.5 g", "")
  rows[12, ] <- c("", "900005", "127", "46000.5", "127", "37.1C", "184.8 g", "", "46000.5", "127", "36.8C", "185.1 g", "", "46000.5", "127", "37.0C", "188.5 g", "", "46000.5", "127", "36.9C", "193.2 g", "", "46000.5", "127", "37.2C", "198.0 g", "")
  rows[13, ] <- c("", "900006", "131", "46000.5", "131", "36.6C", "148.4 g", "", "46000.5", "131", "36.9C", "147.6 g", "", "46000.5", "131", "36.8C", "150.2 g", "", "46000.5", "131", "36.7C", "151.9 g", "", "46000.5", "131", "36.8C", "154.6 g", "")
  rows[14, ] <- c("", "900008", "132", "46000.5", "132", "36.4C", "169.3 g", "", "46000.5", "132", "36.6C", "168.9 g", "", "46000.5", "132", "36.7C", "171.4 g", "", "46000.5", "132", "36.8C", "173.0 g", "", "46000.5", "132", "36.9C", "176.8 g", "")

  writexl::write_xlsx(
    list(sheet1 = as.data.frame(rows, stringsAsFactors = FALSE)),
    path = path,
    col_names = FALSE
  )
}

write_unstructured_weights_xlsx <- function(path) {
  rows <- matrix(NA_character_, nrow = 5, ncol = 6)
  rows[1, 1] <- "Animal observations"
  rows[2, 2] <- "Numbers below are daily lab notes"
  rows[3, ] <- c("1001", "1002", "1003", "1004", "1005", "1006")
  rows[4, ] <- c("46000.5", "46000.5", "46000.5", "46000.5", "46000.5", "46000.5")
  rows[5, ] <- c("36.6C", "36.9C", "37.0C", "36.8C", "37.1C", "36.7C")

  writexl::write_xlsx(
    list(sheet1 = as.data.frame(rows, stringsAsFactors = FALSE)),
    path = path,
    col_names = FALSE
  )
}

testthat::test_that("real-template weights import normalizes to canonical columns", {
  imported <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  expected_core <- c("study_id", "group", "cage_card", "animal_id")
  score_cols <- grep("^score_d\\d+$", names(imported$data), value = TRUE)

  testthat::expect_true(all(expected_core %in% names(imported$data)))
  testthat::expect_true(all(paste0("d", 0:15) %in% names(imported$data)))
  testthat::expect_true(all(vapply(imported$data[paste0("d", 0:15)], is.numeric, logical(1))))
  testthat::expect_true(length(score_cols) >= 16)
  testthat::expect_true(all(vapply(imported$data[score_cols], is.numeric, logical(1))))
})

testthat::test_that("real-template day headers auto-detect correctly", {
  imported <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  day_map <- default_day_mapping(imported)
  detected <- day_map |>
    dplyr::filter(.data$include) |>
    dplyr::pull(.data$day)

  testthat::expect_equal(detected, 0:15)
})

testthat::test_that("score day headers auto-detect correctly", {
  imported <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  score_map <- default_score_day_mapping(imported)
  detected <- score_map |>
    dplyr::filter(.data$include) |>
    dplyr::pull(.data$day)

  testthat::expect_true(length(detected) >= 16)
  testthat::expect_true(all(detected %in% 0:15))
})

testthat::test_that("template examples import cleanly", {
  weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  survival_import <- read_csv_import(project_path("inst", "templates", "example_survival.csv"))

  testthat::expect_equal(weights_import$source_name, "example_weights.csv")
  testthat::expect_equal(survival_import$source_name, "example_survival.csv")
  testthat::expect_true(nrow(weights_import$data) > 0)
  testthat::expect_true(nrow(survival_import$data) > 0)
})

testthat::test_that("real-template import keeps explicit study and group columns populated", {
  imported <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))

  testthat::expect_false(any(is.na(imported$data$group) | !nzchar(imported$data$group)))
  testthat::expect_false(any(is.na(imported$data$study_id) | !nzchar(imported$data$study_id)))
})

testthat::test_that("real-template import parses gram-suffixed weight values", {
  imported <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))

  row_201 <- imported$data |>
    dplyr::filter(.data$animal_id == "201")

  testthat::expect_equal(row_201$d0[[1]], 22.7)
  testthat::expect_equal(row_201$d15[[1]], 24.4)
})

testthat::test_that("real-template import parses score columns into score_d# fields", {
  imported <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  row_201 <- imported$data |>
    dplyr::filter(.data$animal_id == "201")

  testthat::expect_true("score_d0" %in% names(imported$data))
  testthat::expect_true("score_d10" %in% names(imported$data))
  testthat::expect_equal(nrow(row_201), 1)
  testthat::expect_equal(row_201$score_d0[[1]], 0)
  testthat::expect_false(all(is.na(imported$data$score_d10)))
})

testthat::test_that("weights Excel import detects offset headers and rat workbook roles", {
  testthat::skip_if_not_installed("writexl")

  path <- tempfile(fileext = ".xlsx")
  write_offset_weights_xlsx(path)

  imported <- read_weights_excel_import(path, "offset-rat.xlsx")
  day_map <- default_day_mapping(imported)
  validation <- validate_study_data(
    raw_weights = imported,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(imported, weights_field_spec()),
      survival = guess_field_mapping(NULL, survival_field_spec()),
      day_map = day_map,
      survival_day_map = empty_day_map(),
      score_day_map = default_score_day_mapping(imported)
    ),
    infer_survival = TRUE
  )

  testthat::expect_equal(imported$header_row, 6L)
  testthat::expect_true(all(c("study_id", "group", "cage_card", "animal_id", paste0("d", 0:4)) %in% names(imported$data)))
  testthat::expect_true(all(paste0("score_d", 0:4) %in% names(imported$data)))
  testthat::expect_true(all(is.na(imported$data$study_id)))
  testthat::expect_equal(imported$data$group[[1]], "Group A Mock")
  testthat::expect_equal(imported$data$group[[5]], "Group B Challenge")
  testthat::expect_equal(imported$data$cage_card[[1]], "900001")
  testthat::expect_equal(imported$data$animal_id[[1]], "113")
  testthat::expect_equal(imported$data$d0[[1]], 170.9)
  testthat::expect_equal(imported$data$d4[[6]], 198.0)
  testthat::expect_true(all(is.na(imported$data$score_d0)))
  testthat::expect_equal(day_map |> dplyr::filter(.data$include) |> dplyr::pull(.data$day), 0:4)
  testthat::expect_length(validation$hard_errors, 0)
})

testthat::test_that("weights Excel import falls back to row 1 when no plausible header exists", {
  testthat::skip_if_not_installed("writexl")

  path <- tempfile(fileext = ".xlsx")
  write_unstructured_weights_xlsx(path)

  imported <- read_weights_excel_import(path, "unstructured.xlsx")
  validation <- validate_study_data(
    raw_weights = imported,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(imported, weights_field_spec()),
      survival = guess_field_mapping(NULL, survival_field_spec()),
      day_map = default_day_mapping(imported),
      survival_day_map = empty_day_map(),
      score_day_map = default_score_day_mapping(imported)
    ),
    infer_survival = TRUE
  )

  testthat::expect_equal(imported$header_row, 1L)
  testthat::expect_true(any(grepl("Missing required mapped weights fields", validation$hard_errors, fixed = TRUE)))
  testthat::expect_true(any(grepl("At least one valid day column must be selected.", validation$hard_errors, fixed = TRUE)))
})
