testthat::test_that("default direct entry settings include simple matrix defaults", {
  settings <- default_direct_entry_settings()

  testthat::expect_equal(settings$study_name, "")
  testthat::expect_equal(settings$n_groups, 3L)
  testthat::expect_equal(settings$mice_per_group, 4L)
  testthat::expect_equal(settings$group_names, c("Group 1", "Group 2", "Group 3"))
  testthat::expect_equal(settings$dpi_rows, c(0L, 1L, 3L, 5L))
  testthat::expect_equal(settings$mouse_labels$g1, c("M1", "M2", "M3", "M4"))
})

testthat::test_that("matrix column spec and headers follow group/mouse setup", {
  config <- normalize_direct_entry_settings(list(
    n_groups = 2L,
    mice_per_group = 3L,
    group_names = c("Control", "Drug")
  ))

  spec <- direct_entry_matrix_col_spec(config)
  headers <- direct_entry_matrix_headers(config)

  testthat::expect_equal(nrow(spec), 6L)
  testthat::expect_equal(spec$col_key, c("g1_m1", "g1_m2", "g1_m3", "g2_m1", "g2_m2", "g2_m3"))
  testthat::expect_equal(headers[[1]], "Study Day")
  testthat::expect_true(any(grepl("Control / M1", headers, fixed = TRUE)))
  testthat::expect_true(any(grepl("Drug / M3", headers, fixed = TRUE)))
})

testthat::test_that("matrix conversion builds canonical import object with d# columns", {
  config <- normalize_direct_entry_settings(list(
    study_name = "Study-X",
    n_groups = 2L,
    mice_per_group = 2L,
    group_names = c("Control", "Drug"),
    dpi_rows = c(0L, 3L)
  ))

  sheet <- empty_direct_entry_matrix(config, dpi_rows = c(0L, 3L))
  sheet$g1_m1 <- c("20.1 g", "19.4 g")
  sheet$g1_m2 <- c("20.8", "19.9")
  sheet$g2_m1 <- c("21.0", "")
  sheet$g2_m2 <- c("22.1", "21.2")

  imported <- direct_entry_matrix_to_import(sheet, config, source_name = "Manual Data Entry")

  testthat::expect_equal(imported$source_name, "Manual Data Entry")
  testthat::expect_true(all(c("animal_id", "group", "study_id", "cage_card", "sex", "d0", "d3") %in% names(imported$data)))
  testthat::expect_equal(imported$lookup$clean, c("animal_id", "group", "study_id", "cage_card", "sex", "d0", "d3"))
  testthat::expect_equal(unique(imported$data$study_id), "Study-X")
  testthat::expect_equal(nrow(imported$data), 4L)
  testthat::expect_equal(imported$data$d0[imported$data$group == "Control"][1], 20.1)
})

testthat::test_that("matrix conversion rejects invalid DPI values", {
  config <- normalize_direct_entry_settings(list(
    n_groups = 1L,
    mice_per_group = 1L,
    group_names = "Control",
    dpi_rows = c(0L, 3L)
  ))
  sheet <- empty_direct_entry_matrix(config, dpi_rows = c(0L, 3L))
  sheet$g1_m1 <- c("20", "19")

  sheet_bad <- sheet
  sheet_bad$dpi <- c("0", "3.5")
  testthat::expect_error(direct_entry_matrix_to_import(sheet_bad, config), "whole numbers")

  sheet_dup <- sheet
  sheet_dup$dpi <- c("3", "3")
  testthat::expect_error(direct_entry_matrix_to_import(sheet_dup, config), "unique")
})

testthat::test_that("direct entry validation reuses inferred survival behavior", {
  config <- normalize_direct_entry_settings(list(
    study_name = "Study-1",
    n_groups = 1L,
    mice_per_group = 2L,
    group_names = "Control",
    dpi_rows = c(0L, 3L, 7L)
  ))

  sheet <- empty_direct_entry_matrix(config, dpi_rows = c(0L, 3L, 7L))
  sheet$g1_m1 <- c("20", "19", "18")
  sheet$g1_m2 <- c("21", "20", "")

  imported <- direct_entry_matrix_to_import(sheet, config)
  validation <- validate_study_data(
    raw_weights = imported,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = direct_entry_weights_mapping(),
      survival = guess_field_mapping(NULL, survival_field_spec()),
      day_map = direct_entry_day_map(config, sheet),
      survival_day_map = tibble::tibble()
    ),
    infer_survival = TRUE
  )

  inferred_row <- validation$clean_survival |>
    dplyr::filter(.data$animal_id == imported$data$animal_id[[2]])

  testthat::expect_length(validation$hard_errors, 0)
  testthat::expect_true(any(grepl("assumed dead", validation$warnings)))
  testthat::expect_equal(inferred_row$time[[1]], 3L)
  testthat::expect_equal(inferred_row$status[[1]], 1L)
})

testthat::test_that("pasted delimited row values are split and grams suffix is normalized", {
  tab_values <- direct_entry_split_pasted_values("19.5 g\t18.3 g\t18.5 g")
  csv_values <- direct_entry_split_pasted_values("19.5 g, 18.3 g, 18.5 g")

  testthat::expect_equal(tab_values, c("19.5", "18.3", "18.5"))
  testthat::expect_equal(csv_values, c("19.5", "18.3", "18.5"))
  testthat::expect_length(direct_entry_split_pasted_values("19.5 g"), 0L)
  testthat::expect_length(direct_entry_split_pasted_values("19,5 g"), 0L)
})

testthat::test_that("matrix to ui table roundtrip preserves canonical matrix values", {
  config <- normalize_direct_entry_settings(list(
    n_groups = 2L,
    mice_per_group = 2L,
    group_names = c("Control", "Drug"),
    dpi_rows = c(0L, 3L)
  ))
  sheet <- empty_direct_entry_matrix(config, dpi_rows = c(0L, 3L))
  sheet$g1_m1 <- c("19.5", "18.3")
  sheet$g1_m2 <- c("19.1", "18.0")
  sheet$g2_m1 <- c("20.2", "19.9")
  sheet$g2_m2 <- c("20.0", "19.8")

  ui <- direct_entry_matrix_to_ui_table(sheet_df = sheet, config = config)
  restored <- direct_entry_ui_table_to_matrix(ui_df = ui, config = config, base_sheet = sheet)

  testthat::expect_equal(names(ui), c("Group", "Mouse ID", "D0", "D3"))
  testthat::expect_equal(nrow(ui), 4L)
  testthat::expect_equal(restored$dpi, c("0", "3"))
  testthat::expect_equal(restored$g1_m1, c("19.5", "18.3"))
  testthat::expect_equal(restored$g2_m2, c("20.0", "19.8"))
})

testthat::test_that("mouse labels are updated from editable ui mouse id column", {
  config <- normalize_direct_entry_settings(list(
    n_groups = 1L,
    mice_per_group = 2L,
    group_names = "Control",
    dpi_rows = c(0L, 3L)
  ))
  ui <- direct_entry_matrix_to_ui_table(
    sheet_df = empty_direct_entry_matrix(config, dpi_rows = c(0L, 3L)),
    config = config
  )
  ui$`Mouse ID` <- c("Mouse-145", "Mouse-146")

  updated <- direct_entry_mouse_labels_from_ui_table(ui_df = ui, config = config)

  testthat::expect_equal(updated$mouse_labels$g1, c("Mouse-145", "Mouse-146"))
})

testthat::test_that("ui day-column paste expansion fills down the same day column", {
  config <- normalize_direct_entry_settings(list(
    n_groups = 1L,
    mice_per_group = 3L,
    group_names = "Control",
    dpi_rows = c(0L, 3L)
  ))
  ui <- direct_entry_matrix_to_ui_table(
    sheet_df = empty_direct_entry_matrix(config, dpi_rows = c(0L, 3L)),
    config = config
  )

  expanded <- direct_entry_expand_pasted_ui_cells(
    ui_df = ui,
    row_index = 1L,
    col_name = "D0",
    value = "19.5 g\n18.3 g\n17.8 g"
  )

  testthat::expect_equal(expanded$D0[[1]], "19.5")
  testthat::expect_equal(expanded$D0[[2]], "18.3")
  testthat::expect_equal(expanded$D0[[3]], "17.8")
  testthat::expect_equal(expanded$D3[[1]], "")
})

testthat::test_that("clipboard-style CRLF column with padded grams parses correctly", {
  sample <- paste(
    c(
      "      18.9 g ",
      "      18.0 g ",
      "      17.9 g ",
      "      17.6 g ",
      "      19.3 g ",
      "      17.7 g ",
      "      17.9 g ",
      "      18.4 g ",
      "      18.4 g ",
      "      18.6 g ",
      "      19.0 g ",
      "      18.1 g "
    ),
    collapse = "\r\n"
  )

  parsed <- direct_entry_split_pasted_values(sample)

  testthat::expect_equal(length(parsed), 12L)
  testthat::expect_equal(parsed[[1]], "18.9")
  testthat::expect_equal(parsed[[12]], "18.1")
})
