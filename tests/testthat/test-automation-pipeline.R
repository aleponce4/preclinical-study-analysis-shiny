write_offset_weights_xlsx_for_automation <- function(path) {
  rows <- matrix(NA_character_, nrow = 11, ncol = 28)
  rows[1, 1] <- "example_study_1 natural history of Example virus X in example rats"
  rows[3, 2] <- "Protocol EXAMPLE-0000"
  rows[5, ] <- c(
    "Virus / Group", "Cage Card", "Rat ID",
    "D0 12/9/25", "", "", "", "Score",
    "D1 12/10/25", "", "", "", "Score",
    "D2 12/11/25", "", "", "", "Score",
    "D3 12/12/25", "", "", "", "Score",
    "D4 12/13/25", "", "", "", "Score"
  )
  rows[6, ] <- c("Group A Mock", "900001", "113", "46000.5", "113", "36.6C", "170.9 g", "", "46000.5", "113", "35.9C", "161.3 g", "", "46000.5", "113", "36.8C", "169.7g", "", "46000.5", "113", "36.2C", "173.1 g", "", "46000.5", "113", "36.5C", "178.2 g", "")
  rows[7, ] <- c("", "900002", "114", "46000.5", "114", "37.0C", "168.7 g", "", "46000.5", "114", "37.0C", "168.4 g", "", "46000.5", "114", "37.3C", "172.0 g", "", "46000.5", "114", "36.8C", "174.5 g", "", "46000.5", "114", "37.2C", "179.0 g", "")
  rows[8, ] <- c("", "900003", "115", "46000.5", "115", "36.8C", "151.2 g", "", "46000.5", "115", "37.4C", "157.2 g", "", "46000.5", "115", "36.9C", "162.5 g", "", "46000.5", "115", "37.1C", "165.4 g", "", "46000.5", "115", "37.0C", "169.1 g", "")
  rows[9, ] <- c("Group B Challenge", "900007", "129", "46000.5", "129", "37.1C", "156.4 g", "", "46000.5", "129", "37.0C", "155.0 g", "", "46000.5", "129", "37.2C", "156.1 g", "", "46000.5", "129", "37.1C", "159.3 g", "", "46000.5", "129", "37.0C", "163.5 g", "")
  rows[10, ] <- c("", "900005", "127", "46000.5", "127", "37.1C", "184.8 g", "", "46000.5", "127", "36.8C", "185.1 g", "", "46000.5", "127", "37.0C", "188.5 g", "", "46000.5", "127", "36.9C", "193.2 g", "", "46000.5", "127", "37.2C", "198.0 g", "")
  rows[11, ] <- c("", "900006", "131", "46000.5", "131", "36.6C", "148.4 g", "", "46000.5", "131", "36.9C", "147.6 g", "", "46000.5", "131", "36.8C", "150.2 g", "", "46000.5", "131", "36.7C", "151.9 g", "", "46000.5", "131", "36.8C", "154.6 g", "")

  writexl::write_xlsx(
    list(sheet1 = as.data.frame(rows, stringsAsFactors = FALSE)),
    path = path,
    col_names = FALSE
  )
}

test_that("pipeline produces plots and tables from example template data", {
  root <- getOption("labweight.app_root")
  style <- automation_load_styles(file.path(root, "automation", "config", "styles.yml"))$default

  study <- list(
    study_id = "example_pipeline",
    enabled = TRUE,
    source_url = project_path("inst", "templates", "example_weights.csv"),
    source_format = "csv",
    style_id = "default",
    schedule_id = "daily_0900",
    report_title = "Example Pipeline",
    weights_sheet = 1,
    email_to = c("user@example.org"),
    email_cc = character()
  )

  analysis <- automation_prepare_study_analysis(
    study = study,
    style = style,
    workbook_path = study$source_url,
    generated_at = as.POSIXct("2026-03-09 09:00:00", tz = "America/Chicago")
  )

  expect_true(inherits(analysis$plots$weights, "ggplot"))
  expect_true(inherits(analysis$plots$survival, "ggplot"))
  expect_equal(analysis$report_name, "Study Monitoring Report")
  expect_equal(analysis$source_file, "example_weights.csv")
  expect_true(nrow(analysis$tables$weight_summary) > 0)
  expect_true(nrow(analysis$tables$survival_summary) > 0)
  expect_true(length(analysis$warnings) >= 0)
})

test_that("pipeline honors source_format excel even when extension is non-excel", {
  skip_if_not_installed("writexl")
  root <- getOption("labweight.app_root")
  style <- automation_load_styles(file.path(root, "automation", "config", "styles.yml"))$default

  csv_path <- project_path("inst", "templates", "example_weights.csv")
  raw <- readr::read_csv(csv_path, show_col_types = FALSE)
  fake_aspx <- tempfile(fileext = ".aspx")
  writexl::write_xlsx(list(sheet1 = raw), fake_aspx)

  study <- list(
    study_id = "example_excel_non_ext",
    enabled = TRUE,
    source_url = fake_aspx,
    source_format = "excel",
    style_id = "default",
    schedule_id = "daily_0900",
    report_title = "Excel NonExt",
    weights_sheet = 1,
    email_to = c("user@example.org"),
    email_cc = character()
  )

  analysis <- automation_prepare_study_analysis(
    study = study,
    style = style,
    workbook_path = study$source_url,
    generated_at = as.POSIXct("2026-03-09 09:00:00", tz = "America/Chicago")
  )

  expect_true(inherits(analysis$plots$weights, "ggplot"))
  expect_equal(analysis$source_file, basename(fake_aspx))
  expect_true(nrow(analysis$tables$weight_summary) > 0)
})

test_that("automation_import_weights uses hardened Excel importer for offset headers", {
  skip_if_not_installed("writexl")

  path <- tempfile(fileext = ".xlsx")
  write_offset_weights_xlsx_for_automation(path)

  imported <- automation_import_weights(path, source_format = "excel")

  expect_equal(imported$header_row, 5L)
  expect_true(all(c("group", "cage_card", "animal_id", "d0", "d4") %in% names(imported$data)))
  expect_true(all(is.na(imported$data$study_id)))
  expect_equal(imported$data$group[[1]], "Group A Mock")
  expect_equal(imported$data$cage_card[[1]], "900001")
  expect_equal(imported$data$d4[[5]], 198.0)
})

test_that("automation study overrides exclude scheduled sampling cages from KM", {
  wb_path <- project_path("example_workbook.xlsx")
  skip_if_not(file.exists(wb_path), "Workbook example_workbook.xlsx is not present in repo")
  imported <- automation_import_weights(
    path = wb_path,
    source_format = "excel"
  )

  validation <- validate_study_data(
    raw_weights = imported,
    raw_survival = NULL,
    mapping = automation_default_mapping_bundle(imported),
    infer_death_events = TRUE,
    detect_scheduled_sampling = TRUE,
    scheduled_sampling_cages = tibble::tibble(
      cage_card = "900010",
      role = "scheduled_sampling",
      censor_day = 6L
    )
  )
  group_meta <- resolve_group_colors(validation$group_meta, default_shared_style_settings())
  analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  expect_false(analysis$valid)
  expect_match(analysis$message, "No survival-cohort rows are available")
  expect_true(sum(analysis$excluded_summary$`Excluded from KM`, na.rm = TRUE) > 0)
  expect_true(any(validation$scheduled_sampling_review$cage_card == "900010"))
})
