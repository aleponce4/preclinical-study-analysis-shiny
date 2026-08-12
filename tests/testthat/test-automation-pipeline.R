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
  # Invented values only. The unspaced "176.8g" on the first data row is deliberate:
  # it exercises the gram-suffix parser against a missing space.
  rows[6, ] <- c("Group A Mock", "900001", "201", "46000.5", "201", "36.7C", "172.4 g", "", "46001.5", "201", "36.5C", "174.1 g", "", "46002.5", "201", "36.9C", "176.8g", "", "46003.5", "201", "36.6C", "179.2 g", "", "46004.5", "201", "36.8C", "181.5 g", "")
  rows[7, ] <- c("", "900002", "202", "46000.5", "202", "37.1C", "165.3 g", "", "46001.5", "202", "36.9C", "167.0 g", "", "46002.5", "202", "37.2C", "169.4 g", "", "46003.5", "202", "37.0C", "171.8 g", "", "46004.5", "202", "37.3C", "174.2 g", "")
  rows[8, ] <- c("", "900003", "203", "46000.5", "203", "36.8C", "158.6 g", "", "46001.5", "203", "37.0C", "160.2 g", "", "46002.5", "203", "36.7C", "162.7 g", "", "46003.5", "203", "37.1C", "165.1 g", "", "46004.5", "203", "36.9C", "167.9 g", "")
  rows[9, ] <- c("Group B Challenge", "900007", "206", "46000.5", "206", "37.2C", "154.7 g", "", "46001.5", "206", "37.0C", "153.1 g", "", "46002.5", "206", "37.3C", "155.6 g", "", "46003.5", "206", "37.1C", "158.0 g", "", "46004.5", "206", "37.0C", "161.4 g", "")
  rows[10, ] <- c("", "900005", "205", "46000.5", "205", "37.0C", "183.5 g", "", "46001.5", "205", "36.8C", "184.9 g", "", "46002.5", "205", "37.1C", "187.2 g", "", "46003.5", "205", "36.9C", "191.6 g", "", "46004.5", "205", "37.2C", "196.3 g", "")
  rows[11, ] <- c("", "900006", "207", "46000.5", "207", "36.6C", "147.8 g", "", "46001.5", "207", "36.9C", "146.9 g", "", "46002.5", "207", "36.7C", "149.5 g", "", "46003.5", "207", "36.8C", "151.2 g", "", "46004.5", "207", "36.5C", "153.9 g", "")

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
  expect_equal(imported$data$d4[[5]], 196.3)
})
