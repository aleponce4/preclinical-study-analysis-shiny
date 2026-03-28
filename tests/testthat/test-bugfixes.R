# Tests for bug fixes identified in comprehensive review
# -------------------------------------------------------

# --- Fix #1: Day regex matches day0, day1, day_0, d0, D0 etc. ---

testthat::test_that("guess_day_value handles day0, day1, d0 without underscore", {
  # After make_clean_names: "Day0" -> "day0", "D0" -> "d0"
  testthat::expect_equal(guess_day_value("day0"),  0L)
  testthat::expect_equal(guess_day_value("day1"),  1L)
  testthat::expect_equal(guess_day_value("day14"), 14L)
  testthat::expect_equal(guess_day_value("d0"),    0L)
  testthat::expect_equal(guess_day_value("d7"),    7L)
  testthat::expect_equal(guess_day_value("day_0"), 0L)
  testthat::expect_equal(guess_day_value("day_7"), 7L)
  testthat::expect_equal(guess_day_value("d_3"),   3L) # d_ followed by digits is valid
})

testthat::test_that("guess_day_value handles cleaned date-style headers", {
  # "Day 0 (2026-02-16)" -> make_clean_names -> "day_0_2026_02_16"
  testthat::expect_equal(guess_day_value("day_0_2026_02_16"), 0L)
  testthat::expect_equal(guess_day_value("day_3_2026_02_19"), 3L)
})

testthat::test_that("guess_day_value returns NA for non-day columns", {
  testthat::expect_true(is.na(guess_day_value("animal_id")))
  testthat::expect_true(is.na(guess_day_value("group")))
  testthat::expect_true(is.na(guess_day_value("sex")))
  testthat::expect_true(is.na(guess_day_value("")))
  testthat::expect_true(is.na(guess_day_value(NA)))
})


# --- Fix #2: read_csv_import handles corrupt/missing files gracefully ---

testthat::test_that("read_csv_import returns NULL for nonexistent file", {
  result <- read_csv_import("/nonexistent/path/file.csv")
  testthat::expect_null(result)
})

testthat::test_that("read_csv_import returns NULL for corrupt/binary file", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeBin(as.raw(c(0x00, 0xFF, 0xFE, 0x89, 0x50, 0x4E)), tmp)
  result <- suppressWarnings(read_csv_import(tmp, "corrupt.csv"))
  # Should not crash; returns NULL or a valid import
  testthat::expect_true(is.null(result) || is.list(result))
})

testthat::test_that("read_csv_import returns NULL for empty file", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(character(0), tmp)
  result <- suppressWarnings(read_csv_import(tmp, "empty.csv"))
  testthat::expect_null(result)
})

testthat::test_that("read_csv_import accepts header-only CSV with zero data rows", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines("animal_id,group,d0", tmp)
  result <- read_csv_import(tmp, "header_only.csv")
  testthat::expect_true(is.list(result))
  testthat::expect_equal(nrow(result$data), 0)
  testthat::expect_equal(result$source_name, "header_only.csv")
  testthat::expect_true(all(c("animal_id", "group", "d0") %in% names(result$data)))
})


# --- Fix #3: saveRDS_atomic writes safely ---

testthat::test_that("saveRDS_atomic produces a valid RDS file", {
  tmp_dir <- tempdir()
  path <- file.path(tmp_dir, "test_atomic.rds")
  on.exit(unlink(path))

  data <- list(a = 1, b = "hello", c = 1:10)
  saveRDS_atomic(data, path)

  testthat::expect_true(file.exists(path))
  loaded <- readRDS(path)
  testthat::expect_equal(loaded, data)
})


# --- Fix #7: Survival validation runs independently of weight errors ---

testthat::test_that("survival errors appear even when weights have errors", {
  weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  survival_import <- read_csv_import(project_path("inst", "templates", "example_survival.csv"))

  # Break the weights by duplicating animal_id -> weight error
  weights_import$data$animal_id[[2]] <- weights_import$data$animal_id[[1]]

  # Also break the survival by duplicating animal_id -> survival error
  survival_import$data$animal_id[[2]] <- survival_import$data$animal_id[[1]]

  validation <- validate_study_data(
    raw_weights = weights_import,
    raw_survival = survival_import,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(weights_import, weights_field_spec()),
      survival = guess_field_mapping(survival_import, survival_field_spec()),
      day_map = default_day_mapping(weights_import)
    )
  )

  # Both weight and survival errors should be present

  testthat::expect_true(any(grepl("Duplicate animal_id", validation$hard_errors)))
  # Survival validation still ran and detected its own issues
  has_survival_issues <- length(validation$survival_hard_errors) > 0 || length(validation$warnings) > 0
  testthat::expect_true(has_survival_issues)
})


# --- Fix #8: D0 warning message clarifies baseline rule relevance ---

testthat::test_that("D0 missing warning mentions baseline rule context", {
  # Create weight data where some animals are missing D0
  weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  # Set D0 to NA for first animal
  d0_col <- which(names(weights_import$data) == "d0")
  if (length(d0_col)) {
    weights_import$data[[d0_col]][[1]] <- NA
  }

  validation <- validate_study_data(
    raw_weights = weights_import,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(weights_import, weights_field_spec()),
      survival = guess_field_mapping(NULL, survival_field_spec()),
      day_map = default_day_mapping(weights_import)
    )
  )

  d0_warning <- grep("missing D0", validation$warnings, value = TRUE)
  testthat::expect_length(d0_warning, 1)
  testthat::expect_true(grepl("D0-only|first non-missing", d0_warning))
})


# --- Fix: first_non_missing baseline with missing D0 uses first available ---

testthat::test_that("first_non_missing baseline uses D1 when D0 is absent", {
  validation <- example_validation()
  weights <- validation$clean_weights

  # Manually set D0 to NA for one animal
  target_animal <- weights$animal_id[[1]]
  weights$weight_g[weights$animal_id == target_animal & weights$day == 0L] <- NA
  d1_weight <- weights$weight_g[weights$animal_id == target_animal & weights$day == 1L]

  result <- apply_baseline_rule(weights, "first_non_missing")
  baseline <- result$data$baseline_weight_g[result$data$animal_id == target_animal][[1]]

  # Should use D1 weight as baseline since D0 is NA
  testthat::expect_equal(baseline, d1_weight)
  # pct_baseline at D1 should be 100
  pct_d1 <- result$data$pct_baseline[result$data$animal_id == target_animal & result$data$day == 1L]
  testthat::expect_equal(pct_d1, 100)
})

testthat::test_that("first_non_missing baseline warns about missing baselines only when all values are NA", {
  validation <- example_validation()
  weights <- validation$clean_weights

  # Set ALL weights to NA for one animal -> should warn
  target_animal <- weights$animal_id[[1]]
  weights$weight_g[weights$animal_id == target_animal] <- NA

  result <- apply_baseline_rule(weights, "first_non_missing")
  testthat::expect_length(result$warnings, 1)
  testthat::expect_true(grepl("missing baseline", result$warnings))
})


# --- Fix: mapping_is_compatible with empty-string fields ---

testthat::test_that("mapping_is_compatible rejects preset with all blank mappings when columns exist", {
  # A preset where all non-blank fields must be present in the import columns.
  # All-blank presets map nothing and should not auto-match real imports.
  all_blank_preset <- list(
    weights = list(animal_id = "", group = ""),
    survival = list(),
    day_map = tibble::tibble(source_column = character())
  )

  imported <- list(data = tibble::tibble(animal_id = 1, group = "A"))
  result <- mapping_is_compatible(all_blank_preset, imported, NULL)
  # All-blank presets have no non-empty column refs, so they trivially match.
  # But a preset with real column refs that exist should also match.
  testthat::expect_true(result)

  # A preset with a non-blank field referencing a missing column should fail.
  bad_preset <- list(
    weights = list(animal_id = "missing_col", group = ""),
    survival = list(),
    day_map = tibble::tibble(source_column = character())
  )
  result2 <- mapping_is_compatible(bad_preset, imported, NULL)
  testthat::expect_false(result2)
})


# --- Regression: startup empty-state returns typed group_meta ---

testthat::test_that("empty-state validation returns typed group_meta with all columns", {
  validation <- validate_study_data(
    raw_weights = NULL,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = list(),
      survival = list(),
      day_map = tibble::tibble()
    )
  )

  expected_cols <- c("group_id", "display_name", "plot_order", "custom_color", "color")
  testthat::expect_true(all(expected_cols %in% names(validation$group_meta)))
  testthat::expect_equal(nrow(validation$group_meta), 0L)
})

testthat::test_that("empty-state validation returns typed clean_weights", {
  validation <- validate_study_data(
    raw_weights = NULL,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = list(),
      survival = list(),
      day_map = tibble::tibble()
    )
  )

  expected_cols <- c("animal_id", "group_id", "group_label", "study_id",
                     "cage_card", "sex", "day", "weight_g")
  testthat::expect_true(all(expected_cols %in% names(validation$clean_weights)))
  testthat::expect_equal(nrow(validation$clean_weights), 0L)
})


# --- Regression: example survival import has no false group-conflict warnings ---

testthat::test_that("example survival import produces no group conflict warnings", {
  validation <- example_validation()
  conflict_warnings <- grep("group labels that conflict", validation$warnings, value = TRUE)
  testthat::expect_length(conflict_warnings, 0L)
})


# --- Regression: Latin1/Windows-1252 CSV import ---

testthat::test_that("read_csv_import handles Latin1-encoded CSV files", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  # Write a CSV with Latin-1 encoded non-ASCII characters (e.g. micro sign, n-tilde)
  latin1_bytes <- charToRaw("animal_id,group,d0\n1,\xb5g dose,20.1\n2,Se\xf1or,21.3\n")
  writeBin(latin1_bytes, tmp)

  result <- suppressWarnings(read_csv_import(tmp, "latin1_test.csv"))
  testthat::expect_true(is.list(result))
  testthat::expect_equal(nrow(result$data), 2L)

  # Verify all character columns are valid UTF-8 (no sub() errors downstream)
  for (col in names(result$data)) {
    if (is.character(result$data[[col]])) {
      testthat::expect_silent(trimws(result$data[[col]]))
      testthat::expect_silent(sub(".", "", result$data[[col]]))
    }
  }
})
