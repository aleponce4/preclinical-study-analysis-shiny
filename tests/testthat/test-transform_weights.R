testthat::test_that("baseline rules compute expected percentages", {
  validation <- example_validation()
  d0 <- apply_baseline_rule(validation$clean_weights, "d0_only")
  fallback <- apply_baseline_rule(validation$clean_weights, "first_non_missing")

  animal_rows <- d0$data |>
    dplyr::filter(.data$animal_id == "201", .data$day == 0L)

  testthat::expect_equal(animal_rows$pct_baseline[[1]], 100)
  testthat::expect_equal(nrow(d0$data), nrow(fallback$data))
})

testthat::test_that("group overrides replace labels and plot order", {
  validation <- example_validation()
  overrides <- tibble::tibble(
    group_id = validation$group_meta$group_id[1],
    display_name = "Edited Group",
    plot_order = 10L,
    custom_color = "#123456"
  )

  updated <- validate_study_data(
    raw_weights = read_weights_import(project_path("inst", "templates", "example_weights.csv")),
    raw_survival = read_csv_import(project_path("inst", "templates", "example_survival.csv")),
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(read_weights_import(project_path("inst", "templates", "example_weights.csv")), weights_field_spec()),
      survival = guess_field_mapping(read_csv_import(project_path("inst", "templates", "example_survival.csv")), survival_field_spec()),
      day_map = default_day_mapping(read_weights_import(project_path("inst", "templates", "example_weights.csv")))
    ),
    group_overrides = overrides
  )

  testthat::expect_equal(updated$group_meta$display_name[updated$group_meta$group_id == overrides$group_id], "Edited Group")
  testthat::expect_equal(updated$group_meta$custom_color[updated$group_meta$group_id == overrides$group_id], "#123456")
  testthat::expect_equal(updated$group_meta$color[updated$group_meta$group_id == overrides$group_id], "#123456")
})

testthat::test_that("weight summaries expose derived error columns", {
  validation <- example_validation()
  weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
  summary_tbl <- summarise_weights_data(weights_processed$data, mode = "raw", error_style = "ci95")

  testthat::expect_true("group" %in% names(summary_tbl))
  testthat::expect_false("group_id" %in% names(summary_tbl))
  testthat::expect_false("display_name" %in% names(summary_tbl))
  testthat::expect_true(all(c("n", "mean", "sd", "sem", "ci95", "error_low", "error_high") %in% names(summary_tbl)))
  testthat::expect_true(any(!is.na(summary_tbl$ci95)))
})

testthat::test_that("weight_key_stats returns expected columns and one row per group", {
  validation <- example_validation()
  weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))

  stats_tbl <- weight_key_stats(weights_processed$data, group_meta)

  expected_cols <- c("Group", "N", "Baseline (% \u00b1 SD)", "Nadir (% BL)", "End-of-study (% \u00b1 SD)")
  testthat::expect_true(all(expected_cols %in% names(stats_tbl)))
  testthat::expect_equal(nrow(stats_tbl), nrow(group_meta))
  testthat::expect_true(all(stats_tbl$N > 0))
})

testthat::test_that("weight_pairwise_tests returns kw_p and pairwise table structure", {
  validation <- example_validation()
  weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))

  result <- weight_pairwise_tests(weights_processed$data, group_meta)
  result_bonf <- weight_pairwise_tests(weights_processed$data, group_meta, p_adjust_method = "bonferroni")

  testthat::expect_true(is.list(result))
  testthat::expect_true("kw_p" %in% names(result))
  testthat::expect_true("pairwise" %in% names(result))

  n_groups <- nrow(group_meta)
  if (n_groups >= 2) {
    testthat::expect_true(is.numeric(result$kw_p))
    testthat::expect_true(is.na(result$kw_p) || (result$kw_p >= 0 && result$kw_p <= 1))
  }
  if (n_groups >= 3 && !is.null(result$pairwise)) {
    testthat::expect_true("Group A" %in% names(result$pairwise))
    testthat::expect_true("Group B" %in% names(result$pairwise))
    testthat::expect_true("p (BH-adj)" %in% names(result$pairwise))
    testthat::expect_true("p (Bonferroni-adj)" %in% names(result_bonf$pairwise))
  }
})

testthat::test_that("weight_key_stats handles empty input gracefully", {
  empty_tbl <- weight_key_stats(NULL, tibble::tibble())
  testthat::expect_equal(nrow(empty_tbl), 0)
})
