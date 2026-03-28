testthat::test_that("weights and survival plots render", {
  validation <- example_validation()
  weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "grayscale"))
  survival_analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  weights_plot_obj <- plot_weights(
    weights_processed$data,
    group_meta,
    metadata = list(study_title = "Example Study", weight_units = "g"),
    settings = list(
      mode = "raw",
      error_style = "ci95",
      error_display = "bar",
      show_legend_n = TRUE,
      individual_layer = "points",
      start_at_zero = TRUE
    )
  )

  survival_plot_obj <- plot_survival(
    survival_analysis,
    metadata = list(study_title = "Example Study"),
    settings = list(
      show_ci = TRUE,
      show_censor_marks = TRUE,
      show_p_value = FALSE,
      y_axis_mode = "fixed",
      y_min = 0,
      y_max = 100
    )
  )

  testthat::expect_s3_class(weights_plot_obj, "ggplot")
  testthat::expect_s3_class(survival_plot_obj, "ggplot")
  testthat::expect_true(is.finite(survival_analysis$p_value) || is.na(survival_analysis$p_value))
  testthat::expect_false(grepl("Log-rank p", survival_plot_obj$labels$subtitle %||% ""))
})

testthat::test_that("weight plot supports all individual layer modes", {
  validation <- example_validation()
  weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))

  points_plot <- plot_weights(
    weights_processed$data,
    group_meta,
    settings = list(individual_layer = "points", show_mean = TRUE)
  )
  points_only_plot <- plot_weights(
    weights_processed$data,
    group_meta,
    settings = list(individual_layer = "points", show_mean = FALSE, error_style = "sem", error_display = "bar")
  )
  lines_plot <- plot_weights(weights_processed$data, group_meta, settings = list(individual_layer = "lines"))
  none_plot <- plot_weights(weights_processed$data, group_meta, settings = list(individual_layer = "none", show_mean = TRUE))
  points_built <- ggplot2::ggplot_build(points_plot)
  points_only_built <- ggplot2::ggplot_build(points_only_plot)
  points_only_geoms <- vapply(points_only_plot$layers, function(layer) class(layer$geom)[[1]], character(1))

  testthat::expect_s3_class(points_plot, "ggplot")
  testthat::expect_s3_class(points_only_plot, "ggplot")
  testthat::expect_s3_class(lines_plot, "ggplot")
  testthat::expect_s3_class(none_plot, "ggplot")
  testthat::expect_true(all(abs(points_built$data[[1]]$x - round(points_built$data[[1]]$x)) < 1e-9))
  testthat::expect_true(any(abs(points_only_built$data[[1]]$x - round(points_only_built$data[[1]]$x)) > 1e-9))
  testthat::expect_true("GeomErrorbar" %in% points_only_geoms)
})

testthat::test_that("weight plot supports band and error bar displays", {
  validation <- example_validation()
  weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))

  band_plot <- plot_weights(
    weights_processed$data,
    group_meta,
    settings = list(error_style = "sem", error_display = "band", show_mean = TRUE)
  )
  bar_plot <- plot_weights(
    weights_processed$data,
    group_meta,
    settings = list(error_style = "sem", error_display = "bar", show_mean = TRUE)
  )

  band_geoms <- vapply(band_plot$layers, function(layer) class(layer$geom)[[1]], character(1))
  bar_geoms <- vapply(bar_plot$layers, function(layer) class(layer$geom)[[1]], character(1))
  errorbar_layer <- bar_plot$layers[[which(bar_geoms == "GeomErrorbar")[1]]]
  mean_line_layer <- bar_plot$layers[[which(bar_geoms == "GeomLine")[1]]]

  testthat::expect_true("GeomRibbon" %in% band_geoms)
  testthat::expect_true("GeomErrorbar" %in% bar_geoms)
  testthat::expect_true(sum(band_geoms == "GeomLine") >= 1)
  testthat::expect_true(sum(bar_geoms == "GeomLine") >= 1)
  testthat::expect_equal(rlang::as_label(errorbar_layer$mapping$x), "x_mean")
  testthat::expect_equal(rlang::as_label(mean_line_layer$mapping$x), "x_mean")
  testthat::expect_no_warning(ggplot2::ggplot_build(bar_plot))
})

testthat::test_that("scores plot renders across layer and error display modes", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  scores_long <- validation$clean_scores

  line_plot <- plot_scores(
    scores_long,
    group_meta,
    settings = list(individual_layer = "lines", show_mean = TRUE, error_style = "sem", error_display = "band")
  )
  points_plot <- plot_scores(
    scores_long,
    group_meta,
    settings = list(individual_layer = "points", show_mean = FALSE, error_style = "sem", error_display = "bar")
  )

  line_geoms <- vapply(line_plot$layers, function(layer) class(layer$geom)[[1]], character(1))
  points_geoms <- vapply(points_plot$layers, function(layer) class(layer$geom)[[1]], character(1))

  testthat::expect_s3_class(line_plot, "ggplot")
  testthat::expect_s3_class(points_plot, "ggplot")
  testthat::expect_true("GeomRibbon" %in% line_geoms)
  testthat::expect_true("GeomErrorbar" %in% points_geoms)
  testthat::expect_equal(line_plot$labels$y, "Clinical Scores")
  testthat::expect_no_warning(ggplot2::ggplot_build(line_plot))
  testthat::expect_no_warning(ggplot2::ggplot_build(points_plot))
})

testthat::test_that("weight group offsets are symmetric and bounded", {
  one_group <- weight_group_offsets("A")
  two_groups <- weight_group_offsets(c("A", "B"))
  three_groups <- weight_group_offsets(c("A", "B", "C"))
  five_groups <- weight_group_offsets(c("A", "B", "C", "D", "E"))

  testthat::expect_equal(unname(one_group), 0)
  testthat::expect_equal(unname(two_groups), c(-0.05, 0.05), tolerance = 1e-9)
  testthat::expect_equal(unname(three_groups), c(-0.10, 0, 0.10), tolerance = 1e-9)
  testthat::expect_equal(unname(five_groups), -rev(unname(five_groups)), tolerance = 1e-9)
  testthat::expect_equal(unname(five_groups), c(-0.20, -0.10, 0, 0.10, 0.20), tolerance = 1e-9)
})

testthat::test_that("weights and survival default to inset bottom-left legends", {
  validation <- example_validation()
  weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  survival_analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  weights_plot_obj <- plot_weights(weights_processed$data, group_meta)
  survival_plot_obj <- plot_survival(survival_analysis)

  testthat::expect_equal(weights_plot_obj$theme$legend.position.inside, c(0.02, 0.02))
  testthat::expect_equal(weights_plot_obj$theme$legend.justification, c(0, 0))
  testthat::expect_equal(survival_plot_obj$theme$legend.position.inside, c(0.02, 0.02))
  testthat::expect_equal(survival_plot_obj$theme$legend.justification, c(0, 0))
})

testthat::test_that("explicit inset legend positions are available", {
  validation <- example_validation()
  weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  survival_analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  top_right_weights <- plot_weights(
    weights_processed$data,
    group_meta,
    settings = list(legend_position = "inset_top_right")
  )
  bottom_left_survival <- plot_survival(
    survival_analysis,
    settings = list(legend_position = "inset_bottom_left")
  )

  testthat::expect_equal(top_right_weights$theme$legend.position.inside, c(0.98, 0.98))
  testthat::expect_equal(top_right_weights$theme$legend.justification, c(1, 1))
  testthat::expect_equal(bottom_left_survival$theme$legend.position.inside, c(0.02, 0.02))
  testthat::expect_equal(bottom_left_survival$theme$legend.justification, c(0, 0))
})

testthat::test_that("survival plot nudge_lines offsets groups without error", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  survival_analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  nudge_plot <- plot_survival(
    survival_analysis,
    metadata = list(study_title = "Example Study"),
    settings = list(nudge_lines = TRUE, show_ci = TRUE)
  )

  testthat::expect_s3_class(nudge_plot, "ggplot")
  testthat::expect_no_warning(ggplot2::ggplot_build(nudge_plot))
})

testthat::test_that("weight and survival x-axes label each DPI", {
  validation <- example_validation()
  weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  survival_analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  weights_plot_obj <- plot_weights(weights_processed$data, group_meta)
  survival_plot_obj <- plot_survival(survival_analysis)

  weight_x <- weights_plot_obj$scales$get_scales("x")
  survival_x <- survival_plot_obj$scales$get_scales("x")

  testthat::expect_equal(weights_plot_obj$labels$x, "DPI")
  testthat::expect_equal(survival_plot_obj$labels$x, "DPI")
  testthat::expect_equal(weight_x$breaks, 0:15)
  testthat::expect_equal(survival_x$breaks, 0:15)
})

testthat::test_that("survival plot keeps one step group per group_id", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  plot_obj <- plot_survival(analysis, settings = list(show_ci = FALSE))
  built <- ggplot2::ggplot_build(plot_obj)
  expected_groups <- dplyr::n_distinct(analysis$curve_data$group_id)

  testthat::expect_equal(length(unique(built$data[[1]]$group)), expected_groups)
  testthat::expect_equal(length(unique(built$data[[1]]$colour)), expected_groups)
})

testthat::test_that("survfit_to_tibble keeps no-event groups through max follow-up", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  group_counts <- analysis$curve_data |>
    dplyr::count(.data$group_id, name = "rows")

  max_followup <- validation$clean_survival |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(max_time = max(.data$time, na.rm = TRUE), .groups = "drop")

  end_rows <- analysis$curve_data |>
    dplyr::left_join(max_followup, by = "group_id") |>
    dplyr::filter(.data$time == .data$max_time)

  testthat::expect_equal(dplyr::n_distinct(analysis$curve_data$group_id), 4)
  testthat::expect_true(all(group_counts$rows >= 2))
  testthat::expect_equal(dplyr::n_distinct(end_rows$group_id), 4)
})

testthat::test_that("survfit_to_tibble preserves flat curves for fully censored groups", {
  survival_data <- tibble::tibble(
    animal_id = as.character(seq_len(6)),
    group_id = c("g1", "g1", "g2", "g2", "g3", "g3"),
    group_label = c("Group 1", "Group 1", "Group 2", "Group 2", "Group 3", "Group 3"),
    study_id = "study",
    time = c(4L, 7L, 7L, 7L, 7L, 7L),
    status = c(1L, 0L, 0L, 0L, 0L, 0L),
    censored = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    event_type = c("event", NA, NA, NA, NA, NA),
    notes = NA_character_
  )
  group_meta <- tibble::tibble(
    group_id = c("g1", "g2", "g3"),
    display_name = c("Group 1", "Group 2", "Group 3"),
    plot_order = 1:3,
    custom_color = c("#E69F00", "#56B4E9", "#009E73"),
    color = c("#E69F00", "#56B4E9", "#009E73")
  )

  analysis <- compute_survival_analysis(survival_data, group_meta)
  censored_groups <- analysis$curve_data |>
    dplyr::filter(.data$group_id %in% c("g2", "g3")) |>
    dplyr::count(.data$group_id, name = "rows")

  plot_obj <- plot_survival(analysis, settings = list(show_ci = FALSE))
  built <- ggplot2::ggplot_build(plot_obj)
  distinct_x <- built$data[[1]] |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(n_x = dplyr::n_distinct(.data$x), .groups = "drop")

  testthat::expect_true(all(censored_groups$rows >= 2))
  testthat::expect_true(all(
    analysis$curve_data$surv[analysis$curve_data$group_id %in% c("g2", "g3")] == 1
  ))
  testthat::expect_true(all(distinct_x$n_x >= 2))
})

testthat::test_that("survival plot does not nudge overlapping curves unless enabled", {
  base_curve <- tibble::tibble(
    time = c(0, 3, 6, 9),
    surv = c(1, 0.75, 0.5, 0.25),
    lower = c(1, 0.65, 0.4, 0.15),
    upper = c(1, 0.85, 0.6, 0.35),
    plot_order = 1L
  )
  analysis <- list(
    curve_data = dplyr::bind_rows(
      dplyr::mutate(base_curve, group_id = "g1", display_name = "Group 1", color = "#E69F00"),
      dplyr::mutate(base_curve, group_id = "g2", display_name = "Group 2", color = "#56B4E9"),
      dplyr::mutate(base_curve, group_id = "g3", display_name = "Group 3", color = "#009E73")
    ) |>
      dplyr::group_by(.data$group_id) |>
      dplyr::mutate(plot_order = dplyr::cur_group_id()) |>
      dplyr::ungroup(),
    censor_data = tibble::tibble(
      strata = character(),
      time = numeric(),
      surv = numeric(),
      n_censor = integer(),
      group_id = character(),
      display_name = character(),
      color = character(),
      plot_order = integer()
    ),
    p_value = NA_real_
  )

  plot_obj <- plot_survival(analysis, settings = list(show_ci = FALSE, nudge_lines = FALSE))
  built <- ggplot2::ggplot_build(plot_obj)

  y_at_three <- built$data[[1]] |>
    dplyr::filter(.data$x == 3) |>
    dplyr::pull(.data$y)

  testthat::expect_true(survival_has_overlapping_curves(analysis$curve_data))
  testthat::expect_equal(length(unique(built$data[[1]]$group)), 3)
  testthat::expect_equal(length(unique(y_at_three)), 1)
})

testthat::test_that("survival plot allows duplicate display names without collapsing groups", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  group_meta$display_name[] <- "Same Label"
  analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  plot_obj <- plot_survival(analysis, settings = list(show_ci = FALSE))
  built <- ggplot2::ggplot_build(plot_obj)
  expected_groups <- dplyr::n_distinct(analysis$curve_data$group_id)

  testthat::expect_equal(length(unique(built$data[[1]]$group)), expected_groups)
  testthat::expect_no_warning(ggplot2::ggplot_build(plot_obj))
})

testthat::test_that("survival confidence bands remain separated by group_id", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  group_meta$display_name[] <- "Same Label"
  analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  plot_obj <- plot_survival(analysis, settings = list(show_ci = TRUE))
  built <- ggplot2::ggplot_build(plot_obj)
  expected_groups <- dplyr::n_distinct(analysis$curve_data$group_id)

  testthat::expect_true("GeomRibbon" %in% vapply(plot_obj$layers, function(layer) class(layer$geom)[[1]], character(1)))
  testthat::expect_equal(length(unique(built$data[[1]]$group)), expected_groups)
  testthat::expect_no_warning(ggplot2::ggplot_build(plot_obj))
})

testthat::test_that("survival nudge_lines still preserves distinct groups with duplicate labels", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  group_meta$display_name[] <- "Same Label"
  analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  plot_obj <- plot_survival(analysis, settings = list(nudge_lines = TRUE, show_ci = FALSE))
  built <- ggplot2::ggplot_build(plot_obj)
  expected_groups <- dplyr::n_distinct(analysis$curve_data$group_id)

  testthat::expect_equal(length(unique(built$data[[1]]$group)), expected_groups)
  testthat::expect_no_warning(ggplot2::ggplot_build(plot_obj))
})

testthat::test_that("survival plot does not add a fill scale when confidence bands are off", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  survival_analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  no_ci_plot <- plot_survival(
    survival_analysis,
    settings = list(show_ci = FALSE)
  )

  testthat::expect_null(no_ci_plot$labels$fill)
  testthat::expect_no_warning(ggplot2::ggplot_build(no_ci_plot))
})

testthat::test_that("survival plot example groups each span more than one x position", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  plot_obj <- plot_survival(analysis, settings = list(show_ci = FALSE))
  built <- ggplot2::ggplot_build(plot_obj)
  x_counts <- built$data[[1]] |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(n_x = dplyr::n_distinct(.data$x), .groups = "drop")

  testthat::expect_true(all(x_counts$n_x >= 2))
})

testthat::test_that("survival censor marks stay out of the legend", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  survival_analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  plot_obj <- plot_survival(survival_analysis, settings = list(show_censor_marks = TRUE))
  point_layers <- which(vapply(plot_obj$layers, function(layer) class(layer$geom)[[1]], character(1)) == "GeomPoint")

  testthat::expect_true(length(point_layers) >= 1)
  testthat::expect_false(isTRUE(plot_obj$layers[[point_layers[[1]]]]$show.legend))
})

testthat::test_that("survival_summary_table returns enriched columns", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  tbl <- analysis$summary
  required_cols <- c("Group", "N", "Deaths", "Censored", "Median (DPI)", "Median CI low", "Median CI high")

  testthat::expect_true(all(required_cols %in% names(tbl)))
  testthat::expect_true(all(tbl$N >= 0))
  testthat::expect_true(all(tbl$Deaths + tbl$Censored == tbl$N))
})

testthat::test_that("scheduled sampling rows are excluded from KM summary and reported separately", {
  survival_data <- tibble::tibble(
    animal_id = c("s1", "s2", "c1", "c2"),
    group_id = c("A", "A", "A", "A"),
    group_label = c("Group A", "Group A", "Group A", "Group A"),
    study_id = c("S1", "S1", "S1", "S1"),
    time = c(7L, 8L, 6L, 3L),
    status = c(1L, 0L, 0L, 1L),
    censored = c(FALSE, TRUE, TRUE, FALSE),
    event_type = c("inferred_death", NA, "scheduled_sampling_excluded", "scheduled_cohort_death_excluded"),
    notes = NA_character_,
    cohort_role = c("survival", "survival", "scheduled_sampling", "scheduled_sampling"),
    include_in_km = c(TRUE, TRUE, FALSE, FALSE)
  )
  meta <- tibble::tibble(
    group_id = "A",
    display_name = "Group A",
    plot_order = 1L,
    color = "#0072B2",
    custom_color = NA_character_
  )

  analysis <- compute_survival_analysis(survival_data, meta)

  testthat::expect_true(analysis$valid)
  testthat::expect_equal(analysis$scheduled_sampling_handling, "exclude")
  testthat::expect_equal(analysis$summary$N[[1]], 2L)
  testthat::expect_equal(analysis$summary$Deaths[[1]], 1L)
  testthat::expect_equal(analysis$scheduled_summary$`Scheduled cohort rows`[[1]], 2L)
  testthat::expect_equal(analysis$excluded_summary$`Excluded from KM`[[1]], 2L)
  testthat::expect_equal(analysis$excluded_summary$`Planned scheduled sampling`[[1]], 1L)
  testthat::expect_equal(analysis$excluded_summary$`Deaths before planned collection`[[1]], 1L)
})

testthat::test_that("scheduled sampling rows can be included as censored", {
  survival_data <- tibble::tibble(
    animal_id = c("s1", "s2", "c1", "c2"),
    group_id = c("A", "A", "A", "A"),
    group_label = c("Group A", "Group A", "Group A", "Group A"),
    study_id = c("S1", "S1", "S1", "S1"),
    time = c(7L, 8L, 6L, 3L),
    status = c(1L, 0L, 0L, 1L),
    censored = c(FALSE, TRUE, TRUE, FALSE),
    event_type = c("inferred_death", NA, "scheduled_sampling_excluded", "scheduled_cohort_death_excluded"),
    notes = NA_character_,
    cohort_role = c("survival", "survival", "scheduled_sampling", "scheduled_sampling"),
    include_in_km = c(TRUE, TRUE, FALSE, FALSE)
  )
  meta <- tibble::tibble(
    group_id = "A",
    display_name = "Group A",
    plot_order = 1L,
    color = "#0072B2",
    custom_color = NA_character_
  )

  analysis <- compute_survival_analysis(
    survival_data,
    meta,
    scheduled_sampling_handling = "censor"
  )

  testthat::expect_true(analysis$valid)
  testthat::expect_equal(analysis$scheduled_sampling_handling, "censor")
  testthat::expect_equal(analysis$summary$N[[1]], 4L)
  testthat::expect_equal(analysis$summary$Deaths[[1]], 1L)
  testthat::expect_equal(analysis$summary$Censored[[1]], 3L)
  testthat::expect_equal(nrow(analysis$excluded_summary), 0L)
  testthat::expect_equal(analysis$scheduled_summary$`Deaths before planned collection`[[1]], 1L)
})

testthat::test_that("scheduled sampling rows can be counted as deaths", {
  survival_data <- tibble::tibble(
    animal_id = c("s1", "s2", "c1", "c2"),
    group_id = c("A", "A", "A", "A"),
    group_label = c("Group A", "Group A", "Group A", "Group A"),
    study_id = c("S1", "S1", "S1", "S1"),
    time = c(7L, 8L, 6L, 3L),
    status = c(1L, 0L, 0L, 1L),
    censored = c(FALSE, TRUE, TRUE, FALSE),
    event_type = c("inferred_death", NA, "scheduled_sampling_excluded", "scheduled_cohort_death_excluded"),
    notes = NA_character_,
    cohort_role = c("survival", "survival", "scheduled_sampling", "scheduled_sampling"),
    include_in_km = c(TRUE, TRUE, FALSE, FALSE)
  )
  meta <- tibble::tibble(
    group_id = "A",
    display_name = "Group A",
    plot_order = 1L,
    color = "#0072B2",
    custom_color = NA_character_
  )

  analysis <- compute_survival_analysis(
    survival_data,
    meta,
    scheduled_sampling_handling = "death"
  )

  testthat::expect_true(analysis$valid)
  testthat::expect_equal(analysis$scheduled_sampling_handling, "death")
  testthat::expect_equal(analysis$summary$N[[1]], 4L)
  testthat::expect_equal(analysis$summary$Deaths[[1]], 3L)
  testthat::expect_equal(analysis$summary$Censored[[1]], 1L)
  testthat::expect_equal(nrow(analysis$excluded_summary), 0L)
})

testthat::test_that("GraphPad survival export follows scheduled sampling handling mode", {
  survival_data <- tibble::tibble(
    animal_id = c("s1", "c1"),
    group_id = c("A", "A"),
    group_label = c("Group A", "Group A"),
    study_id = c("S1", "S1"),
    time = c(7L, 6L),
    status = c(1L, 0L),
    censored = c(FALSE, TRUE),
    event_type = c("inferred_death", "scheduled_sampling_excluded"),
    notes = NA_character_,
    cohort_role = c("survival", "scheduled_sampling"),
    include_in_km = c(TRUE, FALSE)
  )
  meta <- tibble::tibble(
    group_id = "A",
    display_name = "Group A",
    plot_order = 1L,
    color = "#0072B2",
    custom_color = NA_character_
  )

  excluded_csv <- graphpad_survival_csv(
    survival_data,
    meta,
    scheduled_sampling_handling = "exclude"
  )
  censored_csv <- graphpad_survival_csv(
    survival_data,
    meta,
    scheduled_sampling_handling = "censor"
  )

  testthat::expect_equal(nrow(excluded_csv), 1L)
  testthat::expect_equal(nrow(censored_csv), 2L)
  testthat::expect_equal(sort(stats::na.omit(censored_csv$`Group A`)), c(0L, 1L))
})

testthat::test_that("compute_pairwise_survdiff returns NULL for fewer than 3 groups", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  analysis <- compute_survival_analysis(validation$clean_survival, group_meta)

  n_groups <- dplyr::n_distinct(validation$clean_survival$group_id)
  if (n_groups < 3) {
    testthat::expect_null(analysis$pairwise_tests)
  } else {
    testthat::expect_true(is.data.frame(analysis$pairwise_tests))
    testthat::expect_true(all(c("Group A", "Group B", "p (raw)", "p (BH-adj)") %in% names(analysis$pairwise_tests)))
  }
})

testthat::test_that("compute_pairwise_survdiff works correctly with 3-group data", {
  surv3 <- tibble::tibble(
    animal_id = as.character(1:12),
    group_id  = rep(c("A", "B", "C"), each = 4),
    time      = c(3, 5, 7, 10,  2, 4, 6, 8,  8, 9, 10, 10),
    status    = c(1, 1, 1, 0,   1, 1, 1, 0,  1, 0, 0,  0)
  )
  meta3 <- tibble::tibble(
    group_id     = c("A", "B", "C"),
    display_name = c("Group A", "Group B", "Group C"),
    plot_order   = 1:3,
    color        = c("#E69F00", "#56B4E9", "#009E73"),
    custom_color = NA_character_
  )

  result <- compute_pairwise_survdiff(surv3, meta3)
  result_bonf <- compute_pairwise_survdiff(surv3, meta3, p_adjust_method = "bonferroni")

  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(nrow(result), 3)  # 3 pairs from 3 groups
  testthat::expect_true(all(c("Group A", "Group B", "p (raw)", "p (BH-adj)") %in% names(result)))
  testthat::expect_true(all(c("Group A", "Group B", "p (raw)", "p (Bonferroni-adj)") %in% names(result_bonf)))
  testthat::expect_true(all(result[["p (raw)"]] >= 0 & result[["p (raw)"]] <= 1))
  testthat::expect_true(all(result[["p (BH-adj)"]] >= 0 & result[["p (BH-adj)"]] <= 1))
  testthat::expect_true(all(result_bonf[["p (Bonferroni-adj)"]] >= 0 & result_bonf[["p (Bonferroni-adj)"]] <= 1))
})

testthat::test_that("score pairwise tests support bonferroni adjustment", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))

  result <- score_pairwise_tests(validation$clean_scores, group_meta, p_adjust_method = "bonferroni")
  if (!is.null(result$pairwise)) {
    testthat::expect_true("p (Bonferroni-adj)" %in% names(result$pairwise))
    testthat::expect_true(all(result$pairwise[["p (Bonferroni-adj)"]] >= 0 & result$pairwise[["p (Bonferroni-adj)"]] <= 1))
  }
})

testthat::test_that("survival y-axis uses 25% major breaks and minor breaks", {
  validation <- example_validation()
  group_meta <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "okabe_ito"))
  survival_analysis <- compute_survival_analysis(validation$clean_survival, group_meta)
  survival_plot_obj <- plot_survival(survival_analysis, settings = list(y_axis_mode = "fixed", y_min = 0, y_max = 100))

  survival_y <- survival_plot_obj$scales$get_scales("y")
  testthat::expect_equal(survival_y$breaks, c(0, 0.25, 0.5, 0.75, 1))
  testthat::expect_true(length(survival_y$minor_breaks) > 0)
})
