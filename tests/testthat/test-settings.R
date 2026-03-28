testthat::test_that("settings functions save and load presets in an isolated directory", {
  withr::local_envvar(c(
    LABAPP_CLOUD_MODE = "false",
    LABAPP_STORAGE_MODE = "local_disk",
    LABAPP_AUTH_MODE = "none",
    LABAPP_SHARED_USER = NA_character_,
    LABAPP_SHARED_PASSWORD = NA_character_,
    LABAPP_SHARED_PASSWORD_HASH = NA_character_
  ))

  original <- getOption("labweight.settings_dir")
  temp_settings <- tempfile("settings-")
  options(labweight.settings_dir = temp_settings)
  withr::defer(options(labweight.settings_dir = original))

  save_mapping_preset(
    name = "Lab preset",
    preset = list(
      weights = list(animal_id = "animal_id", group = "group"),
      survival = list(animal_id = "animal_id", event_day = "event_day", censored = "censored"),
      day_map = tibble::tibble(source_label = "D0 [d0]", source_column = "d0", day = 0L, include = TRUE),
      metadata = list(study_title = "Test"),
      infer_survival = FALSE,
      detect_scheduled_sampling = TRUE,
      scheduled_sampling_cages = tibble::tibble(
        cage_card = "449074",
        role = "scheduled_sampling",
        censor_day = 6L
      ),
      direct_entry = list(
        study_name = "Study-X",
        n_groups = 2L,
        mice_per_group = 3L,
        group_names = c("Control", "Drug"),
        mouse_labels = list(
          g1 = c("M1", "M2", "M3"),
          g2 = c("M1", "M2", "M3")
        ),
        dpi_rows = c(0L, 7L)
      ),
      group_overrides = tibble::tibble(
        group_id = "control",
        display_name = "Control",
        plot_order = 1L,
        custom_color = "#123456"
      ),
      shared_style = list(color_mode = "custom", palette_name = "tableau_10"),
      weight_plot = list(error_style = "sd", error_display = "bar", show_legend_n = TRUE, individual_layer = "points"),
      score_plot = list(error_style = "ci95", error_display = "bar", y_axis_mode = "fixed", y_min = 0, y_max = 8),
      survival_analysis = list(scheduled_sampling_handling = "censor"),
      survival_plot = list(show_p_value = FALSE, nudge_lines = TRUE, y_axis_mode = "fixed", y_min = 0, y_max = 90)
    )
  )

  presets <- list_mapping_presets()
  loaded <- load_mapping_preset("Lab preset")

  testthat::expect_true("Lab preset" %in% presets)
  testthat::expect_equal(loaded$metadata$study_title, "Test")
  testthat::expect_false(loaded$infer_survival)
  testthat::expect_equal(loaded$direct_entry$study_name, "Study-X")
  testthat::expect_equal(loaded$direct_entry$n_groups, 2L)
  testthat::expect_equal(loaded$direct_entry$mice_per_group, 3L)
  testthat::expect_equal(loaded$direct_entry$dpi_rows, c(0L, 7L))
  testthat::expect_equal(loaded$group_overrides$custom_color[[1]], "#123456")
  testthat::expect_equal(loaded$shared_style$color_mode, "custom")
  testthat::expect_equal(loaded$shared_style$palette_name, "tableau_10")
  testthat::expect_equal(loaded$weight_plot$error_style, "sd")
  testthat::expect_equal(loaded$weight_plot$error_display, "bar")
  testthat::expect_equal(loaded$weight_plot$individual_layer, "points")
  testthat::expect_true(loaded$weight_plot$show_legend_n)
  testthat::expect_equal(loaded$score_plot$error_style, "ci95")
  testthat::expect_equal(loaded$score_plot$error_display, "bar")
  testthat::expect_equal(loaded$score_plot$y_max, 8)
  testthat::expect_equal(loaded$survival_analysis$scheduled_sampling_handling, "censor")
  testthat::expect_false(loaded$survival_plot$show_p_value)
  testthat::expect_true(loaded$survival_plot$nudge_lines)
  testthat::expect_equal(loaded$survival_plot$y_max, 90)
  testthat::expect_false(loaded$infer_death_events)
  testthat::expect_true(loaded$detect_scheduled_sampling)
  testthat::expect_equal(loaded$scheduled_sampling_cages$cage_card[[1]], "449074")
  testthat::expect_equal(loaded$scheduled_sampling_cages$censor_day[[1]], 6L)

  delete_mapping_preset("Lab preset")
  testthat::expect_false("Lab preset" %in% list_mapping_presets())
})

testthat::test_that("legacy shared style and individual settings normalize forward", {
  shared_style <- normalize_shared_style_settings(list(color_scheme = "grayscale"))
  custom_style <- normalize_shared_style_settings(list(color_scheme = "custom"))
  weight_lines <- normalize_weight_plot_settings(list(show_individuals = TRUE))
  weight_none <- normalize_weight_plot_settings(list(show_individuals = FALSE))

  testthat::expect_equal(shared_style$color_mode, "preset")
  testthat::expect_equal(shared_style$palette_name, "grayscale")
  testthat::expect_equal(custom_style$color_mode, "custom")
  testthat::expect_equal(weight_lines$individual_layer, "lines")
  testthat::expect_equal(weight_none$individual_layer, "none")
})

testthat::test_that("scheduled sampling roles accept friendly labels and YAML-style row lists", {
  normalized <- normalize_scheduled_sampling_cages(list(
    list(cage_card = "449074", role = "Scheduled sampling", censor_day = 6),
    list(cage_card = "449075", role = "survival", censor_day = NA)
  ))

  testthat::expect_equal(normalized$cage_card, c("449074", "449075"))
  testthat::expect_equal(normalized$role, c("scheduled_sampling", "survival"))
  testthat::expect_equal(normalized$censor_day[[1]], 6L)
  testthat::expect_true(is.na(normalized$censor_day[[2]]))
  testthat::expect_equal(
    scheduled_sampling_role_label(c("scheduled_sampling", "survival")),
    c("Scheduled sampling", "Survival")
  )
})

testthat::test_that("stats settings default to BH and accept bonferroni", {
  default_stats <- normalize_stats_settings(list())
  bonf_stats <- normalize_stats_settings(list(p_adjust_method = "bonferroni"))
  fallback_stats <- normalize_stats_settings(list(p_adjust_method = "invalid"))

  testthat::expect_equal(default_stats$p_adjust_method, "bh")
  testthat::expect_equal(bonf_stats$p_adjust_method, "bonferroni")
  testthat::expect_equal(fallback_stats$p_adjust_method, "bh")
})

testthat::test_that("scheduled sampling handling defaults to exclude and normalizes friendly values", {
  default_analysis <- normalize_survival_analysis_settings(list())
  censor_analysis <- normalize_survival_analysis_settings(list(scheduled_sampling_handling = "include_as_censored"))
  death_analysis <- normalize_survival_analysis_settings(list(scheduled_sampling_handling = "Count as deaths"))

  testthat::expect_equal(default_analysis$scheduled_sampling_handling, "exclude")
  testthat::expect_equal(censor_analysis$scheduled_sampling_handling, "censor")
  testthat::expect_equal(death_analysis$scheduled_sampling_handling, "death")
  testthat::expect_equal(scheduled_sampling_handling_label("death"), "Count as deaths")
})

testthat::test_that("legacy survival show_individuals is ignored safely", {
  normalized <- normalize_survival_plot_settings(list(show_ci = TRUE, show_individuals = TRUE))

  testthat::expect_true(normalized$show_ci)
  testthat::expect_false("show_individuals" %in% names(normalized))
})

testthat::test_that("legacy inset legend settings normalize to plot-specific inset corners", {
  weight_settings <- normalize_weight_plot_settings(list(legend_position = "inset"))
  survival_settings <- normalize_survival_plot_settings(list(legend_position = "inset"))

  testthat::expect_equal(weight_settings$legend_position, "inset_bottom_left")
  testthat::expect_equal(survival_settings$legend_position, "inset_top_right")
})

testthat::test_that("missing weight error display defaults to band", {
  normalized <- normalize_weight_plot_settings(list(error_style = "sd"))

  testthat::expect_equal(normalized$error_display, "band")
})

testthat::test_that("missing score error display defaults to band", {
  normalized <- normalize_score_plot_settings(list(error_style = "sd"))

  testthat::expect_equal(normalized$error_display, "band")
})

testthat::test_that("default weight settings prefer points and confidence bands", {
  normalized <- normalize_weight_plot_settings(list())

  testthat::expect_equal(normalized$mode, "pct_baseline")
  testthat::expect_equal(normalized$individual_layer, "points")
  testthat::expect_equal(normalized$error_style, "sem")
  testthat::expect_equal(normalized$error_display, "band")
})

testthat::test_that("weight settings preserve error controls when mean is off", {
  normalized <- normalize_weight_plot_settings(list(
    show_mean = FALSE,
    error_style = "ci95",
    error_display = "bar"
  ))

  testthat::expect_false(normalized$show_mean)
  testthat::expect_equal(normalized$error_style, "ci95")
  testthat::expect_equal(normalized$error_display, "bar")
})

testthat::test_that("app state round-trips direct entry configuration without sheet contents", {
  withr::local_envvar(c(
    LABAPP_CLOUD_MODE = "false",
    LABAPP_STORAGE_MODE = "local_disk",
    LABAPP_AUTH_MODE = "none"
  ))

  original <- getOption("labweight.settings_dir")
  temp_settings <- tempfile("settings-")
  options(labweight.settings_dir = temp_settings)
  withr::defer(options(labweight.settings_dir = original))

  save_app_state(list(
    metadata = list(study_title = "Direct Entry Study"),
    infer_survival = FALSE,
    detect_scheduled_sampling = TRUE,
    scheduled_sampling_cages = tibble::tibble(
      cage_card = "449074",
      role = "scheduled_sampling",
      censor_day = 6L
    ),
    direct_entry = list(
      study_name = "Direct Entry Study",
      n_groups = 2L,
      mice_per_group = 2L,
      group_names = c("Arm A", "Arm B"),
      mouse_labels = list(g1 = c("M1", "M2"), g2 = c("M1", "M2")),
      dpi_rows = c(0L, 3L)
    ),
    group_overrides = empty_group_overrides(),
    shared_style = default_shared_style_settings(),
    survival_analysis = list(scheduled_sampling_handling = "death"),
    weight_plot = default_weight_plot_settings(),
    score_plot = default_score_plot_settings(),
    survival_plot = default_survival_plot_settings(),
    weights_name = "Paste from Excel",
    survival_name = ""
  ))

  loaded <- load_app_state()

  testthat::expect_equal(loaded$metadata$study_title, "Direct Entry Study")
  testthat::expect_false(loaded$infer_survival)
  testthat::expect_false(loaded$infer_death_events)
  testthat::expect_true(loaded$detect_scheduled_sampling)
  testthat::expect_equal(loaded$scheduled_sampling_cages$cage_card[[1]], "449074")
  testthat::expect_equal(loaded$direct_entry$study_name, "Direct Entry Study")
  testthat::expect_equal(loaded$direct_entry$group_names, c("Arm A", "Arm B"))
  testthat::expect_equal(loaded$direct_entry$dpi_rows, c(0L, 3L))
  testthat::expect_equal(loaded$stats$p_adjust_method, "bh")
  testthat::expect_equal(loaded$survival_analysis$scheduled_sampling_handling, "death")
  testthat::expect_equal(loaded$score_plot$y_axis_mode, "fixed")
  testthat::expect_equal(loaded$score_plot$y_max, 10)
  testthat::expect_false("sheet" %in% names(loaded$direct_entry))
})

testthat::test_that("legacy direct entry settings normalize cleanly", {
  normalized <- normalize_direct_entry_settings(list(
    study_name = "Legacy Study",
    n_groups = 2,
    mice_per_group = 2,
    group_names = c(NA_character_, "Arm B"),
    mouse_labels = list(g1 = c(NA_character_, ""), g2 = c("m1", "m2")),
    dpi_rows = c(0L, NA_integer_, 3L, 3L)
  ))

  testthat::expect_equal(normalized$study_name, "Legacy Study")
  testthat::expect_equal(normalized$n_groups, 2L)
  testthat::expect_equal(normalized$mice_per_group, 2L)
  testthat::expect_equal(normalized$group_names, c("Group 1", "Arm B"))
  testthat::expect_equal(normalized$mouse_labels$g1, c("M1", "M2"))
  testthat::expect_equal(normalized$dpi_rows, c(0L, 3L))
})

testthat::test_that("old direct entry schema converts to new defaults", {
  normalized <- normalize_direct_entry_settings(list(
    base_headers = list(animal_id = "Mouse ID", group = "Arm"),
    day_columns = tibble::tibble(
      key = c("d0", "d7"),
      day = c(0L, 7L),
      label = c(NA_character_, "")
    )
  ))

  testthat::expect_equal(normalized$n_groups, default_direct_entry_settings()$n_groups)
  testthat::expect_equal(normalized$mice_per_group, default_direct_entry_settings()$mice_per_group)
  testthat::expect_equal(normalized$dpi_rows, c(0L, 7L))
})

testthat::test_that("session storage mode disables preset and app-state persistence", {
  withr::local_envvar(c(
    LABAPP_CLOUD_MODE = "true",
    LABAPP_STORAGE_MODE = "session",
    LABAPP_AUTH_MODE = "none",
    LABAPP_SHARED_USER = NA_character_,
    LABAPP_SHARED_PASSWORD = NA_character_,
    LABAPP_SHARED_PASSWORD_HASH = NA_character_
  ))

  original <- getOption("labweight.settings_dir")
  temp_settings <- tempfile("settings-session-")
  options(labweight.settings_dir = temp_settings)
  withr::defer(options(labweight.settings_dir = original))

  save_mapping_preset(
    name = "Session preset",
    preset = list(metadata = list(study_title = "Cloud Session"))
  )
  testthat::expect_equal(list_mapping_presets(), character())
  testthat::expect_null(load_mapping_preset("Session preset"))

  save_app_state(list(metadata = list(study_title = "No persist")))
  loaded_state <- load_app_state()
  testthat::expect_equal(loaded_state, list())

  state_path <- file.path(temp_settings, "session_state.rds")
  testthat::expect_false(file.exists(state_path))
})
