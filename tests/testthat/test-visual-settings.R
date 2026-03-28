testthat::test_that("shared palette resolution supports grayscale and custom colors", {
  validation <- example_validation()

  custom <- resolve_group_colors(
    validation$group_meta |>
      dplyr::mutate(custom_color = c("#123456", "#654321")[seq_len(dplyr::n())]),
    list(color_mode = "custom", palette_name = "tableau_10")
  )
  grayscale <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "grayscale"))
  preset <- resolve_group_colors(validation$group_meta, list(color_mode = "preset", palette_name = "brewer_set2"))

  testthat::expect_equal(custom$color, custom$custom_color)
  testthat::expect_true(all(grepl("^#", grayscale$color)))
  testthat::expect_true(all(grepl("^#", preset$color)))
  testthat::expect_false(identical(custom$color, grayscale$color))
})

testthat::test_that("invalid axis settings fall back to auto mode", {
  weight_settings <- prepare_weight_plot_settings(list(y_axis_mode = "fixed", y_min = 10, y_max = 1))
  survival_settings <- prepare_survival_plot_settings(list(y_axis_mode = "fixed", y_min = 80, y_max = 20))

  testthat::expect_equal(weight_settings$settings$y_axis_mode, "auto")
  testthat::expect_true(length(weight_settings$warnings) > 0)
  testthat::expect_equal(survival_settings$settings$y_axis_mode, "auto")
  testthat::expect_true(length(survival_settings$warnings) > 0)
})

testthat::test_that("default plot settings use new control defaults", {
  testthat::expect_equal(default_weight_plot_settings()$individual_layer, "points")
  testthat::expect_equal(default_weight_plot_settings()$mode, "pct_baseline")
  testthat::expect_equal(default_weight_plot_settings()$error_style, "sem")
  testthat::expect_equal(default_weight_plot_settings()$error_display, "band")
  testthat::expect_equal(default_score_plot_settings()$individual_layer, "points")
  testthat::expect_equal(default_score_plot_settings()$error_style, "sem")
  testthat::expect_equal(default_score_plot_settings()$error_display, "band")
  testthat::expect_equal(default_score_plot_settings()$y_axis_mode, "fixed")
  testthat::expect_equal(default_score_plot_settings()$y_min, 0)
  testthat::expect_equal(default_score_plot_settings()$y_max, 10)
  testthat::expect_false(default_survival_plot_settings()$show_ci)
  testthat::expect_false(default_survival_plot_settings()$show_censor_marks)
  testthat::expect_true(default_survival_plot_settings()$nudge_lines)
  testthat::expect_equal(default_survival_plot_settings()$legend_position, "inset_bottom_left")
  testthat::expect_equal(default_survival_plot_settings()$y_axis_mode, "fixed")
  testthat::expect_equal(default_survival_plot_settings()$y_min, 0)
  testthat::expect_equal(default_survival_plot_settings()$y_max, 100)
  testthat::expect_equal(default_shared_style_settings()$color_mode, "preset")
  testthat::expect_equal(default_shared_style_settings()$palette_name, "okabe_ito")
})

testthat::test_that("palette preview helper renders chips", {
  preview <- palette_preview_ui("tableau_10", chips = 6)
  preview_html <- as.character(preview)
  chip_matches <- gregexpr("palette-preview-chip", preview_html, fixed = TRUE)[[1]]

  testthat::expect_true(grepl("palette-preview", preview_html))
  testthat::expect_equal(sum(chip_matches > 0), 6)
})

testthat::test_that("palette gallery helper renders named palette choices", {
  gallery <- palette_gallery_spec()
  gallery_html <- vapply(gallery$choiceNames, as.character, character(1))

  testthat::expect_true(all(c("okabe_ito", "viridis", "tableau_10", "brewer_set2", "grayscale") %in% gallery$choiceValues))
  testthat::expect_true(any(grepl("Okabe-Ito", gallery_html, fixed = TRUE)))
  testthat::expect_true(any(grepl("Viridis", gallery_html, fixed = TRUE)))
  testthat::expect_true(all(grepl("palette-choice-label", gallery_html, fixed = TRUE)))
})

testthat::test_that("validation banner is hidden before load attempts", {
  issues <- list(
    hard_errors = "Missing weights file.",
    survival_hard_errors = character(),
    warnings = "Survival file absent."
  )

  testthat::expect_false(should_render_validation_banner(FALSE, issues))
  testthat::expect_true(should_render_validation_banner(TRUE, issues))
})

testthat::test_that("module conditional panels use unnamespaced expressions with ns prefix", {
  source(project_path("app", "R", "mod_weights.R"), chdir = TRUE)
  source(project_path("app", "R", "mod_scores.R"), chdir = TRUE)
  source(project_path("app", "R", "mod_survival.R"), chdir = TRUE)
  source(project_path("app", "R", "mod_entry.R"), chdir = TRUE)
  source(project_path("app", "R", "mod_import.R"), chdir = TRUE)
  weights_ui <- htmltools::renderTags(mod_weights_ui("weights"))$html
  scores_ui <- htmltools::renderTags(mod_scores_ui("scores"))$html
  survival_ui <- htmltools::renderTags(mod_survival_ui("survival"))$html
  entry_ui <- htmltools::renderTags(mod_entry_ui("entry"))$html
  import_ui <- htmltools::renderTags(mod_import_ui("import"))$html

  testthat::expect_true(any(grepl("input.error_style !==", weights_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("data-ns-prefix=\"weights-\"", weights_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("input.color_mode ===", weights_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("input.error_style !==", scores_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("data-ns-prefix=\"scores-\"", scores_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("y_axis_inputs", survival_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("data-ns-prefix=\"survival-\"", survival_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("control-card-body", weights_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("control-card-body", scores_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("control-card-body", survival_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("Use for analysis", entry_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("Reset Table", entry_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("Add Study Day", entry_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("weights_sheet_ui", entry_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("Study Data File", import_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("Review Scheduled Sampling", import_ui, fixed = TRUE)))
  testthat::expect_false(any(grepl("Survival File", import_ui, fixed = TRUE)))
  testthat::expect_false(any(grepl("Match Survival Columns", import_ui, fixed = TRUE)))
  testthat::expect_false(any(grepl("Build Table", entry_ui, fixed = TRUE)))
  testthat::expect_false(any(grepl("This build is using the built-in editable table for manual entry.", entry_ui, fixed = TRUE)))
})

testthat::test_that("manual entry copy and fallback table labels stay user-friendly", {
  entry_src <- paste(readLines(project_path("app", "R", "mod_entry.R"), warn = FALSE), collapse = "\n")

  testthat::expect_false(grepl("Limited table mode", entry_src, fixed = TRUE))
  testthat::expect_true(grepl("Click 'Use for analysis' to use this table.", entry_src, fixed = TRUE))
  testthat::expect_true(grepl("Mouse ID", entry_src, fixed = TRUE))
})

testthat::test_that("weights error display visibility is not tied to show_mean", {
  source(project_path("app", "R", "mod_weights.R"), chdir = TRUE)
  weights_ui <- htmltools::renderTags(mod_weights_ui("weights"))$html

  testthat::expect_true(any(grepl("Error statistic", weights_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("Error display", weights_ui, fixed = TRUE)))
  testthat::expect_true(any(grepl("input.error_style !==", weights_ui, fixed = TRUE)))
  testthat::expect_false(any(grepl("input.show_mean && input.error_style !==", weights_ui, fixed = TRUE)))
})

testthat::test_that("palette gallery labels include explicit preset names", {
  gallery <- palette_gallery_spec()
  gallery_html <- vapply(gallery$choiceNames, as.character, character(1))

  testthat::expect_true(any(grepl("Okabe-Ito", gallery_html, fixed = TRUE)))
  testthat::expect_true(any(grepl("Viridis", gallery_html, fixed = TRUE)))
  testthat::expect_true(any(grepl("Tableau 10", gallery_html, fixed = TRUE)))
  testthat::expect_true(any(grepl("ColorBrewer Set2", gallery_html, fixed = TRUE)))
  testthat::expect_true(any(grepl("Grayscale", gallery_html, fixed = TRUE)))
  testthat::expect_true(all(grepl("palette-choice-label", gallery_html, fixed = TRUE)))
})

testthat::test_that("changed_named_values returns only updated picker values", {
  current <- list(group_a = "#111111", group_b = "#222222")
  previous <- list(group_a = "#111111", group_b = "#999999")

  result <- changed_named_values(current, previous)

  testthat::expect_equal(result, list(group_b = "#222222"))
})

testthat::test_that("update_group_override_colors only applies changed ids", {
  group_meta <- tibble::tibble(
    group_id = c("group_a", "group_b"),
    display_name = c("Group A", "Group B"),
    plot_order = c(1L, 2L),
    custom_color = c("#111111", "#222222"),
    color = c("#111111", "#222222")
  )

  updated <- update_group_override_colors(group_meta, list(group_b = "#ABCDEF"))

  testthat::expect_equal(updated$custom_color[updated$group_id == "group_a"], "#111111")
  testthat::expect_equal(updated$custom_color[updated$group_id == "group_b"], "#ABCDEF")
})

testthat::test_that("invalid_custom_color_messages only warn in custom mode", {
  group_meta <- tibble::tibble(
    group_id = "group_a",
    display_name = "Group A",
    plot_order = 1L,
    custom_color = "not-a-color",
    color = "#111111"
  )

  testthat::expect_length(
    invalid_custom_color_messages(group_meta, list(color_mode = "preset", palette_name = "okabe_ito")),
    0
  )
  testthat::expect_true(grepl(
    "Invalid custom colors were ignored",
    invalid_custom_color_messages(group_meta, list(color_mode = "custom", palette_name = "okabe_ito"))
  ))
})

testthat::test_that("palette catalog exposes named presets", {
  catalog <- group_palette_catalog()

  testthat::expect_true(all(c("okabe_ito", "viridis", "tableau_10", "brewer_set2", "grayscale") %in% names(catalog)))
  testthat::expect_true(all(vapply(catalog, function(colors) all(grepl("^#[0-9A-F]{6}$", toupper(colors))), logical(1))))
})

testthat::test_that("viridis palette resolves valid discrete colors", {
  colors <- resolve_named_palette("viridis", 12)
  three_colors <- resolve_named_palette("viridis", 3)

  testthat::expect_equal(length(colors), 12)
  testthat::expect_true(all(grepl("^#[0-9A-F]{6}$", toupper(colors))))
  testthat::expect_equal(three_colors, substr(toupper(viridisLite::viridis(3)), 1, 7))
})

testthat::test_that("weight y expansion uses the new autoscale padding", {
  testthat::expect_equal(weight_y_expand(FALSE), c(0.06, 0, 0.10, 0))
  testthat::expect_equal(weight_y_expand(TRUE), c(0.00, 0, 0.10, 0))
})

testthat::test_that("app navbar includes Manual Data Entry and Clinical Scores", {
  withr::local_envvar(c(
    LABAPP_AUTH_MODE = "none",
    LABAPP_CLOUD_MODE = "false",
    LABAPP_STORAGE_MODE = "local_disk",
    LABAPP_LOAD_CREDENTIALS_FILE = "false",
    LABAPP_SHARED_USER = NA_character_,
    LABAPP_SHARED_PASSWORD = NA_character_,
    LABAPP_SHARED_PASSWORD_HASH = NA_character_
  ))

  source(project_path("app", "app.R"), chdir = TRUE)
  app_html <- htmltools::renderTags(ui)$html

  testthat::expect_true(any(grepl("Manual Data Entry", app_html, fixed = TRUE)))
  testthat::expect_true(any(grepl("Clinical Scores", app_html, fixed = TRUE)))
})
