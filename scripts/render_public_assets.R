`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "") || all(is.na(x))) y else x
}

find_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)

  repeat {
    if (file.exists(file.path(current, "VERSION")) && dir.exists(file.path(current, "app", "R"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Project root not found.", call. = FALSE)
    }
    current <- parent
  }
}

root <- find_root()
options(labweight.app_root = root)

source_files <- c(
  "utils.R",
  "runtime_config.R",
  "palettes.R",
  "import_csv.R",
  "mapping.R",
  "settings.R",
  "transform_weights.R",
  "transform_scores.R",
  "transform_survival.R",
  "validate.R",
  "plots_weights.R",
  "plots_scores.R",
  "plots_survival.R",
  "downloads.R"
)

invisible(lapply(source_files, function(file) {
  source(file.path(root, "app", "R", file), chdir = TRUE)
}))

weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
survival_import <- read_csv_import(project_path("inst", "templates", "example_survival.csv"))

validation <- validate_study_data(
  raw_weights = weights_import,
  raw_survival = survival_import,
  mapping = build_mapping_bundle(
    weights = guess_field_mapping(weights_import, weights_field_spec()),
    survival = guess_field_mapping(survival_import, survival_field_spec()),
    day_map = default_day_mapping(weights_import),
    survival_day_map = default_survival_day_mapping(survival_import),
    score_day_map = default_score_day_mapping(weights_import)
  )
)

if (length(validation$hard_errors)) {
  stop(sprintf("Weights validation failed: %s", paste(validation$hard_errors, collapse = "; ")), call. = FALSE)
}

if (length(validation$survival_hard_errors)) {
  stop(sprintf("Survival validation failed: %s", paste(validation$survival_hard_errors, collapse = "; ")), call. = FALSE)
}

group_meta <- resolve_group_colors(
  validation$group_meta |>
    dplyr::mutate(custom_color = default_group_palette(dplyr::n())),
  list(color_scheme = "custom")
)

weights_processed <- apply_baseline_rule(validation$clean_weights, "d0_only")
weight_plot <- plot_weights(
  weights_processed$data,
  group_meta,
  metadata = list(
    study_title = "Example longitudinal mouse study",
    weight_units = "g"
  ),
  settings = list(
    mode = "raw",
    individual_layer = "lines",
    show_mean = TRUE,
    error_style = "sem",
    y_axis_mode = "auto",
    start_at_zero = FALSE,
    show_legend_n = TRUE
  )
)

survival_plot <- plot_survival(
  compute_survival_analysis(validation$clean_survival, group_meta),
  metadata = list(study_title = "Example longitudinal mouse study"),
  settings = list(
    show_ci = FALSE,
    show_censor_marks = TRUE,
    show_death_marks = TRUE,
    show_p_value = TRUE,
    x_axis_mode = "auto"
  )
)

out_dir <- file.path(root, "docs", "img")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

save_plot_file(weight_plot, file.path(out_dir, "weight-plot.png"), width = 6.5, height = 4.2, dpi = 300)
save_plot_file(survival_plot, file.path(out_dir, "survival-plot.png"), width = 6.5, height = 4.2, dpi = 300)

cat(
  paste(
    "Rendered plot assets:",
    file.path("docs", "img", "weight-plot.png"),
    file.path("docs", "img", "survival-plot.png"),
    sep = "\n- "
  ),
  "\n",
  sep = ""
)
