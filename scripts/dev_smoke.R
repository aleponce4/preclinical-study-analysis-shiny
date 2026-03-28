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
  "palettes.R",
  "import_csv.R",
  "mapping.R",
  "settings.R",
  "transform_weights.R",
  "transform_survival.R",
  "validate.R",
  "plots_weights.R",
  "plots_survival.R",
  "downloads.R"
)

invisible(lapply(source_files, function(file) {
  source(file.path(root, "app", "R", file), chdir = TRUE)
}))

weights_import <- read_csv_import(project_path("inst", "templates", "example_weights.csv"))
survival_import <- read_csv_import(project_path("inst", "templates", "example_survival.csv"))

validation <- validate_study_data(
  raw_weights = weights_import,
  raw_survival = survival_import,
  mapping = build_mapping_bundle(
    weights = guess_field_mapping(weights_import, weights_field_spec()),
    survival = guess_field_mapping(survival_import, survival_field_spec()),
    day_map = default_day_mapping(weights_import)
  )
)

if (length(validation$hard_errors)) {
  stop(sprintf("Weights validation failed: %s", paste(validation$hard_errors, collapse = "; ")), call. = FALSE)
}

if (length(validation$survival_hard_errors)) {
  stop(sprintf("Survival validation failed: %s", paste(validation$survival_hard_errors, collapse = "; ")), call. = FALSE)
}

weights_processed_raw <- apply_baseline_rule(validation$clean_weights, "d0_only")
weights_processed_pct <- apply_baseline_rule(validation$clean_weights, "first_non_missing")
survival_analysis <- compute_survival_analysis(validation$clean_survival, validation$group_meta)

shared_group_meta <- resolve_group_colors(
  validation$group_meta |>
    dplyr::mutate(custom_color = default_group_palette(dplyr::n())),
  list(color_scheme = "custom")
)

raw_plot <- plot_weights(
  weights_processed_raw$data,
  shared_group_meta,
  list(study_title = "Smoke test", weight_units = "g"),
  settings = list(
    mode = "raw",
    show_individuals = TRUE,
    show_mean = TRUE,
    error_style = "sd",
    y_axis_mode = "auto",
    start_at_zero = TRUE,
    show_legend_n = TRUE
  )
)
pct_plot <- plot_weights(
  weights_processed_pct$data,
  shared_group_meta,
  list(study_title = "Smoke test", weight_units = "g"),
  settings = list(
    mode = "pct_baseline",
    show_individuals = FALSE,
    show_mean = TRUE,
    error_style = "ci95",
    y_axis_mode = "fixed",
    y_min = 70,
    y_max = 130
  )
)
survival_plot <- plot_survival(
  compute_survival_analysis(validation$clean_survival, shared_group_meta),
  list(study_title = "Smoke test"),
  settings = list(
    show_ci = TRUE,
    show_censor_marks = TRUE,
    show_p_value = TRUE,
    x_axis_mode = "manual",
    x_max = 8
  )
)

artifact_dir <- file.path(root, "tests", "_artifacts")
dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)

save_plot_file(raw_plot, file.path(artifact_dir, "weights_raw.png"))
save_plot_file(pct_plot, file.path(artifact_dir, "weights_pct.pdf"))
save_plot_file(survival_plot, file.path(artifact_dir, "survival.png"))

artifact_files <- c(
  file.path(artifact_dir, "weights_raw.png"),
  file.path(artifact_dir, "weights_pct.pdf"),
  file.path(artifact_dir, "survival.png")
)

if (!all(file.exists(artifact_files))) {
  stop("Smoke test did not generate all expected plot files.", call. = FALSE)
}

message("Smoke test completed successfully.")
