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

options(labweight.app_root = find_root())

source_files <- c(
  "utils.R",
  "runtime_config.R",
  "palettes.R",
  "import_csv.R",
  "mapping.R",
  "settings.R",
  "direct_entry.R",
  "transform_weights.R",
  "transform_scores.R",
  "transform_survival.R",
  "validate.R",
  "plots_weights.R",
  "plots_scores.R",
  "plots_survival.R",
  "downloads.R"
)

source(file.path(getOption("labweight.app_root"), "app", "R", "utils.R"), chdir = TRUE)

invisible(lapply(source_files[-1], function(file) {
  source(project_path("app", "R", file), chdir = TRUE)
}))

example_validation <- function(infer_survival = FALSE) {
  weights_import  <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  survival_import <- read_csv_import(project_path("inst", "templates", "example_survival.csv"))

  validate_study_data(
    raw_weights  = weights_import,
    raw_survival = survival_import,
    mapping = build_mapping_bundle(
      weights          = guess_field_mapping(weights_import,  weights_field_spec()),
      survival         = guess_field_mapping(survival_import, survival_field_spec()),
      day_map          = default_day_mapping(weights_import),
      survival_day_map = default_survival_day_mapping(survival_import),
      score_day_map    = default_score_day_mapping(weights_import)
    ),
    infer_survival = infer_survival
  )
}
