settings_root <- function() {
  ensure_dir(getOption(
    "labweight.settings_dir",
    rappdirs::user_config_dir("lab-weight-survival-app")
  ))
}

empty_group_meta <- function() {
  tibble::tibble(
    group_id = character(),
    display_name = character(),
    plot_order = integer(),
    custom_color = character(),
    color = character()
  )
}

empty_clean_weights <- function() {
  tibble::tibble(
    animal_id = character(),
    group_id = character(),
    group_label = character(),
    study_id = character(),
    cage_card = character(),
    sex = character(),
    day = integer(),
    weight_g = numeric()
  )
}

empty_clean_scores <- function() {
  tibble::tibble(
    animal_id = character(),
    group_id = character(),
    group_label = character(),
    study_id = character(),
    cage_card = character(),
    sex = character(),
    day = integer(),
    score = numeric()
  )
}

empty_clean_survival <- function() {
  tibble::tibble(
    animal_id = character(),
    group_id = character(),
    group_label = character(),
    study_id = character(),
    time = integer(),
    status = integer(),
    censored = logical(),
    event_type = character(),
    notes = character(),
    cohort_role = character(),
    include_in_km = logical()
  )
}

empty_group_overrides <- function() {
  tibble::tibble(
    group_id = character(),
    display_name = character(),
    plot_order = integer(),
    custom_color = character()
  )
}

normalize_group_overrides <- function(x) {
  if (is.null(x)) {
    return(empty_group_overrides())
  }

  overrides <- tibble::as_tibble(x)
  if (!nrow(overrides)) {
    return(empty_group_overrides())
  }

  if ("color" %in% names(overrides) && !"custom_color" %in% names(overrides)) {
    overrides$custom_color <- overrides$color
  }

  if (!"group_id" %in% names(overrides)) {
    return(empty_group_overrides())
  }

  if (!"display_name" %in% names(overrides)) {
    overrides$display_name <- NA_character_
  }

  if (!"plot_order" %in% names(overrides)) {
    overrides$plot_order <- NA_integer_
  }

  if (!"custom_color" %in% names(overrides)) {
    overrides$custom_color <- NA_character_
  }

  overrides |>
    dplyr::transmute(
      group_id = as.character(.data$group_id),
      display_name = as.character(.data$display_name),
      plot_order = coerce_integer(.data$plot_order),
      custom_color = as.character(.data$custom_color)
    ) |>
    dplyr::filter(nzchar(.data$group_id))
}

default_shared_style_settings <- function() {
  list(
    color_mode = "preset",
    palette_name = "okabe_ito"
  )
}

default_direct_entry_settings <- function() {
  n_groups <- 3L
  mice_per_group <- 4L
  list(
    study_name = "",
    n_groups = n_groups,
    mice_per_group = mice_per_group,
    group_names = sprintf("Group %d", seq_len(n_groups)),
    mouse_labels = stats::setNames(
      lapply(seq_len(n_groups), function(i) sprintf("M%d", seq_len(mice_per_group))),
      paste0("g", seq_len(n_groups))
    ),
    dpi_rows = c(0L, 1L, 3L, 5L)
  )
}

default_weight_plot_settings <- function() {
  list(
    mode = "pct_baseline",
    baseline_rule = "d0_only",
    individual_layer = "points",
    show_mean = TRUE,
    error_style = "sem",
    error_display = "band",
    y_axis_mode = "auto",
    start_at_zero = FALSE,
    y_min = NA_real_,
    y_max = NA_real_,
    show_legend_n = FALSE,
    legend_position = "inset_bottom_left",
    point_spread = 0.10
  )
}

default_score_plot_settings <- function() {
  list(
    individual_layer = "points",
    show_mean = TRUE,
    error_style = "sem",
    error_display = "band",
    y_axis_mode = "fixed",
    y_min = 0,
    y_max = 10,
    show_legend_n = FALSE,
    legend_position = "inset_bottom_left",
    point_spread = 0.10
  )
}

default_survival_plot_settings <- function() {
  list(
    show_ci = FALSE,
    show_censor_marks = FALSE,
    show_death_marks = TRUE,
    show_p_value = TRUE,
    vary_linetypes = FALSE,
    nudge_lines = TRUE,
    nudge_amount = 0.012,
    legend_position = "inset_bottom_left",
    y_axis_mode = "fixed",
    y_min = 0,
    y_max = 100
  )
}

default_survival_analysis_settings <- function() {
  list(
    scheduled_sampling_handling = "exclude"
  )
}

default_stats_settings <- function() {
  list(
    p_adjust_method = "bh"
  )
}

normalize_scheduled_sampling_handling <- function(x = NULL) {
  handling <- tolower(trimws(as.character(
    x %||% default_survival_analysis_settings()$scheduled_sampling_handling
  )))
  handling <- gsub("[^a-z0-9]+", "_", handling)
  handling <- gsub("^_+|_+$", "", handling)

  if (handling %in% c("exclude", "excluded")) {
    return("exclude")
  }

  if (handling %in% c("censor", "censored", "include_as_censored", "include_censored")) {
    return("censor")
  }

  if (handling %in% c("death", "deaths", "count_as_deaths", "include_as_deaths")) {
    return("death")
  }

  default_survival_analysis_settings()$scheduled_sampling_handling
}

scheduled_sampling_handling_label <- function(x = NULL) {
  handling <- normalize_scheduled_sampling_handling(x)

  switch(
    handling,
    exclude = "Exclude from KM",
    censor = "Include as censored",
    death = "Count as deaths",
    "Exclude from KM"
  )
}

normalize_survival_analysis_settings <- function(x = list()) {
  defaults <- default_survival_analysis_settings()
  x <- utils::modifyList(defaults, x %||% list())

  list(
    scheduled_sampling_handling = normalize_scheduled_sampling_handling(x$scheduled_sampling_handling)
  )
}

normalize_p_adjust_method <- function(x = NULL) {
  method <- tolower(trimws(as.character(x %||% default_stats_settings()$p_adjust_method)))
  if (!method %in% c("bh", "bonferroni")) {
    method <- default_stats_settings()$p_adjust_method
  }
  method
}

normalize_stats_settings <- function(x = list()) {
  defaults <- default_stats_settings()
  x <- utils::modifyList(defaults, x %||% list())

  list(
    p_adjust_method = normalize_p_adjust_method(x$p_adjust_method)
  )
}

normalize_shared_style_settings <- function(x = list()) {
  defaults <- default_shared_style_settings()
  x <- x %||% list()

  if (!is.null(x$color_scheme) && is.null(x$color_mode)) {
    legacy_scheme <- tolower(as.character(x$color_scheme))
    if (identical(legacy_scheme, "custom")) {
      x$color_mode <- "custom"
      x$palette_name <- defaults$palette_name
    } else if (identical(legacy_scheme, "grayscale")) {
      x$color_mode <- "preset"
      x$palette_name <- "grayscale"
    } else {
      x$color_mode <- "preset"
      x$palette_name <- defaults$palette_name
    }
  }

  color_mode <- tolower(as.character(x$color_mode %||% defaults$color_mode))
  if (!color_mode %in% c("preset", "custom")) {
    color_mode <- defaults$color_mode
  }

  palette_name <- tolower(as.character(x$palette_name %||% defaults$palette_name))
  if (!palette_name %in% names(group_palette_catalog())) {
    palette_name <- defaults$palette_name
  }

  list(
    color_mode = color_mode,
    palette_name = palette_name
  )
}

normalize_weight_plot_settings <- function(x = list()) {
  defaults <- default_weight_plot_settings()
  raw_x <- x %||% list()
  if (!is.null(raw_x$show_individuals) && is.null(raw_x$individual_layer)) {
    raw_x$individual_layer <- if (isTRUE(raw_x$show_individuals)) "lines" else "none"
  }
  x <- utils::modifyList(defaults, raw_x)

  mode <- tolower(as.character(x$mode %||% defaults$mode))
  if (!mode %in% c("raw", "pct_baseline")) {
    mode <- defaults$mode
  }

  baseline_rule <- tolower(as.character(x$baseline_rule %||% defaults$baseline_rule))
  if (!baseline_rule %in% c("d0_only", "first_non_missing")) {
    baseline_rule <- defaults$baseline_rule
  }

  individual_layer <- tolower(as.character(x$individual_layer %||% defaults$individual_layer))
  if (!individual_layer %in% c("none", "points", "lines")) {
    individual_layer <- defaults$individual_layer
  }

  error_style <- tolower(as.character(x$error_style %||% defaults$error_style))
  if (!error_style %in% c("sem", "sd", "ci95", "none")) {
    error_style <- defaults$error_style
  }

  error_display <- tolower(as.character(x$error_display %||% defaults$error_display))
  if (!error_display %in% c("band", "bar")) {
    error_display <- defaults$error_display
  }

  y_axis_mode <- tolower(as.character(x$y_axis_mode %||% defaults$y_axis_mode))
  if (!y_axis_mode %in% c("auto", "fixed")) {
    y_axis_mode <- defaults$y_axis_mode
  }

  legend_position <- tolower(as.character(x$legend_position %||% defaults$legend_position))
  if (identical(legend_position, "inset")) {
    legend_position <- "inset_bottom_left"
  }
  if (!legend_position %in% c("inset_bottom_left", "inset_top_right", "top", "bottom", "none")) {
    legend_position <- defaults$legend_position
  }

  point_spread <- max(0, min(1, as.numeric(x$point_spread %||% defaults$point_spread)))
  if (is.na(point_spread)) point_spread <- defaults$point_spread
  show_mean <- isTRUE(x$show_mean)
  if (identical(individual_layer, "none") && !show_mean) {
    show_mean <- TRUE
  }

  list(
    mode = mode,
    baseline_rule = baseline_rule,
    individual_layer = individual_layer,
    show_mean = show_mean,
    error_style = error_style,
    error_display = error_display,
    y_axis_mode = y_axis_mode,
    start_at_zero = isTRUE(x$start_at_zero),
    y_min = coerce_numeric(x$y_min),
    y_max = coerce_numeric(x$y_max),
    show_legend_n = isTRUE(x$show_legend_n),
    legend_position = legend_position,
    point_spread = point_spread
  )
}

normalize_score_plot_settings <- function(x = list()) {
  defaults <- default_score_plot_settings()
  raw_x <- x %||% list()
  if (!is.null(raw_x$show_individuals) && is.null(raw_x$individual_layer)) {
    raw_x$individual_layer <- if (isTRUE(raw_x$show_individuals)) "lines" else "none"
  }
  x <- utils::modifyList(defaults, raw_x)

  individual_layer <- tolower(as.character(x$individual_layer %||% defaults$individual_layer))
  if (!individual_layer %in% c("none", "points", "lines")) {
    individual_layer <- defaults$individual_layer
  }

  error_style <- tolower(as.character(x$error_style %||% defaults$error_style))
  if (!error_style %in% c("sem", "sd", "ci95", "none")) {
    error_style <- defaults$error_style
  }

  error_display <- tolower(as.character(x$error_display %||% defaults$error_display))
  if (!error_display %in% c("band", "bar")) {
    error_display <- defaults$error_display
  }

  y_axis_mode <- tolower(as.character(x$y_axis_mode %||% defaults$y_axis_mode))
  if (!y_axis_mode %in% c("auto", "fixed")) {
    y_axis_mode <- defaults$y_axis_mode
  }

  legend_position <- tolower(as.character(x$legend_position %||% defaults$legend_position))
  if (identical(legend_position, "inset")) {
    legend_position <- "inset_bottom_left"
  }
  if (!legend_position %in% c("inset_bottom_left", "inset_top_right", "top", "bottom", "none")) {
    legend_position <- defaults$legend_position
  }

  point_spread <- max(0, min(1, as.numeric(x$point_spread %||% defaults$point_spread)))
  if (is.na(point_spread)) point_spread <- defaults$point_spread
  show_mean <- isTRUE(x$show_mean)
  if (identical(individual_layer, "none") && !show_mean) {
    show_mean <- TRUE
  }

  list(
    individual_layer = individual_layer,
    show_mean = show_mean,
    error_style = error_style,
    error_display = error_display,
    y_axis_mode = y_axis_mode,
    y_min = coerce_numeric(x$y_min),
    y_max = coerce_numeric(x$y_max),
    show_legend_n = isTRUE(x$show_legend_n),
    legend_position = legend_position,
    point_spread = point_spread
  )
}

normalize_survival_plot_settings <- function(x = list()) {
  defaults <- default_survival_plot_settings()
  x <- utils::modifyList(defaults, x %||% list())

  y_axis_mode <- tolower(as.character(x$y_axis_mode %||% defaults$y_axis_mode))
  if (!y_axis_mode %in% c("auto", "fixed")) {
    y_axis_mode <- defaults$y_axis_mode
  }

  legend_position <- tolower(as.character(x$legend_position %||% defaults$legend_position))
  if (identical(legend_position, "inset")) {
    legend_position <- "inset_top_right"
  }
  if (!legend_position %in% c("inset_bottom_left", "inset_top_right", "top", "bottom", "none")) {
    legend_position <- defaults$legend_position
  }

  nudge_amount <- as.numeric(x$nudge_amount %||% defaults$nudge_amount)
  if (is.na(nudge_amount)) {
    nudge_amount <- defaults$nudge_amount
  }
  nudge_amount <- max(0, min(0.05, nudge_amount))

  list(
    show_ci = isTRUE(x$show_ci),
    show_death_marks = isTRUE(x$show_death_marks),
    show_p_value = isTRUE(x$show_p_value),
    vary_linetypes = isTRUE(x$vary_linetypes),
    nudge_lines = isTRUE(x$nudge_lines),
    nudge_amount = nudge_amount,
    legend_position = legend_position,
    y_axis_mode = y_axis_mode,
    y_min = coerce_numeric(x$y_min),
    y_max = coerce_numeric(x$y_max)
  )
}

normalize_direct_entry_settings <- function(x = list()) {
  defaults <- default_direct_entry_settings()
  x <- x %||% list()

  # Backward compatibility: older schema used base_headers/day_columns.
  if (!is.null(x$base_headers) || !is.null(x$day_columns)) {
    legacy_days <- tibble::as_tibble(x$day_columns %||% tibble::tibble())
    if ("day" %in% names(legacy_days)) {
      legacy_days <- vapply(legacy_days$day, coerce_integer, integer(1))
      legacy_days <- sort(unique(legacy_days[!is.na(legacy_days) & legacy_days >= 0]))
    } else {
      legacy_days <- integer()
    }

    x <- utils::modifyList(
      defaults,
      list(
        study_name = "",
        dpi_rows = if (length(legacy_days)) legacy_days else defaults$dpi_rows
      )
    )
  }

  normalize_text <- function(value, default = "") {
    if (is.null(value) || !length(value)) {
      return(default)
    }
    value <- as.character(value[[1]])
    if (is.na(value)) {
      return(default)
    }
    trimws(value)
  }

  n_groups <- coerce_integer(x$n_groups %||% defaults$n_groups)
  if (is.na(n_groups) || n_groups < 1L) {
    n_groups <- defaults$n_groups
  }

  mice_per_group <- coerce_integer(x$mice_per_group %||% defaults$mice_per_group)
  if (is.na(mice_per_group) || mice_per_group < 1L) {
    mice_per_group <- defaults$mice_per_group
  }

  default_group_names <- sprintf("Group %d", seq_len(n_groups))
  supplied_group_names <- as.character(x$group_names %||% character())
  supplied_group_names[is.na(supplied_group_names)] <- ""
  group_names <- rep("", n_groups)
  if (length(supplied_group_names)) {
    group_names[seq_len(min(length(supplied_group_names), n_groups))] <-
      trimws(supplied_group_names[seq_len(min(length(supplied_group_names), n_groups))])
  }
  blank_groups <- !nzchar(group_names)
  group_names[blank_groups] <- default_group_names[blank_groups]

  supplied_mouse_labels <- x$mouse_labels %||% list()
  mouse_labels <- vector("list", length = n_groups)
  names(mouse_labels) <- paste0("g", seq_len(n_groups))
  for (i in seq_len(n_groups)) {
    from_index <- NULL
    if (is.list(supplied_mouse_labels) && length(supplied_mouse_labels) >= i) {
      from_index <- supplied_mouse_labels[[i]]
    }
    from_name <- NULL
    if (is.list(supplied_mouse_labels) && !is.null(names(supplied_mouse_labels))) {
      from_name <- supplied_mouse_labels[[paste0("g", i)]]
    }
    from_index <- from_index %||% from_name
    labels_i <- as.character(from_index %||% character())
    labels_i[is.na(labels_i)] <- ""
    labels_i <- trimws(labels_i)
    if (length(labels_i) < mice_per_group) {
      labels_i <- c(labels_i, rep("", mice_per_group - length(labels_i)))
    }
    labels_i <- labels_i[seq_len(mice_per_group)]
    blank_idx <- !nzchar(labels_i)
    labels_i[blank_idx] <- sprintf("M%d", which(blank_idx))
    mouse_labels[[i]] <- labels_i
  }

  dpi_rows <- vapply(as.list(x$dpi_rows %||% defaults$dpi_rows), coerce_integer, integer(1))
  dpi_rows <- sort(unique(dpi_rows[!is.na(dpi_rows) & dpi_rows >= 0]))
  if (!length(dpi_rows)) {
    dpi_rows <- defaults$dpi_rows
  }

  list(
    study_name = normalize_text(x$study_name, defaults$study_name),
    n_groups = n_groups,
    mice_per_group = mice_per_group,
    group_names = group_names,
    mouse_labels = mouse_labels,
    dpi_rows = dpi_rows
  )
}

empty_scheduled_sampling_cages <- function() {
  tibble::tibble(
    cage_card = character(),
    role = character(),
    censor_day = integer()
  )
}

scheduled_sampling_role_value <- function(x, default = "survival", strict = FALSE) {
  raw <- trimws(as.character(x %||% ""))
  if (!nzchar(raw)) {
    return(default)
  }

  normalized <- tolower(raw)
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)

  if (normalized %in% c("survival", "survival_group")) {
    return("survival")
  }

  if (normalized %in% c("scheduled_sampling", "scheduledsampling")) {
    return("scheduled_sampling")
  }

  if (isTRUE(strict)) {
    return(NA_character_)
  }

  default
}

scheduled_sampling_role_label <- function(x) {
  normalized <- unname(vapply(
    x,
    function(value) scheduled_sampling_role_value(value, default = "survival"),
    character(1)
  ))

  ifelse(
    normalized == "scheduled_sampling",
    "Scheduled sampling",
    "Survival"
  )
}

coerce_scheduled_sampling_cages_input <- function(x = NULL) {
  if (is.null(x)) {
    return(empty_scheduled_sampling_cages())
  }

  if (inherits(x, "data.frame")) {
    return(tibble::as_tibble(x))
  }

  if (is.list(x) && length(x)) {
    outer_names <- names(x)
    is_row_list <- (is.null(outer_names) || !length(outer_names) || all(!nzchar(outer_names))) &&
      all(vapply(x, function(value) is.list(value) || inherits(value, "data.frame"), logical(1)))

    if (isTRUE(is_row_list)) {
      return(dplyr::bind_rows(x))
    }
  }

  tibble::as_tibble(x)
}

normalize_infer_death_events <- function(x = NULL) {
  if (is.null(x)) {
    return(TRUE)
  }

  isTRUE(x)
}

normalize_infer_survival <- function(x = NULL) {
  normalize_infer_death_events(x)
}

normalize_detect_scheduled_sampling <- function(x = NULL) {
  if (is.null(x)) {
    return(TRUE)
  }

  isTRUE(x)
}

normalize_scheduled_sampling_cages <- function(x = NULL) {
  if (is.null(x)) {
    return(empty_scheduled_sampling_cages())
  }

  cages <- coerce_scheduled_sampling_cages_input(x)
  if (!nrow(cages)) {
    return(empty_scheduled_sampling_cages())
  }

  if (!"cage_card" %in% names(cages)) {
    return(empty_scheduled_sampling_cages())
  }

  if (!"role" %in% names(cages)) {
    cages$role <- "survival"
  }

  if (!"censor_day" %in% names(cages)) {
    cages$censor_day <- NA_integer_
  }

  cages |>
    dplyr::transmute(
      cage_card = trimws(as.character(.data$cage_card)),
      role = unname(vapply(.data$role, scheduled_sampling_role_value, character(1), default = "survival")),
      censor_day = coerce_integer(.data$censor_day)
    ) |>
    dplyr::filter(nzchar(.data$cage_card)) |>
    dplyr::distinct(.data$cage_card, .keep_all = TRUE)
}

presets_dir <- function() {
  ensure_dir(file.path(settings_root(), "presets"))
}

state_file_path <- function() {
  file.path(settings_root(), "session_state.rds")
}

sanitize_preset_name <- function(name) {
  janitor::make_clean_names(name %||% "preset")
}

preset_path <- function(name) {
  file.path(presets_dir(), paste0(sanitize_preset_name(name), ".rds"))
}

save_mapping_preset <- function(name, preset) {
  if (runtime_session_storage()) {
    return(invisible(NULL))
  }

  if (!nzchar(name %||% "")) {
    stop("Preset name cannot be blank.", call. = FALSE)
  }

  payload <- list(
    name = name,
    saved_at = Sys.time(),
    weights = preset$weights %||% list(),
    survival = preset$survival %||% list(),
    day_map = tibble::as_tibble(preset$day_map %||% tibble::tibble()),
    score_day_map = tibble::as_tibble(preset$score_day_map %||% tibble::tibble()),
    metadata = preset$metadata %||% list(),
    infer_death_events = normalize_infer_death_events(preset$infer_death_events %||% preset$infer_survival),
    detect_scheduled_sampling = normalize_detect_scheduled_sampling(preset$detect_scheduled_sampling),
    scheduled_sampling_cages = normalize_scheduled_sampling_cages(preset$scheduled_sampling_cages),
    direct_entry = normalize_direct_entry_settings(preset$direct_entry),
    group_overrides = normalize_group_overrides(preset$group_overrides),
    shared_style = normalize_shared_style_settings(preset$shared_style),
    stats = normalize_stats_settings(preset$stats),
    survival_analysis = normalize_survival_analysis_settings(preset$survival_analysis),
    weight_plot = normalize_weight_plot_settings(preset$weight_plot),
    score_plot = normalize_score_plot_settings(preset$score_plot),
    survival_plot = normalize_survival_plot_settings(preset$survival_plot)
  )

  saveRDS_atomic(payload, preset_path(name))
  invisible(payload)
}

load_mapping_preset <- function(name) {
  if (runtime_session_storage()) {
    return(NULL)
  }

  path <- preset_path(name)
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch({
    payload <- readRDS(path)
    payload$infer_death_events <- normalize_infer_death_events(payload$infer_death_events %||% payload$infer_survival)
    payload$infer_survival <- payload$infer_death_events
    payload$detect_scheduled_sampling <- normalize_detect_scheduled_sampling(payload$detect_scheduled_sampling)
    payload$scheduled_sampling_cages <- normalize_scheduled_sampling_cages(payload$scheduled_sampling_cages)
    payload$direct_entry <- normalize_direct_entry_settings(payload$direct_entry)
    payload$group_overrides <- normalize_group_overrides(payload$group_overrides)
    payload$shared_style <- normalize_shared_style_settings(payload$shared_style)
    payload$stats <- normalize_stats_settings(payload$stats)
    payload$survival_analysis <- normalize_survival_analysis_settings(payload$survival_analysis)
    payload$weight_plot <- normalize_weight_plot_settings(payload$weight_plot)
    payload$score_plot <- normalize_score_plot_settings(payload$score_plot)
    payload$survival_plot <- normalize_survival_plot_settings(payload$survival_plot)
    payload
  }, error = function(e) NULL)
}

list_mapping_presets <- function() {
  if (runtime_session_storage()) {
    return(character())
  }

  files <- list.files(presets_dir(), pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) {
    return(character())
  }

  payloads <- lapply(files, function(f) tryCatch(readRDS(f), error = function(e) NULL))
  files <- files[!vapply(payloads, is.null, logical(1))]
  payloads <- payloads[!vapply(payloads, is.null, logical(1))]
  names <- vapply(seq_along(payloads), function(i) {
    payloads[[i]]$name %||% tools::file_path_sans_ext(basename(files[[i]]))
  }, character(1))
  mtimes <- file.info(files)$mtime

  names[order(mtimes, decreasing = TRUE)]
}

delete_mapping_preset <- function(name) {
  if (runtime_session_storage()) {
    return(invisible(FALSE))
  }

  path <- preset_path(name)
  if (file.exists(path)) {
    file.remove(path)
  }
}

load_recent_compatible_preset <- function(weights_imported = NULL, survival_imported = NULL) {
  presets <- list_mapping_presets()
  if (!length(presets)) {
    return(NULL)
  }

  for (preset_name in presets) {
    preset <- load_mapping_preset(preset_name)
    if (mapping_is_compatible(preset, weights_imported, survival_imported)) {
      return(preset)
    }
  }

  NULL
}

save_app_state <- function(state) {
  if (runtime_session_storage()) {
    return(invisible(NULL))
  }

  payload <- list(
    saved_at = Sys.time(),
    metadata = state$metadata %||% list(),
    infer_death_events = normalize_infer_death_events(state$infer_death_events %||% state$infer_survival),
    detect_scheduled_sampling = normalize_detect_scheduled_sampling(state$detect_scheduled_sampling),
    scheduled_sampling_cages = normalize_scheduled_sampling_cages(state$scheduled_sampling_cages),
    direct_entry = normalize_direct_entry_settings(state$direct_entry),
    weights_name = state$weights_name %||% "",
    survival_name = state$survival_name %||% "",
    group_overrides = normalize_group_overrides(state$group_overrides),
    shared_style = normalize_shared_style_settings(state$shared_style),
    stats = normalize_stats_settings(state$stats),
    survival_analysis = normalize_survival_analysis_settings(state$survival_analysis),
    weight_plot = normalize_weight_plot_settings(state$weight_plot),
    score_plot = normalize_score_plot_settings(state$score_plot),
    survival_plot = normalize_survival_plot_settings(state$survival_plot)
  )

  saveRDS_atomic(payload, state_file_path())
  invisible(payload)
}

load_app_state <- function() {
  if (runtime_session_storage()) {
    return(list())
  }

  path <- state_file_path()
  if (!file.exists(path)) {
    return(list())
  }

  tryCatch({
    payload <- readRDS(path)
    payload$infer_death_events <- normalize_infer_death_events(payload$infer_death_events %||% payload$infer_survival)
    payload$infer_survival <- payload$infer_death_events
    payload$detect_scheduled_sampling <- normalize_detect_scheduled_sampling(payload$detect_scheduled_sampling)
    payload$scheduled_sampling_cages <- normalize_scheduled_sampling_cages(payload$scheduled_sampling_cages)
    payload$direct_entry <- normalize_direct_entry_settings(payload$direct_entry)
    payload$group_overrides <- normalize_group_overrides(payload$group_overrides)
    payload$shared_style <- normalize_shared_style_settings(payload$shared_style)
    payload$stats <- normalize_stats_settings(payload$stats)
    payload$survival_analysis <- normalize_survival_analysis_settings(payload$survival_analysis)
    payload$weight_plot <- normalize_weight_plot_settings(payload$weight_plot)
    payload$score_plot <- normalize_score_plot_settings(payload$score_plot)
    payload$survival_plot <- normalize_survival_plot_settings(payload$survival_plot)
    payload
  }, error = function(e) list())
}
