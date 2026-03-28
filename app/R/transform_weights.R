build_group_meta <- function(weights_long) {
  groups <- weights_long |>
    dplyr::distinct(.data$group_id, .data$group_label) |>
    dplyr::arrange(.data$group_label)
  n_groups <- nrow(groups)

  if (!n_groups) {
    return(tibble::tibble(
      group_id = character(),
      display_name = character(),
      plot_order = integer(),
      custom_color = character(),
      color = character()
    ))
  }

  auto_colors <- default_group_palette(n_groups)

  dplyr::mutate(
    groups,
    display_name = .data$group_label,
    plot_order = seq_len(n_groups),
    custom_color = auto_colors,
    color = auto_colors
  ) |>
    dplyr::select("group_id", "display_name", "plot_order", "custom_color", "color")
}

merge_group_overrides <- function(base_group_meta, overrides = NULL) {
  if (is.null(overrides) || !nrow(overrides)) {
    return(base_group_meta)
  }

  overrides <- normalize_group_overrides(overrides)

  base_group_meta |>
    dplyr::left_join(
      overrides |>
        dplyr::select(
          "group_id",
          override_display_name = "display_name",
          override_plot_order = "plot_order",
          override_custom_color = "custom_color"
        ),
      by = "group_id"
    ) |>
    dplyr::mutate(
      display_name = dplyr::coalesce(.data$override_display_name, .data$display_name),
      plot_order = dplyr::coalesce(coerce_integer(.data$override_plot_order), .data$plot_order),
      custom_color = dplyr::coalesce(.data$override_custom_color, .data$custom_color),
      color = dplyr::coalesce(normalize_hex_color(.data$override_custom_color), .data$color)
    ) |>
    dplyr::select("group_id", "display_name", "plot_order", "custom_color", "color") |>
    dplyr::arrange(.data$plot_order, .data$display_name)
}

apply_group_metadata <- function(weights_long, group_meta) {
  if (is.null(weights_long) || !nrow(weights_long)) {
    return(weights_long)
  }

  weights_long |>
    dplyr::left_join(group_meta, by = "group_id") |>
    dplyr::mutate(
      display_name = dplyr::coalesce(.data$display_name, .data$group_label),
      plot_order = dplyr::coalesce(.data$plot_order, 999L),
      custom_color = dplyr::coalesce(.data$custom_color, default_group_palette(1)),
      color = dplyr::coalesce(.data$color, default_group_palette(1))
    )
}

standardize_weights_data <- function(raw_weights, field_mapping, day_mapping, jump_threshold = 0.20) {
  errors <- character()
  warnings <- character()

  if (is.null(raw_weights) || is.null(raw_weights$data)) {
    errors <- c(errors, "Missing weights file.")
    return(list(
      data = empty_clean_weights(),
      group_meta = empty_group_meta(),
      errors = errors,
      warnings = warnings
    ))
  }

  data <- raw_weights$data
  required_fields <- c("animal_id", "group")

  missing_fields <- required_fields[vapply(required_fields, function(field) {
    column <- field_mapping[[field]] %||% ""
    !nzchar(column) || !column %in% names(data)
  }, logical(1))]

  if (length(missing_fields)) {
    errors <- c(
      errors,
      sprintf(
        "Missing required mapped weights fields: %s.",
        paste(missing_fields, collapse = ", ")
      )
    )
  }

  day_map <- tibble::as_tibble(day_mapping %||% tibble::tibble()) |>
    dplyr::mutate(
      source_column = as.character(.data$source_column),
      day = coerce_integer(.data$day),
      include = as.logical(.data$include)
    ) |>
    dplyr::filter(.data$include %in% TRUE)

  if (!nrow(day_map)) {
    errors <- c(errors, "At least one valid day column must be selected.")
  }

  invalid_days <- day_map |>
    dplyr::filter(is.na(.data$day))

  if (nrow(invalid_days)) {
    errors <- c(errors, "One or more selected day columns do not have a valid day integer.")
  }

  duplicate_days <- day_map |>
    dplyr::count(.data$day) |>
    dplyr::filter(.data$n > 1L, !is.na(.data$day))

  if (nrow(duplicate_days)) {
    errors <- c(errors, "Selected day columns contain duplicate parsed day integers.")
  }

  missing_day_columns <- setdiff(day_map$source_column, names(data))
  if (length(missing_day_columns)) {
    errors <- c(
      errors,
      sprintf(
        "Selected day columns were not found in the weights file: %s.",
        paste(missing_day_columns, collapse = ", ")
      )
    )
  }

  if (length(errors)) {
    return(list(
      data = empty_clean_weights(),
      group_meta = empty_group_meta(),
      errors = unique(errors),
      warnings = unique(warnings)
    ))
  }

  standardized <- tibble::tibble(
    animal_id = trimws(as.character(data[[field_mapping$animal_id]])),
    group_label = trimws(as.character(data[[field_mapping$group]])),
    study_id = if (nzchar(field_mapping$study_id %||% "")) trimws(as.character(data[[field_mapping$study_id]])) else NA_character_,
    cage_card = if (nzchar(field_mapping$cage_card %||% "")) trimws(as.character(data[[field_mapping$cage_card]])) else NA_character_,
    sex = if (nzchar(field_mapping$sex %||% "")) trimws(as.character(data[[field_mapping$sex]])) else NA_character_
  )

  if (anyDuplicated(standardized$animal_id)) {
    errors <- c(errors, "Duplicate animal_id values found in weights.")
  }

  if (any(!nzchar(standardized$animal_id) | is.na(standardized$animal_id))) {
    errors <- c(errors, "Blank animal_id values found in weights.")
  }

  if (any(!nzchar(standardized$group_label) | is.na(standardized$group_label))) {
    errors <- c(errors, "Blank group values found in weights.")
  }

  group_lookup <- standardized |>
    dplyr::distinct(.data$group_label) |>
    dplyr::mutate(group_id = clean_group_id(.data$group_label))

  if (anyDuplicated(group_lookup$group_id)) {
    errors <- c(
      errors,
      "Two or more group labels collapse to the same machine-safe group_id."
    )
  }

  if (length(errors)) {
    return(list(
      data = empty_clean_weights(),
      group_meta = empty_group_meta(),
      errors = unique(errors),
      warnings = unique(warnings)
    ))
  }

  selected_columns <- day_map$source_column
  selected_lookup <- day_map |>
    dplyr::select("source_column", "day")

  long_data <- standardized |>
    dplyr::mutate(.row_id = seq_len(nrow(standardized))) |>
    dplyr::bind_cols(data |>
      dplyr::select(dplyr::all_of(selected_columns))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_columns),
      names_to = "source_column",
      values_to = "weight_raw"
    ) |>
    dplyr::left_join(selected_lookup, by = "source_column") |>
    dplyr::left_join(group_lookup, by = "group_label")

  raw_weight_text <- trimws(as.character(long_data$weight_raw))
  parsed_weights <- coerce_numeric(long_data$weight_raw)
  non_missing_input <- !is.na(long_data$weight_raw) & nzchar(raw_weight_text)
  coercion_count <- sum(non_missing_input & is.na(parsed_weights), na.rm = TRUE)

  if (coercion_count > 0) {
    warnings <- c(
      warnings,
      sprintf("%s weight value(s) could not be parsed and were set to missing.", coercion_count)
    )
  }

  long_data <- dplyr::mutate(long_data, weight_g = parsed_weights)

  zero_count <- sum(long_data$weight_g == 0, na.rm = TRUE)
  if (zero_count > 0) {
    warnings <- c(
      warnings,
      sprintf(
        "%s weight value(s) of 0 were treated as missing (a mouse cannot weigh 0 g).",
        zero_count
      )
    )
    long_data$weight_g[long_data$weight_g == 0 & !is.na(long_data$weight_g)] <- NA_real_
  }

  if (all(is.na(long_data$weight_g))) {
    errors <- c(errors, "No numeric weight values remain after parsing.")
  }

  negative_count <- sum(long_data$weight_g < 0, na.rm = TRUE)
  if (negative_count > 0) {
    warnings <- c(
      warnings,
      sprintf("%s negative weight value(s) were found.", negative_count)
    )
  }

  d0_missing <- long_data |>
    dplyr::filter(.data$day == 0L) |>
    dplyr::group_by(.data$animal_id) |>
    dplyr::summarise(has_d0 = any(!is.na(.data$weight_g)), .groups = "drop") |>
    dplyr::filter(!.data$has_d0)

  if (nrow(d0_missing)) {
    warnings <- c(
      warnings,
      sprintf(
        "%s animal(s) are missing D0 weights (affects D0-only baseline rule; use first non-missing fallback if intentional).",
        nrow(d0_missing)
      )
    )
  }

  jump_count <- long_data |>
    dplyr::filter(!is.na(.data$weight_g)) |>
    dplyr::arrange(.data$animal_id, .data$day) |>
    dplyr::group_by(.data$animal_id) |>
    dplyr::mutate(relative_change = abs((.data$weight_g - dplyr::lag(.data$weight_g)) / dplyr::lag(.data$weight_g))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$relative_change), .data$relative_change > jump_threshold) |>
    nrow()

  if (jump_count > 0) {
    warnings <- c(
      warnings,
      sprintf(
        "%s large day-to-day weight jump(s) exceeded the %.0f%% warning threshold.",
        jump_count,
        jump_threshold * 100
      )
    )
  }

  clean_weights <- long_data |>
    dplyr::select(
      "animal_id",
      "group_id",
      "group_label",
      "study_id",
      "cage_card",
      "sex",
      "day",
      "weight_g"
    )

  list(
    data = clean_weights,
    group_meta = build_group_meta(clean_weights),
    errors = unique(errors),
    warnings = unique(warnings)
  )
}

apply_baseline_rule <- function(weights_long, baseline_rule = c("d0_only", "first_non_missing")) {
  baseline_rule <- match.arg(baseline_rule)

  if (is.null(weights_long) || !nrow(weights_long)) {
    return(list(data = tibble::tibble(), warnings = character()))
  }

  baseline_tbl <- weights_long |>
    dplyr::arrange(.data$animal_id, .data$day) |>
    dplyr::group_by(.data$animal_id) |>
    dplyr::summarise(
      baseline_weight_g = dplyr::case_when(
        baseline_rule == "d0_only" ~ .data$weight_g[match(0L, .data$day)] %||% NA_real_,
        TRUE ~ {
          valid <- .data$weight_g[!is.na(.data$weight_g)]
          if (length(valid)) valid[[1]] else NA_real_
        }
      ),
      .groups = "drop"
    )

  data <- weights_long |>
    dplyr::left_join(baseline_tbl, by = "animal_id") |>
    dplyr::mutate(
      pct_baseline = dplyr::if_else(
        !is.na(.data$baseline_weight_g) & .data$baseline_weight_g != 0,
        100 * .data$weight_g / .data$baseline_weight_g,
        NA_real_
      )
    )

  missing_baseline <- data |>
    dplyr::distinct(.data$animal_id, .data$baseline_weight_g) |>
    dplyr::filter(is.na(.data$baseline_weight_g))

  warnings <- if (nrow(missing_baseline)) {
    sprintf(
      "%s animal(s) are missing baseline weights under the %s rule.",
      nrow(missing_baseline),
      gsub("_", " ", baseline_rule)
    )
  } else {
    character()
  }

  list(data = data, warnings = warnings)
}

summarise_weights_data <- function(weights_long,
                                   mode = c("raw", "pct_baseline"),
                                   error_style = c("sem", "sd", "ci95", "none")) {
  mode <- match.arg(mode)
  error_style <- match.arg(error_style)
  value_col <- if (identical(mode, "raw")) "weight_g" else "pct_baseline"

  weights_long |>
    dplyr::group_by(.data$group_id, .data$display_name, .data$day) |>
    dplyr::summarise(
      n = sum(!is.na(.data[[value_col]])),
      mean = mean(.data[[value_col]], na.rm = TRUE),
      sd = stats::sd(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean = dplyr::if_else(.data$n > 0, .data$mean, NA_real_),
      sd = dplyr::if_else(.data$n > 1, .data$sd, NA_real_),
      sem = dplyr::if_else(.data$n > 1, .data$sd / sqrt(.data$n), NA_real_),
      ci95 = NA_real_
    ) |>
    dplyr::mutate(
      ci95 = replace(
        .data$ci95,
        .data$n > 1,
        stats::qt(0.975, df = .data$n[.data$n > 1] - 1) * .data$sem[.data$n > 1]
      )
    ) |>
    dplyr::mutate(
      error_width = dplyr::case_when(
        error_style == "sem" ~ .data$sem,
        error_style == "sd" ~ .data$sd,
        error_style == "ci95" ~ .data$ci95,
        TRUE ~ NA_real_
      ),
      error_low = dplyr::if_else(!is.na(.data$error_width), .data$mean - .data$error_width, NA_real_),
      error_high = dplyr::if_else(!is.na(.data$error_width), .data$mean + .data$error_width, NA_real_)
    ) |>
    dplyr::transmute(
      group = .data$display_name,
      day = .data$day,
      n = .data$n,
      mean = .data$mean,
      sd = .data$sd,
      sem = .data$sem,
      ci95 = .data$ci95,
      error_low = .data$error_low,
      error_high = .data$error_high
    ) |>
    dplyr::arrange(.data$group, .data$day)
}

weight_key_stats <- function(weights_long, group_meta) {
  if (is.null(weights_long) || !nrow(weights_long)) {
    return(tibble::tibble())
  }

  group_order <- group_meta |>
    dplyr::arrange(.data$plot_order, .data$display_name) |>
    dplyr::pull(.data$display_name)

  final_day <- max(weights_long$day, na.rm = TRUE)

  n_per_group <- weights_long |>
    dplyr::distinct(.data$display_name, .data$animal_id) |>
    dplyr::count(.data$display_name, name = "N")

  baseline_stats <- weights_long |>
    dplyr::filter(.data$day == 0L, !is.na(.data$pct_baseline)) |>
    dplyr::group_by(.data$display_name) |>
    dplyr::summarise(
      baseline_mean = mean(.data$pct_baseline, na.rm = TRUE),
      baseline_sd   = stats::sd(.data$pct_baseline, na.rm = TRUE),
      .groups = "drop"
    )

  group_day_means <- weights_long |>
    dplyr::filter(!is.na(.data$pct_baseline)) |>
    dplyr::group_by(.data$display_name, .data$day) |>
    dplyr::summarise(day_mean = mean(.data$pct_baseline, na.rm = TRUE), .groups = "drop")

  nadir_stats <- group_day_means |>
    dplyr::group_by(.data$display_name) |>
    dplyr::slice_min(.data$day_mean, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(.data$display_name, nadir_pct = .data$day_mean, nadir_day = .data$day)

  end_stats <- weights_long |>
    dplyr::filter(.data$day == final_day, !is.na(.data$pct_baseline)) |>
    dplyr::group_by(.data$display_name) |>
    dplyr::summarise(
      end_mean = mean(.data$pct_baseline, na.rm = TRUE),
      end_sd   = stats::sd(.data$pct_baseline, na.rm = TRUE),
      .groups = "drop"
    )

  n_per_group |>
    dplyr::left_join(baseline_stats, by = "display_name") |>
    dplyr::left_join(nadir_stats,    by = "display_name") |>
    dplyr::left_join(end_stats,      by = "display_name") |>
    dplyr::mutate(display_name = factor(.data$display_name, levels = group_order)) |>
    dplyr::arrange(.data$display_name) |>
    dplyr::mutate(display_name = as.character(.data$display_name)) |>
    dplyr::transmute(
      Group               = .data$display_name,
      N                   = .data$N,
      `Baseline (% ± SD)` = sprintf("%.1f \u00b1 %.1f", .data$baseline_mean, .data$baseline_sd),
      `Nadir (% BL)`      = sprintf("%.1f (DPI %d)", .data$nadir_pct, as.integer(.data$nadir_day)),
      `End-of-study (% ± SD)` = sprintf("%.1f \u00b1 %.1f", .data$end_mean, .data$end_sd)
    )
}

weight_pairwise_tests <- function(weights_long, group_meta, p_adjust_method = "bh") {
  if (is.null(weights_long) || !nrow(weights_long)) {
    return(list(kw_p = NA_real_, pairwise = NULL))
  }

  final_day <- max(weights_long$day, na.rm = TRUE)
  end_data <- weights_long |>
    dplyr::filter(.data$day == final_day, !is.na(.data$pct_baseline))

  groups <- unique(end_data$display_name)
  if (length(groups) < 2) {
    return(list(kw_p = NA_real_, pairwise = NULL))
  }

  kw_result <- tryCatch(
    stats::kruskal.test(pct_baseline ~ display_name, data = end_data),
    error = function(e) NULL
  )
  kw_p <- if (!is.null(kw_result)) kw_result$p.value else NA_real_

  if (length(groups) < 3) {
    return(list(kw_p = kw_p, pairwise = NULL))
  }

  adjusted_col <- p_adjusted_column_name(p_adjust_method)
  pw_result <- tryCatch(
    stats::pairwise.wilcox.test(
      end_data$pct_baseline,
      end_data$display_name,
      p.adjust.method = p_adjust_method_r(p_adjust_method)
    ),
    error = function(e) NULL
  )

  if (is.null(pw_result)) {
    return(list(kw_p = kw_p, pairwise = NULL))
  }

  p_mat <- pw_result$p.value
  rows_list <- lapply(rownames(p_mat), function(r) {
    lapply(colnames(p_mat), function(cc) {
      val <- p_mat[r, cc]
      if (!is.na(val)) {
        row <- data.frame(`Group A` = cc, `Group B` = r, check.names = FALSE, stringsAsFactors = FALSE)
        row[[adjusted_col]] <- val
        row
      }
    })
  })
  pairwise_tbl <- do.call(rbind, Filter(Negate(is.null), unlist(rows_list, recursive = FALSE)))

  list(kw_p = kw_p, pairwise = pairwise_tbl)
}
