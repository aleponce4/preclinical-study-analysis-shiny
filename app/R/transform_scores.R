standardize_scores_data <- function(raw_weights, field_mapping, score_day_mapping) {
  warnings <- character()

  if (is.null(raw_weights) || is.null(raw_weights$data)) {
    return(list(data = empty_clean_scores(), warnings = warnings))
  }

  data <- raw_weights$data
  required_fields <- c("animal_id", "group")

  missing_fields <- required_fields[vapply(required_fields, function(field) {
    column <- field_mapping[[field]] %||% ""
    !nzchar(column) || !column %in% names(data)
  }, logical(1))]
  if (length(missing_fields)) {
    warnings <- c(
      warnings,
      sprintf(
        "Clinical scores could not be parsed because required mapped fields are missing: %s.",
        paste(missing_fields, collapse = ", ")
      )
    )
    return(list(data = empty_clean_scores(), warnings = unique(warnings)))
  }

  score_map <- tibble::as_tibble(score_day_mapping %||% tibble::tibble())
  if (!"source_column" %in% names(score_map)) score_map$source_column <- character(nrow(score_map))
  if (!"day" %in% names(score_map)) score_map$day <- rep(NA_integer_, nrow(score_map))
  if (!"include" %in% names(score_map)) score_map$include <- rep(FALSE, nrow(score_map))

  score_map <- score_map |>
    dplyr::mutate(
      source_column = as.character(.data$source_column),
      day = coerce_integer(.data$day),
      include = as.logical(.data$include)
    ) |>
    dplyr::filter(.data$include %in% TRUE, !is.na(.data$day))

  if (!nrow(score_map)) {
    return(list(data = empty_clean_scores(), warnings = warnings))
  }

  missing_score_columns <- setdiff(score_map$source_column, names(data))
  if (length(missing_score_columns)) {
    warnings <- c(
      warnings,
      sprintf(
        "Some detected clinical score columns were not found and were skipped: %s.",
        paste(missing_score_columns, collapse = ", ")
      )
    )
    score_map <- score_map |>
      dplyr::filter(.data$source_column %in% names(data))
  }

  if (!nrow(score_map)) {
    return(list(data = empty_clean_scores(), warnings = unique(warnings)))
  }

  standardized <- tibble::tibble(
    animal_id = trimws(as.character(data[[field_mapping$animal_id]])),
    group_label = trimws(as.character(data[[field_mapping$group]])),
    study_id = if (nzchar(field_mapping$study_id %||% "")) trimws(as.character(data[[field_mapping$study_id]])) else NA_character_,
    cage_card = if (nzchar(field_mapping$cage_card %||% "")) trimws(as.character(data[[field_mapping$cage_card]])) else NA_character_,
    sex = if (nzchar(field_mapping$sex %||% "")) trimws(as.character(data[[field_mapping$sex]])) else NA_character_
  ) |>
    dplyr::filter(
      !is.na(.data$animal_id), nzchar(.data$animal_id),
      !is.na(.data$group_label), nzchar(.data$group_label)
    )

  if (!nrow(standardized)) {
    return(list(data = empty_clean_scores(), warnings = unique(warnings)))
  }

  group_lookup <- standardized |>
    dplyr::distinct(.data$group_label) |>
    dplyr::mutate(group_id = clean_group_id(.data$group_label))

  selected_columns <- score_map$source_column
  selected_lookup <- score_map |>
    dplyr::select("source_column", "day")

  long_data <- standardized |>
    dplyr::bind_cols(data |>
      dplyr::select(dplyr::all_of(selected_columns))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_columns),
      names_to = "source_column",
      values_to = "score_raw"
    ) |>
    dplyr::left_join(selected_lookup, by = "source_column") |>
    dplyr::left_join(group_lookup, by = "group_label")

  raw_score_text <- trimws(as.character(long_data$score_raw))
  parsed_scores <- parse_score_values(long_data$score_raw)
  non_missing_input <- !is.na(long_data$score_raw) & nzchar(raw_score_text)
  coercion_count <- sum(non_missing_input & is.na(parsed_scores), na.rm = TRUE)
  if (coercion_count > 0) {
    warnings <- c(
      warnings,
      sprintf("%s clinical score value(s) could not be parsed and were set to missing.", coercion_count)
    )
  }

  out_of_range_mask <- !is.na(parsed_scores) & (parsed_scores < 0 | parsed_scores > 10)
  out_of_range_count <- sum(out_of_range_mask, na.rm = TRUE)
  if (out_of_range_count > 0) {
    warnings <- c(
      warnings,
      sprintf(
        "%s clinical score value(s) were outside the expected 0-10 range and were set to missing.",
        out_of_range_count
      )
    )
    parsed_scores[out_of_range_mask] <- NA_real_
  }

  clean_scores <- long_data |>
    dplyr::mutate(score = parsed_scores) |>
    dplyr::select(
      "animal_id",
      "group_id",
      "group_label",
      "study_id",
      "cage_card",
      "sex",
      "day",
      "score"
    )

  if (!any(!is.na(clean_scores$score))) {
    warnings <- c(
      warnings,
      "Clinical score columns were detected, but they did not contain any numeric score values."
    )
    return(list(data = empty_clean_scores(), warnings = unique(warnings)))
  }

  list(data = clean_scores, warnings = unique(warnings))
}

summarise_scores_data <- function(scores_long,
                                  error_style = c("sem", "sd", "ci95", "none")) {
  error_style <- match.arg(error_style)

  scores_long |>
    dplyr::group_by(.data$group_id, .data$display_name, .data$day) |>
    dplyr::summarise(
      n = sum(!is.na(.data$score)),
      mean = mean(.data$score, na.rm = TRUE),
      sd = stats::sd(.data$score, na.rm = TRUE),
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

score_key_stats <- function(scores_long, group_meta) {
  if (is.null(scores_long) || !nrow(scores_long)) {
    return(tibble::tibble())
  }

  format_mean_sd <- function(mean_value, sd_value) {
    if (is.na(mean_value)) {
      return("NA")
    }
    if (is.na(sd_value)) {
      return(sprintf("%.2f \u00b1 NA", mean_value))
    }
    sprintf("%.2f \u00b1 %.2f", mean_value, sd_value)
  }

  format_peak <- function(score_value, day_value) {
    if (is.na(score_value) || is.na(day_value)) {
      return("NA")
    }
    sprintf("%.2f (DPI %d)", score_value, as.integer(day_value))
  }

  group_order <- group_meta |>
    dplyr::arrange(.data$plot_order, .data$display_name) |>
    dplyr::pull(.data$display_name)

  final_day <- max(scores_long$day, na.rm = TRUE)

  n_per_group <- scores_long |>
    dplyr::distinct(.data$display_name, .data$animal_id) |>
    dplyr::count(.data$display_name, name = "N")

  baseline_stats <- scores_long |>
    dplyr::filter(.data$day == 0L, !is.na(.data$score)) |>
    dplyr::group_by(.data$display_name) |>
    dplyr::summarise(
      baseline_mean = mean(.data$score, na.rm = TRUE),
      baseline_sd = stats::sd(.data$score, na.rm = TRUE),
      .groups = "drop"
    )

  group_day_means <- scores_long |>
    dplyr::filter(!is.na(.data$score)) |>
    dplyr::group_by(.data$display_name, .data$day) |>
    dplyr::summarise(day_mean = mean(.data$score, na.rm = TRUE), .groups = "drop")

  peak_stats <- group_day_means |>
    dplyr::group_by(.data$display_name) |>
    dplyr::slice_max(.data$day_mean, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(.data$display_name, peak_score = .data$day_mean, peak_day = .data$day)

  end_stats <- scores_long |>
    dplyr::filter(.data$day == final_day, !is.na(.data$score)) |>
    dplyr::group_by(.data$display_name) |>
    dplyr::summarise(
      end_mean = mean(.data$score, na.rm = TRUE),
      end_sd = stats::sd(.data$score, na.rm = TRUE),
      .groups = "drop"
    )

  n_per_group |>
    dplyr::left_join(baseline_stats, by = "display_name") |>
    dplyr::left_join(peak_stats, by = "display_name") |>
    dplyr::left_join(end_stats, by = "display_name") |>
    dplyr::mutate(display_name = factor(.data$display_name, levels = group_order)) |>
    dplyr::arrange(.data$display_name) |>
    dplyr::mutate(display_name = as.character(.data$display_name)) |>
    dplyr::transmute(
      Group = .data$display_name,
      N = .data$N,
      `Baseline score (mean +/- SD)` = mapply(format_mean_sd, .data$baseline_mean, .data$baseline_sd),
      `Peak score` = mapply(format_peak, .data$peak_score, .data$peak_day),
      `End-of-study score (mean +/- SD)` = mapply(format_mean_sd, .data$end_mean, .data$end_sd)
    )
}

score_pairwise_tests <- function(scores_long, group_meta, p_adjust_method = "bh") {
  if (is.null(scores_long) || !nrow(scores_long)) {
    return(list(kw_p = NA_real_, pairwise = NULL))
  }

  final_day <- max(scores_long$day, na.rm = TRUE)
  end_data <- scores_long |>
    dplyr::filter(.data$day == final_day, !is.na(.data$score))

  groups <- unique(end_data$display_name)
  if (length(groups) < 2) {
    return(list(kw_p = NA_real_, pairwise = NULL))
  }

  kw_result <- tryCatch(
    stats::kruskal.test(score ~ display_name, data = end_data),
    error = function(e) NULL
  )
  kw_p <- if (!is.null(kw_result)) kw_result$p.value else NA_real_

  if (length(groups) < 3) {
    return(list(kw_p = kw_p, pairwise = NULL))
  }

  adjusted_col <- p_adjusted_column_name(p_adjust_method)
  pw_result <- tryCatch(
    stats::pairwise.wilcox.test(
      end_data$score,
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
