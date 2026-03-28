automation_import_weights <- function(path, source_name = basename(path), sheet = 1, source_format = NULL) {
  requested_format <- tolower(trimws(as.character(source_format %||% "")))
  if (nzchar(requested_format)) {
    if (!requested_format %in% c("excel", "csv")) {
      stop(sprintf("Unsupported source_format '%s'.", requested_format), call. = FALSE)
    }

    if (identical(requested_format, "excel")) {
      return(read_weights_excel_import(path, source_name = source_name, sheet = sheet))
    }

    return(read_weights_import(path, source_name = source_name))
  }

  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    return(read_weights_excel_import(path, source_name = source_name, sheet = sheet))
  }

  read_weights_import(path, source_name = source_name)
}

automation_default_mapping_bundle <- function(weights_imported) {
  build_mapping_bundle(
    weights = guess_field_mapping(weights_imported, weights_field_spec()),
    survival = guess_field_mapping(NULL, survival_field_spec()),
    day_map = default_day_mapping(weights_imported),
    survival_day_map = empty_day_map(),
    score_day_map = default_score_day_mapping(weights_imported)
  )
}

automation_metadata_for_study <- function(study, generated_at = Sys.time()) {
  list(
    study_title = study$report_title %||% study$study_id,
    study_id = study$study_id,
    subtitle = NULL,
    weight_units = "g"
  )
}

automation_study_highlights <- function(weights_long,
                                        clean_survival,
                                        scheduled_sampling_review = empty_scheduled_sampling_review(),
                                        warnings = character()) {
  total_animals <- dplyr::n_distinct(weights_long$animal_id)
  latest_day <- if (nrow(weights_long)) max(weights_long$day, na.rm = TRUE) else NA_integer_
  previous_day <- if (nrow(weights_long) && any(weights_long$day < latest_day, na.rm = TRUE)) {
    max(weights_long$day[weights_long$day < latest_day], na.rm = TRUE)
  } else {
    NA_integer_
  }

  today_weights <- if (is.finite(latest_day)) {
    weights_long |>
      dplyr::filter(.data$day == latest_day, !is.na(.data$weight_g)) |>
      dplyr::distinct(.data$animal_id, .data$group_label)
  } else {
    tibble::tibble()
  }
  active_today_count <- nrow(today_weights)

  low_weight_animals <- if (nrow(weights_long)) {
    weights_long |>
      dplyr::filter(!is.na(.data$pct_baseline), .data$pct_baseline < 80) |>
      dplyr::distinct(.data$animal_id)
  } else {
    tibble::tibble()
  }
  low_weight_count <- nrow(low_weight_animals)

  nadir_row <- if (nrow(weights_long)) {
    weights_long |>
      dplyr::filter(!is.na(.data$pct_baseline)) |>
      dplyr::arrange(.data$pct_baseline, .data$day) |>
      dplyr::slice(1)
  } else {
    tibble::tibble()
  }

  previous_weights <- if (is.finite(previous_day)) {
    weights_long |>
      dplyr::filter(.data$day == previous_day, !is.na(.data$weight_g)) |>
      dplyr::select(
        "animal_id",
        "group_label",
        "cage_card",
        prev_weight_g = "weight_g"
      )
  } else {
    tibble::tibble()
  }

  current_weights <- if (is.finite(latest_day)) {
    weights_long |>
      dplyr::filter(.data$day == latest_day, !is.na(.data$weight_g)) |>
      dplyr::select(
        "animal_id",
        "group_label",
        "cage_card",
        weight_g
      )
  } else {
    tibble::tibble()
  }

  overnight_missing <- if (nrow(previous_weights)) {
    previous_weights |>
      dplyr::anti_join(current_weights, by = "animal_id")
  } else {
    tibble::tibble()
  }

  if (nrow(overnight_missing) && nrow(scheduled_sampling_review) && is.finite(previous_day)) {
    planned_cages <- scheduled_sampling_review |>
      dplyr::filter(.data$role == "scheduled_sampling", .data$censor_day == previous_day) |>
      dplyr::pull(.data$cage_card)

    overnight_missing <- overnight_missing |>
      dplyr::filter(!.data$cage_card %in% planned_cages)
  }

  overnight_loss <- if (nrow(previous_weights) && nrow(current_weights)) {
    previous_weights |>
      dplyr::inner_join(current_weights, by = c("animal_id", "group_label")) |>
      dplyr::mutate(
        pct_change = 100 * (.data$weight_g - .data$prev_weight_g) / .data$prev_weight_g
      ) |>
      dplyr::filter(!is.na(.data$pct_change), .data$pct_change <= -20)
  } else {
    tibble::tibble()
  }

  alerts <- c(
    if (nrow(overnight_missing)) {
      sprintf(
        "Animals newly missing at DPI %d (check for death/euthanasia): %s",
        latest_day,
        paste(overnight_missing$animal_id, collapse = ", ")
      )
    },
    if (nrow(overnight_loss)) {
      sprintf(
        "Animals with >20%% day-over-day weight loss at DPI %d: %s",
        latest_day,
        paste(
          sprintf("%s (%.1f%%)", overnight_loss$animal_id, abs(overnight_loss$pct_change)),
          collapse = ", "
        )
      )
    }
  )

  filtered_warnings <- warnings[!grepl(
    "^Survival inferred from study data",
    warnings %||% character(),
    ignore.case = TRUE
  )]

  items <- c(
    if (total_animals > 0) sprintf("Animals monitored: %d", total_animals),
    if (total_animals > 0 && is.finite(latest_day)) sprintf("Animals present today (DPI %d): %d", latest_day, active_today_count),
    if (low_weight_count > 0) {
      sprintf(
        "Animals that dropped below 80%% of baseline weight: %d of %d",
        low_weight_count,
        total_animals
      )
    } else if (total_animals > 0) {
      "No animals dropped below 80% of baseline weight."
    },
    if (nrow(nadir_row)) {
      sprintf(
        "Lowest observed weight: %.1f%% of baseline in %s at DPI %d",
        nadir_row$pct_baseline[[1]],
        nadir_row$group_label[[1]],
        as.integer(nadir_row$day[[1]])
      )
    }
  )

  list(
    items = items,
    alerts = alerts,
    warnings = filtered_warnings,
    latest_day = latest_day,
    previous_day = previous_day,
    animals_present_today = active_today_count
  )
}

automation_prepare_study_analysis <- function(study,
                                              style,
                                              workbook_path,
                                              generated_at = Sys.time()) {
  imported <- automation_import_weights(
    path = workbook_path,
    source_name = basename(workbook_path),
    sheet = study$weights_sheet %||% 1,
    source_format = study$source_format %||% "excel"
  )

  if (is.null(imported) || is.null(imported$data) || !nrow(imported$data)) {
    stop(sprintf("Study '%s' workbook has no readable rows.", study$study_id), call. = FALSE)
  }

  validation <- validate_study_data(
    raw_weights = imported,
    raw_survival = NULL,
    mapping = automation_default_mapping_bundle(imported),
    infer_death_events = TRUE,
    detect_scheduled_sampling = TRUE,
    scheduled_sampling_cages = study$scheduled_sampling_overrides %||% empty_scheduled_sampling_cages()
  )

  if (length(validation$hard_errors)) {
    stop(
      sprintf("Study '%s' failed weight validation: %s", study$study_id, paste(validation$hard_errors, collapse = "; ")),
      call. = FALSE
    )
  }

  if (length(validation$survival_hard_errors)) {
    stop(
      sprintf("Study '%s' failed survival validation: %s", study$study_id, paste(validation$survival_hard_errors, collapse = "; ")),
      call. = FALSE
    )
  }

  shared_style <- style$shared_style %||% default_shared_style_settings()
  group_meta <- resolve_group_colors(validation$group_meta, shared_style)
  metadata <- automation_metadata_for_study(study, generated_at = generated_at)

  weight_settings_prepared <- prepare_weight_plot_settings(style$weight_plot %||% default_weight_plot_settings())
  score_settings_prepared <- prepare_score_plot_settings(style$score_plot %||% default_score_plot_settings())
  survival_settings_prepared <- prepare_survival_plot_settings(style$survival_plot %||% default_survival_plot_settings())
  stats_settings <- normalize_stats_settings(style$stats %||% default_stats_settings())

  weights_processed <- apply_baseline_rule(
    validation$clean_weights,
    baseline_rule = weight_settings_prepared$settings$baseline_rule
  )

  weights_plot <- plot_weights(
    weights_long = weights_processed$data,
    group_meta = group_meta,
    metadata = metadata,
    settings = weight_settings_prepared$settings
  ) + ggplot2::labs(title = NULL, subtitle = NULL)

  score_data <- validation$clean_scores
  has_scores <- !is.null(score_data) && nrow(score_data) > 0
  scores_plot <- if (has_scores) {
    plot_scores(
      scores_long = score_data,
      group_meta = group_meta,
      metadata = metadata,
      settings = score_settings_prepared$settings
    ) + ggplot2::labs(title = NULL, subtitle = NULL)
  } else {
    NULL
  }

  survival_analysis <- compute_survival_analysis(
    validation$clean_survival,
    group_meta,
    p_adjust_method = stats_settings$p_adjust_method
  )

  if (!isTRUE(survival_analysis$valid)) {
    stop(
      sprintf("Study '%s' survival analysis invalid: %s", study$study_id, survival_analysis$message %||% "unknown issue"),
      call. = FALSE
    )
  }

  survival_plot <- plot_survival(
    analysis = survival_analysis,
    metadata = metadata,
    settings = survival_settings_prepared$settings
  ) + ggplot2::labs(title = NULL, subtitle = NULL)

  weight_tests <- weight_pairwise_tests(
    weights_processed$data,
    group_meta,
    p_adjust_method = stats_settings$p_adjust_method
  )

  score_tests <- if (has_scores) {
    score_pairwise_tests(
      score_data,
      group_meta,
      p_adjust_method = stats_settings$p_adjust_method
    )
  } else {
    list(kw_p = NA_real_, pairwise = NULL)
  }

  list(
    study = study,
    report_name = "Study Monitoring Report",
    source_path = workbook_path,
    source_file = basename(workbook_path),
    generated_at = generated_at,
    metadata = metadata,
    warnings = compact_chr(c(
      validation$warnings,
      weights_processed$warnings,
      weight_settings_prepared$warnings,
      score_settings_prepared$warnings,
      survival_settings_prepared$warnings
    )),
    summary = automation_study_highlights(
      weights_long = weights_processed$data,
      clean_survival = validation$clean_survival,
      scheduled_sampling_review = validation$scheduled_sampling_review,
      warnings = compact_chr(c(
        validation$warnings,
        weights_processed$warnings,
        weight_settings_prepared$warnings,
        score_settings_prepared$warnings,
        survival_settings_prepared$warnings
      ))
    ),
    p_adjust_method = stats_settings$p_adjust_method,
    plots = list(
      weights = weights_plot,
      scores = scores_plot,
      survival = survival_plot
    ),
    tables = list(
      weight_key_stats = weight_key_stats(weights_processed$data, group_meta),
      weight_summary = summarise_weights_data(
        weights_processed$data,
        mode = weight_settings_prepared$settings$mode,
        error_style = weight_settings_prepared$settings$error_style
      ),
      weight_pairwise = weight_tests$pairwise,
      score_key_stats = if (has_scores) score_key_stats(score_data, group_meta) else tibble::tibble(),
      score_summary = if (has_scores) {
        summarise_scores_data(score_data, error_style = score_settings_prepared$settings$error_style)
      } else {
        tibble::tibble()
      },
      score_pairwise = if (has_scores) score_tests$pairwise else NULL,
      survival_summary = survival_analysis$summary,
      survival_pairwise = survival_analysis$pairwise_tests
    ),
    metrics = list(
      weight_kw_p = weight_tests$kw_p,
      score_kw_p = score_tests$kw_p,
      survival_logrank_p = survival_analysis$p_value,
      final_weight_day = if (nrow(weights_processed$data)) max(weights_processed$data$day, na.rm = TRUE) else NA_integer_,
      final_score_day = if (has_scores) max(score_data$day, na.rm = TRUE) else NA_integer_
    ),
    flags = list(
      has_scores = has_scores,
      inferred_survival = TRUE
    )
  )
}
