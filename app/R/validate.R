validate_study_data <- function(raw_weights, raw_survival, mapping, group_overrides = NULL,
                                infer_survival = NULL,
                                infer_death_events = NULL,
                                detect_scheduled_sampling = TRUE,
                                scheduled_sampling_cages = NULL) {
  weights_result <- standardize_weights_data(
    raw_weights = raw_weights,
    field_mapping = mapping$weights %||% list(),
    day_mapping = mapping$day_map %||% tibble::tibble()
  )

  group_meta <- merge_group_overrides(weights_result$group_meta, group_overrides)
  clean_weights <- apply_group_metadata(weights_result$data, group_meta)
  scores_result <- standardize_scores_data(
    raw_weights = raw_weights,
    field_mapping = mapping$weights %||% list(),
    score_day_mapping = mapping$score_day_map %||% tibble::tibble()
  )
  clean_scores <- apply_group_metadata(scores_result$data, group_meta)

  no_survival_file <- is.null(raw_survival) || is.null(raw_survival$data)
  infer_death_events <- if (is.null(infer_death_events)) {
    normalize_infer_survival(infer_survival)
  } else {
    normalize_infer_death_events(infer_death_events)
  }
  detect_scheduled_sampling <- normalize_detect_scheduled_sampling(detect_scheduled_sampling)
  scheduled_sampling_cages <- normalize_scheduled_sampling_cages(scheduled_sampling_cages)

  use_inferred <- no_survival_file &&
                  !length(weights_result$errors) && nrow(clean_weights) > 0

  survival_result <- if (use_inferred) {
    inferred <- infer_survival_from_weights(
      clean_weights = clean_weights,
      infer_death_events = infer_death_events,
      detect_scheduled_sampling = detect_scheduled_sampling,
      scheduled_sampling_cages = scheduled_sampling_cages
    )
    list(
      data = inferred$data,
      errors = inferred$errors %||% character(),
      warnings = inferred$warnings %||% character(),
      cage_review = inferred$cage_review %||% empty_scheduled_sampling_review()
    )
  } else if (no_survival_file) {
    standardize_survival_data(
      raw_survival     = raw_survival,
      field_mapping    = mapping$survival %||% list(),
      clean_weights    = clean_weights,
      survival_day_map = mapping$survival_day_map %||% tibble::tibble()
    )
  } else {
    standardize_survival_data(
      raw_survival     = raw_survival,
      field_mapping    = mapping$survival %||% list(),
      clean_weights    = if (length(weights_result$errors)) empty_clean_weights() else clean_weights,
      survival_day_map = mapping$survival_day_map %||% tibble::tibble()
    )
  }

  clean_survival <- apply_survival_group_metadata(survival_result$data, group_meta)

  list(
    hard_errors = unique(weights_result$errors),
    survival_hard_errors = unique(survival_result$errors),
    warnings = unique(c(weights_result$warnings, scores_result$warnings, survival_result$warnings)),
    clean_weights = clean_weights,
    clean_scores = clean_scores,
    clean_survival = clean_survival,
    group_meta = group_meta,
    scheduled_sampling_review = survival_result$cage_review %||% empty_scheduled_sampling_review()
  )
}
