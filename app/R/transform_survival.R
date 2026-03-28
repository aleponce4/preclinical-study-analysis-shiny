pivot_wide_survival <- function(data, survival_day_map) {
  included  <- survival_day_map[survival_day_map$include, ]
  day_cols  <- included$source_column
  day_ints  <- included$day
  meta_cols <- setdiff(names(data), day_cols)

  censored_keywords <- c("y", "yes", "true", "1", "censored", "c",
                          "removed", "removed_other", "removed_study_end")

  result <- lapply(seq_len(nrow(data)), function(i) {
    row_meta   <- data[i, meta_cols, drop = FALSE]
    event_day  <- NA_integer_
    event_type <- NA_character_
    censored   <- NA

    for (j in seq_along(day_cols)) {
      raw        <- data[[day_cols[j]]][i]
      cell       <- trimws(as.character(raw %||% ""))
      cell_lower <- tolower(cell)
      if (!nzchar(cell) || cell_lower %in% c("alive", "a")) next

      event_day  <- day_ints[j]
      censored   <- cell_lower %in% censored_keywords
      event_type <- if (censored) NA_character_ else cell
      break
    }

    if (is.na(event_day)) {
      event_day  <- day_ints[length(day_ints)]
      censored   <- TRUE
      event_type <- NA_character_
    }

    cbind(row_meta,
          event_day  = event_day,
          event_type = event_type,
          censored   = censored,
          stringsAsFactors = FALSE)
  })

  tibble::as_tibble(dplyr::bind_rows(result))
}

empty_scheduled_sampling_review <- function() {
  tibble::tibble(
    group_label = character(),
    cage_card = character(),
    n_mice = integer(),
    observed_last_day_pattern = character(),
    suggested_role = character(),
    role = character(),
    censor_day = integer()
  )
}

build_scheduled_sampling_cage_review <- function(clean_weights,
                                                 detect_scheduled_sampling = TRUE,
                                                 scheduled_sampling_cages = NULL) {
  if (is.null(clean_weights) || !nrow(clean_weights)) {
    return(list(
      data = empty_scheduled_sampling_review(),
      warnings = character(),
      errors = character(),
      study_end = NA_integer_
    ))
  }

  observed <- clean_weights |>
    dplyr::filter(!is.na(.data$weight_g)) |>
    dplyr::group_by(.data$animal_id) |>
    dplyr::summarise(
      group_label = dplyr::first(.data$group_label),
      cage_card = trimws(as.character(dplyr::first(.data$cage_card))),
      last_day = max(.data$day, na.rm = TRUE),
      .groups = "drop"
    )

  if (!nrow(observed)) {
    return(list(
      data = empty_scheduled_sampling_review(),
      warnings = character(),
      errors = character(),
      study_end = NA_integer_
    ))
  }

  study_end <- max(observed$last_day, na.rm = TRUE)
  observed_days <- clean_weights$day[!is.na(clean_weights$day)]
  day_min <- if (length(observed_days)) min(observed_days) else NA_integer_
  day_max <- if (length(observed_days)) max(observed_days) else NA_integer_

  cage_observed <- observed |>
    dplyr::filter(!is.na(.data$cage_card), nzchar(.data$cage_card))

  if (!nrow(cage_observed)) {
    return(list(
      data = empty_scheduled_sampling_review(),
      warnings = character(),
      errors = character(),
      study_end = as.integer(study_end)
    ))
  }

  cage_summary <- cage_observed |>
    dplyr::group_by(.data$cage_card) |>
    dplyr::summarise(
      group_label = paste(unique(stats::na.omit(.data$group_label)), collapse = " / "),
      n_mice = dplyr::n(),
      min_last = min(.data$last_day, na.rm = TRUE),
      max_last = max(.data$last_day, na.rm = TRUE),
      observed_last_day_pattern = paste(sort(unique(.data$last_day)), collapse = ","),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      suggested_role = dplyr::if_else(
        isTRUE(detect_scheduled_sampling) &
          .data$n_mice >= 2L &
          .data$min_last == .data$max_last &
          .data$max_last < study_end,
        "scheduled_sampling",
        "survival"
      ),
      role = .data$suggested_role,
      censor_day = dplyr::if_else(
        .data$suggested_role == "scheduled_sampling",
        as.integer(.data$max_last),
        NA_integer_
      )
    ) |>
    dplyr::select(
      "group_label",
      "cage_card",
      "n_mice",
      "observed_last_day_pattern",
      "suggested_role",
      "role",
      "censor_day"
    )

  overrides <- normalize_scheduled_sampling_cages(scheduled_sampling_cages)
  warnings <- character()
  errors <- character()

  if (nrow(overrides)) {
    unmatched <- setdiff(overrides$cage_card, cage_summary$cage_card)
    if (length(unmatched)) {
      warnings <- c(
        warnings,
        sprintf(
          "Scheduled sampling overrides reference cage_card values absent from weights: %s.",
          paste(unmatched, collapse = ", ")
        )
      )
    }

    cage_summary <- cage_summary |>
      dplyr::left_join(
        overrides |>
          dplyr::rename(override_role = role, override_censor_day = censor_day),
        by = "cage_card"
      ) |>
      dplyr::mutate(
        role = dplyr::coalesce(.data$override_role, .data$role),
        censor_day = dplyr::case_when(
          .data$role == "scheduled_sampling" ~ dplyr::coalesce(.data$override_censor_day, .data$censor_day),
          TRUE ~ NA_integer_
        )
      ) |>
      dplyr::select(-"override_role", -"override_censor_day")
  }

  invalid_censor <- cage_summary |>
    dplyr::filter(
      .data$role == "scheduled_sampling",
      is.na(.data$censor_day) |
        (!is.na(day_min) & .data$censor_day < day_min) |
        (!is.na(day_max) & .data$censor_day > day_max)
    )

  if (nrow(invalid_censor)) {
    errors <- c(
      errors,
      sprintf(
        "Scheduled sampling cages must have a valid censor DPI within the observed study day range: %s.",
        paste(invalid_censor$cage_card, collapse = ", ")
      )
    )
  }

  list(
    data = cage_summary,
    warnings = unique(warnings),
    errors = unique(errors),
    study_end = as.integer(study_end)
  )
}

infer_survival_from_weights <- function(clean_weights,
                                        infer_death_events = TRUE,
                                        detect_scheduled_sampling = TRUE,
                                        scheduled_sampling_cages = NULL) {
  if (is.null(clean_weights) || !nrow(clean_weights)) {
    return(list(
      data = empty_clean_survival(),
      warnings = "No weight data available to infer survival.",
      errors = character(),
      cage_review = empty_scheduled_sampling_review()
    ))
  }

  observed <- clean_weights |>
    dplyr::filter(!is.na(.data$weight_g)) |>
    dplyr::group_by(.data$animal_id) |>
    dplyr::summarise(
      group_id    = dplyr::first(.data$group_id),
      group_label = dplyr::first(.data$group_label),
      study_id    = dplyr::first(.data$study_id),
      cage_card   = trimws(as.character(dplyr::first(.data$cage_card))),
      last_day    = max(.data$day, na.rm = TRUE),
      .groups     = "drop"
    )

  study_end <- if (nrow(observed)) {
    max(observed$last_day, na.rm = TRUE)
  } else {
    max(clean_weights$day, na.rm = TRUE)
  }

  cage_review <- build_scheduled_sampling_cage_review(
    clean_weights = clean_weights,
    detect_scheduled_sampling = detect_scheduled_sampling,
    scheduled_sampling_cages = scheduled_sampling_cages
  )

  scheduled_lookup <- cage_review$data |>
    dplyr::filter(.data$role == "scheduled_sampling") |>
    dplyr::transmute(
      cage_card = .data$cage_card,
      scheduled_censor_day = .data$censor_day
    )

  inferred <- observed |>
    dplyr::left_join(scheduled_lookup, by = "cage_card") |>
    dplyr::mutate(
      is_scheduled_sampling = !is.na(.data$scheduled_censor_day),
      cohort_role = dplyr::if_else(.data$is_scheduled_sampling, "scheduled_sampling", "survival"),
      include_in_km = !.data$is_scheduled_sampling,
      scheduled_death_before_collection = .data$is_scheduled_sampling & .data$last_day < .data$scheduled_censor_day,
      time = dplyr::case_when(
        .data$scheduled_death_before_collection ~ as.integer(.data$last_day),
        .data$is_scheduled_sampling ~ as.integer(.data$scheduled_censor_day),
        TRUE ~ as.integer(.data$last_day)
      ),
      status = dplyr::case_when(
        .data$scheduled_death_before_collection ~ 1L,
        .data$is_scheduled_sampling ~ 0L,
        isTRUE(infer_death_events) & .data$last_day < study_end ~ 1L,
        TRUE ~ 0L
      ),
      censored = .data$status == 0L,
      event_type = dplyr::case_when(
        .data$scheduled_death_before_collection ~ "scheduled_cohort_death_excluded",
        .data$is_scheduled_sampling ~ "scheduled_sampling_excluded",
        isTRUE(infer_death_events) & .data$last_day < study_end ~ "inferred_death",
        TRUE ~ NA_character_
      ),
      notes = dplyr::case_when(
        .data$scheduled_death_before_collection ~ "Excluded from Kaplan-Meier: death occurred in scheduled sampling cohort before planned collection.",
        .data$is_scheduled_sampling ~ "Excluded from Kaplan-Meier: planned scheduled sampling cohort removal.",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(
      "animal_id", "group_id", "group_label", "study_id",
      "time", "status", "censored", "event_type", "notes", "cohort_role", "include_in_km"
    )

  inference_warning <- if (isTRUE(infer_death_events)) {
    "Survival inferred from study data: mice missing from later weigh-ins are assumed dead on their last recorded day; cages marked as scheduled sampling are excluded from Kaplan-Meier and summarized separately."
  } else {
    "Survival inferred from study data without death-event inference: mice are censored at their last recorded day; cages marked as scheduled sampling are excluded from Kaplan-Meier and summarized separately."
  }

  list(
    data = inferred,
    warnings = unique(c(inference_warning, cage_review$warnings)),
    errors = unique(cage_review$errors),
    cage_review = cage_review$data
  )
}

standardize_survival_data <- function(raw_survival, field_mapping, clean_weights,
                                       survival_day_map = NULL) {
  warnings <- character()
  errors <- character()

  if (is.null(raw_survival) || is.null(raw_survival$data)) {
    warnings <- c(warnings, "Survival file absent.")
    return(list(data = empty_clean_survival(), errors = errors, warnings = warnings))
  }

  data <- raw_survival$data

  wide_mode <- !is.null(survival_day_map) && is.data.frame(survival_day_map) &&
               nrow(survival_day_map) > 0 && any(survival_day_map$include)

  if (wide_mode) {
    data <- pivot_wide_survival(data, survival_day_map)
    if (!nzchar(field_mapping$event_day  %||% "")) field_mapping$event_day  <- "event_day"
    if (!nzchar(field_mapping$censored   %||% "")) field_mapping$censored   <- "censored"
    if (!nzchar(field_mapping$event_type %||% "")) field_mapping$event_type <- "event_type"
  }

  required_fields   <- c("animal_id", "event_day", "censored")
  required_to_check <- if (wide_mode) c("animal_id") else required_fields

  missing_fields <- required_to_check[vapply(required_to_check, function(field) {
    column <- field_mapping[[field]] %||% ""
    !nzchar(column) || !column %in% names(data)
  }, logical(1))]

  if (length(missing_fields)) {
    errors <- c(
      errors,
      sprintf(
        "Missing required mapped survival fields: %s.",
        paste(missing_fields, collapse = ", ")
      )
    )
    return(list(data = empty_clean_survival(), errors = unique(errors), warnings = unique(warnings)))
  }

  standardized <- tibble::tibble(
    animal_id = trimws(as.character(data[[field_mapping$animal_id]])),
    time = coerce_integer(data[[field_mapping$event_day]]),
    censored = parse_yes_no(data[[field_mapping$censored]]),
    event_type = if (nzchar(field_mapping$event_type %||% "")) trimws(as.character(data[[field_mapping$event_type]])) else NA_character_,
    group_label_survival = if (nzchar(field_mapping$group %||% "")) trimws(as.character(data[[field_mapping$group]])) else NA_character_,
    notes = if (nzchar(field_mapping$notes %||% "")) trimws(as.character(data[[field_mapping$notes]])) else NA_character_,
    study_id = if (nzchar(field_mapping$study_id %||% "")) trimws(as.character(data[[field_mapping$study_id]])) else NA_character_
  )

  if (anyDuplicated(standardized$animal_id)) {
    errors <- c(errors, "Duplicate animal_id values found in survival.")
  }

  if (!nrow(clean_weights) || !all(c("animal_id", "group_id", "group_label", "study_id") %in% names(clean_weights))) {
    warnings <- c(warnings, "Weight data is not available; survival group assignment could not be validated.")
    return(list(data = empty_clean_survival(), errors = unique(errors), warnings = unique(warnings)))
  }

  weights_reference <- clean_weights |>
    dplyr::distinct(.data$animal_id, .data$group_id, .data$group_label, .data$study_id)

  joined <- standardized |>
    dplyr::left_join(weights_reference, by = "animal_id", suffix = c("_survival", ""))

  unmatched_count <- sum(is.na(joined$group_id))
  if (nrow(joined) > 0 && unmatched_count == nrow(joined)) {
    errors <- c(
      errors,
      "Survival rows reference animal_id values absent from weights for all rows."
    )
  } else if (unmatched_count > 0) {
    warnings <- c(
      warnings,
      sprintf("%s survival row(s) do not match an animal in weights.", unmatched_count)
    )
  }

  group_conflicts <- joined |>
    dplyr::filter(
      !is.na(.data$group_label_survival),
      nzchar(.data$group_label_survival),
      !is.na(.data$group_label)
    ) |>
    dplyr::mutate(survival_group_id = clean_group_id(.data$group_label_survival, allow_dupes = TRUE)) |>
    dplyr::filter(.data$survival_group_id != .data$group_id)

  if (nrow(group_conflicts)) {
    warnings <- c(
      warnings,
      sprintf("%s survival row(s) have group labels that conflict with weights.", nrow(group_conflicts))
    )
  }

  censored_na <- sum(is.na(joined$censored))
  if (censored_na > 0) {
    warnings <- c(
      warnings,
      sprintf("%s survival row(s) have unrecognized censored values and were dropped.", censored_na)
    )
  }

  invalid_time <- sum(is.na(joined$time))
  if (invalid_time > 0) {
    warnings <- c(
      warnings,
      sprintf("%s survival row(s) have invalid event_day values and were dropped.", invalid_time)
    )
  }

  observed_days <- if (nrow(clean_weights) && any(!is.na(clean_weights$day))) {
    range(clean_weights$day, na.rm = TRUE)
  } else {
    c(NA_real_, NA_real_)
  }
  out_of_range <- if (all(is.na(observed_days))) {
    tibble::tibble()
  } else {
    joined |>
      dplyr::filter(!is.na(.data$time), (.data$time < observed_days[[1]] | .data$time > observed_days[[2]]))
  }

  if (nrow(out_of_range)) {
    warnings <- c(
      warnings,
      sprintf("%s survival row(s) fall outside the observed weights day range.", nrow(out_of_range))
    )
  }

  clean_survival <- joined |>
    dplyr::filter(!is.na(.data$group_id), !is.na(.data$time), !is.na(.data$censored)) |>
    dplyr::mutate(
      status = dplyr::if_else(.data$censored, 0L, 1L),
      event_type = dplyr::na_if(.data$event_type, ""),
      cohort_role = "survival",
      include_in_km = TRUE
    ) |>
    dplyr::select(
      "animal_id",
      "group_id",
      "group_label",
      "study_id",
      "time",
      "status",
      "censored",
      "event_type",
      "notes",
      "cohort_role",
      "include_in_km"
    )

  weights_ids <- clean_weights |>
    dplyr::distinct(.data$animal_id)

  missing_survival <- weights_ids |>
    dplyr::anti_join(
      clean_survival |>
        dplyr::distinct(.data$animal_id),
      by = "animal_id"
    )

  if (nrow(missing_survival) && nrow(clean_survival)) {
    warnings <- c(
      warnings,
      sprintf("%s weight animal(s) are missing a survival row.", nrow(missing_survival))
    )
  }

  list(
    data = clean_survival,
    errors = unique(errors),
    warnings = unique(warnings)
  )
}

apply_survival_group_metadata <- function(clean_survival, group_meta) {
  if (is.null(clean_survival) || !nrow(clean_survival)) {
    return(clean_survival)
  }

  clean_survival |>
    dplyr::left_join(group_meta, by = "group_id") |>
    dplyr::mutate(
      display_name = dplyr::coalesce(.data$display_name, .data$group_label),
      plot_order = dplyr::coalesce(.data$plot_order, 999L),
      color = dplyr::coalesce(.data$color, "#4E79A7")
    )
}
