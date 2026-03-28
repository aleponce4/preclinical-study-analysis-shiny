prepare_survival_plot_settings <- function(settings = list()) {
  settings <- normalize_survival_plot_settings(settings)
  warnings <- character()

  if (identical(settings$y_axis_mode, "fixed")) {
    if (is.na(settings$y_min) || is.na(settings$y_max) || settings$y_min >= settings$y_max ||
        settings$y_min < 0 || settings$y_max > 100) {
      warnings <- c(warnings, "Fixed y-axis limits require a valid 0-100 range with min smaller than max. Falling back to auto scale.")
      settings$y_axis_mode <- "auto"
      settings$y_min <- NA_real_
      settings$y_max <- NA_real_
    }
  }

  list(settings = settings, warnings = unique(warnings))
}

survfit_to_tibble <- function(fit, data) {
  summary_fit <- summary(fit)
  group_followup <- tibble::tibble(
    group_id = as.character(data$group_id),
    time = data$time
  ) |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(
      max_time = max(.data$time, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$max_time))

  if (!length(summary_fit$time)) {
    group_ids <- group_followup$group_id
    base_rows <- dplyr::bind_rows(
      tibble::tibble(
        strata = sprintf("group_id=%s", group_ids),
        time = 0,
        surv = 1,
        lower = 1,
        upper = 1,
        n_risk = NA_integer_,
        n_event = 0L,
        group_id = group_ids
      ),
      tibble::tibble(
        strata = sprintf("group_id=%s", group_ids),
        time = group_followup$max_time,
        surv = 1,
        lower = 1,
        upper = 1,
        n_risk = NA_integer_,
        n_event = 0L,
        group_id = group_ids
      )
    )
    return(base_rows |> dplyr::arrange(.data$group_id, .data$time))
  }

  curve_data <- tibble::tibble(
    strata = {
      strata_values <- as.character(summary_fit$strata)
      if (!length(strata_values)) {
        group_ids <- unique(as.character(data$group_id))
        if (length(group_ids) == 1L) {
          rep(sprintf("group_id=%s", group_ids[[1]]), length(summary_fit$time))
        } else {
          rep(NA_character_, length(summary_fit$time))
        }
      } else {
        strata_values
      }
    },
    time = summary_fit$time,
    surv = summary_fit$surv,
    lower = summary_fit$lower %||% rep(NA_real_, length(summary_fit$time)),
    upper = summary_fit$upper %||% rep(NA_real_, length(summary_fit$time)),
    n_risk = summary_fit$n.risk,
    n_event = summary_fit$n.event
  ) |>
    dplyr::mutate(group_id = sub("^group_id=", "", .data$strata))

  start_rows <- tibble::tibble(
    strata = sprintf("group_id=%s", group_followup$group_id),
    time = 0,
    surv = 1,
    lower = 1,
    upper = 1,
    n_risk = NA_integer_,
    n_event = 0L,
    group_id = group_followup$group_id
  )

  end_rows_with_events <- curve_data |>
    dplyr::group_by(.data$group_id, .data$strata) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::left_join(group_followup, by = "group_id") |>
    dplyr::mutate(time = .data$max_time) |>
    dplyr::select(-"max_time")

  groups_with_events <- unique(curve_data$group_id)
  end_rows_no_events <- group_followup |>
    dplyr::filter(!.data$group_id %in% groups_with_events) |>
    dplyr::transmute(
      strata = sprintf("group_id=%s", .data$group_id),
      time = .data$max_time,
      surv = 1,
      lower = 1,
      upper = 1,
      n_risk = NA_integer_,
      n_event = 0L,
      group_id = .data$group_id
    )

  dplyr::bind_rows(start_rows, curve_data, end_rows_with_events, end_rows_no_events) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$group_id, .data$time)
}

censor_points_to_tibble <- function(fit, data) {
  censored_summary <- summary(fit, censored = TRUE)

  if (!length(censored_summary$time)) {
    return(tibble::tibble(
      strata = character(),
      time = numeric(),
      surv = numeric(),
      n_censor = integer(),
      group_id = character()
    ))
  }

  strata_values <- as.character(censored_summary$strata)
  if (!length(strata_values)) {
    group_ids <- unique(as.character(data$group_id))
    if (length(group_ids) == 1L) {
      strata_values <- rep(sprintf("group_id=%s", group_ids[[1]]), length(censored_summary$time))
    } else {
      strata_values <- rep(NA_character_, length(censored_summary$time))
    }
  }

  tibble::tibble(
    strata = strata_values,
    time = censored_summary$time,
    surv = censored_summary$surv,
    n_censor = censored_summary$n.censor %||% rep(0L, length(censored_summary$time))
  ) |>
    dplyr::mutate(group_id = sub("^group_id=", "", .data$strata)) |>
    dplyr::filter(.data$n_censor > 0)
}

survival_curve_signatures <- function(curve_data) {
  if (is.null(curve_data) || !nrow(curve_data)) {
    return(tibble::tibble(group_id = character(), signature = character()))
  }

  curve_data |>
    dplyr::arrange(.data$group_id, .data$time, .data$surv) |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(
      signature = paste(sprintf("%.10f|%.10f", .data$time, .data$surv), collapse = "||"),
      .groups = "drop"
    )
}

survival_has_overlapping_curves <- function(curve_data) {
  signatures <- survival_curve_signatures(curve_data)
  nrow(signatures) > 1 && anyDuplicated(signatures$signature) > 0
}

survival_summary_table <- function(fit, survival_clean, group_meta) {
  fit_table <- summary(fit)$table

  if (is.null(dim(fit_table))) {
    fit_table <- matrix(fit_table, nrow = 1)
    rownames(fit_table) <- names(fit$strata) %||% sprintf("group_id=%s", unique(survival_clean$group_id))
  }

  fit_tbl <- as.data.frame(fit_table, stringsAsFactors = FALSE)
  fit_tbl$strata <- rownames(fit_table)
  fit_tbl$group_id <- sub("^group_id=", "", fit_tbl$strata)

  ci_low  <- suppressWarnings(as.numeric(fit_tbl[["0.95LCL"]]))
  ci_high <- suppressWarnings(as.numeric(fit_tbl[["0.95UCL"]]))
  if (is.null(ci_low))  ci_low  <- rep(NA_real_, nrow(fit_tbl))
  if (is.null(ci_high)) ci_high <- rep(NA_real_, nrow(fit_tbl))

  median_ci_tbl <- tibble::tibble(
    group_id  = fit_tbl$group_id,
    median_days = suppressWarnings(as.numeric(fit_tbl$median)),
    median_ci_low  = ci_low,
    median_ci_high = ci_high
  )

  survival_clean |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(
      n = dplyr::n(),
      deaths = sum(.data$status == 1L, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(censored = .data$n - .data$deaths) |>
    dplyr::left_join(median_ci_tbl, by = "group_id") |>
    dplyr::left_join(group_meta, by = "group_id") |>
    dplyr::transmute(
      Group            = .data$display_name,
      N                = .data$n,
      Deaths           = .data$deaths,
      Censored         = .data$censored,
      `Median (DPI)`   = .data$median_days,
      `Median CI low`  = .data$median_ci_low,
      `Median CI high` = .data$median_ci_high
    ) |>
    dplyr::arrange(.data$Group)
}

scheduled_cohort_summary_table <- function(survival_clean, group_meta) {
  if (is.null(survival_clean) || !nrow(survival_clean)) {
    return(tibble::tibble())
  }

  scheduled <- if ("cohort_role" %in% names(survival_clean)) {
    survival_clean |>
      dplyr::filter(.data$cohort_role == "scheduled_sampling")
  } else if ("include_in_km" %in% names(survival_clean)) {
    survival_clean |>
      dplyr::filter(!dplyr::coalesce(.data$include_in_km, TRUE))
  } else {
    tibble::tibble()
  }

  if (!nrow(scheduled)) {
    return(tibble::tibble())
  }

  scheduled |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(
      `Scheduled cohort rows` = dplyr::n(),
      `Planned scheduled sampling` = sum(.data$event_type == "scheduled_sampling_excluded", na.rm = TRUE),
      `Deaths before planned collection` = sum(.data$event_type == "scheduled_cohort_death_excluded", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(group_meta, by = "group_id") |>
    dplyr::transmute(
      Group = .data$display_name,
      `Scheduled cohort rows`,
      `Planned scheduled sampling`,
      `Deaths before planned collection`
    ) |>
    dplyr::arrange(.data$Group)
}

excluded_survival_summary_table <- function(survival_clean, group_meta) {
  summary_tbl <- scheduled_cohort_summary_table(survival_clean, group_meta)
  if (!nrow(summary_tbl)) {
    return(tibble::tibble())
  }

  summary_tbl |>
    dplyr::rename(`Excluded from KM` = `Scheduled cohort rows`)
}

apply_scheduled_sampling_handling <- function(survival_clean, scheduled_sampling_handling = "exclude") {
  handling <- normalize_scheduled_sampling_handling(scheduled_sampling_handling)

  if (is.null(survival_clean) || !nrow(survival_clean)) {
    return(list(
      handling = handling,
      data = survival_clean,
      scheduled_rows = 0L
    ))
  }

  scheduled_mask <- if ("cohort_role" %in% names(survival_clean)) {
    dplyr::coalesce(survival_clean$cohort_role == "scheduled_sampling", FALSE)
  } else if ("include_in_km" %in% names(survival_clean)) {
    !dplyr::coalesce(survival_clean$include_in_km, TRUE)
  } else {
    rep(FALSE, nrow(survival_clean))
  }

  scheduled_rows <- sum(scheduled_mask, na.rm = TRUE)
  if (!scheduled_rows) {
    return(list(
      handling = handling,
      data = survival_clean,
      scheduled_rows = 0L
    ))
  }

  analysis_data <- survival_clean
  if (!"include_in_km" %in% names(analysis_data)) {
    analysis_data$include_in_km <- TRUE
  }

  if (identical(handling, "exclude")) {
    analysis_data <- analysis_data[!scheduled_mask, , drop = FALSE]
  } else if (identical(handling, "censor")) {
    analysis_data <- analysis_data |>
      dplyr::mutate(
        status = dplyr::if_else(scheduled_mask, 0L, .data$status),
        censored = dplyr::if_else(scheduled_mask, TRUE, .data$censored),
        include_in_km = dplyr::if_else(scheduled_mask, TRUE, dplyr::coalesce(.data$include_in_km, TRUE))
      )
  } else if (identical(handling, "death")) {
    analysis_data <- analysis_data |>
      dplyr::mutate(
        status = dplyr::if_else(scheduled_mask, 1L, .data$status),
        censored = dplyr::if_else(scheduled_mask, FALSE, .data$censored),
        include_in_km = dplyr::if_else(scheduled_mask, TRUE, dplyr::coalesce(.data$include_in_km, TRUE))
      )
  }

  list(
    handling = handling,
    data = analysis_data,
    scheduled_rows = as.integer(scheduled_rows)
  )
}

compute_pairwise_survdiff <- function(survival_clean, group_meta, p_adjust_method = "bh") {
  groups <- unique(as.character(survival_clean$group_id))
  if (length(groups) < 3) return(NULL)

  id_to_name <- stats::setNames(group_meta$display_name, group_meta$group_id)
  pairs <- utils::combn(groups, 2, simplify = FALSE)

  results <- lapply(pairs, function(pair) {
    sub_data <- survival_clean[as.character(survival_clean$group_id) %in% pair, ]
    sub_data$group_id <- droplevels(factor(as.character(sub_data$group_id)))
    diff_fit <- tryCatch(
      suppressWarnings(survival::survdiff(survival::Surv(time, status) ~ group_id, data = sub_data)),
      error = function(e) NULL
    )
    if (is.null(diff_fit)) return(NULL)
    p_val <- suppressWarnings(stats::pchisq(diff_fit$chisq, df = 1, lower.tail = FALSE))
    if (is.nan(p_val) || is.na(p_val)) return(NULL)
    tibble::tibble(
      group_a = id_to_name[[pair[[1]]]] %||% pair[[1]],
      group_b = id_to_name[[pair[[2]]]] %||% pair[[2]],
      p_raw   = p_val
    )
  })

  results <- dplyr::bind_rows(Filter(Negate(is.null), results))
  if (!nrow(results)) return(NULL)

  adjusted_col <- p_adjusted_column_name(p_adjust_method)
  results |>
    dplyr::mutate(p_adjusted = stats::p.adjust(.data$p_raw, method = p_adjust_method_r(p_adjust_method))) |>
    dplyr::transmute(
      `Group A` = .data$group_a,
      `Group B` = .data$group_b,
      `p (raw)` = .data$p_raw,
      !!adjusted_col := .data$p_adjusted
    )
}

compute_survival_analysis <- function(survival_clean,
                                      group_meta,
                                      p_adjust_method = "bh",
                                      scheduled_sampling_handling = "exclude") {
  if (is.null(survival_clean) || !nrow(survival_clean)) {
    return(list(
      valid = FALSE,
      message = "No valid survival data available.",
      scheduled_sampling_handling = normalize_scheduled_sampling_handling(scheduled_sampling_handling),
      scheduled_summary = tibble::tibble(),
      excluded_summary = tibble::tibble()
    ))
  }

  handling_result <- apply_scheduled_sampling_handling(
    survival_clean,
    scheduled_sampling_handling = scheduled_sampling_handling
  )
  scheduled_summary <- scheduled_cohort_summary_table(survival_clean, group_meta)
  excluded_summary <- if (identical(handling_result$handling, "exclude")) {
    excluded_survival_summary_table(survival_clean, group_meta)
  } else {
    tibble::tibble()
  }
  analysis_data <- handling_result$data

  if (!nrow(analysis_data)) {
    return(list(
      valid = FALSE,
      message = "No survival-cohort rows are available for Kaplan-Meier analysis.",
      scheduled_sampling_handling = handling_result$handling,
      scheduled_summary = scheduled_summary,
      excluded_summary = excluded_summary
    ))
  }

  ordered_groups <- group_meta |>
    dplyr::filter(.data$group_id %in% analysis_data$group_id) |>
    dplyr::arrange(.data$plot_order, .data$display_name)

  analysis_data <- analysis_data |>
    dplyr::mutate(group_id = factor(.data$group_id, levels = ordered_groups$group_id))

  fit <- survival::survfit(survival::Surv(time, status) ~ group_id, data = analysis_data)
  curve_data <- survfit_to_tibble(fit, analysis_data) |>
    dplyr::left_join(ordered_groups, by = "group_id") |>
    dplyr::mutate(display_name = dplyr::coalesce(.data$display_name, as.character(.data$group_id)))
  censor_data <- censor_points_to_tibble(fit, analysis_data) |>
    dplyr::left_join(ordered_groups, by = "group_id") |>
    dplyr::mutate(display_name = dplyr::coalesce(.data$display_name, as.character(.data$group_id)))

  p_value <- NA_real_
  if (dplyr::n_distinct(analysis_data$group_id) > 1 && sum(analysis_data$status, na.rm = TRUE) > 0) {
    diff_fit <- survival::survdiff(survival::Surv(time, status) ~ group_id, data = analysis_data)
    p_value <- stats::pchisq(diff_fit$chisq, df = length(diff_fit$n) - 1, lower.tail = FALSE)
  }

  list(
    valid = TRUE,
    message = NULL,
    scheduled_sampling_handling = handling_result$handling,
    fit = fit,
    curve_data = curve_data,
    censor_data = censor_data,
    p_value = p_value,
    summary = survival_summary_table(fit, analysis_data, ordered_groups),
    scheduled_summary = scheduled_summary,
    excluded_summary = excluded_summary,
    pairwise_tests = compute_pairwise_survdiff(analysis_data, ordered_groups, p_adjust_method = p_adjust_method)
  )
}

plot_survival <- function(analysis,
                          metadata = list(),
                          settings = default_survival_plot_settings()) {
  prepared <- prepare_survival_plot_settings(settings)
  settings <- prepared$settings
  uses_fill_scale <- FALSE

  ordered_meta <- analysis$curve_data |>
    dplyr::distinct(.data$group_id, .data$display_name, .data$color, .data$plot_order) |>
    dplyr::arrange(.data$plot_order, .data$display_name, .data$group_id)
  day_min <- floor(min(analysis$curve_data$time, na.rm = TRUE))
  day_max <- ceiling(max(analysis$curve_data$time, na.rm = TRUE))
  day_breaks <- seq(day_min, day_max, by = 1)

  colors <- ordered_meta$color
  names(colors) <- ordered_meta$group_id
  legend_labels <- stats::setNames(ordered_meta$display_name, ordered_meta$group_id)

  fixed_mode <- identical(settings$y_axis_mode, "fixed")
  y_min_tick <- if (fixed_mode) settings$y_min / 100 else 0
  y_max_tick <- if (fixed_mode) settings$y_max / 100 else 1
  y_data_limits <- c(y_min_tick, y_max_tick)
  y_max_plot <- if (isTRUE(all.equal(y_max_tick, 1))) 1.05 else y_max_tick
  y_coord_limits <- c(y_min_tick, y_max_plot)
  major_candidates <- seq(0, 1, by = 0.25)
  minor_candidates <- seq(0, 1, by = 0.05)
  y_major_breaks <- major_candidates[major_candidates >= y_min_tick & major_candidates <= y_max_tick]
  y_minor_breaks <- minor_candidates[minor_candidates >= y_min_tick & minor_candidates <= y_max_tick]
  y_minor_breaks <- setdiff(y_minor_breaks, y_major_breaks)

  # Per-group vertical nudge so overlapping KM lines stay distinguishable.
  if (isTRUE(settings$nudge_lines) && nrow(ordered_meta) > 1) {
    apply_surv_nudge <- function(x, nudge, min_val, max_val) {
      out <- x + nudge
      out <- pmax(min_val, pmin(max_val, out))
      # Keep true floor values pinned (0%), but allow top values to spread in headroom.
      anchor <- !is.na(x) & (x <= (min_val + 1e-9))
      out[anchor] <- x[anchor]
      out[is.na(x)] <- NA_real_
      out
    }

    n_grp      <- nrow(ordered_meta)
    nudge_step <- as.numeric(settings$nudge_amount %||% 0.012)
    if (is.na(nudge_step)) nudge_step <- 0.012
    nudge_step <- max(0, min(0.05, nudge_step))
    nudge_tbl  <- tibble::tibble(
      group_id = ordered_meta$group_id,
      .nudge = ((seq_len(n_grp) - 1) - (n_grp - 1) / 2) * nudge_step
    )
    curve_data_plot <- analysis$curve_data |>
      dplyr::left_join(nudge_tbl, by = "group_id") |>
      dplyr::mutate(
        surv  = apply_surv_nudge(.data$surv,  .data$.nudge, y_data_limits[[1]], y_coord_limits[[2]]),
        lower = apply_surv_nudge(.data$lower, .data$.nudge, y_data_limits[[1]], y_coord_limits[[2]]),
        upper = apply_surv_nudge(.data$upper, .data$.nudge, y_data_limits[[1]], y_coord_limits[[2]])
      ) |>
      dplyr::select(-".nudge")
  } else {
    curve_data_plot <- analysis$curve_data
  }

  curve_data_plot <- curve_data_plot |>
    dplyr::mutate(group_id_plot = factor(as.character(.data$group_id), levels = ordered_meta$group_id))

  censor_data_plot <- analysis$censor_data |>
    dplyr::mutate(group_id_plot = factor(as.character(.data$group_id), levels = ordered_meta$group_id))

  subtitle <- metadata$subtitle %||% NULL
  if (isTRUE(settings$show_p_value) && !is.na(analysis$p_value)) {
    p_text <- sprintf("Log-rank p = %.3g", analysis$p_value)
    subtitle <- paste(compact_chr(c(subtitle, p_text)), collapse = " | ")
  }

  plot <- ggplot2::ggplot(
    curve_data_plot,
    if (isTRUE(settings$vary_linetypes)) {
      ggplot2::aes(x = .data$time, y = .data$surv, color = .data$group_id_plot, linetype = .data$group_id_plot, group = .data$group_id_plot)
    } else {
      ggplot2::aes(x = .data$time, y = .data$surv, color = .data$group_id_plot, group = .data$group_id_plot)
    }
  )

  if (isTRUE(settings$show_ci)) {
    ci_data <- curve_data_plot |>
      dplyr::filter(!is.na(.data$lower), !is.na(.data$upper))

    if (nrow(ci_data)) {
      # Convert to step-function form so the CI band matches the stepped KM line.
      # For each row i, duplicate it at time[i+1] while keeping lower/upper from row i,
      # creating the horizontal segment of each step.
      ci_step <- dplyr::bind_rows(
        ci_data,
        ci_data |>
          dplyr::group_by(.data$group_id) |>
          dplyr::arrange(.data$time, .by_group = TRUE) |>
          dplyr::mutate(time = dplyr::lead(.data$time)) |>
          dplyr::filter(!is.na(.data$time)) |>
          dplyr::ungroup()
      ) |>
        dplyr::arrange(.data$group_id, .data$time) |>
        dplyr::mutate(group_id_plot = factor(as.character(.data$group_id), levels = ordered_meta$group_id))

      plot <- plot +
        ggplot2::geom_ribbon(
          data = ci_step,
          ggplot2::aes(
            x = .data$time,
            ymin = .data$lower,
            ymax = .data$upper,
            fill = .data$group_id_plot,
            group = .data$group_id_plot
          ),
          inherit.aes = FALSE,
          alpha = 0.16,
          color = NA
        )
      uses_fill_scale <- TRUE
    }
  }

  step_alpha <- if (isTRUE(settings$vary_linetypes)) 0.85 else 1
  plot <- plot +
    ggplot2::geom_step(linewidth = 1.1, alpha = step_alpha)

  # Death dots: placed at top of the KM drop (prev_surv), not the bottom.
  # geom_step(direction="hv") draws horizontal at surv[i-1] then drops to surv[i],
  # so the visual event point is (time[i], surv[i-1]).
  drop_points <- if ("n_event" %in% names(curve_data_plot)) {
    curve_data_plot |>
      dplyr::group_by(.data$group_id) |>
      dplyr::mutate(dot_y = dplyr::lag(.data$surv, default = 1)) |>
      dplyr::ungroup() |>
      dplyr::filter(dplyr::coalesce(.data$n_event, 0L) > 0L)
  } else {
    curve_data_plot[0, , drop = FALSE]
  }

  if (isTRUE(settings$show_death_marks) && nrow(drop_points)) {
    plot <- plot +
      ggplot2::geom_point(
        data = drop_points,
        ggplot2::aes(x = .data$time, y = .data$dot_y, color = .data$group_id_plot),
        inherit.aes = FALSE,
        shape = 16,
        size = 1.9,
        stroke = 0,
        show.legend = FALSE
      )
  }

  n_groups <- nrow(ordered_meta)
  line_types <- rep_len(
    c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
    n_groups
  )
  names(line_types) <- ordered_meta$group_id

  plot <- plot +
    ggplot2::scale_color_manual(
      values = colors,
      breaks = ordered_meta$group_id,
      labels = legend_labels,
      drop = FALSE
    ) +
    {
      if (isTRUE(settings$vary_linetypes)) {
        ggplot2::scale_linetype_manual(
          values = line_types,
          breaks = ordered_meta$group_id,
          labels = legend_labels,
          drop = FALSE
        )
      } else {
        NULL
      }
    } +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(shape = NA)),
      linetype = ggplot2::guide_legend(override.aes = list(shape = NA))
    ) +
    ggplot2::scale_x_continuous(
      breaks = day_breaks,
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_min_tick, y_max_plot),
      breaks = y_major_breaks,
      minor_breaks = y_minor_breaks,
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = metadata$study_title %||% "Kaplan-Meier Survival",
      subtitle = subtitle,
      x = "DPI",
      y = "Percent Survival",
      color = "Group",
      linetype = if (isTRUE(settings$vary_linetypes)) "Group" else ggplot2::waiver()
    ) +
    ggplot2::theme_bw(base_size = 13, base_family = "sans") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black", family = "sans", face = "bold"),
      axis.text = ggplot2::element_text(colour = "black", face = "bold"),
      axis.title = ggplot2::element_text(colour = "black", face = "bold"),
      legend.text = ggplot2::element_text(colour = "black", face = "bold"),
      legend.title = ggplot2::element_text(colour = "black", face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", linewidth = 0.8),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.5),
      axis.ticks.length = grid::unit(0.18, "cm"),
      plot.title = ggplot2::element_text(face = "bold", colour = "black"),
      plot.subtitle = ggplot2::element_text(colour = "black", face = "bold"),
      plot.title.position = "plot"
    ) +
    legend_theme_for_position(
      settings$legend_position %||% "inset",
      inset_x = 0.98, inset_y = 0.98, inset_just = c(1, 1)
    )

  if (uses_fill_scale) {
    plot <- plot +
      ggplot2::scale_fill_manual(
        values = colors,
        breaks = ordered_meta$group_id,
        labels = legend_labels,
        drop = FALSE
      ) +
      ggplot2::labs(fill = "Group")
  }

  plot
}
