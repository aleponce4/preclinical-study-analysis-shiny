prepare_weight_plot_settings <- function(settings = list()) {
  settings <- normalize_weight_plot_settings(settings)
  warnings <- character()

  if (identical(settings$individual_layer, "none") && !settings$show_mean) {
    warnings <- c(warnings, "At least one weight layer must remain visible. Falling back to show the group mean.")
    settings$show_mean <- TRUE
  }

  if (identical(settings$y_axis_mode, "fixed")) {
    if (is.na(settings$y_min) || is.na(settings$y_max) || settings$y_min >= settings$y_max) {
      warnings <- c(warnings, "Fixed y-axis limits require both min and max, with min smaller than max. Falling back to auto scale.")
      settings$y_axis_mode <- "auto"
      settings$y_min <- NA_real_
      settings$y_max <- NA_real_
    }
  }

  list(settings = settings, warnings = unique(warnings))
}

weight_y_expand <- function(start_at_zero = FALSE) {
  if (isTRUE(start_at_zero)) {
    return(ggplot2::expansion(mult = c(0.00, 0.10)))
  }

  ggplot2::expansion(mult = c(0.06, 0.10))
}

weight_group_offsets <- function(levels, width = 0.10) {
  levels <- as.character(levels %||% character())
  n_levels <- length(levels)
  if (!n_levels) {
    return(stats::setNames(numeric(), character()))
  }

  half_spread <- width * (n_levels - 1) / 2
  offsets <- seq(-half_spread, half_spread, length.out = n_levels)

  stats::setNames(offsets, levels)
}

weight_legend_labels <- function(weights_long, group_meta, show_legend_n = FALSE) {
  ordered_meta <- group_meta |>
    dplyr::arrange(.data$plot_order, .data$display_name)

  if (!isTRUE(show_legend_n)) {
    return(stats::setNames(ordered_meta$display_name, ordered_meta$display_name))
  }

  cohort_n <- weights_long |>
    dplyr::distinct(.data$group_id, .data$animal_id) |>
    dplyr::count(.data$group_id, name = "cohort_n")

  legend_tbl <- ordered_meta |>
    dplyr::left_join(cohort_n, by = "group_id") |>
    dplyr::mutate(
      cohort_n = dplyr::coalesce(.data$cohort_n, 0L),
      legend_label = sprintf("%s (N=%d)", .data$display_name, .data$cohort_n)
    )

  stats::setNames(legend_tbl$legend_label, legend_tbl$display_name)
}

plot_weights <- function(weights_long,
                         group_meta,
                         metadata = list(),
                         settings = default_weight_plot_settings()) {
  prepared <- prepare_weight_plot_settings(settings)
  settings <- prepared$settings

  value_col <- if (identical(settings$mode, "raw")) "weight_g" else "pct_baseline"
  y_label <- if (identical(settings$mode, "raw")) {
    sprintf("Weight (%s)", metadata$weight_units %||% "g")
  } else {
    "% Change in Body Weight"
  }

  ordered_meta <- group_meta |>
    dplyr::arrange(.data$plot_order, .data$display_name)
  day_min <- floor(min(weights_long$day, na.rm = TRUE))
  day_max <- ceiling(max(weights_long$day, na.rm = TRUE))
  day_breaks <- seq(day_min, day_max, by = 1)

  legend_labels <- weight_legend_labels(weights_long, ordered_meta, settings$show_legend_n)
  levels <- ordered_meta$display_name
  colors <- ordered_meta$color
  names(colors) <- ordered_meta$display_name
  offset_map <- weight_group_offsets(levels, width = settings$point_spread %||% 0.10)
  stagger_points <- identical(settings$individual_layer, "points") && !isTRUE(settings$show_mean)
  show_point_summary <- identical(settings$individual_layer, "points") && !isTRUE(settings$show_mean)
  summary_error_style <- if (show_point_summary && identical(settings$error_style, "none")) "sem" else settings$error_style

  plot_data <- weights_long |>
    dplyr::mutate(
      group_display = factor(.data$display_name, levels = levels),
      x_stagger = .data$day + unname(offset_map[as.character(.data$display_name)]),
      x_point = if (stagger_points) .data$x_stagger else .data$day
    )
  uses_fill_scale <- isTRUE(settings$show_mean) &&
    !identical(summary_error_style, "none") &&
    !identical(settings$error_display, "bar")

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$day, y = .data[[value_col]], color = .data$group_display))

  if (identical(settings$individual_layer, "lines")) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(group = .data$animal_id), alpha = 0.35, linewidth = 0.5, na.rm = TRUE)
  } else if (identical(settings$individual_layer, "points")) {
    plot <- plot +
      ggplot2::geom_point(
        data = plot_data,
        ggplot2::aes(x = .data$x_point, y = .data[[value_col]], color = .data$group_display),
        inherit.aes = FALSE,
        alpha = 0.55, size = 1.8, na.rm = TRUE
      )
  }

  if (isTRUE(settings$show_mean) || show_point_summary) {
    summary_data <- summarise_weights_data(
      plot_data,
      mode = settings$mode,
      error_style = summary_error_style
    ) |>
      dplyr::mutate(
        group_display = factor(.data$group, levels = levels),
        x_stagger = .data$day + unname(offset_map[as.character(.data$group)]),
        x_mean = if (show_point_summary) .data$x_stagger else .data$day
      )

    if (!identical(summary_error_style, "none")) {
      if (show_point_summary || identical(settings$error_display, "bar")) {
        plot <- plot +
          ggplot2::geom_errorbar(
            data = summary_data,
            ggplot2::aes(
              x = .data$x_mean,
              y = .data$mean,
              ymin = .data$error_low,
              ymax = .data$error_high,
              color = .data$group_display
            ),
            inherit.aes = FALSE,
            width = if (show_point_summary) 0.10 else 0.16,
            linewidth = if (show_point_summary) 0.42 else 0.5,
            alpha = if (show_point_summary) 0.72 else 1,
            na.rm = TRUE,
            show.legend = FALSE
          )
      } else {
        plot <- plot +
          ggplot2::geom_ribbon(
            data = summary_data,
            ggplot2::aes(
              x = .data$x_mean,
              ymin = .data$error_low,
              ymax = .data$error_high,
              fill = .data$group_display,
              group = .data$group_display
            ),
            inherit.aes = FALSE,
            alpha = 0.16,
            color = NA,
            na.rm = TRUE
          )
      }
    }

    if (show_point_summary) {
      plot <- plot +
        ggplot2::geom_point(
          data = summary_data,
          ggplot2::aes(x = .data$x_mean, y = .data$mean, color = .data$group_display),
          inherit.aes = FALSE,
          size = 1.5,
          alpha = 0.9,
          na.rm = TRUE,
          show.legend = FALSE
        )
    } else {
      plot <- plot +
        ggplot2::geom_line(
          data = summary_data,
          ggplot2::aes(x = .data$x_mean, y = .data$mean, color = .data$group_display, group = .data$group_display),
          inherit.aes = FALSE,
          linewidth = 1.1,
          na.rm = TRUE
        )
    }
  }

  plot <- plot +
    ggplot2::scale_color_manual(values = colors, breaks = levels, labels = legend_labels, drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = day_breaks
    ) +
    ggplot2::labs(
      title = metadata$study_title %||% "Weight Trajectory",
      subtitle = metadata$subtitle %||% NULL,
      x = "DPI",
      y = y_label,
      color = "Group"
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
      inset_x = 0.02, inset_y = 0.02, inset_just = c(0, 0)
    )

  if (uses_fill_scale) {
    plot <- plot +
      ggplot2::scale_fill_manual(values = colors, breaks = levels, labels = legend_labels, drop = FALSE) +
      ggplot2::labs(fill = "Group")
  }

  if (!identical(settings$y_axis_mode, "fixed")) {
    plot <- plot +
      ggplot2::scale_y_continuous(expand = weight_y_expand(settings$start_at_zero))
  }

  if (identical(settings$y_axis_mode, "fixed")) {
    plot <- plot + ggplot2::coord_cartesian(ylim = c(settings$y_min, settings$y_max))
  } else if (isTRUE(settings$start_at_zero)) {
    plot <- plot + ggplot2::coord_cartesian(ylim = c(0, NA))
  }

  plot
}
