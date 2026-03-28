weight_plot_filename <- function(metadata, fallback, mode = c("raw", "pct_baseline"), ext = c("png", "pdf", "tiff")) {
  mode <- match.arg(mode)
  ext <- match.arg(ext)
  sprintf("%s_weights_%s.%s", safe_study_slug(metadata, fallback), mode, ext)
}

score_plot_filename <- function(metadata, fallback, ext = c("png", "pdf", "tiff")) {
  ext <- match.arg(ext)
  sprintf("%s_scores.%s", safe_study_slug(metadata, fallback), ext)
}

survival_plot_filename <- function(metadata, fallback, ext = c("png", "pdf", "tiff")) {
  ext <- match.arg(ext)
  sprintf("%s_survival.%s", safe_study_slug(metadata, fallback), ext)
}

weight_csv_filename <- function(metadata, fallback, mode = c("raw", "pct_baseline")) {
  mode <- match.arg(mode)
  sprintf("%s_weights_%s_graphpad.csv", safe_study_slug(metadata, fallback), mode)
}

score_csv_filename <- function(metadata, fallback) {
  sprintf("%s_scores_graphpad.csv", safe_study_slug(metadata, fallback))
}

survival_csv_filename <- function(metadata, fallback) {
  sprintf("%s_survival_graphpad.csv", safe_study_slug(metadata, fallback))
}

# Pivot long weight data to wide GraphPad "Grouped" format.
# Rows = days; columns = "{display_name}_{animal_id}" ordered by group then animal_id.
# Values: weight_g (raw) or pct_baseline (% baseline). Missing weights → blank cell.
graphpad_weights_csv <- function(weights_long, group_meta, mode = c("raw", "pct_baseline")) {
  mode      <- match.arg(mode)
  value_col <- if (identical(mode, "raw")) "weight_g" else "pct_baseline"

  if (!value_col %in% names(weights_long)) {
    return(data.frame(Day = integer()))
  }

  ordered_meta <- dplyr::arrange(group_meta, .data$plot_order, .data$display_name)
  gm_lookup    <- stats::setNames(ordered_meta$display_name, ordered_meta$group_id)

  ordered_animals <- weights_long |>
    dplyr::distinct(.data$group_id, .data$animal_id) |>
    dplyr::mutate(
      display_name = gm_lookup[.data$group_id],
      col_name     = paste(.data$display_name, .data$animal_id, sep = "_")
    ) |>
    dplyr::arrange(match(.data$group_id, ordered_meta$group_id), .data$animal_id)

  col_order <- ordered_animals$col_name

  tagged <- weights_long |>
    dplyr::mutate(
      col_name = paste(gm_lookup[.data$group_id], .data$animal_id, sep = "_")
    ) |>
    dplyr::select("day", "col_name", value = dplyr::all_of(value_col))

  wide <- tagged |>
    tidyr::pivot_wider(names_from = "col_name", values_from = "value") |>
    dplyr::arrange(.data$day) |>
    dplyr::rename(Day = "day")

  present_cols <- intersect(col_order, names(wide))
  wide[, c("Day", present_cols), drop = FALSE]
}

graphpad_scores_csv <- function(scores_long, group_meta) {
  if (is.null(scores_long) || !nrow(scores_long) || !"score" %in% names(scores_long)) {
    return(data.frame(Day = integer()))
  }

  ordered_meta <- dplyr::arrange(group_meta, .data$plot_order, .data$display_name)
  gm_lookup <- stats::setNames(ordered_meta$display_name, ordered_meta$group_id)

  ordered_animals <- scores_long |>
    dplyr::distinct(.data$group_id, .data$animal_id) |>
    dplyr::mutate(
      display_name = gm_lookup[.data$group_id],
      col_name = paste(.data$display_name, .data$animal_id, sep = "_")
    ) |>
    dplyr::arrange(match(.data$group_id, ordered_meta$group_id), .data$animal_id)

  col_order <- ordered_animals$col_name

  tagged <- scores_long |>
    dplyr::mutate(
      col_name = paste(gm_lookup[.data$group_id], .data$animal_id, sep = "_")
    ) |>
    dplyr::select("day", "col_name", value = .data$score)

  wide <- tagged |>
    tidyr::pivot_wider(names_from = "col_name", values_from = "value") |>
    dplyr::arrange(.data$day) |>
    dplyr::rename(Day = "day")

  present_cols <- intersect(col_order, names(wide))
  wide[, c("Day", present_cols), drop = FALSE]
}

# Build GraphPad Prism "Survival" table format.
# One Time column (all groups stacked) + one column per group (1=event, 0=censored, blank=other group).
# Convention: 1 = event occurred (died), 0 = censored — matches internal status column directly.
graphpad_survival_csv <- function(clean_survival,
                                  group_meta,
                                  scheduled_sampling_handling = "exclude") {
  if (is.null(clean_survival) || !nrow(clean_survival)) return(data.frame())

  clean_survival <- apply_scheduled_sampling_handling(
    clean_survival,
    scheduled_sampling_handling = scheduled_sampling_handling
  )$data

  if (!nrow(clean_survival)) return(data.frame())

  ordered_meta <- group_meta |>
    dplyr::arrange(.data$plot_order, .data$display_name) |>
    dplyr::filter(.data$group_id %in% clean_survival$group_id)

  group_names <- ordered_meta$display_name

  group_dfs <- lapply(seq_len(nrow(ordered_meta)), function(i) {
    rows <- clean_survival |>
      dplyr::filter(.data$group_id == ordered_meta$group_id[[i]]) |>
      dplyr::arrange(.data$animal_id)

    df <- data.frame(Time = rows$time, stringsAsFactors = FALSE)
    for (j in seq_along(group_names)) {
      df[[group_names[[j]]]] <- if (j == i) rows$status else NA_integer_
    }
    df
  })

  result <- do.call(rbind, group_dfs)
  rownames(result) <- NULL
  as.data.frame(result, check.names = FALSE, stringsAsFactors = FALSE)
}

save_plot_file <- function(plot_object, file, width = 5, height = 3, dpi = 300) {
  ext <- tolower(tools::file_ext(file))
  if (identical(ext, "tif")) {
    ext <- "tiff"
  }

  extra_args <- switch(
    ext,
    tiff = list(device = "tiff", compression = "lzw"),
    png = list(device = "png"),
    pdf = list(device = "pdf"),
    list()
  )

  do.call(
    ggplot2::ggsave,
    c(
      list(
        filename = file,
        plot = plot_object,
        width = width,
        height = height,
        dpi = dpi,
        units = "in"
      ),
      extra_args
    )
  )
}
