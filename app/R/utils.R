`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "") || all(is.na(x))) {
    y
  } else {
    x
  }
}

compact_chr <- function(x) {
  unique(stats::na.omit(x[nzchar(x)]))
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)

  repeat {
    has_version <- file.exists(file.path(current, "VERSION"))
    has_app_dir <- dir.exists(file.path(current, "app", "R"))

    if (has_version && has_app_dir) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }

    current <- parent
  }

  normalizePath(start, winslash = "/", mustWork = FALSE)
}

project_path <- function(...) {
  file.path(getOption("labweight.app_root", find_project_root()), ...)
}

read_version <- function() {
  version_file <- project_path("VERSION")
  if (!file.exists(version_file)) {
    return("0.0.0")
  }

  trimws(readLines(version_file, warn = FALSE, n = 1))
}

safe_study_slug <- function(metadata = list(), fallback = "study") {
  candidate <- metadata$study_id %||% metadata$study_title %||% fallback
  janitor::make_clean_names(candidate)
}

clean_group_id <- function(x, allow_dupes = FALSE) {
  normalized <- as.character(x)
  normalized <- stringr::str_replace_all(normalized, c("\u00B5" = "u", "\u03BC" = "u"))
  janitor::make_clean_names(normalized, allow_dupes = allow_dupes)
}

changed_named_values <- function(current, previous = list()) {
  current <- current %||% list()
  previous <- previous %||% list()
  current_names <- names(current) %||% character()
  if (!length(current_names)) {
    return(list())
  }

  changed_names <- current_names[vapply(current_names, function(name) {
    !identical(current[[name]], previous[[name]])
  }, logical(1))]

  if (!length(changed_names)) {
    return(list())
  }

  updates <- current[changed_names]
  names(updates) <- changed_names
  updates
}

update_group_override_colors <- function(group_meta, color_updates = list()) {
  overrides <- tibble::as_tibble(group_meta) |>
    dplyr::select("group_id", "display_name", "plot_order", "custom_color")

  if (!length(color_updates) || !nrow(overrides)) {
    return(normalize_group_overrides(overrides))
  }

  for (group_id in names(color_updates)) {
    input_value <- color_updates[[group_id]]
    if (is.null(input_value) || !nzchar(input_value)) {
      next
    }

    row_id <- which(overrides$group_id == group_id)
    if (!length(row_id)) {
      next
    }

    overrides$custom_color[row_id] <- input_value
  }

  normalize_group_overrides(overrides)
}

invalid_custom_color_messages <- function(group_meta, shared_style = default_shared_style_settings()) {
  shared_style <- normalize_shared_style_settings(shared_style)
  if (
    is.null(group_meta) ||
    !nrow(group_meta) ||
    !"custom_color" %in% names(group_meta) ||
    !identical(shared_style$color_mode, "custom")
  ) {
    return(character())
  }

  invalid_groups <- tibble::as_tibble(group_meta) |>
    dplyr::filter(!is.na(.data$custom_color), nzchar(.data$custom_color), is.na(normalize_hex_color(.data$custom_color)))

  if (!nrow(invalid_groups)) {
    return(character())
  }

  sprintf(
    "Invalid custom colors were ignored for: %s.",
    paste(invalid_groups$display_name, collapse = ", ")
  )
}

format_issue_list <- function(title, items, class = "issue-block") {
  if (!length(items)) {
    return(NULL)
  }

  shiny::div(
    class = class,
    shiny::tags$strong(title),
    shiny::tags$ul(lapply(items, shiny::tags$li))
  )
}

should_render_validation_banner <- function(has_attempted_load, issues) {
  if (!isTRUE(has_attempted_load)) {
    return(FALSE)
  }

  length(compact_chr(unlist(c(
    issues$hard_errors,
    issues$survival_hard_errors,
    issues$warnings
  )))) > 0
}

disabled_numeric_input <- function(input_id, label, value = NA_real_, disabled = FALSE) {
  input_tag <- shiny::numericInput(input_id, label, value = value)
  if (isTRUE(disabled)) {
    input_tag <- htmltools::tagQuery(input_tag)$find("input")$addAttrs(disabled = "disabled")$allTags()
  }
  input_tag
}

disabled_checkbox_input <- function(input_id, label, value = FALSE, disabled = FALSE) {
  input_tag <- shiny::checkboxInput(input_id, label, value = value)
  if (isTRUE(disabled)) {
    input_tag <- htmltools::tagQuery(input_tag)$find("input")$addAttrs(disabled = "disabled")$allTags()
  }
  input_tag
}

parse_yes_no <- function(x) {
  normalized <- tolower(trimws(as.character(x)))

  dplyr::case_when(
    normalized %in% c("y", "yes", "true", "1", "t") ~ TRUE,
    normalized %in% c("n", "no", "false", "0", "f") ~ FALSE,
    normalized %in% c("", "na", "n/a") | is.na(normalized) ~ NA,
    TRUE ~ NA
  )
}

coerce_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

coerce_integer <- function(x) {
  suppressWarnings(as.integer(as.character(x)))
}

legend_position_choices <- function() {
  c(
    "Inset bottom left" = "inset_bottom_left",
    "Inset top right" = "inset_top_right",
    "Top bar" = "top",
    "Bottom bar" = "bottom",
    "Hidden" = "none"
  )
}

p_adjust_method_label <- function(method = "bh") {
  method <- normalize_p_adjust_method(method)
  if (identical(method, "bonferroni")) "Bonferroni" else "BH"
}

p_adjust_method_r <- function(method = "bh") {
  method <- normalize_p_adjust_method(method)
  if (identical(method, "bonferroni")) "bonferroni" else "BH"
}

p_adjusted_column_name <- function(method = "bh") {
  sprintf("p (%s-adj)", p_adjust_method_label(method))
}

p_adjust_method_choices <- function() {
  c(
    "Benjamini-Hochberg (BH)" = "bh",
    "Bonferroni" = "bonferroni"
  )
}

legend_theme_for_position <- function(position = "inset",
                                      inset_x = 0.98,
                                      inset_y = 0.98,
                                      inset_just = c(1, 1)) {
  base <- list(
    legend.key.size = ggplot2::unit(0.8, "lines"),
    legend.text = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 10, face = "bold")
  )

  if (identical(position, "none")) {
    return(do.call(ggplot2::theme, c(base, list(
      legend.position = "none"
    ))))
  }

  if (identical(position, "top") || identical(position, "bottom")) {
    return(do.call(ggplot2::theme, c(base, list(
      legend.position = position,
      legend.direction = "horizontal",
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank()
    ))))
  }

  if (identical(position, "inset_top_right")) {
    inset_x <- 0.98
    inset_y <- 0.98
    inset_just <- c(1, 1)
  } else if (identical(position, "inset_bottom_left")) {
    inset_x <- 0.02
    inset_y <- 0.02
    inset_just <- c(0, 0)
  }

  # Default: inset (boxed inside plot)
  do.call(ggplot2::theme, c(base, list(
    legend.position = "inside",
    legend.position.inside = c(inset_x, inset_y),
    legend.justification = inset_just,
    legend.background = ggplot2::element_rect(
      fill = grDevices::adjustcolor("white", alpha.f = 0.76),
      colour = grDevices::adjustcolor("grey70", alpha.f = 0.75),
      linewidth = 0.3
    )
  )))
}

is_valid_hex_color <- function(x) {
  normalized <- trimws(as.character(x))
  !is.na(normalized) & grepl("^#[0-9A-Fa-f]{6}$", normalized)
}

normalize_hex_color <- function(x) {
  normalized <- trimws(as.character(x))
  normalized[!is_valid_hex_color(normalized)] <- NA_character_
  toupper(normalized)
}

label_choices <- function(imported) {
  if (is.null(imported) || is.null(imported$lookup) || !nrow(imported$lookup)) {
    return(stats::setNames(list(), character()))
  }

  vals <- as.list(imported$lookup$clean)
  names(vals) <- sprintf("%s [%s]", imported$lookup$original, imported$lookup$clean)
  vals
}

empty_df <- function(...) {
  tibble::tibble(...)
}

saveRDS_atomic <- function(object, file) {
  tmp <- tempfile(tmpdir = dirname(file))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(object, tmp)
  if (!file.rename(tmp, file)) {
    if (!file.copy(tmp, file, overwrite = TRUE)) {
      stop(sprintf("Failed to write '%s': both rename and copy failed.", file), call. = FALSE)
    }
  }
  invisible(file)
}

ordered_group_levels <- function(group_meta) {
  group_meta |>
    dplyr::arrange(.data$plot_order, .data$display_name) |>
    dplyr::pull(.data$display_name)
}
