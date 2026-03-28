direct_entry_widget_available <- function() {
  requireNamespace("rhandsontable", quietly = TRUE)
}

direct_entry_group_keys <- function(config = default_direct_entry_settings()) {
  config <- normalize_direct_entry_settings(config)
  paste0("g", seq_len(config$n_groups))
}

direct_entry_matrix_col_spec <- function(config = default_direct_entry_settings()) {
  config <- normalize_direct_entry_settings(config)
  keys <- direct_entry_group_keys(config)

  spec <- lapply(seq_len(config$n_groups), function(i) {
    group_name <- config$group_names[[i]]
    mouse_labels <- config$mouse_labels[[i]] %||% sprintf("M%d", seq_len(config$mice_per_group))
    tibble::tibble(
      group_index = i,
      group_key = keys[[i]],
      group_name = group_name,
      mouse_index = seq_len(config$mice_per_group),
      mouse_label = mouse_labels[seq_len(config$mice_per_group)],
      col_key = sprintf("%s_m%d", keys[[i]], seq_len(config$mice_per_group)),
      col_label = sprintf("%s / %s", group_name, mouse_labels[seq_len(config$mice_per_group)])
    )
  })

  dplyr::bind_rows(spec)
}

direct_entry_matrix_headers <- function(config = default_direct_entry_settings()) {
  spec <- direct_entry_matrix_col_spec(config)
  c("Study Day", spec$col_label)
}

direct_entry_col_order <- function(config = default_direct_entry_settings()) {
  spec <- direct_entry_matrix_col_spec(config)
  c("dpi", spec$col_key)
}

direct_entry_col_headers <- function(config = default_direct_entry_settings()) {
  direct_entry_matrix_headers(config)
}

direct_entry_resize_config <- function(config = default_direct_entry_settings(),
                                       n_groups = NULL,
                                       mice_per_group = NULL) {
  config <- normalize_direct_entry_settings(config)
  config$n_groups <- n_groups %||% config$n_groups
  config$mice_per_group <- mice_per_group %||% config$mice_per_group
  normalize_direct_entry_settings(config)
}

direct_entry_parse_dpi_values <- function(x) {
  values <- trimws(as.character(x))
  values[is.na(values)] <- ""
  vapply(values, function(value) {
    if (!nzchar(value)) {
      return(NA_integer_)
    }
    if (!grepl("^[-+]?\\d+$", value, perl = TRUE)) {
      return(NA_integer_)
    }
    suppressWarnings(as.integer(value))
  }, integer(1))
}

direct_entry_normalize_clipboard_text <- function(x) {
  text <- as.character(x %||% "")
  text <- iconv(text, from = "", to = "UTF-8", sub = "")
  # Remove invisible separators that often appear in Excel/web clipboard payloads.
  text <- gsub("[\u200B\u200C\u200D\uFEFF]", "", text, perl = TRUE)
  # Normalize uncommon unicode spaces to plain spaces for predictable parsing.
  gsub("[\u00A0\u1680\u180E\u2000-\u200A\u202F\u205F\u3000]", " ", text, perl = TRUE)
}

direct_entry_split_pasted_values <- function(value) {
  text <- direct_entry_normalize_clipboard_text(value)
  text <- gsub("\r\n?", "\n", text, perl = TRUE)
  text <- trimws(text)
  if (!nzchar(text)) {
    return(character())
  }

  has_tab_or_newline <- grepl("[\t\n;]", text, perl = TRUE)
  comma_matches <- gregexpr(",", text, fixed = TRUE)[[1]]
  comma_count <- if (length(comma_matches) == 1L && comma_matches[[1]] == -1L) 0L else length(comma_matches)
  has_csv_like_commas <- grepl(",\\s+", text, perl = TRUE) || comma_count >= 2L

  if (!has_tab_or_newline && !has_csv_like_commas) {
    return(character())
  }

  split_pattern <- if (has_tab_or_newline) "[\t\n;,]+" else ","
  tokens <- trimws(unlist(strsplit(text, split_pattern, perl = TRUE), use.names = FALSE))
  tokens <- trimws(direct_entry_normalize_clipboard_text(tokens))
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) < 2L) {
    return(character())
  }

  parsed <- parse_weight_values(tokens)
  if (any(is.na(parsed))) {
    return(character())
  }

  format(parsed, trim = TRUE, scientific = FALSE)
}

direct_entry_matrix_row_spec <- function(config = default_direct_entry_settings()) {
  spec <- direct_entry_matrix_col_spec(config)
  spec |>
    dplyr::select("group_index", "mouse_index", "group_name", "col_key", "mouse_label")
}

direct_entry_ui_day_values <- function(sheet_df, config = default_direct_entry_settings()) {
  config <- normalize_direct_entry_settings(config)
  sheet <- sync_direct_entry_matrix(
    sheet_df,
    config = config,
    rows = nrow(tibble::as_tibble(sheet_df %||% tibble::tibble()))
  )
  parsed <- direct_entry_parse_dpi_values(sheet$dpi)
  days <- parsed[!is.na(parsed) & parsed >= 0L]
  if (!length(days)) {
    days <- config$dpi_rows
  }
  as.integer(sort(unique(days)))
}

direct_entry_matrix_to_ui_table <- function(sheet_df,
                                            config = default_direct_entry_settings()) {
  config <- normalize_direct_entry_settings(config)
  row_spec <- direct_entry_matrix_row_spec(config)
  sheet <- sync_direct_entry_matrix(
    sheet_df,
    config = config,
    rows = nrow(tibble::as_tibble(sheet_df %||% tibble::tibble()))
  )
  days <- direct_entry_ui_day_values(sheet, config)

  parsed <- direct_entry_parse_dpi_values(sheet$dpi)
  day_index <- stats::setNames(seq_len(nrow(sheet)), as.character(parsed))

  ui <- tibble::tibble(
    Group = as.character(row_spec$group_name),
    `Mouse ID` = as.character(row_spec$mouse_label)
  )

  for (day in days) {
    day_col <- paste0("D", day)
    row_id <- suppressWarnings(as.integer(day_index[[as.character(day)]]))
    if (is.na(row_id) || row_id < 1L || row_id > nrow(sheet)) {
      ui[[day_col]] <- rep("", nrow(row_spec))
    } else {
      ui[[day_col]] <- as.character(sheet[row_id, row_spec$col_key, drop = TRUE])
    }
  }

  tibble::as_tibble(ui)
}

direct_entry_mouse_labels_from_ui_table <- function(ui_df,
                                                    config = default_direct_entry_settings()) {
  config <- normalize_direct_entry_settings(config)
  ui <- tibble::as_tibble(ui_df %||% tibble::tibble())
  row_spec <- direct_entry_matrix_row_spec(config)

  if (!"Mouse ID" %in% names(ui) || !nrow(row_spec)) {
    return(config)
  }

  ids <- as.character(ui$`Mouse ID`)
  if (length(ids) < nrow(row_spec)) {
    ids <- c(ids, rep("", nrow(row_spec) - length(ids)))
  }
  ids <- trimws(ids[seq_len(nrow(row_spec))])

  updated <- config
  for (i in seq_len(nrow(row_spec))) {
    group_idx <- row_spec$group_index[[i]]
    mouse_idx <- row_spec$mouse_index[[i]]
    labels <- updated$mouse_labels[[group_idx]] %||% sprintf("M%d", seq_len(updated$mice_per_group))
    labels[[mouse_idx]] <- ids[[i]]
    updated$mouse_labels[[group_idx]] <- labels
  }

  normalize_direct_entry_settings(updated)
}

direct_entry_ui_table_to_matrix <- function(ui_df,
                                            config = default_direct_entry_settings(),
                                            base_sheet = NULL) {
  config <- normalize_direct_entry_settings(config)
  row_spec <- direct_entry_matrix_row_spec(config)
  ui <- tibble::as_tibble(ui_df %||% tibble::tibble())

  day_cols <- names(ui)[grepl("^D\\d+$", names(ui), perl = TRUE)]
  days <- suppressWarnings(as.integer(sub("^D", "", day_cols)))
  days <- days[!is.na(days) & days >= 0L]
  if (!length(days)) {
    days <- direct_entry_ui_day_values(base_sheet, config)
  }
  if (!length(days)) {
    days <- config$dpi_rows
  }
  days <- as.integer(sort(unique(days)))
  day_cols <- paste0("D", days)

  matrix <- sync_direct_entry_matrix(
    empty_direct_entry_matrix(config, dpi_rows = days),
    config = config,
    rows = length(days)
  )
  matrix$dpi <- as.character(days)

  if (nrow(ui) < nrow(row_spec)) {
    pad <- tibble::as_tibble(stats::setNames(
      replicate(length(names(ui)), rep("", nrow(row_spec) - nrow(ui)), simplify = FALSE),
      names(ui)
    ))
    ui <- dplyr::bind_rows(ui, pad)
  }

  for (i in seq_len(nrow(row_spec))) {
    col_key <- row_spec$col_key[[i]]
    if (!length(day_cols)) {
      matrix[[col_key]] <- rep("", nrow(matrix))
      next
    }
    values <- vapply(day_cols, function(day_col) {
      as.character(ui[[day_col]][[i]] %||% "")
    }, character(1))
    matrix[[col_key]] <- unname(values)
  }

  tibble::as_tibble(matrix)
}

direct_entry_expand_pasted_ui_cells <- function(ui_df,
                                                row_index = NULL,
                                                col_name = NULL,
                                                value = NULL) {
  ui <- tibble::as_tibble(ui_df %||% tibble::tibble())
  if (!nrow(ui) || !ncol(ui)) {
    return(ui)
  }

  day_positions <- which(grepl("^D\\d+$", names(ui), perl = TRUE))
  if (!length(day_positions)) {
    return(ui)
  }

  apply_tokens <- function(row_id, col_id, tokens) {
    if (!length(tokens) || is.na(row_id) || is.na(col_id)) return(invisible(NULL))
    if (!col_id %in% day_positions) return(invisible(NULL))
    row_targets <- seq.int(row_id, min(nrow(ui), row_id + length(tokens) - 1L))
    if (!length(row_targets)) return(invisible(NULL))
    limit <- min(length(row_targets), length(tokens))
    for (i in seq_len(limit)) {
      ui[[col_id]][[row_targets[[i]]]] <<- tokens[[i]]
    }
    invisible(NULL)
  }

  if (!is.null(row_index) && !is.null(col_name) && !is.null(value)) {
    row_id <- coerce_integer(row_index)
    col_id <- match(as.character(col_name), names(ui))
    if (!is.na(row_id) && row_id >= 1L && row_id <= nrow(ui) && !is.na(col_id)) {
      tokens <- direct_entry_split_pasted_values(value)
      apply_tokens(row_id, col_id, tokens)
    }
    return(ui)
  }

  for (row_id in seq_len(nrow(ui))) {
    for (col_id in day_positions) {
      tokens <- direct_entry_split_pasted_values(ui[[col_id]][[row_id]])
      apply_tokens(row_id, col_id, tokens)
    }
  }

  ui
}

direct_entry_expand_pasted_cells <- function(sheet_df,
                                             config = default_direct_entry_settings(),
                                             row_index = NULL,
                                             col_name = NULL,
                                             value = NULL) {
  sheet <- sync_direct_entry_matrix(sheet_df, config = config, rows = nrow(tibble::as_tibble(sheet_df)))
  if (!nrow(sheet) || ncol(sheet) < 2L) {
    return(sheet)
  }

  apply_tokens <- function(row_id, col_id, tokens) {
    if (!length(tokens) || is.na(row_id) || is.na(col_id)) return(invisible(NULL))
    targets <- seq.int(col_id, min(ncol(sheet), col_id + length(tokens) - 1L))
    token_idx <- seq_along(targets)
    for (i in seq_along(targets)) {
      sheet[[targets[[i]]]][[row_id]] <<- tokens[[token_idx[[i]]]]
    }
    invisible(NULL)
  }

  if (!is.null(row_index) && !is.null(col_name) && !is.null(value)) {
    row_id <- coerce_integer(row_index)
    col_id <- match(as.character(col_name), names(sheet))
    if (!is.na(row_id) && row_id >= 1L && row_id <= nrow(sheet) && !is.na(col_id) && col_id >= 2L) {
      tokens <- direct_entry_split_pasted_values(value)
      apply_tokens(row_id, col_id, tokens)
    }
    return(sheet)
  }

  for (row_id in seq_len(nrow(sheet))) {
    for (col_id in seq.int(2L, ncol(sheet))) {
      tokens <- direct_entry_split_pasted_values(sheet[[col_id]][[row_id]])
      apply_tokens(row_id, col_id, tokens)
    }
  }

  sheet
}

empty_direct_entry_matrix <- function(config = default_direct_entry_settings(), dpi_rows = NULL) {
  config <- normalize_direct_entry_settings(config)
  spec <- direct_entry_matrix_col_spec(config)
  dpi_rows <- dpi_rows %||% config$dpi_rows
  dpi_rows <- vapply(as.list(dpi_rows), coerce_integer, integer(1))
  dpi_rows <- dpi_rows[!is.na(dpi_rows) & dpi_rows >= 0]
  if (!length(dpi_rows)) {
    dpi_rows <- config$dpi_rows
  }

  data <- c(
    list(dpi = as.character(dpi_rows)),
    stats::setNames(
      replicate(nrow(spec), rep("", length(dpi_rows)), simplify = FALSE),
      spec$col_key
    )
  )

  tibble::as_tibble(data)
}

sync_direct_entry_matrix <- function(sheet_df,
                                     config = default_direct_entry_settings(),
                                     rows = NULL) {
  config <- normalize_direct_entry_settings(config)
  current <- tibble::as_tibble(sheet_df %||% tibble::tibble())
  spec <- direct_entry_matrix_col_spec(config)
  ordered_cols <- c("dpi", spec$col_key)

  if (!nrow(current)) {
    current <- empty_direct_entry_matrix(config, dpi_rows = config$dpi_rows)
  }

  missing_cols <- setdiff(ordered_cols, names(current))
  for (col in missing_cols) {
    current[[col]] <- rep("", nrow(current))
  }

  current <- current |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::coalesce(as.character(.x), ""))) |>
    dplyr::select(dplyr::all_of(ordered_cols))

  target_rows <- max(
    nrow(current),
    length(config$dpi_rows),
    coerce_integer(rows) %||% 0L,
    1L
  )

  if (nrow(current) < target_rows) {
    extra <- tibble::as_tibble(stats::setNames(
      replicate(length(ordered_cols), rep("", target_rows - nrow(current)), simplify = FALSE),
      ordered_cols
    ))
    current <- dplyr::bind_rows(current, extra)
  }

  dpi_values <- trimws(as.character(current$dpi))
  defaults <- as.character(config$dpi_rows)
  if (!any(nzchar(dpi_values))) {
    fill_n <- min(length(defaults), nrow(current))
    if (fill_n > 0) {
      current$dpi[seq_len(fill_n)] <- defaults[seq_len(fill_n)]
    }
  }

  tibble::as_tibble(current)
}

empty_direct_entry_sheet <- function(config = default_direct_entry_settings(), rows = 20) {
  config <- normalize_direct_entry_settings(config)
  sheet <- empty_direct_entry_matrix(config, dpi_rows = config$dpi_rows)
  sync_direct_entry_matrix(sheet, config, rows = max(coerce_integer(rows), 1L, na.rm = TRUE))
}

sync_direct_entry_sheet <- function(sheet_df, config = default_direct_entry_settings(), rows = NULL) {
  sync_direct_entry_matrix(sheet_df, config = config, rows = rows)
}

direct_entry_matrix_day_map <- function(config = default_direct_entry_settings(), sheet_df = NULL) {
  config <- normalize_direct_entry_settings(config)
  if (is.null(sheet_df)) {
    days <- config$dpi_rows
  } else {
    sheet <- sync_direct_entry_matrix(sheet_df, config = config)
    keep_rows <- apply(sheet[, setdiff(names(sheet), "dpi"), drop = FALSE], 1, function(row) {
      any(nzchar(trimws(as.character(row))))
    }) |>
      as.logical()
    parsed_days <- direct_entry_parse_dpi_values(sheet$dpi)
    days <- parsed_days[keep_rows & !is.na(parsed_days) & parsed_days >= 0]
    days <- sort(unique(days))
  }

  if (!length(days)) {
    days <- config$dpi_rows
  }

  tibble::tibble(
    source_label = paste0("D", days),
    source_column = paste0("d", days),
    day = as.integer(days),
    include = TRUE
  )
}

direct_entry_day_map <- function(config = default_direct_entry_settings(), sheet_df = NULL) {
  direct_entry_matrix_day_map(config = config, sheet_df = sheet_df)
}

direct_entry_weights_mapping <- function() {
  list(
    animal_id = "animal_id",
    group = "group",
    study_id = "study_id",
    cage_card = "cage_card",
    sex = "sex"
  )
}

blank_direct_entry_value <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

direct_entry_safe_id_part <- function(x, fallback = "id") {
  value <- trimws(as.character(x %||% ""))
  if (!nzchar(value)) {
    return(fallback)
  }
  value <- gsub("[^A-Za-z0-9]+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (nzchar(value)) value else fallback
}

direct_entry_matrix_to_import <- function(sheet_df,
                                          config = default_direct_entry_settings(),
                                          source_name = "Manual Data Entry") {
  config <- normalize_direct_entry_settings(config)
  sheet <- sync_direct_entry_matrix(
    sheet_df,
    config = config,
    rows = nrow(tibble::as_tibble(sheet_df %||% tibble::tibble()))
  )
  spec <- direct_entry_matrix_col_spec(config)
  weight_cols <- spec$col_key

  row_has_values <- apply(sheet[, c("dpi", weight_cols), drop = FALSE], 1, function(row) {
    any(nzchar(trimws(as.character(row))))
  })
  sheet <- sheet[row_has_values, , drop = FALSE]

  if (!nrow(sheet)) {
    return(list(
      data = tibble::tibble(),
      lookup = tibble::tibble(
        original = c("Animal ID", "Group", "Study ID", "Cage Card", "Sex"),
        clean = c("animal_id", "group", "study_id", "cage_card", "sex")
      ),
      source_name = source_name,
      source_path = NA_character_
    ))
  }

  dpi_values <- direct_entry_parse_dpi_values(sheet$dpi)
  if (any(is.na(dpi_values))) {
    bad_rows <- which(is.na(dpi_values))
    stop(
      sprintf(
        "Study Day values must be whole numbers. Row%s %s could not be parsed.",
        if (length(bad_rows) == 1L) "" else "s",
        paste(bad_rows, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (any(dpi_values < 0L)) {
    stop("Study Day values must be non-negative.", call. = FALSE)
  }
  if (anyDuplicated(dpi_values)) {
    stop("Study Day values must be unique (no duplicate days).", call. = FALSE)
  }

  day_cols <- paste0("d", sort(unique(dpi_values)))
  day_by_row <- paste0("d", dpi_values)

  base_ids <- vapply(seq_len(nrow(spec)), function(i) {
    paste(
      direct_entry_safe_id_part(spec$group_name[[i]], fallback = sprintf("g%d", spec$group_index[[i]])),
      direct_entry_safe_id_part(spec$mouse_label[[i]], fallback = sprintf("m%d", spec$mouse_index[[i]])),
      sep = "_"
    )
  }, character(1))
  animal_ids <- make.unique(base_ids, sep = "_")

  rows <- lapply(seq_len(nrow(spec)), function(i) {
    raw_values <- sheet[[spec$col_key[[i]]]]
    parsed_values <- parse_weight_values(raw_values)
    values_by_day <- stats::setNames(parsed_values, day_by_row)
    day_values <- as.numeric(values_by_day[day_cols])

    row <- list(
      animal_id = animal_ids[[i]],
      group = as.character(spec$group_name[[i]]),
      study_id = as.character(config$study_name),
      cage_card = as.character(spec$group_name[[i]]),
      sex = ""
    )
    row[day_cols] <- as.list(day_values)
    row
  })

  data <- tibble::as_tibble(dplyr::bind_rows(rows))

  if (length(day_cols)) {
    keep_animals <- apply(data[, day_cols, drop = FALSE], 1, function(row) any(!is.na(row)))
    data <- data[keep_animals, , drop = FALSE]
  }

  lookup <- tibble::tibble(
    original = c("Animal ID", "Group", "Study ID", "Cage Card", "Sex", paste0("D", sub("^d", "", day_cols))),
    clean = c("animal_id", "group", "study_id", "cage_card", "sex", day_cols)
  )

  list(
    data = tibble::as_tibble(data),
    lookup = lookup,
    source_name = source_name,
    source_path = NA_character_
  )
}

direct_entry_to_import <- function(sheet_df,
                                   config = default_direct_entry_settings(),
                                   source_name = "Manual Data Entry") {
  direct_entry_matrix_to_import(
    sheet_df = sheet_df,
    config = config,
    source_name = source_name
  )
}

add_day_column_config <- function(config = default_direct_entry_settings(), day) {
  config <- normalize_direct_entry_settings(config)
  day <- coerce_integer(day)
  if (is.na(day) || day < 0L) {
    stop("Day must be a non-negative whole number.", call. = FALSE)
  }
  if (day %in% config$dpi_rows) {
    stop(sprintf("Day %s already exists.", day), call. = FALSE)
  }
  config$dpi_rows <- sort(unique(c(config$dpi_rows, day)))
  normalize_direct_entry_settings(config)
}

rename_day_column_config <- function(config = default_direct_entry_settings(),
                                     old_key,
                                     new_day,
                                     new_label = NULL) {
  config <- normalize_direct_entry_settings(config)
  old_day <- suppressWarnings(as.integer(sub("^d", "", as.character(old_key))))
  if (is.na(old_day) || !old_day %in% config$dpi_rows) {
    stop(sprintf("Day column '%s' was not found.", old_key), call. = FALSE)
  }
  new_day <- coerce_integer(new_day)
  if (is.na(new_day) || new_day < 0L) {
    stop("Day must be a non-negative whole number.", call. = FALSE)
  }
  if (new_day != old_day && new_day %in% config$dpi_rows) {
    stop(sprintf("Day %s already exists.", new_day), call. = FALSE)
  }
  config$dpi_rows[config$dpi_rows == old_day] <- new_day
  config$dpi_rows <- sort(unique(config$dpi_rows))
  normalize_direct_entry_settings(config)
}

remove_day_column_config <- function(config = default_direct_entry_settings(), key) {
  config <- normalize_direct_entry_settings(config)
  day <- suppressWarnings(as.integer(sub("^d", "", as.character(key))))
  if (is.na(day) || !day %in% config$dpi_rows) {
    return(config)
  }
  if (length(config$dpi_rows) <= 1L) {
    stop("At least one day row is required.", call. = FALSE)
  }
  config$dpi_rows <- setdiff(config$dpi_rows, day)
  normalize_direct_entry_settings(config)
}

rename_direct_entry_sheet_column <- function(sheet_df, old_key, new_key) {
  sheet_df <- tibble::as_tibble(sheet_df %||% tibble::tibble())
  if (!old_key %in% names(sheet_df) || identical(old_key, new_key)) {
    return(sheet_df)
  }
  names(sheet_df)[names(sheet_df) == old_key] <- new_key
  tibble::as_tibble(sheet_df)
}

rename_direct_entry_sheet_columns <- function(sheet_df, rename_map = character()) {
  sheet_df <- tibble::as_tibble(sheet_df %||% tibble::tibble())
  if (!length(rename_map)) {
    return(sheet_df)
  }
  rename_map <- rename_map[nzchar(names(rename_map)) & nzchar(rename_map)]
  rename_map <- rename_map[names(rename_map) %in% names(sheet_df)]
  if (!length(rename_map)) {
    return(sheet_df)
  }

  tmp <- paste0("__tmp_de_", seq_along(rename_map))
  for (i in seq_along(rename_map)) {
    names(sheet_df)[names(sheet_df) == names(rename_map)[[i]]] <- tmp[[i]]
  }
  for (i in seq_along(rename_map)) {
    names(sheet_df)[names(sheet_df) == tmp[[i]]] <- rename_map[[i]]
  }

  tibble::as_tibble(sheet_df)
}
