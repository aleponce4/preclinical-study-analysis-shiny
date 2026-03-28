read_csv_import <- function(path, source_name = basename(path)) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }

  # Detect encoding upfront. Accept any reasonable guess (lab CSVs from Excel
  # on Windows are typically ISO-8859-1/Windows-1252 with low-confidence scores).
  detected_encoding <- tryCatch({
    guess <- readr::guess_encoding(path)
    preferred <- guess[guess$encoding %in% c("UTF-8", "ISO-8859-1", "ISO-8859-2",
                                              "windows-1252", "ASCII"), ]
    if (nrow(preferred)) preferred$encoding[[1]] else if (nrow(guess)) guess$encoding[[1]] else "UTF-8"
  }, error = function(e) "UTF-8")

  raw_data <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                    name_repair = "unique_quiet",
                    locale = readr::locale(encoding = detected_encoding)),
    error = function(e) NULL
  )

  # Check if the read produced invalid UTF-8 — if so, re-read with Latin-1.
  has_invalid_utf8 <- function(df) {
    for (col in names(df)) {
      if (is.character(df[[col]]) && any(!validUTF8(stats::na.omit(df[[col]]))))
        return(TRUE)
    }
    any(!validUTF8(names(df)))
  }

  if (!is.null(raw_data) && has_invalid_utf8(raw_data)) {
    raw_data <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                      name_repair = "unique_quiet",
                      locale = readr::locale(encoding = "Latin1")),
      error = function(e) raw_data
    )
  }

  if (is.null(raw_data)) {
    # Final attempt with Latin-1
    raw_data <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                      name_repair = "unique_quiet",
                      locale = readr::locale(encoding = "Latin1")),
      error = function(e2) {
        warning(sprintf("Failed to read CSV '%s': %s", source_name, conditionMessage(e2)))
        return(NULL)
      }
    )
  }

  if (is.null(raw_data)) {
    return(NULL)
  }

  if (!ncol(raw_data)) {
    return(NULL)
  }

  # Final safety net: coerce any remaining non-UTF-8 bytes
  raw_data <- raw_data |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      function(x) iconv(x, from = "", to = "UTF-8", sub = "")
    ))
  names(raw_data) <- iconv(names(raw_data), from = "", to = "UTF-8", sub = "")

  lookup <- tibble::tibble(
    original = names(raw_data),
    clean = janitor::make_clean_names(names(raw_data))
  )

  names(raw_data) <- lookup$clean

  list(
    data = raw_data,
    lookup = lookup,
    source_name = source_name,
    source_path = normalizePath(path, winslash = "/", mustWork = FALSE)
  )
}

extract_day_anchor <- function(original, clean) {
  original <- trimws(as.character(original %||% ""))
  clean <- trimws(as.character(clean %||% ""))

  original_match <- stringr::str_match(tolower(original), "^d\\s*(\\d+)\\b")
  if (!is.na(original_match[[1, 2]])) {
    return(as.integer(original_match[[1, 2]]))
  }

  clean_match <- stringr::str_match(tolower(clean), "^d(\\d+)(?:_|$)")
  if (!is.na(clean_match[[1, 2]])) {
    return(as.integer(clean_match[[1, 2]]))
  }

  NA_integer_
}

forward_fill_character <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out)] <- ""

  last_seen <- ""
  for (i in seq_along(out)) {
    if (nzchar(out[[i]])) {
      last_seen <- out[[i]]
    } else if (nzchar(last_seen)) {
      out[[i]] <- last_seen
    }
  }

  out[!nzchar(out)] <- NA_character_
  out
}

parse_weight_values <- function(x) {
  values <- trimws(as.character(x))
  values[is.na(values)] <- ""

  is_plain_numeric <- grepl("^[-+]?\\d+(?:\\.\\d+)?$", values, perl = TRUE)
  is_grams <- grepl("(?i)^[-+]?\\d+(?:\\.\\d+)?\\s*g\\b", values, perl = TRUE)

  parsed <- rep(NA_real_, length(values))
  parsed[is_plain_numeric] <- suppressWarnings(as.numeric(values[is_plain_numeric]))
  parsed[is_grams] <- suppressWarnings(readr::parse_number(values[is_grams], na = c("", "NA")))
  parsed
}

parse_score_values <- function(x) {
  values <- trimws(as.character(x))
  values[is.na(values)] <- ""

  is_plain_numeric <- grepl("^[-+]?\\d+(?:\\.\\d+)?$", values, perl = TRUE)
  parsed <- rep(NA_real_, length(values))
  parsed[is_plain_numeric] <- suppressWarnings(as.numeric(values[is_plain_numeric]))
  parsed
}

score_weight_column <- function(column_values, clean_name = "") {
  values <- trimws(as.character(column_values))
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) {
    return(-Inf)
  }

  grams_hits <- sum(grepl("(?i)^[-+]?\\d+(?:\\.\\d+)?\\s*g\\b", values, perl = TRUE))
  decimal_hits <- sum(grepl("\\d+\\.\\d+", values, perl = TRUE))
  temp_hits <- sum(grepl("(?i)\\d+(?:\\.\\d+)?\\s*c\\b", values, perl = TRUE))
  date_hits <- sum(grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}", values, perl = TRUE))
  score_hits <- sum(grepl("(?i)^score\\b", values, perl = TRUE))

  parsed_numeric <- suppressWarnings(readr::parse_number(values, na = c("", "NA")))
  numeric_hits <- sum(!is.na(parsed_numeric))
  plausible_weight_hits <- sum(!is.na(parsed_numeric) & parsed_numeric >= 5 & parsed_numeric <= 60)
  id_like_hits <- sum(!is.na(parsed_numeric) & parsed_numeric > 100 &
                        abs(parsed_numeric - round(parsed_numeric)) < 1e-9)

  name_penalty <- if (grepl("score", clean_name, ignore.case = TRUE)) 12 else 0

  (grams_hits * 8) +
    (decimal_hits * 3) +
    (plausible_weight_hits * 1.5) +
    (numeric_hits * 0.25) -
    (id_like_hits * 3) -
    (temp_hits * 8) -
    (date_hits * 8) -
    (score_hits * 4) -
    name_penalty
}

choose_day_weight_column <- function(data, lookup, anchor_idx, next_anchor_idx) {
  max_idx <- nrow(lookup)
  fallback_idx <- anchor_idx + 3L
  fallback_valid <- fallback_idx <= max_idx

  candidate_end <- min(next_anchor_idx - 1L, anchor_idx + 5L, max_idx)
  candidate_idx <- seq.int(anchor_idx + 1L, candidate_end)
  if (!length(candidate_idx)) {
    return(if (fallback_valid) fallback_idx else anchor_idx)
  }

  scores <- vapply(candidate_idx, function(idx) {
    col_name <- lookup$clean[idx]
    if (length(col_name) != 1L || is.na(col_name) || !col_name %in% names(data)) return(-Inf)
    score_weight_column(data[[col_name]], col_name)
  }, numeric(1))

  best_idx <- candidate_idx[[which.max(scores)]]
  best_score <- max(scores, na.rm = TRUE)

  if (!is.finite(best_score) || best_score <= 0) {
    return(if (fallback_valid) fallback_idx else best_idx)
  }

  if (fallback_valid && best_score < 2) {
    return(fallback_idx)
  }

  best_idx
}

score_score_column <- function(column_values, original_name = "", clean_name = "") {
  values <- trimws(as.character(column_values))
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) {
    return(-Inf)
  }

  plain_numeric_hits <- sum(grepl("^[-+]?\\d+(?:\\.\\d+)?$", values, perl = TRUE))
  parsed_scores <- parse_score_values(values)
  numeric_hits <- sum(!is.na(parsed_scores))
  plausible_score_hits <- sum(!is.na(parsed_scores) & parsed_scores >= 0 & parsed_scores <= 10)

  date_hits <- sum(grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}", values, perl = TRUE))
  temp_hits <- sum(grepl("(?i)\\d+(?:\\.\\d+)?\\s*c\\b", values, perl = TRUE))
  grams_hits <- sum(grepl("(?i)\\d+(?:\\.\\d+)?\\s*g\\b", values, perl = TRUE))
  id_like_hits <- sum(!is.na(parsed_scores) & parsed_scores > 100 & abs(parsed_scores - round(parsed_scores)) < 1e-9)

  name_bonus <- if (
    grepl("score", original_name, ignore.case = TRUE) ||
      grepl("score", clean_name, ignore.case = TRUE)
  ) 8 else 0

  (plain_numeric_hits * 3) +
    (numeric_hits * 2) +
    (plausible_score_hits * 2.5) +
    name_bonus -
    (date_hits * 8) -
    (temp_hits * 8) -
    (grams_hits * 8) -
    (id_like_hits * 2)
}

choose_day_score_column <- function(data, lookup, anchor_idx, next_anchor_idx) {
  max_idx <- nrow(lookup)
  candidate_end <- min(next_anchor_idx - 1L, anchor_idx + 5L, max_idx)
  candidate_idx <- seq.int(anchor_idx + 1L, candidate_end)
  if (!length(candidate_idx)) {
    return(NA_integer_)
  }

  is_score_name <- grepl("score", lookup$original[candidate_idx], ignore.case = TRUE) |
    grepl("score", lookup$clean[candidate_idx], ignore.case = TRUE)
  is_score_name[is.na(is_score_name)] <- FALSE
  explicit_score_idx <- candidate_idx[is_score_name]
  if (length(explicit_score_idx)) {
    return(explicit_score_idx[[1]])
  }

  scores <- vapply(candidate_idx, function(idx) {
    col_name <- lookup$clean[idx]
    if (length(col_name) != 1L || is.na(col_name) || !col_name %in% names(data)) return(-Inf)
    score_score_column(
      column_values = data[[col_name]],
      original_name = lookup$original[idx],
      clean_name = col_name
    )
  }, numeric(1))

  best_idx <- candidate_idx[[which.max(scores)]]
  best_score <- max(scores, na.rm = TRUE)
  if (!is.finite(best_score) || best_score <= 0) {
    return(NA_integer_)
  }

  best_idx
}

score_excel_header_row <- function(row_values) {
  values <- trimws(as.character(row_values))
  values[is.na(values)] <- ""
  values <- values[nzchar(values)]

  if (!length(values)) {
    return(-Inf)
  }

  day_hits <- sum(grepl("(?i)^d\\s*\\d+\\b", values, perl = TRUE))
  id_hits <- sum(grepl("(?i)\\b(mouse|animal|rat)\\s*id\\b|^(mouse|animal|rat)$|^id$", values, perl = TRUE))
  meta_hits <- sum(grepl(
    "(?i)virus\\s*/\\s*group|\\bgroup\\b|\\bcage(?:\\s*card)?\\b|\\bscore\\b|\\btreatment\\b|\\bcohort\\b",
    values,
    perl = TRUE
  ))

  numeric_hits <- sum(grepl("^[-+]?\\d+(?:\\.\\d+)?$", values, perl = TRUE))
  excel_serial_hits <- sum(grepl("^\\d{4,}(?:\\.\\d+)?$", values, perl = TRUE))
  slash_date_hits <- sum(grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}", values, perl = TRUE))
  temp_hits <- sum(grepl("(?i)^[-+]?\\d+(?:\\.\\d+)?\\s*c\\b", values, perl = TRUE))
  weight_hits <- sum(grepl("(?i)^[-+]?\\d+(?:\\.\\d+)?\\s*g\\b", values, perl = TRUE))

  sparse_penalty <- if (length(values) < 3L) {
    12
  } else if (length(values) < 5L) {
    4
  } else {
    0
  }

  (day_hits * 8) +
    (id_hits * 6) +
    (meta_hits * 4) -
    (numeric_hits * 2) -
    (excel_serial_hits * 3) -
    (slash_date_hits * 4) -
    (temp_hits * 6) -
    (weight_hits * 6) -
    sparse_penalty
}

detect_excel_header_row <- function(raw_data, max_rows = 10L, min_score = 8) {
  if (is.null(raw_data) || !nrow(raw_data)) {
    return(1L)
  }

  candidate_rows <- seq_len(min(max_rows, nrow(raw_data)))
  scores <- vapply(candidate_rows, function(idx) {
    score_excel_header_row(unlist(raw_data[idx, , drop = FALSE], use.names = FALSE))
  }, numeric(1))

  best_idx <- candidate_rows[[which.max(scores)]]
  best_score <- max(scores, na.rm = TRUE)

  if (!is.finite(best_score) || best_score < min_score) {
    return(1L)
  }

  as.integer(best_idx)
}

build_excel_import <- function(raw_data, source_name, path, header_row = 1L) {
  if (is.null(raw_data) || !nrow(raw_data) || !ncol(raw_data)) {
    return(NULL)
  }

  header_row <- max(1L, min(as.integer(header_row), nrow(raw_data)))
  if (header_row >= nrow(raw_data)) {
    return(NULL)
  }

  original_names <- trimws(as.character(unlist(raw_data[header_row, , drop = FALSE], use.names = FALSE)))
  original_names[is.na(original_names)] <- ""
  repaired_names <- vctrs::vec_as_names(original_names, repair = "unique_quiet")

  data_rows <- raw_data[seq.int(header_row + 1L, nrow(raw_data)), , drop = FALSE]
  data_rows <- as.data.frame(data_rows, stringsAsFactors = FALSE, optional = TRUE)
  names(data_rows) <- repaired_names

  for (col in names(data_rows)) {
    if (!is.character(data_rows[[col]])) {
      data_rows[[col]] <- as.character(data_rows[[col]])
    }
    data_rows[[col]] <- iconv(data_rows[[col]], to = "UTF-8", sub = "byte")
  }

  lookup <- tibble::tibble(
    original = repaired_names,
    clean = janitor::make_clean_names(repaired_names)
  )
  names(data_rows) <- lookup$clean

  list(
    data = tibble::as_tibble(data_rows),
    lookup = lookup,
    source_name = source_name,
    source_path = normalizePath(path, winslash = "/", mustWork = FALSE),
    header_row = header_row
  )
}

non_empty_character_values <- function(x) {
  values <- trimws(as.character(x))
  values[is.na(values)] <- ""
  values[nzchar(values)]
}

score_group_role_column <- function(column_values, clean_name = "") {
  values <- non_empty_character_values(column_values)
  if (!length(values)) {
    return(-Inf)
  }

  alpha_hits <- sum(grepl("[A-Za-z]", values, perl = TRUE))
  numeric_hits <- sum(grepl("^[-+]?\\d+(?:\\.\\d+)?$", values, perl = TRUE))
  alias_bonus <- dplyr::case_when(
    clean_name %in% c("group", "virus_group", "virus_group_treatment", "treatment", "cohort") ~ 12,
    clean_name %in% c("cage_card", "cage", "cage_id") ~ 5,
    clean_name %in% c("virus", "study") ~ 2,
    TRUE ~ 0
  )
  duplicate_bonus <- if (length(unique(values)) < length(values)) 2 else 0

  alias_bonus + (alpha_hits * 1.5) - (numeric_hits * 3) + duplicate_bonus
}

score_cage_role_column <- function(column_values, clean_name = "") {
  values <- non_empty_character_values(column_values)
  if (!length(values)) {
    return(-Inf)
  }

  numeric_hits <- sum(grepl("^[-+]?\\d+(?:\\.\\d+)?$", values, perl = TRUE))
  alpha_hits <- sum(grepl("[A-Za-z]", values, perl = TRUE))
  alias_bonus <- if (clean_name %in% c("cage_card", "cage", "cage_id")) 6 else 0

  alias_bonus + (numeric_hits * 2) - (alpha_hits * 1.5)
}

pick_best_matching_column <- function(data, candidates, scorer) {
  available <- unique(candidates[candidates %in% names(data)])
  if (!length(available)) {
    return("")
  }

  scores <- vapply(available, function(name) scorer(data[[name]], name), numeric(1))
  available[[which.max(scores)]]
}

lookup_original_name <- function(lookup, clean_name, default = "") {
  match_idx <- match(clean_name, lookup$clean)
  if (length(match_idx) != 1L || is.na(match_idx)) {
    return(default)
  }
  lookup$original[[match_idx]]
}

normalize_real_weights_template <- function(imported) {
  if (is.null(imported) || is.null(imported$data) || is.null(imported$lookup)) {
    return(imported)
  }

  data <- tibble::as_tibble(imported$data)
  lookup <- tibble::as_tibble(imported$lookup)
  if (!nrow(lookup) || !ncol(data)) {
    return(imported)
  }

  animal_col <- pick_best_matching_column(
    data,
    c("mouse_id", "animal_id", "rat_id", "mouse", "rat", "id"),
    function(column_values, clean_name) {
      alias_bonus <- if (clean_name %in% c("mouse_id", "animal_id", "rat_id")) 10 else 5
      values <- non_empty_character_values(column_values)
      if (!length(values)) {
        return(-Inf)
      }
      alpha_penalty <- sum(grepl("[A-Za-z]", values, perl = TRUE))
      numeric_bonus <- sum(grepl("^[-+]?\\d+(?:\\.\\d+)?$", values, perl = TRUE))
      alias_bonus + numeric_bonus - (alpha_penalty * 2)
    }
  )

  group_col <- pick_best_matching_column(
    data,
    c("group", "virus_group", "virus_group_treatment", "treatment", "cohort", "cage_card", "cage", "virus"),
    score_group_role_column
  )

  explicit_study_col <- unique(c("study_id", "study", "studyid"))
  study_col <- explicit_study_col[explicit_study_col %in% names(data)]
  study_col <- if (length(study_col)) {
    study_col[[1]]
  } else if ("virus" %in% names(data) && !identical(group_col, "virus")) {
    "virus"
  } else {
    ""
  }

  cage_col <- pick_best_matching_column(
    data,
    c("cage_card", "cage", "cage_id"),
    score_cage_role_column
  )
  if (!nzchar(cage_col) && nzchar(group_col) && group_col %in% c("cage_card", "cage", "cage_id")) {
    cage_col <- group_col
  }

  if (!nzchar(group_col) || !nzchar(animal_col)) {
    return(imported)
  }

  day_anchors <- lookup |>
    dplyr::mutate(
      idx = dplyr::row_number(),
      day = vapply(seq_len(dplyr::n()), function(i) {
        extract_day_anchor(.data$original[[i]], .data$clean[[i]])
      }, integer(1))
    ) |>
    dplyr::filter(!is.na(.data$day)) |>
    dplyr::distinct(.data$day, .keep_all = TRUE) |>
    dplyr::arrange(.data$idx)

  if (!nrow(day_anchors)) {
    return(imported)
  }

  next_anchor_idx <- c(day_anchors$idx[-1], nrow(lookup) + 1L)
  weight_idx <- vapply(seq_len(nrow(day_anchors)), function(i) {
    choose_day_weight_column(
      data = data,
      lookup = lookup,
      anchor_idx = day_anchors$idx[[i]],
      next_anchor_idx = next_anchor_idx[[i]]
    )
  }, integer(1))
  score_idx <- vapply(seq_len(nrow(day_anchors)), function(i) {
    choose_day_score_column(
      data = data,
      lookup = lookup,
      anchor_idx = day_anchors$idx[[i]],
      next_anchor_idx = next_anchor_idx[[i]]
    )
  }, integer(1))

  day_anchors <- day_anchors |>
    dplyr::mutate(
      weight_idx = weight_idx,
      weight_col = lookup$clean[.data$weight_idx],
      day_col = paste0("d", .data$day),
      score_idx_raw = score_idx,
      score_idx = dplyr::if_else(.data$score_idx_raw <= nrow(lookup), .data$score_idx_raw, NA_integer_),
      score_col = dplyr::if_else(is.na(.data$score_idx), NA_character_, lookup$clean[.data$score_idx]),
      score_day_col = paste0("score_d", .data$day)
    ) |>
    dplyr::filter(!is.na(.data$weight_col), .data$weight_col %in% names(data)) |>
    dplyr::distinct(.data$day_col, .keep_all = TRUE)

  if (!nrow(day_anchors)) {
    return(imported)
  }

  study_values <- if (nzchar(study_col)) {
    forward_fill_character(data[[study_col]])
  } else {
    rep(NA_character_, nrow(data))
  }
  group_values <- forward_fill_character(data[[group_col]])
  cage_values <- if (nzchar(cage_col)) {
    forward_fill_character(data[[cage_col]])
  } else {
    rep(NA_character_, nrow(data))
  }

  canonical <- tibble::tibble(
    study_id = study_values,
    group = group_values,
    cage_card = cage_values,
    animal_id = trimws(as.character(data[[animal_col]]))
  )

  for (i in seq_len(nrow(day_anchors))) {
    canonical[[day_anchors$day_col[[i]]]] <- parse_weight_values(data[[day_anchors$weight_col[[i]]]])
    if (!is.na(day_anchors$score_col[[i]]) && day_anchors$score_col[[i]] %in% names(data)) {
      canonical[[day_anchors$score_day_col[[i]]]] <- parse_score_values(data[[day_anchors$score_col[[i]]]])
    } else {
      canonical[[day_anchors$score_day_col[[i]]]] <- NA_real_
    }
  }

  canonical <- canonical |>
    dplyr::filter(!is.na(.data$animal_id), nzchar(.data$animal_id))

  canonical_lookup <- dplyr::bind_rows(
    tibble::tibble(
      original = c(
        lookup_original_name(lookup, study_col, "Study ID"),
        lookup_original_name(lookup, group_col, "Group"),
        lookup_original_name(lookup, cage_col, "Cage Card"),
        lookup_original_name(lookup, animal_col, "Animal ID")
      ),
      clean = c("study_id", "group", "cage_card", "animal_id")
    ),
    tibble::tibble(
      original = day_anchors$original,
      clean = day_anchors$day_col
    ),
    tibble::tibble(
      original = sprintf("Score D%d", day_anchors$day),
      clean = day_anchors$score_day_col
    )
  )

  list(
    data = canonical,
    lookup = canonical_lookup,
    source_name = imported$source_name,
    source_path = imported$source_path,
    header_row = imported$header_row %||% 1L
  )
}

read_weights_import <- function(path, source_name = basename(path)) {
  imported <- read_csv_import(path, source_name = source_name)
  if (is.null(imported)) {
    return(NULL)
  }

  normalize_real_weights_template(imported)
}

read_excel_import <- function(path, source_name = basename(path), sheet = 1, detect_header_row = FALSE) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)

  raw_data <- tryCatch(
    readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    ),
    error = function(e) NULL
  )
  if (is.null(raw_data) || nrow(raw_data) == 0) return(NULL)

  header_row <- if (isTRUE(detect_header_row)) detect_excel_header_row(raw_data) else 1L
  build_excel_import(raw_data, source_name = source_name, path = path, header_row = header_row)
}

read_weights_excel_import <- function(path, source_name = basename(path), sheet = 1) {
  imported <- read_excel_import(path, source_name, sheet = sheet, detect_header_row = TRUE)
  if (is.null(imported)) return(NULL)
  normalize_real_weights_template(imported)
}

template_path_candidates <- function(fname) {
  app_dir <- getOption("labweight.app_dir", ".")
  unique(c(
    file.path(app_dir, "inst", "templates", fname),     # shinyapps.io/deployed app dir
    project_path("app", "inst", "templates", fname),    # packaged installer
    project_path("inst", "templates", fname)            # dev source of truth
  ))
}

resolve_template_path <- function(fname) {
  candidates <- template_path_candidates(fname)
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) existing[[1]] else NA_character_
}

resolve_template_path_strict <- function(fname) {
  path <- resolve_template_path(fname)
  candidates <- template_path_candidates(fname)
  if (is.na(path) || !nzchar(path)) {
    stop(
      sprintf(
        "Example data file '%s' was not found. Checked: %s",
        fname,
        paste(candidates, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  path
}

validate_example_import_schema <- function(imported, kind = c("weights", "survival"), source_path = NULL) {
  kind <- match.arg(kind)
  source_path <- source_path %||% imported$source_path %||% "<unknown>"

  if (is.null(imported) || is.null(imported$data) || !ncol(imported$data)) {
    stop(
      sprintf("Example %s data could not be read from '%s'.", kind, source_path),
      call. = FALSE
    )
  }

  cols <- names(imported$data)
  has_col <- function(name) name %in% cols
  has_day_cols <- any(grepl("^d(?:ay)?_?\\d+$", cols, ignore.case = TRUE))

  if (identical(kind, "weights")) {
    missing <- c("animal_id", "group")[!c(has_col("animal_id"), has_col("group"))]
    if (length(missing) || !has_day_cols) {
      problems <- c(
        if (length(missing)) sprintf("missing required column(s): %s", paste(missing, collapse = ", ")),
        if (!has_day_cols) "missing day columns (expected names like d0/day0)"
      )
      stop(
        sprintf(
          "Example weights data at '%s' failed schema checks: %s.",
          source_path,
          paste(problems, collapse = "; ")
        ),
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }

  has_event_schema <- has_col("event_day") && has_col("censored")
  if (!has_col("animal_id") || (!has_day_cols && !has_event_schema)) {
    problems <- c(
      if (!has_col("animal_id")) "missing required column: animal_id",
      if (!has_day_cols && !has_event_schema) {
        "missing survival day columns (e.g., day3/day7) or tidy event columns (event_day + censored)"
      }
    )
    stop(
      sprintf(
        "Example survival data at '%s' failed schema checks: %s.",
        source_path,
        paste(problems, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

read_example_import <- function(kind = c("weights", "survival")) {
  kind <- match.arg(kind)
  fname <- paste0("example_", kind, ".csv")
  path <- resolve_template_path_strict(fname)
  imported <- if (identical(kind, "weights")) {
    read_weights_import(path, basename(path))
  } else {
    read_csv_import(path, basename(path))
  }

  validate_example_import_schema(imported, kind = kind, source_path = path)
  imported
}

resolve_template_path_with_fallback <- function(fname) {
  candidates <- template_path_candidates(fname)
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) {
    return(existing[[1]])
  }
  candidates[[1]]
}

example_import_path <- function(kind = c("weights", "survival")) {
  kind <- match.arg(kind)
  resolve_template_path_with_fallback(paste0("example_", kind, ".csv"))
}

template_import_path <- function(kind = c("weights", "survival")) {
  example_import_path(kind)
}

load_example_import <- function(kind = c("weights", "survival")) {
  read_example_import(kind)
}

preview_import_data <- function(imported, n = 10) {
  if (is.null(imported) || is.null(imported$data)) {
    return(tibble::tibble())
  }

  utils::head(imported$data, n = n)
}
