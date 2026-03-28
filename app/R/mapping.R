weights_field_spec <- function() {
  tibble::tribble(
    ~field,       ~label,          ~required,
    "animal_id",  "Animal ID",     TRUE,
    "group",      "Group",         TRUE,
    "study_id",   "Study ID",      FALSE,
    "cage_card",  "Cage Card",     FALSE,
    "sex",        "Sex",           FALSE
  )
}

survival_field_spec <- function() {
  tibble::tribble(
    ~field,       ~label,          ~required,
    "animal_id",  "Animal ID",     TRUE,
    "event_day",  "Event Day",     TRUE,
    "censored",   "Censored",      TRUE,
    "event_type", "Event Type",    FALSE,
    "group",      "Group",         FALSE,
    "notes",      "Notes",         FALSE,
    "study_id",   "Study ID",      FALSE
  )
}

field_aliases <- function() {
  list(
    animal_id = c("animal_id", "animal", "animalid", "id"),
    group = c("group", "group_id", "treatment"),
    study_id = c("study_id", "study", "studyid"),
    cage_card = c("cage_card", "cage", "cagecard"),
    sex = c("sex"),
    event_day = c("event_day", "death_day", "day_of_event"),
    event_type = c("event_type", "status_reason"),
    censored = c("censored", "censor", "censored_y_n"),
    notes = c("notes", "note", "comment")
  )
}

guess_field_mapping <- function(imported, spec) {
  if (is.null(imported) || is.null(imported$data)) {
    return(stats::setNames(as.list(rep("", nrow(spec))), spec$field))
  }

  columns <- names(imported$data)
  aliases <- field_aliases()

  mapping <- lapply(spec$field, function(field) {
    matched <- aliases[[field]]
    candidate <- matched[matched %in% columns][1]
    candidate %||% ""
  })

  stats::setNames(mapping, spec$field)
}

guess_day_value <- function(column_name) {
  column_name <- as.character(column_name)
  column_name[is.na(column_name)] <- ""
  matched <- stringr::str_match(column_name, "^(?:d(?:ay)?_?)(\\d+)")
  ifelse(is.na(matched[, 2]), NA_integer_, as.integer(matched[, 2]))
}

default_day_mapping <- function(imported) {
  if (is.null(imported) || is.null(imported$lookup) || !nrow(imported$lookup)) {
    return(tibble::tibble(
      source_label = character(),
      source_column = character(),
      day = integer(),
      include = logical()
    ))
  }

  imported$lookup |>
    dplyr::mutate(
      source_label = sprintf("%s [%s]", .data$original, .data$clean),
      source_column = .data$clean,
      day = guess_day_value(.data$clean),
      include = !is.na(.data$day)
    ) |>
    dplyr::select("source_label", "source_column", "day", "include")
}

guess_score_day_value <- function(column_name) {
  column_name <- as.character(column_name)
  column_name[is.na(column_name)] <- ""
  matched <- stringr::str_match(column_name, "^score_d(\\d+)$")
  ifelse(is.na(matched[, 2]), NA_integer_, as.integer(matched[, 2]))
}

default_score_day_mapping <- function(imported) {
  if (is.null(imported) || is.null(imported$lookup) || !nrow(imported$lookup)) {
    return(tibble::tibble(
      source_label = character(),
      source_column = character(),
      day = integer(),
      include = logical()
    ))
  }

  imported$lookup |>
    dplyr::mutate(
      source_label = sprintf("%s [%s]", .data$original, .data$clean),
      source_column = .data$clean,
      day = guess_score_day_value(.data$clean),
      include = !is.na(.data$day)
    ) |>
    dplyr::select("source_label", "source_column", "day", "include")
}

default_survival_day_mapping <- function(imported) {
  if (is.null(imported) || is.null(imported$lookup) || !nrow(imported$lookup)) {
    return(tibble::tibble(
      source_label = character(),
      source_column = character(),
      day = integer(),
      include = logical()
    ))
  }

  imported$lookup |>
    dplyr::mutate(
      source_label = sprintf("%s [%s]", .data$original, .data$clean),
      source_column = .data$clean,
      day     = guess_day_value(.data$clean),
      include = !is.na(.data$day)
    ) |>
    dplyr::select("source_label", "source_column", "day", "include")
}

merge_day_mapping <- function(default_map, saved_map) {
  if (is.null(saved_map) || !nrow(saved_map)) {
    return(default_map)
  }

  saved_map <- tibble::as_tibble(saved_map)

  default_map |>
    dplyr::left_join(
      saved_map |>
        dplyr::select("source_column", saved_day = "day", saved_include = "include"),
      by = "source_column"
    ) |>
    dplyr::mutate(
      day = dplyr::coalesce(coerce_integer(.data$saved_day), .data$day),
      include = dplyr::coalesce(as.logical(.data$saved_include), .data$include)
    ) |>
    dplyr::select("source_label", "source_column", "day", "include")
}

empty_day_map <- function() {
  tibble::tibble(
    source_label = character(),
    source_column = character(),
    day = integer(),
    include = logical()
  )
}

build_mapping_bundle <- function(weights, survival = list(), day_map = empty_day_map(),
                                  survival_day_map = empty_day_map(),
                                  score_day_map = empty_day_map()) {
  list(
    weights          = weights,
    survival         = survival,
    day_map          = tibble::as_tibble(day_map %||% empty_day_map()),
    survival_day_map = tibble::as_tibble(survival_day_map %||% empty_day_map()),
    score_day_map    = tibble::as_tibble(score_day_map %||% empty_day_map())
  )
}

mapping_is_compatible <- function(preset, weights_imported = NULL, survival_imported = NULL) {
  if (is.null(preset)) {
    return(FALSE)
  }

  weights_columns <- names(weights_imported$data %||% tibble::tibble())
  survival_columns <- names(survival_imported$data %||% tibble::tibble())
  has_survival_import <- !is.null(survival_imported) && !is.null(survival_imported$data)

  weights_vals <- Filter(nzchar, unlist(preset$weights %||% list()))
  survival_vals <- Filter(nzchar, unlist(preset$survival %||% list()))

  weights_ok <- !length(weights_vals) || all(weights_vals %in% weights_columns)
  survival_ok <- !length(survival_vals) || !has_survival_import || all(survival_vals %in% survival_columns)
  day_ok <- all((preset$day_map$source_column %||% character()) %in% weights_columns)
  score_day_ok <- all((preset$score_day_map$source_column %||% character()) %in% weights_columns)
  survival_day_ok <- !has_survival_import || all(
    (preset$survival_day_map$source_column %||% character()) %in% survival_columns
  )

  isTRUE(weights_ok) && isTRUE(survival_ok) && isTRUE(day_ok) && isTRUE(score_day_ok) && isTRUE(survival_day_ok)
}
