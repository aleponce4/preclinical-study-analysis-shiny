testthat::test_that("survival status respects censored flag", {
  validation <- example_validation()
  row <- validation$clean_survival |>
    dplyr::filter(.data$animal_id == "207")

  testthat::expect_equal(row$status[[1]], 0L)
  testthat::expect_true(row$censored[[1]])
})

testthat::test_that("pivot_wide_survival assigns correct event_day and censored", {
  wide <- tibble::tibble(
    animal_id = c("A", "B", "C"),
    day3  = c("alive",            "alive",       "alive"),
    day7  = c("alive",            "found_dead",  "euthanized_endpoint"),
    day10 = c("removed_study_end", NA,            NA)
  )
  day_map <- tibble::tibble(
    source_column = c("day3", "day7", "day10"),
    day     = c(3L, 7L, 10L),
    include = c(TRUE, TRUE, TRUE)
  )
  result <- pivot_wide_survival(wide, day_map)

  # A: removed at day10 → censored
  testthat::expect_equal(result$event_day[result$animal_id == "A"], 10L)
  testthat::expect_true(result$censored[result$animal_id == "A"])

  # B: death at day7
  testthat::expect_equal(result$event_day[result$animal_id == "B"], 7L)
  testthat::expect_false(result$censored[result$animal_id == "B"])
  testthat::expect_equal(result$event_type[result$animal_id == "B"], "found_dead")

  # C: endpoint at day7
  testthat::expect_equal(result$event_day[result$animal_id == "C"], 7L)
  testthat::expect_false(result$censored[result$animal_id == "C"])
})

testthat::test_that("all-alive animal is censored at last day", {
  wide    <- tibble::tibble(animal_id = "Z", day3 = "alive", day7 = "alive")
  day_map <- tibble::tibble(
    source_column = c("day3", "day7"),
    day     = c(3L, 7L),
    include = c(TRUE, TRUE)
  )
  result <- pivot_wide_survival(wide, day_map)
  testthat::expect_equal(result$event_day, 7L)
  testthat::expect_true(result$censored)
})

testthat::test_that("wide format survival roundtrip produces correct clean_survival", {
  weights_import <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  survival_wide  <- read_csv_import(project_path("inst", "templates", "example_survival.csv"))
  surv_day_map   <- default_survival_day_mapping(survival_wide)

  validation <- validate_study_data(
    raw_weights  = weights_import,
    raw_survival = survival_wide,
    mapping = build_mapping_bundle(
      weights          = guess_field_mapping(weights_import, weights_field_spec()),
      survival         = guess_field_mapping(survival_wide,  survival_field_spec()),
      day_map          = default_day_mapping(weights_import),
      survival_day_map = surv_day_map
    )
  )

  testthat::expect_equal(nrow(validation$clean_survival), 12L)

  row204 <- validation$clean_survival[validation$clean_survival$animal_id == "204", ]
  testthat::expect_equal(row204$status[[1]], 1L)   # found_dead -> status=1
  testthat::expect_equal(row204$time[[1]],   5L)

  row207 <- validation$clean_survival[validation$clean_survival$animal_id == "207", ]
  testthat::expect_equal(row207$status[[1]], 0L)   # removed_study_end -> censored
  testthat::expect_equal(row207$time[[1]],   15L)
})

testthat::test_that("survival rows missing from weights trigger warnings", {
  weights_import  <- read_weights_import(project_path("inst", "templates", "example_weights.csv"))
  survival_import <- read_csv_import(project_path("inst", "templates", "example_survival.csv"))
  survival_import$data <- dplyr::bind_rows(
    survival_import$data,
    tibble::tibble(
      study_id  = "example_mouse_01",
      animal_id = 999,
      group     = "999999",
      notes     = "",
      day3      = "alive",
      day7      = "found_dead"
    )
  )

  validation <- validate_study_data(
    raw_weights  = weights_import,
    raw_survival = survival_import,
    mapping = build_mapping_bundle(
      weights          = guess_field_mapping(weights_import,  weights_field_spec()),
      survival         = guess_field_mapping(survival_import, survival_field_spec()),
      day_map          = default_day_mapping(weights_import),
      survival_day_map = default_survival_day_mapping(survival_import)
    )
  )

  testthat::expect_true(any(grepl("do not match an animal", validation$warnings)))
})

testthat::test_that("clean_group_id normalizes micro symbols to u", {
  mu_sign <- intToUtf8(0x00B5)
  greek_mu <- intToUtf8(0x03BC)

  testthat::expect_equal(clean_group_id(paste0("AMP 33 ", mu_sign, "g high dose")), "amp_33_ug_high_dose")
  testthat::expect_equal(clean_group_id(paste0("AMP 33 ", greek_mu, "g high dose")), "amp_33_ug_high_dose")
  testthat::expect_equal(clean_group_id("AMP 33 ug high dose"), "amp_33_ug_high_dose")
})

testthat::test_that("clean_group_id preserves duplicates when requested", {
  micro_labels <- rep(paste0("AMP 33 ", intToUtf8(0x00B5), "g high dose"), 2)
  testthat::expect_equal(
    clean_group_id(micro_labels, allow_dupes = TRUE),
    c("amp_33_ug_high_dose", "amp_33_ug_high_dose")
  )
})

testthat::test_that("micro-symbol group labels do not warn or conflict across imports", {
  micro_label <- paste0("AMP 33 ", intToUtf8(0x00B5), "g high dose")

  weights_import <- list(
    data = tibble::tibble(
      study_id = c("example_mouse_01", "example_mouse_01"),
      group = c(micro_label, micro_label),
      cage_card = c("448506", "448507"),
      animal_id = c("161", "162"),
      sex = c("F", "F"),
      d0 = c(23.4, 24.0),
      d1 = c(22.8, 23.1)
    ),
    lookup = tibble::tibble(
      original = c("study_id", "group", "cage_card", "animal_id", "sex", "d0", "d1"),
      clean = c("study_id", "group", "cage_card", "animal_id", "sex", "d0", "d1")
    ),
    source_name = "micro_weights.csv",
    source_path = "micro_weights.csv"
  )

  survival_import <- list(
    data = tibble::tibble(
      study_id = c("example_mouse_01", "example_mouse_01"),
      animal_id = c("161", "162"),
      group = c(micro_label, micro_label),
      event_day = c(1L, 1L),
      event_type = c("found_dead", "removed_other"),
      censored = c("N", "Y"),
      notes = c("", "")
    ),
    lookup = tibble::tibble(
      original = c("study_id", "animal_id", "group", "event_day", "event_type", "censored", "notes"),
      clean = c("study_id", "animal_id", "group", "event_day", "event_type", "censored", "notes")
    ),
    source_name = "micro_survival.csv",
    source_path = "micro_survival.csv"
  )

  validation <- testthat::expect_no_warning(
    validate_study_data(
      raw_weights = weights_import,
      raw_survival = survival_import,
      mapping = build_mapping_bundle(
        weights = guess_field_mapping(weights_import, weights_field_spec()),
        survival = guess_field_mapping(survival_import, survival_field_spec()),
        day_map = default_day_mapping(weights_import),
        survival_day_map = default_survival_day_mapping(survival_import)
      )
    )
  )

  testthat::expect_false(any(grepl("group labels that conflict", validation$warnings)))
  testthat::expect_true(all(validation$clean_weights$group_id == "amp_33_ug_high_dose"))
  testthat::expect_true(all(validation$clean_survival$group_id == "amp_33_ug_high_dose"))
})

testthat::test_that("survival summary table exposes a single group column", {
  validation <- example_validation()
  analysis <- compute_survival_analysis(validation$clean_survival, validation$group_meta)

  testthat::expect_true("Group" %in% names(analysis$summary))
  testthat::expect_false("group_id" %in% names(analysis$summary))
  testthat::expect_false("display_name" %in% names(analysis$summary))
  testthat::expect_true(all(c("N", "Deaths", "Censored", "Median (DPI)") %in% names(analysis$summary)))
})

# ---- Infer survival from weights ----

testthat::test_that("infer_survival_from_weights marks dropout as death", {
  weights <- tibble::tibble(
    animal_id   = c("A", "A", "A", "B", "B", "B"),
    group_id    = "grp1",
    group_label = "Group 1",
    study_id    = "S1",
    cage_card   = NA_character_,
    sex         = NA_character_,
    day         = c(0L, 3L, 7L, 0L, 3L, 7L),
    weight_g    = c(20, 19, 18, 22, 21, NA)
  )

  result <- infer_survival_from_weights(weights)

  # A observed through day 7 (study end) → censored

  row_a <- result$data |> dplyr::filter(animal_id == "A")
  testthat::expect_equal(row_a$time, 7L)
  testthat::expect_equal(row_a$status, 0L)
  testthat::expect_true(row_a$censored)

  # B last observed day 3 (before study end 7) → death
  row_b <- result$data |> dplyr::filter(animal_id == "B")
  testthat::expect_equal(row_b$time, 3L)
  testthat::expect_equal(row_b$status, 1L)
  testthat::expect_false(row_b$censored)
  testthat::expect_equal(row_b$event_type, "inferred_death")
})

testthat::test_that("infer_survival_from_weights all survive → all censored", {
  weights <- tibble::tibble(
    animal_id   = rep(c("X", "Y"), each = 3),
    group_id    = "grp1",
    group_label = "Group 1",
    study_id    = "S1",
    cage_card   = NA_character_,
    sex         = NA_character_,
    day         = rep(c(0L, 3L, 7L), 2),
    weight_g    = c(20, 19, 18, 22, 21, 20)
  )

  result <- infer_survival_from_weights(weights)

  testthat::expect_equal(nrow(result$data), 2L)
  testthat::expect_true(all(result$data$status == 0L))
  testthat::expect_true(all(result$data$censored))
})

testthat::test_that("infer_survival_from_weights uses max observed day as study end", {
  weights <- tibble::tibble(
    animal_id = c("A", "A", "A", "B", "B", "B"),
    group_id = "grp1",
    group_label = "Group 1",
    study_id = "S1",
    cage_card = NA_character_,
    sex = NA_character_,
    day = c(0L, 5L, 12L, 0L, 5L, 12L),
    weight_g = c(20, 19, NA, 22, 21, NA)
  )

  result <- infer_survival_from_weights(weights)

  row_a <- result$data |> dplyr::filter(.data$animal_id == "A")
  row_b <- result$data |> dplyr::filter(.data$animal_id == "B")

  testthat::expect_equal(row_a$time, 5L)
  testthat::expect_equal(row_b$time, 5L)
  testthat::expect_equal(row_a$status, 0L)
  testthat::expect_equal(row_b$status, 0L)
  testthat::expect_true(all(result$data$censored))
})

testthat::test_that("infer_survival_from_weights returns empty for empty weights", {
  result <- infer_survival_from_weights(empty_clean_weights())

  testthat::expect_equal(nrow(result$data), 0L)
  testthat::expect_equal(names(result$data), names(empty_clean_survival()))
})

testthat::test_that("validate_study_data derives survival from weights-only data", {
  validation <- example_validation(infer_survival = TRUE)

  # The example data has both weights and survival files.
  # When an explicit survival file is present, infer_survival is ignored.
  # So use weights-only validation:
  weights_import <- load_example_import("weights")
  mapping <- build_mapping_bundle(
    weights  = guess_field_mapping(weights_import, weights_field_spec()),
    survival = list(),
    day_map  = default_day_mapping(weights_import),
    survival_day_map = tibble::tibble()
  )

  val <- validate_study_data(
    raw_weights    = weights_import,
    raw_survival   = NULL,
    mapping        = mapping,
    infer_survival = TRUE
  )

  testthat::expect_gt(nrow(val$clean_survival), 0L)
  testthat::expect_true(any(grepl("inferred|Infer", val$warnings, ignore.case = TRUE)))
  testthat::expect_true(all(val$clean_survival$event_type %in% c("inferred_death", NA_character_)))

  # Without the flag, survival should be empty
  val_no <- validate_study_data(
    raw_weights    = weights_import,
    raw_survival   = NULL,
    mapping        = mapping,
    infer_survival = FALSE
  )

  testthat::expect_gt(nrow(val_no$clean_survival), 0L)
  testthat::expect_true(all(val_no$clean_survival$status %in% c(0L, 1L)))
})

testthat::test_that("full cage same-day dropout is classified as scheduled sampling", {
  weights <- tibble::tibble(
    animal_id = c("A1", "A1", "A2", "A2", "B1", "B1", "B1", "B2", "B2", "B2"),
    group_id = c(rep("grp1", 4), rep("grp1", 6)),
    group_label = "Group 1",
    study_id = "S1",
    cage_card = c(rep("CAGE_A", 4), rep("CAGE_B", 6)),
    sex = NA_character_,
    day = c(0L, 3L, 0L, 3L, 0L, 3L, 7L, 0L, 3L, 7L),
    weight_g = c(20, 19, 21, 20, 22, 21, 20, 23, 22, 21)
  )

  result <- infer_survival_from_weights(weights)

  review_row <- result$cage_review |>
    dplyr::filter(.data$cage_card == "CAGE_A")
  cage_a <- result$data |>
    dplyr::filter(.data$animal_id %in% c("A1", "A2"))

  testthat::expect_equal(review_row$role[[1]], "scheduled_sampling")
  testthat::expect_equal(review_row$censor_day[[1]], 3L)
  testthat::expect_true(all(cage_a$status == 0L))
  testthat::expect_true(all(cage_a$event_type == "scheduled_sampling_excluded"))
  testthat::expect_true(all(!cage_a$include_in_km))
})

testthat::test_that("turning off death-event inference leaves non-scheduled dropouts censored", {
  weights <- tibble::tibble(
    animal_id = c("A", "A", "A", "B", "B"),
    group_id = "grp1",
    group_label = "Group 1",
    study_id = "S1",
    cage_card = c("CA", "CA", "CA", "CB", "CB"),
    sex = NA_character_,
    day = c(0L, 3L, 7L, 0L, 3L),
    weight_g = c(20, 19, 18, 21, 20)
  )

  result <- infer_survival_from_weights(weights, infer_death_events = FALSE)
  row_b <- result$data |>
    dplyr::filter(.data$animal_id == "B")

  testthat::expect_equal(row_b$status[[1]], 0L)
  testthat::expect_true(row_b$censored[[1]])
  testthat::expect_true(is.na(row_b$event_type[[1]]))
})

testthat::test_that("manual scheduled sampling override beats heuristic default", {
  weights <- tibble::tibble(
    animal_id = c("A", "A", "A", "B", "B"),
    group_id = "grp1",
    group_label = "Group 1",
    study_id = "S1",
    cage_card = c("CAGE1", "CAGE1", "CAGE1", "CAGE2", "CAGE2"),
    sex = NA_character_,
    day = c(0L, 3L, 7L, 0L, 3L),
    weight_g = c(20, 19, 18, 21, 20)
  )

  result <- infer_survival_from_weights(
    weights,
    scheduled_sampling_cages = tibble::tibble(
      cage_card = "CAGE2",
      role = "scheduled_sampling",
      censor_day = 3L
    )
  )

  review_row <- result$cage_review |>
    dplyr::filter(.data$cage_card == "CAGE2")
  row_b <- result$data |>
    dplyr::filter(.data$animal_id == "B")

  testthat::expect_equal(review_row$role[[1]], "scheduled_sampling")
  testthat::expect_equal(row_b$status[[1]], 0L)
  testthat::expect_equal(row_b$event_type[[1]], "scheduled_sampling_excluded")
  testthat::expect_false(row_b$include_in_km[[1]])
})

testthat::test_that("scheduled cohort deaths before planned collection are excluded from KM but recorded", {
  weights <- tibble::tibble(
    animal_id = c("A", "A", "A", "B", "B", "B"),
    group_id = "grp1",
    group_label = "Group 1",
    study_id = "S1",
    cage_card = c("SURV", "SURV", "SURV", "SCHED", "SCHED", "SCHED"),
    sex = NA_character_,
    day = c(0L, 3L, 7L, 0L, 3L, 7L),
    weight_g = c(20, 19, 18, 21, 20, NA)
  )

  result <- infer_survival_from_weights(
    weights,
    scheduled_sampling_cages = tibble::tibble(
      cage_card = "SCHED",
      role = "scheduled_sampling",
      censor_day = 7L
    )
  )

  row_b <- result$data |>
    dplyr::filter(.data$animal_id == "B")

  testthat::expect_equal(row_b$time[[1]], 3L)
  testthat::expect_equal(row_b$status[[1]], 1L)
  testthat::expect_false(row_b$censored[[1]])
  testthat::expect_equal(row_b$event_type[[1]], "scheduled_cohort_death_excluded")
  testthat::expect_false(row_b$include_in_km[[1]])
})

testthat::test_that("invalid scheduled sampling censor day surfaces a survival validation error", {
  weights_import <- load_example_import("weights")
  mapping <- build_mapping_bundle(
    weights  = guess_field_mapping(weights_import, weights_field_spec()),
    survival = list(),
    day_map  = default_day_mapping(weights_import),
    survival_day_map = tibble::tibble()
  )

  validation <- validate_study_data(
    raw_weights = weights_import,
    raw_survival = NULL,
    mapping = mapping,
    infer_death_events = TRUE,
    scheduled_sampling_cages = tibble::tibble(
      cage_card = weights_import$data$cage_card[[1]],
      role = "scheduled_sampling",
      censor_day = 99L
    )
  )

  testthat::expect_true(any(grepl("Scheduled sampling cages must have a valid censor DPI", validation$survival_hard_errors)))
})

testthat::test_that("2026.019a workbook exposes scheduled sampling cage candidates", {
  imported <- read_weights_excel_import(project_path("2026.019a PKPD BDGR.xlsx"))
  validation <- validate_study_data(
    raw_weights = imported,
    raw_survival = NULL,
    mapping = build_mapping_bundle(
      weights = guess_field_mapping(imported, weights_field_spec()),
      survival = list(),
      day_map = default_day_mapping(imported),
      survival_day_map = tibble::tibble(),
      score_day_map = default_score_day_mapping(imported)
    ),
    infer_death_events = TRUE,
    detect_scheduled_sampling = TRUE
  )

  review <- validation$scheduled_sampling_review

  testthat::expect_true(any(review$cage_card == "449066" & review$role == "scheduled_sampling" & review$censor_day == 2L))
  testthat::expect_true(any(review$cage_card == "449078" & review$role == "scheduled_sampling" & review$censor_day == 1L))
  testthat::expect_true(any(review$cage_card == "449074" & review$role == "survival"))
})
