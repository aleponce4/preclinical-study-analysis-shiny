# Generates inst/templates/example_weights.csv and example_survival.csv
# Regenerate with: Rscript scripts/_gen_example.R

`%||%` <- function(a, b) if (!is.null(a)) a else b

out_dir <- file.path(dirname(dirname(normalizePath(sys.frames()[[1]]$ofile %||% "."))),
                     "inst", "templates")
if (!dir.exists(out_dir)) out_dir <- file.path("inst", "templates")

# ── Study design ──────────────────────────────────────────────────────────────
study_id  <- "example_mouse_01"
d0_date   <- as.Date("2026-02-01")
days      <- 0:15  # D0–D15

groups <- list(
  list(
    name = "Control",
    cage = "100001",
    mice = 201:206,
    time = "8:00",
    w0 = c(22.5, 21.8, 23.2, 22.1, 22.9, 21.6),
    severity = c(-0.05, -0.02, 0.00, 0.03, 0.06, -0.04),
    die_after = rep(15, 6)
  ),
  list(
    name = "Challenge",
    cage = "100002",
    mice = 211:216,
    time = "8:05",
    w0 = c(22.3, 21.9, 23.0, 22.7, 22.5, 21.7),
    severity = c(-0.08, 0.04, 0.10, 0.00, 0.15, 0.06),
    die_after = c(4, 5, 6, 7, 6, 8)
  ),
  list(
    name = "Treatment A High",
    cage = "100003",
    mice = 221:226,
    time = "8:10",
    w0 = c(22.8, 21.5, 23.5, 22.2, 22.9, 21.8),
    severity = c(-0.06, 0.03, -0.02, 0.12, -0.04, 0.01),
    die_after = c(15, 15, 15, 12, 15, 15)
  ),
  list(
    name = "Treatment A Low",
    cage = "100004",
    mice = 231:236,
    time = "8:15",
    w0 = c(22.0, 21.7, 23.3, 22.4, 22.8, 21.9),
    severity = c(0.08, 0.12, 0.00, -0.03, 0.05, 0.10),
    die_after = c(7, 9, 12, 15, 15, 13)
  )
)

weight_profiles <- list(
  "Control" = c(0.0, 0.2, 0.3, 0.5, 0.6, 0.8, 0.9, 1.0, 1.2, 1.3, 1.5, 1.6, 1.7, 1.9, 2.0, 2.1),
  "Challenge" = c(0.0, -0.8, -1.7, -2.8, -4.0, -5.1, -6.0, -6.7, -7.2, -7.6, -7.9, -8.1, -8.3, -8.4, -8.5, -8.6),
  "Treatment A High" = c(0.0, -0.5, -1.2, -1.9, -2.6, -3.0, -3.1, -2.7, -2.2, -1.7, -1.2, -0.8, -0.3, 0.1, 0.4, 0.7),
  "Treatment A Low" = c(0.0, -0.7, -1.5, -2.4, -3.2, -3.8, -4.1, -4.0, -3.6, -3.1, -2.7, -2.2, -1.7, -1.2, -0.8, -0.4)
)

score_profiles <- list(
  "Control" = rep(0, length(days)),
  "Challenge" = c(0, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
  "Treatment A High" = c(0, 1, 1, 2, 3, 3, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0),
  "Treatment A Low" = c(0, 1, 2, 2, 3, 3, 3, 3, 2, 2, 1, 1, 1, 0, 0, 0)
)

temp_profiles <- list(
  "Control" = c(38.0, 38.1, 38.0, 38.1, 38.0, 38.1, 38.0, 38.1, 38.0, 38.1, 38.0, 38.1, 38.0, 38.1, 38.0, 38.1),
  "Challenge" = c(38.0, 38.5, 39.0, 39.5, 39.9, 40.2, 40.4, 40.5, 40.6, 40.6, 40.7, 40.7, 40.7, 40.7, 40.7, 40.7),
  "Treatment A High" = c(38.0, 38.4, 38.9, 39.2, 39.4, 39.2, 38.9, 38.6, 38.4, 38.2, 38.0, 37.9, 37.9, 38.0, 38.0, 38.1),
  "Treatment A Low" = c(38.0, 38.5, 39.0, 39.4, 39.8, 40.0, 40.1, 39.8, 39.5, 39.2, 38.9, 38.6, 38.3, 38.1, 38.0, 38.0)
)

weight_wobble <- function(mouse, day) {
  c(-0.1, 0.0, 0.1, 0.2, -0.2, 0.1)[((mouse + day) %% 6) + 1]
}

score_wobble <- function(mouse, day) {
  c(0.0, 0.2, -0.1, 0.1)[((mouse + day) %% 4) + 1]
}

temp_wobble <- function(mouse, day) {
  c(-0.1, 0.0, 0.1, 0.0)[((mouse + day) %% 4) + 1]
}

clamp <- function(x, lower, upper) max(lower, min(upper, x))

weight_traj <- function(group_name, w0, day, alive, severity, mouse) {
  if (!alive) return(NA_real_)

  base_delta <- weight_profiles[[group_name]][day + 1]
  value <- w0 + base_delta * (1 + severity) + weight_wobble(mouse, day)
  round(clamp(value, 12.0, 30.0), 1)
}

score_traj <- function(group_name, day, alive, severity, mouse) {
  if (!alive) return(0)

  base <- score_profiles[[group_name]][day + 1]
  adjusted <- round(base + severity * 2 + score_wobble(mouse, day))
  clamp(adjusted, 0, 4)
}

temp_traj <- function(group_name, day, alive, severity, mouse) {
  if (!alive) return(NA_real_)

  base <- temp_profiles[[group_name]][day + 1]
  value <- base + severity * 0.4 + temp_wobble(mouse, day)
  round(clamp(value, 37.4, 40.8), 1)
}

# ── Build weights CSV ─────────────────────────────────────────────────────────
# Header
day_dates <- format(d0_date + days, "%m/%d/%y")
# Remove leading zero from month/day for Windows-style dates
day_dates <- sub("^0", "", gsub("/0", "/", day_dates))

header_fields <- c("Study ID", "Group", "Cage Card", "Mouse ID")
for (d in days) {
  header_fields <- c(header_fields,
                     paste0("D", d, " ", day_dates[d + 1]),
                     "", "", "", "Score")
}

rows <- list(header_fields)

for (g in groups) {
  for (mi in seq_along(g$mice)) {
    mouse  <- g$mice[mi]
    w0     <- g$w0[mi]
    sev    <- g$severity[mi]
    da     <- g$die_after[mi]

    id_fields <- c(study_id, g$name, g$cage, as.character(mouse))

    day_fields <- character(0)
    for (d in days) {
      alive <- d <= da
      date_str <- format(d0_date + d, "%-m/%-d/%Y")   # no leading zeros
      ts   <- paste0(date_str, " ", g$time)
      w    <- weight_traj(g$name, w0, d, alive, sev, mouse)
      s    <- score_traj(g$name, d, alive, sev, mouse)
      temp <- temp_traj(g$name, d, alive, sev, mouse)

      if (alive) {
        day_fields <- c(day_fields,
                        ts,
                        as.character(mouse),
                        paste0(temp, "C"),
                        paste0(w, " g"),
                        as.character(s))
      } else {
        day_fields <- c(day_fields, "", "", "", "", "")
      }
    }

    rows[[length(rows) + 1]] <- c(id_fields, day_fields)
  }
}

# Write - quote fields that contain commas
write_csv_row <- function(fields) {
  escaped <- vapply(fields, function(f) {
    if (grepl(",", f)) paste0('"', f, '"') else f
  }, character(1))
  paste(escaped, collapse = ",")
}

weights_lines <- vapply(rows, write_csv_row, character(1))
writeLines(weights_lines, file.path(out_dir, "example_weights.csv"))
cat("Wrote example_weights.csv\n")

# ── Build survival CSV ────────────────────────────────────────────────────────
# Checkpoints (subset of days actually used in study)
check_days <- c(3, 5, 7, 10, 15)

surv_header <- c("study_id", "animal_id", "group", "notes",
                 paste0("Day", check_days))

surv_rows <- list(surv_header)

for (g in groups) {
  for (mi in seq_along(g$mice)) {
    mouse <- g$mice[mi]
    da    <- g$die_after[mi]
    # da = last alive day; mouse dies on day da+1

    cells <- vapply(check_days, function(cd) {
      if (cd <= da) {
        if (cd == max(check_days[check_days <= da]) && da == 15) {
          "removed_study_end"  # last checkpoint for survivors
        } else {
          "alive"
        }
      } else if (cd == check_days[check_days > da][1]) {
        "found_dead"   # first checkpoint after death
      } else {
        ""             # subsequent checkpoints after death: blank
      }
    }, character(1))

    surv_rows[[length(surv_rows) + 1]] <- c(
      study_id,
      as.character(mouse),
      g$name,
      "",   # notes
      cells
    )
  }
}

surv_lines <- vapply(surv_rows, function(fields) {
  escaped <- vapply(fields, function(f) {
    if (grepl(",", f)) paste0('"', f, '"') else f
  }, character(1))
  paste(escaped, collapse = ",")
}, character(1))

writeLines(surv_lines, file.path(out_dir, "example_survival.csv"))
cat("Wrote example_survival.csv\n")
