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
  list(name = "Control",          cage = "100001", mice = 201:204,
       time = "8:00",
       # D0 weights, slow gain ~0.15/day, score always 0, temp stable ~38.0
       w0 = c(22.5, 21.8, 23.2, 22.1),
       gain = c(0.15, 0.13, 0.18, 0.14),
       die_after = c(15, 15, 15, 15)),   # all survive

  list(name = "Challenge",        cage = "100002", mice = 211:214,
       time = "8:05",
       w0 = c(22.3, 21.9, 23.0, 22.7),
       die_after = c(4, 4, 6, 6)),       # dead D5 or D7 (last weight D4 or D6)

  list(name = "Treatment A High", cage = "100003", mice = 221:224,
       time = "8:10",
       w0 = c(22.8, 21.5, 23.5, 22.2),
       die_after = c(15, 9, 15, 15)),    # 222 dead D10 (last weight D9)

  list(name = "Treatment A Low",  cage = "100004", mice = 231:234,
       time = "8:15",
       w0 = c(22.0, 21.7, 23.3, 22.4),
       die_after = c(6, 9, 15, 15))      # 231 dead D7, 232 dead D10
)

# Weight / score / temp trajectory by group type
weight_traj <- function(group_name, w0, day, alive) {
  if (!alive) return(NA_real_)
  if (group_name == "Control") {
    return(round(w0 + day * 0.15 + (day %% 2) * 0.1, 1))
  }
  # Non-control groups: lose weight until nadir, then recover or reach an endpoint
  if (group_name == "Challenge") {
    # ~1.2-1.5 g/day loss, steepening
    return(round(w0 - day * 1.4 - (day > 2) * 0.4, 1))
  }
  if (group_name == "Treatment A High") {
    if (day <= 4) return(round(w0 - day * 1.05, 1))
    if (day <= 6) return(round(w0 - 4 * 1.05 - (day - 4) * 0.1, 1))  # plateau
    return(round(w0 - 4 * 1.05 - 2 * 0.1 + (day - 6) * 0.57, 1))   # recovery
  }
  if (group_name == "Treatment A Low") {
    if (day <= 6) return(round(w0 - day * 1.25, 1))
    return(round(w0 - 6 * 1.25 + (day - 6) * 0.45, 1))              # partial recovery
  }
  w0
}

score_traj <- function(group_name, day, alive) {
  if (!alive || group_name == "Control") return(0)
  if (group_name == "Challenge") {
    return(min(4, c(0,1,2,2,3,3,4)[day + 1]))
  }
  if (group_name == "Treatment A High") {
    profile <- c(0,1,2,3,3,2,2,1,1,0,0,0,0,0,0,0)
    return(profile[min(day + 1, length(profile))])
  }
  if (group_name == "Treatment A Low") {
    profile <- c(0,1,2,3,3,3,3,2,2,1,1,1,0,0,0,0)
    return(profile[min(day + 1, length(profile))])
  }
  0
}

temp_traj <- function(group_name, day, alive) {
  if (!alive) return(NA_real_)
  if (group_name == "Control") {
    return(round(38.0 + (day %% 2) * 0.1, 1))
  }
  if (group_name == "Challenge") {
    temps <- c(38.0, 38.7, 39.4, 39.9, 40.2, 40.4, 40.5, 40.6)
    return(temps[min(day + 1, length(temps))])
  }
  if (group_name == "Treatment A High") {
    profile <- c(38.0, 38.6, 39.2, 39.5, 39.2, 38.8, 38.5, 38.3, 38.1, 38.0,
                 37.9, 37.8, 37.9, 38.0, 38.0, 38.1)
    return(profile[min(day + 1, length(profile))])
  }
  if (group_name == "Treatment A Low") {
    profile <- c(37.8, 38.5, 39.2, 39.7, 40.1, 40.3, 40.4, 39.9, 39.5, 39.1,
                 38.7, 38.3, 38.0, 37.9, 37.9, 38.0)
    return(profile[min(day + 1, length(profile))])
  }
  38.0
}

# ── Build weights CSV ─────────────────────────────────────────────────────────
# Header
day_dates <- format(d0_date + days, "%m/%d/%y")
# Remove leading zero from month/day for Windows-style dates
day_dates <- sub("^0", "", gsub("/0", "/", day_dates))

header_fields <- c("Group", "Cage Card", "Mouse ID")
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
    da     <- g$die_after[mi]

    # Identity: only first mouse in each cage gets study_id and cage
    if (mi == 1) {
      id_fields <- c(study_id, g$name, as.character(mouse))
    } else {
      id_fields <- c("", "", as.character(mouse))
    }

    day_fields <- character(0)
    for (d in days) {
      alive <- d <= da
      date_str <- format(d0_date + d, "%-m/%-d/%Y")   # no leading zeros
      ts   <- paste0(date_str, " ", g$time)
      w    <- weight_traj(g$name, w0, d, alive)
      s    <- score_traj(g$name, d, alive)
      temp <- temp_traj(g$name, d, alive)

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
