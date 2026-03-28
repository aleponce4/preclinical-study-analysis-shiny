automation_report_template_path <- function(root) {
  file.path(root, "automation", "reports", "study_report.Rmd")
}

automation_report_output_dir <- function(root, scheduled_at = Sys.time()) {
  date_dir <- format(as.POSIXct(scheduled_at), "%Y-%m-%d", tz = "UTC")
  ensure_dir(file.path(root, "automation", "output", date_dir))
}

automation_report_filename <- function(study_id, scheduled_at = Sys.time(), ext = "pdf") {
  stamp <- format(as.POSIXct(scheduled_at), "%Y%m%d-%H%M", tz = "UTC")
  sprintf("%s-report-%s.%s", janitor::make_clean_names(study_id), stamp, ext)
}

automation_preview_assets_dir <- function(root, study_id, scheduled_at = Sys.time()) {
  out_dir <- automation_report_output_dir(root, scheduled_at = scheduled_at)
  stem <- tools::file_path_sans_ext(automation_report_filename(study_id, scheduled_at = scheduled_at, ext = "html"))
  ensure_dir(file.path(out_dir, paste0(stem, "-assets")))
}

automation_html_escape <- function(x) {
  htmltools::htmlEscape(as.character(x %||% ""))
}

automation_table_html <- function(tbl, digits = NULL) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(htmltools::tags$p("No data available."))
  }

  display_tbl <- tbl
  if (!is.null(digits)) {
    numeric_cols <- vapply(display_tbl, is.numeric, logical(1))
    for (col in names(display_tbl)[numeric_cols]) {
      display_tbl[[col]] <- format(round(display_tbl[[col]], digits = digits), nsmall = digits, trim = TRUE)
    }
  }

  htmltools::tags$table(
    class = "report-table",
    htmltools::tags$thead(
      htmltools::tags$tr(lapply(names(display_tbl), function(name) {
        htmltools::tags$th(automation_html_escape(name))
      }))
    ),
    htmltools::tags$tbody(
      lapply(seq_len(nrow(display_tbl)), function(i) {
        htmltools::tags$tr(lapply(display_tbl[i, , drop = FALSE], function(value) {
          htmltools::tags$td(automation_html_escape(value[[1]]))
        }))
      })
    )
  )
}

automation_metric_html <- function(label, value) {
  htmltools::tags$p(
    class = "metric-line",
    htmltools::tags$strong(paste0(label, ": ")),
    automation_html_escape(value)
  )
}

automation_preview_css <- function() {
  paste(
    "body { font-family: Arial, sans-serif; margin: 2.25rem auto 3rem; max-width: 1100px; color: #000; line-height: 1.55; background: #f8fbfd; }",
    "h1, h2, h3 { color: #000; margin-top: 0; }",
    ".report-shell { background: white; border: 1px solid #d9e2ec; border-radius: 14px; padding: 2rem 2.25rem; box-shadow: 0 18px 40px rgba(15, 23, 42, 0.08); }",
    ".report-header { display: flex; justify-content: space-between; gap: 2rem; align-items: flex-start; border-bottom: 2px solid #d9e2ec; padding-bottom: 0.85rem; margin-bottom: 0.95rem; }",
    ".report-kicker { color: #000; font-size: 0.78rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; margin-bottom: 0.2rem; }",
    ".report-title { font-size: 2.15rem; line-height: 1.1; margin: 0; }",
    ".report-subtitle { font-size: 1rem; font-weight: 700; color: #000; margin: 0.35rem 0 0; }",
    ".report-date { min-width: 230px; text-align: right; }",
    ".report-date-label { color: #000; font-size: 0.82rem; font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase; }",
    ".report-date-value { font-size: 1rem; font-weight: 700; color: #000; margin-top: 0.25rem; }",
    ".meta-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 0.45rem 1.5rem; margin: 0.95rem 0 0; }",
    ".meta-item { font-size: 0.94rem; }",
    ".meta-label { color: #000; font-weight: 700; margin-right: 0.35rem; }",
    ".outline-bar { border: 1px solid #d9e2ec; border-radius: 10px; padding: 0.75rem 0.95rem; background: #f8fbfd; margin-top: 1rem; }",
    ".outline-label { color: #000; font-size: 0.78rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; margin-bottom: 0.25rem; }",
    ".outline-items { color: #000; font-weight: 600; display: flex; flex-wrap: wrap; gap: 0.5rem; }",
    ".outline-chip { border: 1px solid #cbd2d9; border-radius: 999px; padding: 0.18rem 0.55rem; background: white; }",
    ".section { margin-top: 1.5rem; padding-top: 1rem; border-top: 2px solid #d9e2ec; }",
    ".section-title { font-size: 1.45rem; margin-bottom: 0.15rem; }",
    ".section-subtitle { color: #000; margin: 0 0 0.95rem; }",
    ".warning-list { background: #fff8e1; border: 1px solid #f0d98a; padding: 1rem 1.25rem; border-radius: 10px; margin: 0; }",
    ".plot-block { margin: 1rem 0 1.35rem; }",
    ".plot-block img { width: 100%; border: 1px solid #d9e2ec; border-radius: 10px; background: white; }",
    ".metric-line { margin: 0.4rem 0 0.85rem; }",
    ".report-table { width: 100%; border-collapse: collapse; margin: 0.5rem 0 1.5rem 0; font-size: 0.95rem; }",
    ".report-table th, .report-table td { border: 1px solid #d9e2ec; padding: 0.5rem 0.6rem; text-align: left; vertical-align: top; }",
    ".report-table thead th { background: #f0f4f8; color: #000; }",
    ".table-title { font-size: 1.05rem; font-weight: 700; color: #000; margin: 1rem 0 0.35rem; }",
    ".muted-note { color: #000; margin-top: 0.4rem; }",
    sep = "\n"
  )
}

automation_preview_plot_path <- function(assets_dir, name) {
  file.path(assets_dir, sprintf("%s.png", name))
}

automation_render_preview_report <- function(root,
                                             analysis,
                                             scheduled_at,
                                             verbose = FALSE) {
  out_dir <- automation_report_output_dir(root, scheduled_at = scheduled_at)
  out_file <- file.path(
    out_dir,
    automation_report_filename(analysis$study$study_id, scheduled_at = scheduled_at, ext = "html")
  )
  assets_dir <- automation_preview_assets_dir(root, analysis$study$study_id, scheduled_at = scheduled_at)
  assets_href <- basename(assets_dir)

  weight_plot_path <- automation_preview_plot_path(assets_dir, "weights")
  survival_plot_path <- automation_preview_plot_path(assets_dir, "survival")
  save_plot_file(analysis$plots$weights, weight_plot_path, width = 11, height = 5.9, dpi = 180)
  save_plot_file(analysis$plots$survival, survival_plot_path, width = 11, height = 5.9, dpi = 180)

  score_plot_block <- htmltools::tags$p("No clinical score columns were detected in the input data.")
  if (isTRUE(analysis$flags$has_scores) && inherits(analysis$plots$scores, "ggplot")) {
    score_plot_path <- automation_preview_plot_path(assets_dir, "scores")
    save_plot_file(analysis$plots$scores, score_plot_path, width = 11, height = 5.9, dpi = 180)
    score_plot_block <- htmltools::tags$div(
      class = "plot-block",
      htmltools::tags$img(src = file.path(assets_href, basename(score_plot_path)), alt = "Clinical score plot")
    )
  }

  warnings_block <- if (length(analysis$summary$warnings %||% character())) {
    htmltools::tags$ul(class = "warning-list", lapply(analysis$summary$warnings, function(item) htmltools::tags$li(item)))
  } else {
    htmltools::tags$p("No other warnings.")
  }

  html <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$title(sprintf("Report Preview - %s", analysis$study$report_title %||% analysis$study$study_id)),
      htmltools::tags$style(htmltools::HTML(automation_preview_css()))
    ),
    htmltools::tags$body(
      htmltools::tags$div(
        class = "report-shell",
        htmltools::tags$div(
          class = "report-header",
          htmltools::tags$div(
            htmltools::tags$div(class = "report-kicker", automation_html_escape(analysis$report_name %||% "Study Monitoring Report")),
            htmltools::tags$h1(class = "report-title", automation_html_escape(analysis$study$report_title %||% analysis$study$study_id)),
            htmltools::tags$p(class = "report-subtitle", "Weights, survival, and clinical score overview")
          ),
          htmltools::tags$div(
            class = "report-date",
            htmltools::tags$div(class = "report-date-label", "Generated"),
            htmltools::tags$div(class = "report-date-value", automation_html_escape(format(analysis$generated_at, "%Y-%m-%d %H:%M %Z")))
          )
        ),
        htmltools::tags$div(
          class = "meta-grid",
          htmltools::tags$div(class = "meta-item", htmltools::tags$span(class = "meta-label", "Study ID"), automation_html_escape(analysis$study$study_id)),
          htmltools::tags$div(class = "meta-item", htmltools::tags$span(class = "meta-label", "Source file"), automation_html_escape(analysis$source_file)),
          htmltools::tags$div(class = "meta-item", htmltools::tags$span(class = "meta-label", "Survival source"), "Inferred from weights"),
          htmltools::tags$div(class = "meta-item", htmltools::tags$span(class = "meta-label", "P-adjust method"), toupper(automation_html_escape(analysis$p_adjust_method)))
        ),
        htmltools::tags$div(
          class = "outline-bar",
          htmltools::tags$div(class = "outline-label", "Report outline"),
          htmltools::tags$div(
            class = "outline-items",
            htmltools::tags$span(class = "outline-chip", "1. Summary"),
            htmltools::tags$span(class = "outline-chip", "2. Weights"),
            htmltools::tags$span(class = "outline-chip", "3. Survival"),
            htmltools::tags$span(class = "outline-chip", "4. Clinical Scores")
          )
        ),
        htmltools::tags$div(
          class = "section",
          htmltools::tags$h2(class = "section-title", "1. Summary"),
          htmltools::tags$p(class = "section-subtitle", "Key events and notable warnings from this report run"),
          if (length(analysis$summary$items %||% character())) {
            htmltools::tags$ul(lapply(analysis$summary$items, function(item) htmltools::tags$li(item)))
          } else {
            htmltools::tags$p("No study summary items available.")
          },
          htmltools::tags$div(class = "table-title", "Overnight Alerts"),
          if (length(analysis$summary$alerts %||% character())) {
            htmltools::tags$ul(lapply(analysis$summary$alerts, function(item) htmltools::tags$li(item)))
          } else {
            htmltools::tags$p("No overnight alerts.")
          },
          htmltools::tags$div(class = "table-title", "Other Warnings"),
          warnings_block
        ),
        htmltools::tags$div(
          class = "section",
          htmltools::tags$h2(class = "section-title", "2. Weights"),
          htmltools::tags$p(class = "section-subtitle", "Trajectory plot, snapshot table, and final-day statistical tests"),
          htmltools::tags$div(class = "plot-block", htmltools::tags$img(src = file.path(assets_href, basename(weight_plot_path)), alt = "Weight plot")),
          htmltools::tags$div(class = "table-title", "Key Summary"),
          automation_table_html(analysis$tables$weight_key_stats),
          htmltools::tags$div(class = "table-title", "Statistical Tests"),
          automation_metric_html("Kruskal-Wallis p-value at final DPI", if (is.na(analysis$metrics$weight_kw_p)) "NA" else sprintf("%.3g", analysis$metrics$weight_kw_p)),
          automation_table_html(analysis$tables$weight_pairwise, digits = 3)
        ),
        htmltools::tags$div(
          class = "section",
          htmltools::tags$h2(class = "section-title", "3. Survival"),
          htmltools::tags$p(class = "section-subtitle", "Kaplan-Meier plot, survival overview, and pairwise comparisons"),
          htmltools::tags$div(class = "plot-block", htmltools::tags$img(src = file.path(assets_href, basename(survival_plot_path)), alt = "Survival plot")),
          htmltools::tags$div(class = "table-title", "Key Summary"),
          automation_table_html(analysis$tables$survival_summary, digits = 1),
          htmltools::tags$div(class = "table-title", "Statistical Tests"),
          automation_metric_html("Log-rank p-value", if (is.na(analysis$metrics$survival_logrank_p)) "NA" else sprintf("%.3g", analysis$metrics$survival_logrank_p)),
          automation_table_html(analysis$tables$survival_pairwise, digits = 3)
        ),
        htmltools::tags$div(
          class = "section",
          htmltools::tags$h2(class = "section-title", "4. Clinical Scores"),
          htmltools::tags$p(class = "section-subtitle", "Clinical score plot and key summary statistics when available"),
          score_plot_block,
          if (isTRUE(analysis$flags$has_scores)) {
            htmltools::tagList(
              htmltools::tags$div(class = "table-title", "Key Summary"),
              automation_table_html(analysis$tables$score_key_stats),
              htmltools::tags$div(class = "table-title", "Statistical Tests"),
              automation_metric_html("Kruskal-Wallis p-value at final DPI", if (is.na(analysis$metrics$score_kw_p)) "NA" else sprintf("%.3g", analysis$metrics$score_kw_p)),
              if (!is.null(analysis$tables$score_pairwise) && nrow(analysis$tables$score_pairwise)) automation_table_html(analysis$tables$score_pairwise, digits = 3) else htmltools::tags$p(class = "muted-note", "Pairwise clinical score comparisons were not available for this study.")
            )
          } else {
            htmltools::tags$p(class = "muted-note", "No clinical score columns were detected in the input data.")
          }
        )
      )
    )
  )

  htmltools::save_html(html, file = out_file, background = "white", libdir = NULL)
  out_file
}

automation_render_study_report <- function(root,
                                           analysis,
                                           scheduled_at,
                                           verbose = FALSE) {
  template <- automation_report_template_path(root)
  if (!file.exists(template)) {
    stop(sprintf("Report template not found: %s", template), call. = FALSE)
  }

  out_dir <- automation_report_output_dir(root, scheduled_at = scheduled_at)
  out_file <- file.path(
    out_dir,
    automation_report_filename(analysis$study$study_id, scheduled_at = scheduled_at)
  )

  render_env <- new.env(parent = globalenv())

  tryCatch(
    {
      rmarkdown::render(
        input = template,
        output_file = basename(out_file),
        output_dir = dirname(out_file),
        params = list(report = analysis),
        envir = render_env,
        quiet = !isTRUE(verbose)
      )
      out_file
    },
    error = function(e) {
      stop(
        sprintf("Failed to render report for study '%s': %s", analysis$study$study_id, conditionMessage(e)),
        call. = FALSE
      )
    }
  )
}
