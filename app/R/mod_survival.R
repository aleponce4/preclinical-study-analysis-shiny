mod_survival_ui <- function(id) {
  ns <- shiny::NS(id)
  palette_spec <- palette_gallery_spec()

  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      class = "control-card",
      bslib::card_header("Survival"),
      bslib::card_body(
        class = "control-card-body",
        shiny::checkboxInput(ns("show_ci"), "Show confidence band", value = FALSE),
        shiny::checkboxInput(ns("show_death_marks"), "Show death marks", value = TRUE),
        shiny::checkboxInput(ns("show_p_value"), "Show log-rank p-value", value = TRUE),
        shiny::checkboxInput(ns("vary_linetypes"), "Vary line styles", value = FALSE),
        shiny::checkboxInput(ns("nudge_lines"), "Nudge overlapping lines", value = FALSE),
        shiny::conditionalPanel(
          condition = "input.nudge_lines === true",
          ns = ns,
          shiny::sliderInput(
            ns("nudge_amount"),
            "Nudge strength",
            min = 0,
            max = 0.05,
            value = 0.012,
            step = 0.001
          )
        ),
        shiny::selectInput(
          ns("legend_position"),
          "Legend",
          choices = legend_position_choices(),
          selected = "inset"
        ),
        shiny::selectInput(
          ns("p_adjust_method"),
          "Pairwise p-adjust method",
          choices = p_adjust_method_choices(),
          selected = "bh"
        ),
        shiny::selectInput(
          ns("scheduled_sampling_handling"),
          "Scheduled-sampling handling",
          choices = c(
            "Exclude from KM (Recommended)" = "exclude",
            "Include as censored" = "censor",
            "Count as deaths" = "death"
          ),
          selected = "exclude"
        ),
        shiny::selectInput(
          ns("y_axis_mode"),
          "Y-axis range (%)",
          choices = c("Auto scale" = "auto", "Manual min/max" = "fixed"),
          selected = "fixed"
        ),
        shiny::helpText('Choose "Manual min/max" to set % survival limits below.'),
        shiny::uiOutput(ns("y_axis_inputs")),
        shiny::hr(),
        shiny::selectInput(
          ns("color_mode"),
          "Palette mode",
          choices = c("Preset palette" = "preset", "Custom" = "custom")
        ),
        shiny::conditionalPanel(
          condition = "input.color_mode === 'preset'",
          ns = ns,
          shiny::div(
            class = "palette-gallery",
            shiny::radioButtons(
              ns("palette_name"),
              "Preset palette",
              choiceNames = palette_spec$choiceNames,
              choiceValues = palette_spec$choiceValues,
              selected = "okabe_ito"
            )
          )
        ),
        shiny::uiOutput(ns("color_pickers")),
        shiny::uiOutput(ns("style_warning")),
        shiny::hr(),
        shiny::uiOutput(ns("survival_status")),
        shiny::hr(),
        shiny::uiOutput(ns("survival_downloads"))
      )
    ),
    shiny::tagList(
      bslib::card(
        bslib::card_header("Kaplan-Meier Plot"),
        shiny::plotOutput(ns("survival_plot"), height = "600px")
      ),
      bslib::accordion(
        id = ns("summary_accordion"),
        open = FALSE,
        bslib::accordion_panel(
          "Statistics",
          DT::DTOutput(ns("survival_km_table")),
          shiny::uiOutput(ns("survival_exclusion_caption")),
          DT::DTOutput(ns("survival_exclusion_table")),
          shiny::uiOutput(ns("survival_logrank_caption")),
          shiny::uiOutput(ns("survival_pairwise_header")),
          DT::DTOutput(ns("survival_pairwise_table"))
        )
      )
    )
  )
}

mod_survival_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    same_numeric <- function(a, b, tol = 1e-9) {
      is.numeric(a) && is.numeric(b) &&
        length(a) == 1L && length(b) == 1L &&
        !is.na(a) && !is.na(b) &&
        abs(a - b) <= tol
    }

    shiny::observeEvent(app_state$survival_plot_settings(), {
      settings <- app_state$survival_plot_settings()
      shiny::updateCheckboxInput(session, "show_ci", value = settings$show_ci)
      shiny::updateCheckboxInput(session, "show_death_marks", value = settings$show_death_marks)
      shiny::updateCheckboxInput(session, "show_p_value", value = settings$show_p_value)
      shiny::updateCheckboxInput(session, "vary_linetypes", value = settings$vary_linetypes)
      shiny::updateCheckboxInput(session, "nudge_lines", value = settings$nudge_lines)
      if (is.null(input$nudge_amount) || !same_numeric(input$nudge_amount, settings$nudge_amount)) {
        shiny::updateSliderInput(session, "nudge_amount", value = settings$nudge_amount)
      }
      shiny::updateSelectInput(session, "legend_position", selected = settings$legend_position)
      shiny::updateSelectInput(session, "y_axis_mode", selected = settings$y_axis_mode)
    }, ignoreInit = FALSE)

    shiny::observeEvent(app_state$stats_settings(), {
      shiny::updateSelectInput(
        session,
        "p_adjust_method",
        selected = app_state$stats_settings()$p_adjust_method
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(app_state$survival_analysis_settings(), {
      shiny::updateSelectInput(
        session,
        "scheduled_sampling_handling",
        selected = app_state$survival_analysis_settings()$scheduled_sampling_handling
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$p_adjust_method, {
      app_state$set_stats_settings(list(p_adjust_method = input$p_adjust_method))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$scheduled_sampling_handling, {
      app_state$set_survival_analysis_settings(list(
        scheduled_sampling_handling = input$scheduled_sampling_handling
      ))
    }, ignoreInit = TRUE)

    output$y_axis_inputs <- shiny::renderUI({
      manual_mode <- identical(input$y_axis_mode %||% app_state$survival_plot_settings()$y_axis_mode, "fixed")
      shiny::tagList(
        disabled_numeric_input(
          session$ns("y_min"),
          "Min %",
          value = shiny::isolate(input$y_min %||% app_state$survival_plot_settings()$y_min),
          disabled = !manual_mode
        ),
        disabled_numeric_input(
          session$ns("y_max"),
          "Max %",
          value = shiny::isolate(input$y_max %||% app_state$survival_plot_settings()$y_max),
          disabled = !manual_mode
        )
      )
    })

    # ---- Color controls ----

    shiny::observeEvent(app_state$shared_style(), {
      shiny::updateSelectInput(session, "color_mode", selected = app_state$shared_style()$color_mode)
      shiny::updateRadioButtons(session, "palette_name", selected = app_state$shared_style()$palette_name)
    }, ignoreInit = FALSE)

    shiny::observeEvent(list(input$color_mode, input$palette_name), {
      app_state$set_shared_style(list(
        color_mode = input$color_mode,
        palette_name = input$palette_name
      ))
    }, ignoreInit = TRUE)

    output$color_pickers <- shiny::renderUI({
      group_meta <- app_state$resolved_group_meta()
      if (!nrow(group_meta) || !identical(app_state$shared_style()$color_mode, "custom")) {
        return(NULL)
      }

      shiny::tagList(lapply(seq_len(nrow(group_meta)), function(i) {
        colourpicker::colourInput(
          inputId = session$ns(paste0("group_color_", group_meta$group_id[[i]])),
          label = group_meta$display_name[[i]],
          value = group_meta$custom_color[[i]],
          palette = "limited",
          allowTransparent = FALSE,
          showColour = "both"
        )
      }))
    })

    output$style_warning <- shiny::renderUI({
      if (!identical(app_state$shared_style()$color_mode, "custom")) {
        return(NULL)
      }
      format_issue_list(
        "Style warnings",
        invalid_custom_color_messages(app_state$validation()$group_meta, app_state$shared_style()),
        class = "issue-block issue-block-warning"
      )
    })

    color_picker_values <- shiny::reactive({
      group_meta <- app_state$resolved_group_meta()
      if (!nrow(group_meta) || !identical(app_state$shared_style()$color_mode, "custom")) {
        return(list())
      }
      input_ids <- paste0("group_color_", group_meta$group_id)
      values <- lapply(input_ids, function(id) input[[id]])
      stats::setNames(values, group_meta$group_id)
    })

    last_color_picker_values <- shiny::reactiveVal(list())

    shiny::observeEvent(color_picker_values(), {
      current_values <- color_picker_values()
      if (!length(current_values) || !identical(app_state$shared_style()$color_mode, "custom")) {
        last_color_picker_values(list())
        return(invisible(NULL))
      }

      group_meta <- app_state$validation()$group_meta
      if (!nrow(group_meta)) {
        return(invisible(NULL))
      }

      color_updates <- changed_named_values(current_values, last_color_picker_values())
      last_color_picker_values(current_values)

      if (length(color_updates)) {
        app_state$set_group_overrides(update_group_override_colors(group_meta, color_updates))
      }
    }, ignoreInit = TRUE)

    # ---- Plot settings ----

    survival_plot_settings <- shiny::reactive({
      defaults <- app_state$survival_plot_settings()

      normalize_survival_plot_settings(list(
        show_ci = input$show_ci %||% defaults$show_ci,
        show_death_marks = input$show_death_marks %||% defaults$show_death_marks,
        show_p_value = input$show_p_value %||% defaults$show_p_value,
        vary_linetypes = input$vary_linetypes %||% defaults$vary_linetypes,
        nudge_lines = input$nudge_lines %||% defaults$nudge_lines,
        nudge_amount = input$nudge_amount %||% defaults$nudge_amount,
        legend_position = input$legend_position %||% defaults$legend_position,
        y_axis_mode = input$y_axis_mode %||% defaults$y_axis_mode,
        y_min = input$y_min %||% defaults$y_min,
        y_max = input$y_max %||% defaults$y_max
      ))
    }) |>
      shiny::debounce(150)

    shiny::observeEvent(survival_plot_settings(), {
      app_state$set_survival_plot_settings(survival_plot_settings())
    }, ignoreInit = TRUE)

    prepared_survival_settings <- shiny::reactive({
      prepare_survival_plot_settings(survival_plot_settings())
    })

    survival_analysis <- shiny::reactive({
      validation <- app_state$validation()

      if (length(validation$hard_errors)) {
        return(list(valid = FALSE, message = "Weights errors must be fixed before survival can be analysed."))
      }

      if (length(validation$survival_hard_errors)) {
        return(list(
          valid = FALSE,
          message = paste(validation$survival_hard_errors, collapse = " ")
        ))
      }

      if (!nrow(validation$clean_survival)) {
        return(list(valid = FALSE, message = "No valid survival rows are currently available."))
      }

      compute_survival_analysis(
        validation$clean_survival,
        app_state$resolved_group_meta(),
        p_adjust_method = app_state$stats_settings()$p_adjust_method,
        scheduled_sampling_handling = app_state$survival_analysis_settings()$scheduled_sampling_handling
      )
    })

    output$survival_status <- shiny::renderUI({
      validation <- app_state$validation()
      analysis <- survival_analysis()

      if (!analysis$valid) {
        scheduled_rows <- if (is.data.frame(analysis$scheduled_summary) && nrow(analysis$scheduled_summary)) {
          sum(analysis$scheduled_summary$`Scheduled cohort rows`, na.rm = TRUE)
        } else {
          0L
        }
        scheduled_note <- if (scheduled_rows > 0) {
          handling <- analysis$scheduled_sampling_handling %||% "exclude"
          note_text <- switch(
            handling,
            exclude = sprintf("Scheduled cohort rows excluded from Kaplan-Meier: %d.", scheduled_rows),
            censor = sprintf("Scheduled cohort rows included in Kaplan-Meier as censored: %d.", scheduled_rows),
            death = sprintf("Scheduled cohort rows counted in Kaplan-Meier as deaths: %d. This can bias survival downward.", scheduled_rows),
            sprintf("Scheduled cohort rows detected: %d.", scheduled_rows)
          )
          shiny::div(
            class = if (identical(handling, "death")) "issue-block issue-block-warning" else "issue-block issue-block-ok",
            note_text
          )
        } else {
          NULL
        }
        return(shiny::tagList(
          shiny::div(class = "issue-block issue-block-warning", analysis$message),
          scheduled_note,
          format_issue_list(
            "Survival warnings",
            compact_chr(c(validation$warnings, validation$survival_hard_errors, prepared_survival_settings()$warnings)),
            class = "issue-block issue-block-warning"
          )
        ))
      }

      warning_block <- format_issue_list(
        "Current survival warnings",
        compact_chr(c(validation$warnings, prepared_survival_settings()$warnings)),
        class = "issue-block issue-block-warning"
      )

      scheduled_rows <- if (is.data.frame(analysis$scheduled_summary) && nrow(analysis$scheduled_summary)) {
        sum(analysis$scheduled_summary$`Scheduled cohort rows`, na.rm = TRUE)
      } else {
        0L
      }
      scheduled_note <- if (scheduled_rows > 0) {
        handling <- analysis$scheduled_sampling_handling %||% "exclude"
        note_text <- switch(
          handling,
          exclude = sprintf("Scheduled cohort rows excluded from Kaplan-Meier: %d.", scheduled_rows),
          censor = sprintf("Scheduled cohort rows included in Kaplan-Meier as censored: %d.", scheduled_rows),
          death = sprintf("Scheduled cohort rows counted in Kaplan-Meier as deaths: %d. This can bias survival downward.", scheduled_rows),
          sprintf("Scheduled cohort rows detected: %d.", scheduled_rows)
        )
        shiny::div(
          class = if (identical(handling, "death")) "issue-block issue-block-warning" else "issue-block issue-block-ok",
          note_text
        )
      } else {
        NULL
      }

      shiny::tagList(
        warning_block %||% shiny::div(class = "issue-block issue-block-ok", "Survival data are ready to plot."),
        scheduled_note
      )
    })

    output$survival_downloads <- shiny::renderUI({
      analysis <- survival_analysis()
      ns <- session$ns

      if (!analysis$valid) {
        return(shiny::div(
          class = "button-row",
          shiny::tags$button("Download PNG",            class = "btn btn-secondary", disabled = "disabled"),
          shiny::tags$button("Download TIFF",           class = "btn btn-secondary", disabled = "disabled"),
          shiny::tags$button("Download PDF",            class = "btn btn-secondary", disabled = "disabled"),
          shiny::tags$button("Download CSV (GraphPad)", class = "btn btn-secondary", disabled = "disabled")
        ))
      }

      shiny::div(
        class = "button-row",
        shiny::downloadButton(ns("download_png"),  "Download PNG"),
        shiny::downloadButton(ns("download_tiff"), "Download TIFF"),
        shiny::downloadButton(ns("download_pdf"),  "Download PDF"),
        shiny::downloadButton(ns("download_csv"),  "Download CSV (GraphPad)")
      )
    })

    output$survival_plot <- shiny::renderPlot({
      analysis <- survival_analysis()
      shiny::validate(shiny::need(
        isTRUE(analysis$valid),
        analysis$message %||% "No survival data available. Load a study data file on the Import tab, or enable 'Infer death events from missing future weights' on the Manual Data Entry tab."
      ))
      plot_survival(
        analysis,
        metadata = app_state$metadata(),
        settings = prepared_survival_settings()$settings
      )
    }, res = 144)

    output$survival_km_table <- DT::renderDT({
      analysis <- survival_analysis()
      shiny::req(analysis$valid)
      DT::datatable(
        analysis$summary,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE)
      ) |>
        DT::formatRound(columns = c("Median (DPI)", "Median CI low", "Median CI high"), digits = 1)
    })

    output$survival_exclusion_caption <- shiny::renderUI({
      analysis <- survival_analysis()
      shiny::req(is.data.frame(analysis$scheduled_summary), nrow(analysis$scheduled_summary) > 0)
      handling <- analysis$scheduled_sampling_handling %||% "exclude"
      label <- switch(
        handling,
        exclude = "Scheduled cohort summary (excluded from Kaplan-Meier):",
        censor = "Scheduled cohort summary (included as censored):",
        death = "Scheduled cohort summary (counted as deaths):",
        "Scheduled cohort summary:"
      )
      shiny::p(
        shiny::strong(label),
        style = "margin-top: 0.8em; margin-bottom: 0.2em;"
      )
    })

    output$survival_exclusion_table <- DT::renderDT({
      analysis <- survival_analysis()
      shiny::req(is.data.frame(analysis$scheduled_summary), nrow(analysis$scheduled_summary) > 0)
      DT::datatable(
        analysis$scheduled_summary,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE)
      )
    })

    output$survival_logrank_caption <- shiny::renderUI({
      analysis <- survival_analysis()
      shiny::req(analysis$valid)
      p <- analysis$p_value
      if (is.na(p)) return(NULL)
      p_text <- if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
      shiny::p(
        shiny::strong("Overall log-rank test: "),
        sprintf("p = %s", p_text),
        style = "margin-top: 0.6em; margin-bottom: 0;"
      )
    })

    output$survival_pairwise_header <- shiny::renderUI({
      analysis <- survival_analysis()
      shiny::req(analysis$valid)
      if (is.null(analysis$pairwise_tests)) return(NULL)
      method <- normalize_p_adjust_method(app_state$stats_settings()$p_adjust_method)
      shiny::p(
        shiny::strong(sprintf("Pairwise log-rank (%s-adjusted):", p_adjust_method_label(method))),
        style = "margin-top: 0.8em; margin-bottom: 0.2em;"
      )
    })

    output$survival_pairwise_table <- DT::renderDT({
      analysis <- survival_analysis()
      shiny::req(analysis$valid, !is.null(analysis$pairwise_tests))
      method <- normalize_p_adjust_method(app_state$stats_settings()$p_adjust_method)
      adjusted_col <- p_adjusted_column_name(method)
      DT::datatable(
        analysis$pairwise_tests,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE)
      ) |>
        DT::formatSignif(columns = c("p (raw)", adjusted_col), digits = 3)
    })

    output$download_png <- shiny::downloadHandler(
      filename = function() {
        survival_plot_filename(app_state$metadata(), app_state$survival_name(), ext = "png")
      },
      content = function(file) {
        analysis <- survival_analysis()
        shiny::req(analysis$valid)
        save_plot_file(
          plot_survival(
            analysis,
            metadata = app_state$metadata(),
            settings = prepared_survival_settings()$settings
          ),
          file,
          dpi = 300
        )
      }
    )

    output$download_tiff <- shiny::downloadHandler(
      filename = function() {
        survival_plot_filename(app_state$metadata(), app_state$survival_name(), ext = "tiff")
      },
      content = function(file) {
        analysis <- survival_analysis()
        shiny::req(analysis$valid)
        save_plot_file(
          plot_survival(
            analysis,
            metadata = app_state$metadata(),
            settings = prepared_survival_settings()$settings
          ),
          file,
          dpi = 300
        )
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        survival_plot_filename(app_state$metadata(), app_state$survival_name(), ext = "pdf")
      },
      content = function(file) {
        analysis <- survival_analysis()
        shiny::req(analysis$valid)
        save_plot_file(
          plot_survival(
            analysis,
            metadata = app_state$metadata(),
            settings = prepared_survival_settings()$settings
          ),
          file,
          dpi = 300
        )
      }
    )

    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        survival_csv_filename(app_state$metadata(), app_state$survival_name())
      },
      content = function(file) {
        validation <- app_state$validation()
        shiny::req(
          !length(validation$hard_errors),
          !length(validation$survival_hard_errors),
          nrow(validation$clean_survival) > 0
        )
        csv_df <- graphpad_survival_csv(
          clean_survival = validation$clean_survival,
          group_meta     = app_state$resolved_group_meta(),
          scheduled_sampling_handling = app_state$survival_analysis_settings()$scheduled_sampling_handling
        )
        utils::write.csv(csv_df, file, row.names = FALSE, na = "")
      }
    )
  })
}
