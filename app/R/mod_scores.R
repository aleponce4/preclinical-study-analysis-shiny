mod_scores_ui <- function(id) {
  ns <- shiny::NS(id)
  palette_spec <- palette_gallery_spec()

  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      class = "control-card",
      bslib::card_header("Controls"),
      bslib::card_body(
        class = "control-card-body",
        shiny::radioButtons(
          ns("individual_layer"),
          "Individual animals",
          choices = c("None" = "none", "Points" = "points", "Lines" = "lines"),
          selected = "points"
        ),
        shiny::conditionalPanel(
          condition = "input.individual_layer === 'points' && !input.show_mean",
          ns = ns,
          shiny::sliderInput(
            ns("point_spread"),
            "Group stagger",
            min = 0, max = 1, value = 0.10, step = 0.01
          )
        ),
        shiny::checkboxInput(ns("show_mean"), "Show trendline?", value = TRUE),
        shiny::selectInput(
          ns("error_style"),
          "Error statistic",
          choices = c("SEM" = "sem", "SD" = "sd", "95% CI" = "ci95", "None" = "none"),
          selected = "sem"
        ),
        shiny::conditionalPanel(
          condition = "input.error_style !== 'none'",
          ns = ns,
          shiny::tagList(
            shiny::selectInput(
              ns("error_display"),
              "Error display",
              choices = c("Confidence band" = "band", "Error bars" = "bar"),
              selected = "band"
            ),
            shiny::helpText("Error settings are applied when the trendline layer is shown.")
          )
        ),
        shiny::selectInput(
          ns("y_axis_mode"),
          "Y-axis range",
          choices = c("Auto scale" = "auto", "Manual min/max" = "fixed"),
          selected = "fixed"
        ),
        shiny::helpText('Choose "Manual min/max" to edit the limits below.'),
        shiny::uiOutput(ns("y_axis_inputs")),
        shiny::checkboxInput(ns("show_legend_n"), "Show N in legend", value = FALSE),
        shiny::selectInput(
          ns("p_adjust_method"),
          "Pairwise p-adjust method",
          choices = p_adjust_method_choices(),
          selected = "bh"
        ),
        shiny::selectInput(
          ns("legend_position"),
          "Legend",
          choices = legend_position_choices(),
          selected = "inset_bottom_left"
        ),
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
        shiny::uiOutput(ns("scores_status")),
        shiny::hr(),
        shiny::downloadButton(ns("download_png"), "Download PNG"),
        shiny::downloadButton(ns("download_tiff"), "Download TIFF"),
        shiny::downloadButton(ns("download_pdf"), "Download PDF"),
        shiny::downloadButton(ns("download_csv"), "Download CSV (GraphPad)")
      )
    ),
    shiny::tagList(
      bslib::card(
        bslib::card_header(
          "Clinical Scores",
          shiny::span(
            style = "font-size: 0.8em; color: #6c757d; font-weight: normal; margin-left: 0.5em;",
            "(Score range: 0\u201310)"
          )
        ),
        shiny::plotOutput(ns("scores_plot"), height = "600px")
      ),
      bslib::accordion(
        id = ns("summary_accordion"),
        open = FALSE,
        bslib::accordion_panel(
          "Statistics",
          DT::DTOutput(ns("scores_stats_table")),
          shiny::uiOutput(ns("scores_test_caption")),
          shiny::uiOutput(ns("scores_pairwise_header")),
          DT::DTOutput(ns("scores_pairwise_table"))
        ),
        bslib::accordion_panel(
          "Data Table",
          DT::DTOutput(ns("scores_summary"))
        )
      )
    )
  )
}

mod_scores_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(app_state$score_plot_settings(), {
      settings <- app_state$score_plot_settings()
      shiny::updateRadioButtons(session, "individual_layer", selected = settings$individual_layer)
      shiny::updateCheckboxInput(session, "show_mean", value = settings$show_mean)
      shiny::updateSelectInput(session, "error_style", selected = settings$error_style)
      shiny::updateSelectInput(session, "error_display", selected = settings$error_display)
      shiny::updateSelectInput(session, "y_axis_mode", selected = settings$y_axis_mode)
      shiny::updateNumericInput(session, "y_min", value = settings$y_min)
      shiny::updateNumericInput(session, "y_max", value = settings$y_max)
      shiny::updateCheckboxInput(session, "show_legend_n", value = settings$show_legend_n)
      shiny::updateSelectInput(session, "legend_position", selected = settings$legend_position)
      shiny::updateSliderInput(session, "point_spread", value = settings$point_spread)
    }, ignoreInit = FALSE)

    shiny::observeEvent(app_state$stats_settings(), {
      shiny::updateSelectInput(
        session,
        "p_adjust_method",
        selected = app_state$stats_settings()$p_adjust_method
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$p_adjust_method, {
      app_state$set_stats_settings(list(p_adjust_method = input$p_adjust_method))
    }, ignoreInit = TRUE)

    output$y_axis_inputs <- shiny::renderUI({
      manual_mode <- identical(input$y_axis_mode %||% app_state$score_plot_settings()$y_axis_mode, "fixed")
      shiny::tagList(
        disabled_numeric_input(
          session$ns("y_min"),
          "Min",
          value = shiny::isolate(input$y_min %||% app_state$score_plot_settings()$y_min),
          disabled = !manual_mode
        ),
        disabled_numeric_input(
          session$ns("y_max"),
          "Max",
          value = shiny::isolate(input$y_max %||% app_state$score_plot_settings()$y_max),
          disabled = !manual_mode
        )
      )
    })

    shiny::observeEvent(input$show_mean, {
      if (isTRUE(input$show_mean)) {
        shiny::updateSelectInput(
          session,
          "error_display",
          choices = c("Confidence band" = "band", "Error bars" = "bar"),
          selected = "band"
        )
      } else {
        shiny::updateSelectInput(
          session,
          "error_display",
          choices = c("Error bars" = "bar"),
          selected = "bar"
        )
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(
      list(input$individual_layer, input$show_mean),
      {
        if (identical(input$individual_layer, "none") && isFALSE(input$show_mean)) {
          shiny::updateCheckboxInput(session, "show_mean", value = TRUE)
          shiny::showNotification("At least one clinical score layer must remain visible. Restored the trendline.", type = "warning")
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      list(input$individual_layer, input$show_mean, input$error_style),
      {
        if (identical(input$individual_layer, "points") && isFALSE(input$show_mean) && identical(input$error_style, "none")) {
          shiny::updateSelectInput(session, "error_style", selected = "sem")
        }
      },
      ignoreInit = TRUE
    )

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

    score_plot_settings <- shiny::reactive({
      defaults <- app_state$score_plot_settings()

      normalize_score_plot_settings(list(
        individual_layer = input$individual_layer %||% defaults$individual_layer,
        show_mean = input$show_mean %||% defaults$show_mean,
        error_style = input$error_style %||% defaults$error_style,
        error_display = input$error_display %||% defaults$error_display,
        y_axis_mode = input$y_axis_mode %||% defaults$y_axis_mode,
        y_min = input$y_min %||% defaults$y_min,
        y_max = input$y_max %||% defaults$y_max,
        show_legend_n = input$show_legend_n %||% defaults$show_legend_n,
        legend_position = input$legend_position %||% defaults$legend_position,
        point_spread = input$point_spread %||% defaults$point_spread
      ))
    })

    shiny::observeEvent(score_plot_settings(), {
      app_state$set_score_plot_settings(score_plot_settings())
    }, ignoreInit = TRUE)

    prepared_score_settings <- shiny::reactive({
      prepare_score_plot_settings(score_plot_settings())
    })

    processed_scores <- shiny::reactive({
      validation <- app_state$validation()
      shiny::req(!length(validation$hard_errors))
      validation$clean_scores
    })

    output$scores_status <- shiny::renderUI({
      validation <- app_state$validation()

      if (length(validation$hard_errors)) {
        return(format_issue_list(
          "Scores plotting is blocked until these issues are fixed.",
          validation$hard_errors,
          class = "issue-block issue-block-error"
        ))
      }

      if (!nrow(processed_scores())) {
        return(shiny::div(
          class = "issue-block issue-block-warning",
          "No clinical score data are available. Score columns may be missing or contain only blank/non-numeric values."
        ))
      }

      warning_block <- format_issue_list(
        "Current scores warnings",
        compact_chr(c(
          validation$warnings,
          prepared_score_settings()$warnings
        )),
        class = "issue-block issue-block-warning"
      )

      warning_block %||% shiny::div(class = "issue-block issue-block-ok", "Clinical scores are ready to plot.")
    })

    output$scores_plot <- shiny::renderPlot({
      shiny::req(!length(app_state$validation()$hard_errors))
      shiny::validate(
        shiny::need(
          !is.null(processed_scores()) && nrow(processed_scores()) > 0,
          "No clinical score data available.\nAdd a score column to your weights file and map it in the Import tab, or check that the detected score columns contain numeric values."
        )
      )

      plot_scores(
        scores_long = processed_scores(),
        group_meta = app_state$resolved_group_meta(),
        metadata = app_state$metadata(),
        settings = prepared_score_settings()$settings
      )
    }, res = 144)

    output$scores_stats_table <- DT::renderDT({
      shiny::req(!length(app_state$validation()$hard_errors))
      shiny::req(nrow(processed_scores()) > 0)
      stats_tbl <- score_key_stats(processed_scores(), app_state$resolved_group_meta())
      shiny::req(nrow(stats_tbl) > 0)
      DT::datatable(stats_tbl, rownames = FALSE, options = list(dom = "t", pageLength = 50, scrollX = TRUE))
    })

    output$scores_test_caption <- shiny::renderUI({
      shiny::req(!length(app_state$validation()$hard_errors))
      shiny::req(nrow(processed_scores()) > 0)
      tests <- score_pairwise_tests(processed_scores(), app_state$resolved_group_meta())
      if (is.na(tests$kw_p)) return(NULL)
      p <- tests$kw_p
      p_text <- if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
      final_day <- max(processed_scores()$day, na.rm = TRUE)
      shiny::p(
        shiny::strong(sprintf("Kruskal-Wallis at DPI %d: ", as.integer(final_day))),
        sprintf("p = %s", p_text),
        style = "margin-top: 0.6em; margin-bottom: 0;"
      )
    })

    output$scores_pairwise_header <- shiny::renderUI({
      shiny::req(!length(app_state$validation()$hard_errors))
      shiny::req(nrow(processed_scores()) > 0)
      method <- normalize_p_adjust_method(app_state$stats_settings()$p_adjust_method)
      tests <- score_pairwise_tests(
        processed_scores(),
        app_state$resolved_group_meta(),
        p_adjust_method = method
      )
      if (is.null(tests$pairwise)) return(NULL)
      shiny::p(
        shiny::strong(sprintf("Pairwise Wilcoxon at final DPI (%s-adjusted):", p_adjust_method_label(method))),
        style = "margin-top: 0.8em; margin-bottom: 0.2em;"
      )
    })

    output$scores_pairwise_table <- DT::renderDT({
      shiny::req(!length(app_state$validation()$hard_errors))
      shiny::req(nrow(processed_scores()) > 0)
      method <- normalize_p_adjust_method(app_state$stats_settings()$p_adjust_method)
      adjusted_col <- p_adjusted_column_name(method)
      tests <- score_pairwise_tests(
        processed_scores(),
        app_state$resolved_group_meta(),
        p_adjust_method = method
      )
      if (is.null(tests$pairwise)) {
        return(DT::datatable(
          data.frame(Note = "Pairwise comparison requires 3 or more groups with data."),
          rownames = FALSE,
          options = list(dom = "t", pageLength = 1)
        ))
      }
      DT::datatable(
        tests$pairwise,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 50, scrollX = TRUE)
      ) |>
        DT::formatSignif(columns = adjusted_col, digits = 3)
    })

    output$scores_summary <- DT::renderDT({
      shiny::req(!length(app_state$validation()$hard_errors))
      shiny::req(nrow(processed_scores()) > 0)

      summary_tbl <- summarise_scores_data(
        processed_scores(),
        error_style = prepared_score_settings()$settings$error_style
      )

      DT::datatable(summary_tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatRound(columns = c("mean", "sd", "sem", "ci95", "error_low", "error_high"), digits = 2)
    })

    output$download_png <- shiny::downloadHandler(
      filename = function() {
        score_plot_filename(
          app_state$metadata(),
          app_state$weights_name(),
          ext = "png"
        )
      },
      content = function(file) {
        shiny::req(!length(app_state$validation()$hard_errors))
        shiny::req(nrow(processed_scores()) > 0)
        plot_object <- plot_scores(
          scores_long = processed_scores(),
          group_meta = app_state$resolved_group_meta(),
          metadata = app_state$metadata(),
          settings = prepared_score_settings()$settings
        )
        save_plot_file(plot_object, file, dpi = 300)
      }
    )

    output$download_tiff <- shiny::downloadHandler(
      filename = function() {
        score_plot_filename(
          app_state$metadata(),
          app_state$weights_name(),
          ext = "tiff"
        )
      },
      content = function(file) {
        shiny::req(!length(app_state$validation()$hard_errors))
        shiny::req(nrow(processed_scores()) > 0)
        plot_object <- plot_scores(
          scores_long = processed_scores(),
          group_meta = app_state$resolved_group_meta(),
          metadata = app_state$metadata(),
          settings = prepared_score_settings()$settings
        )
        save_plot_file(plot_object, file, dpi = 300)
      }
    )

    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        score_plot_filename(
          app_state$metadata(),
          app_state$weights_name(),
          ext = "pdf"
        )
      },
      content = function(file) {
        shiny::req(!length(app_state$validation()$hard_errors))
        shiny::req(nrow(processed_scores()) > 0)
        plot_object <- plot_scores(
          scores_long = processed_scores(),
          group_meta = app_state$resolved_group_meta(),
          metadata = app_state$metadata(),
          settings = prepared_score_settings()$settings
        )
        save_plot_file(plot_object, file, dpi = 300)
      }
    )

    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        score_csv_filename(
          app_state$metadata(),
          app_state$weights_name()
        )
      },
      content = function(file) {
        shiny::req(!length(app_state$validation()$hard_errors))
        shiny::req(nrow(processed_scores()) > 0)
        csv_df <- graphpad_scores_csv(
          scores_long = processed_scores(),
          group_meta = app_state$resolved_group_meta()
        )
        utils::write.csv(csv_df, file, row.names = FALSE, na = "")
      }
    )
  })
}
