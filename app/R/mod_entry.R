mod_entry_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(2, 10),
    bslib::card(
        class = "control-card manual-entry-setup-card",
        bslib::card_header("Manual Data Entry"),
        bslib::card_body(
          class = "control-card-body",
          shiny::div(
            style = "font-weight: 700; color: #154c79; margin-bottom: 0.3em; font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.04em;",
            "Step 1: Study Setup"
          ),
        shiny::textInput(ns("study_name"), "Study Name"),
        shiny::numericInput(ns("n_groups"), "How many groups?", value = 3, min = 1, step = 1),
        shiny::numericInput(ns("mice_per_group"), "How many mice per group?", value = 4, min = 1, step = 1),
        shiny::checkboxInput(ns("infer_survival"), "Infer death events from missing future weights", value = TRUE),
        shiny::helpText("Animals with no weights recorded after a given day are assumed to have died on that day."),
        shiny::uiOutput(ns("group_name_inputs")),
        shiny::div(
          style = "font-weight: 700; color: #154c79; margin-top: 1em; margin-bottom: 0.3em; font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.04em;",
          "Step 2: Apply to Analysis"
        ),
        shiny::div(
          class = "button-row",
          shiny::actionButton(ns("reset_table"), "Reset Table"),
          shiny::actionButton(ns("apply_sheet"), "Use for analysis", class = "btn-primary"),
          shiny::actionButton(ns("clear_sheet"), "Clear table")
        ),
        shiny::uiOutput(ns("entry_source_status"))
      )
    ),
    shiny::tagList(
      bslib::accordion(
        id = ns("entry_help_accordion"),
        open = FALSE,
        bslib::accordion_panel(
          "How to use this tab",
          shiny::tags$ol(
            shiny::tags$li(
              shiny::tags$strong("Set up your study"),
              " in the left panel: choose the number of groups and animals per group, then name each group."
            ),
            shiny::tags$li(
              "Use ",
              shiny::tags$strong("Add Study Day"),
              " and ",
              shiny::tags$strong("Remove Last Day"),
              " to manage day columns (e.g. D0, D3, D7)."
            ),
            shiny::tags$li(
              "Each row is one animal. Edit ",
              shiny::tags$strong("Mouse ID"),
              " directly, then enter or paste ",
              shiny::tags$strong("weight in grams"),
              " across the day columns."
            ),
            shiny::tags$li(
              "Click ",
              shiny::tags$strong("\u201cUse for analysis\u201d"),
              " to send this table to the Weights, Survival, and Clinical Scores tabs."
            )
          )
        )
      ),
      bslib::card(
        class = "manual-entry-table-card",
        bslib::card_header(
          "Weight Entry Table",
          shiny::uiOutput(ns("table_summary_badge"), inline = TRUE)
        ),
        shiny::div(
          class = "button-row",
          shiny::actionButton(
            ns("add_day_row"),
            "Add Study Day",
            title = "Adds a new row for a checkpoint day (e.g. Day 8)"
          ),
          shiny::actionButton(
            ns("remove_day_row"),
            "Remove Last Day",
            title = "Removes the last study day row from the table"
          )
        ),
        shiny::helpText(
          "Tip: edit Mouse ID directly, then paste one mouse row across day columns (D0, D3, D7...)."
        ),
        shiny::uiOutput(ns("weights_sheet_ui"))
      ),
      shiny::uiOutput(ns("sheet_status"))
    )
  )
}

mod_entry_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    min_rows <- 6L
    initial_config <- shiny::isolate(app_state$direct_entry_settings())
    sheet_rv <- shiny::reactiveVal(
      sync_direct_entry_matrix(
        empty_direct_entry_matrix(initial_config, dpi_rows = initial_config$dpi_rows),
        initial_config,
        rows = max(min_rows, length(initial_config$dpi_rows))
      )
    )
    dirty_rv <- shiny::reactiveVal(FALSE)
    last_apply_error_rv <- shiny::reactiveVal(NULL)

    config_from_inputs <- function(base_config = app_state$direct_entry_settings()) {
      config <- direct_entry_resize_config(
        base_config,
        n_groups = input$n_groups %||% base_config$n_groups,
        mice_per_group = input$mice_per_group %||% base_config$mice_per_group
      )

      config$study_name <- trimws(as.character(input$study_name %||% config$study_name))

      group_names <- config$group_names
      for (i in seq_len(config$n_groups)) {
        group_names[[i]] <- as.character(input[[paste0("group_name_", i)]] %||% group_names[[i]])
      }
      config$group_names <- group_names

      normalize_direct_entry_settings(config)
    }

    shiny::observeEvent(app_state$direct_entry_settings(), {
      config <- app_state$direct_entry_settings()
      shiny::updateTextInput(session, "study_name", value = config$study_name %||% "")
      shiny::updateNumericInput(session, "n_groups", value = config$n_groups)
      shiny::updateNumericInput(session, "mice_per_group", value = config$mice_per_group)
      sheet_rv(sync_direct_entry_matrix(
        sheet_rv(),
        config = config,
        rows = max(min_rows, nrow(sheet_rv()))
      ))
    }, ignoreInit = FALSE)

    shiny::observeEvent(app_state$infer_survival(), {
      shiny::updateCheckboxInput(session, "infer_survival", value = app_state$infer_survival())
    }, ignoreInit = FALSE)

    output$group_name_inputs <- shiny::renderUI({
      config <- app_state$direct_entry_settings()
      shiny::tagList(lapply(seq_len(config$n_groups), function(i) {
        shiny::textInput(
          session$ns(paste0("group_name_", i)),
          sprintf("Group %d name", i),
          value = config$group_names[[i]]
        )
      }))
    })

    setup_inputs <- shiny::reactive({
      list(
        study_name = input$study_name,
        n_groups = input$n_groups,
        mice_per_group = input$mice_per_group
      )
    }) |>
      shiny::debounce(400)

    shiny::observeEvent(setup_inputs(), {
      current <- app_state$direct_entry_settings()
      updated <- config_from_inputs(current)
      if (!identical(updated, current)) {
        app_state$set_direct_entry_settings(updated)
        dirty_rv(TRUE)
      }
    }, ignoreInit = TRUE)

    # Debounce matches setup_inputs so both structural changes and name changes
    # settle at the same 400ms rhythm, preventing race conditions when the user
    # changes n_groups and a group name in rapid succession.
    group_name_values <- shiny::reactive({
      config <- app_state$direct_entry_settings()
      vapply(seq_len(config$n_groups), function(i) {
        as.character(input[[paste0("group_name_", i)]] %||% config$group_names[[i]])
      }, character(1))
    }) |>
      shiny::debounce(400)

    shiny::observeEvent(group_name_values(), {
      current <- app_state$direct_entry_settings()
      updated <- current
      updated$group_names <- as.list(group_name_values()) |>
        unlist(use.names = FALSE)
      updated <- normalize_direct_entry_settings(updated)
      if (!identical(updated, current)) {
        app_state$set_direct_entry_settings(updated)
        dirty_rv(TRUE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$reset_table, {
      config <- config_from_inputs()
      app_state$set_direct_entry_settings(config)
      sheet_rv(sync_direct_entry_matrix(
        empty_direct_entry_matrix(config, dpi_rows = config$dpi_rows),
        config = config,
        rows = max(min_rows, length(config$dpi_rows))
      ))
      dirty_rv(TRUE)
      shiny::showNotification("Table reset with the current setup.", type = "message")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$add_day_row, {
      config <- config_from_inputs()
      sheet <- sync_direct_entry_matrix(sheet_rv(), config = config, rows = max(min_rows, nrow(sheet_rv())))
      parsed_dpi <- direct_entry_parse_dpi_values(sheet$dpi)
      next_day <- if (any(!is.na(parsed_dpi))) max(parsed_dpi, na.rm = TRUE) + 1L else 0L
      blank_row <- tibble::as_tibble(stats::setNames(
        as.list(c(as.character(next_day), rep("", ncol(sheet) - 1L))),
        names(sheet)
      ))
      sheet_rv(dplyr::bind_rows(sheet, blank_row))
      dirty_rv(TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$remove_day_row, {
      sheet <- tibble::as_tibble(sheet_rv())
      if (nrow(sheet) <= 1L) {
        shiny::showNotification("At least one day row is required.", type = "warning")
        return(invisible(NULL))
      }
      sheet_rv(sheet[seq_len(nrow(sheet) - 1L), , drop = FALSE])
      dirty_rv(TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$clear_sheet, {
      config <- config_from_inputs()
      sheet_rv(sync_direct_entry_matrix(
        empty_direct_entry_matrix(config, dpi_rows = config$dpi_rows),
        config = config,
        rows = max(min_rows, length(config$dpi_rows))
      ))
      dirty_rv(TRUE)
    }, ignoreInit = TRUE)

    output$weights_sheet_ui <- shiny::renderUI({
      if (direct_entry_widget_available()) {
        return(rhandsontable::rHandsontableOutput(session$ns("weights_sheet"), width = "100%"))
      }

      shiny::tagList(
        DT::DTOutput(session$ns("weights_sheet_fallback"))
      )
    })

    current_ui_table <- function(config = app_state$direct_entry_settings(), sheet = sheet_rv()) {
      direct_entry_matrix_to_ui_table(sheet_df = sheet, config = config)
    }

    group_start_row_indexes <- function(config = app_state$direct_entry_settings()) {
      row_spec <- direct_entry_matrix_row_spec(config)
      which(row_spec$group_index > 1L & row_spec$mouse_index == 1L)
    }

    apply_ui_table_update <- function(ui_table,
                                      edited_row = NULL,
                                      edited_col = NULL,
                                      edited_value = NULL) {
      current <- app_state$direct_entry_settings()
      ui <- tibble::as_tibble(ui_table %||% tibble::tibble())
      if (!nrow(ui)) {
        return(invisible(NULL))
      }

      if (!is.null(edited_row) && !is.null(edited_col)) {
        row_id <- coerce_integer(edited_row)
        col_id <- match(as.character(edited_col), names(ui))
        if (is.na(row_id) || row_id < 1L || row_id > nrow(ui) || is.na(col_id)) {
          return(invisible(NULL))
        }

        if (!identical(names(ui)[[col_id]], "Group")) {
          expanded <- direct_entry_expand_pasted_ui_cells(
            ui_df = ui,
            row_index = row_id,
            col_name = names(ui)[[col_id]],
            value = edited_value
          )
          if (identical(expanded, ui)) {
            ui[[col_id]][[row_id]] <- as.character(edited_value %||% "")
          } else {
            ui <- expanded
          }
        }
      } else {
        ui <- direct_entry_expand_pasted_ui_cells(ui_df = ui)
      }

      updated_config <- direct_entry_mouse_labels_from_ui_table(ui, current)
      if (!identical(updated_config, current)) {
        app_state$set_direct_entry_settings(updated_config)
      }

      matrix <- direct_entry_ui_table_to_matrix(
        ui_df = ui,
        config = updated_config,
        base_sheet = sheet_rv()
      )
      sheet_rv(sync_direct_entry_matrix(
        matrix,
        config = updated_config,
        rows = max(min_rows, nrow(matrix))
      ))
      dirty_rv(TRUE)
      invisible(NULL)
    }

    if (direct_entry_widget_available()) {
      output$weights_sheet <- rhandsontable::renderRHandsontable({
        config <- app_state$direct_entry_settings()
        ui_table <- current_ui_table(config = config)

        rh <- rhandsontable::rhandsontable(
          ui_table,
          rowHeaders = NULL,
          colHeaders = names(ui_table),
          stretchH = "all",
          manualColumnResize = TRUE,
          contextMenu = TRUE,
          minSpareRows = 0
        )

        rh <- rhandsontable::hot_col(
          rh,
          col = "Group",
          type = "text",
          readOnly = TRUE,
          className = "entry-group-name-col",
          width = 180
        )
        rh <- rhandsontable::hot_col(rh, col = "Mouse ID", type = "text", width = 140)
        day_cols <- names(ui_table)[grepl("^D\\d+$", names(ui_table), perl = TRUE)]
        for (day_col in day_cols) {
          rh <- rhandsontable::hot_col(rh, col = day_col, type = "numeric", width = 88)
        }

        group_break_rows <- group_start_row_indexes(config = config)
        group_break_rows <- group_break_rows[group_break_rows > 1L]
        if (length(group_break_rows) && ncol(ui_table) > 0L) {
          border_spec <- lapply(group_break_rows, function(row_id) {
            list(
              range = list(
                from = list(row = as.integer(row_id - 1L), col = 0L),
                to = list(row = as.integer(row_id - 1L), col = as.integer(ncol(ui_table) - 1L))
              ),
              top = list(width = 2, color = "#9bb3c7")
            )
          })
          rh <- tryCatch(
            rhandsontable::hot_table(rh, customBorders = border_spec),
            error = function(e) rh
          )
        }

        rh
      })

      shiny::observeEvent(input$weights_sheet, {
        updated <- rhandsontable::hot_to_r(input$weights_sheet)
        if (is.null(updated)) {
          return(invisible(NULL))
        }
        apply_ui_table_update(updated)
      }, ignoreInit = TRUE)
    }

    output$weights_sheet_fallback <- DT::renderDT({
      config <- app_state$direct_entry_settings()
      ui_table <- current_ui_table(config = config)
      start_rows <- as.integer(group_start_row_indexes(config = config) - 1L)
      starts_json <- jsonlite::toJSON(unname(start_rows), auto_unbox = TRUE)

      DT::datatable(
        ui_table,
        class = "compact cell-border stripe manual-entry-dt",
        rownames = FALSE,
        editable = list(
          target = "cell",
          disable = list(columns = c(0))
        ),
        selection = "none",
        options = list(
          dom = "t",
          pageLength = max(min_rows, nrow(ui_table)),
          ordering = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE,
          createdRow = DT::JS(
            sprintf(
              "function(row, data, dataIndex) {
                 var starts = %s;
                 if (starts.indexOf(dataIndex) !== -1) {
                   $(row).addClass('entry-group-row-start');
                 }
               }",
              starts_json
            )
          ),
          columnDefs = list(
            list(
              targets = 0,
              className = "entry-group-name-col",
              width = "180px"
            ),
            list(
              targets = 1,
              width = "140px"
            ),
            list(
              targets = "_all",
              width = "88px"
            )
          )
        )
      )
    }, server = FALSE)

    shiny::observeEvent(input$weights_sheet_fallback_cell_edit, {
      info <- input$weights_sheet_fallback_cell_edit
      config <- app_state$direct_entry_settings()
      ui <- current_ui_table(config = config)
      if (!nrow(ui)) {
        return(invisible(NULL))
      }
      row_id <- coerce_integer(info$row)
      col_id <- coerce_integer(info$col) + 1L
      if (is.na(row_id) || row_id < 1L || row_id > nrow(ui) ||
          is.na(col_id) || col_id < 1L || col_id > ncol(ui)) {
        return(invisible(NULL))
      }

      col_name <- names(ui)[[col_id]]
      if (identical(col_name, "Group")) {
        return(invisible(NULL))
      }

      apply_ui_table_update(
        ui_table = ui,
        edited_row = row_id,
        edited_col = col_name,
        edited_value = info$value
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$apply_sheet, {
      config <- config_from_inputs()
      app_state$set_direct_entry_settings(config)

      imported <- tryCatch(
        direct_entry_matrix_to_import(
          sheet_df = sheet_rv(),
          config = config,
          source_name = "Manual Data Entry"
        ),
        error = function(e) {
          last_apply_error_rv(conditionMessage(e))
          shiny::showNotification(conditionMessage(e), type = "warning")
          NULL
        }
      )

      if (is.null(imported)) {
        return(invisible(NULL))
      }
      if (!nrow(imported$data)) {
        msg <- "The table is empty. Enter at least one study day with weights first."
        last_apply_error_rv(msg)
        shiny::showNotification(msg, type = "warning")
        return(invisible(NULL))
      }
      last_apply_error_rv(NULL)

      app_state$set_infer_survival(input$infer_survival %||% TRUE)
      app_state$clear_survival_source()
      app_state$set_weights_source(
        imported = imported,
        mapping = direct_entry_weights_mapping(),
        day_map = default_day_mapping(imported),
        score_day_map = default_score_day_mapping(imported),
        source_name = "Manual Data Entry",
        apply_recent_preset = FALSE,
        source_kind = "entry"
      )

      if (nzchar(config$study_name %||% "")) {
        metadata <- app_state$metadata()
        metadata$study_title <- config$study_name
        if (!nzchar(metadata$study_id %||% "")) {
          metadata$study_id <- config$study_name
        }
        app_state$set_metadata(metadata)
      }

      dirty_rv(FALSE)
      shiny::showNotification("Manual table is now the active analysis source.", type = "message")
    }, ignoreInit = TRUE)

    output$entry_source_status <- shiny::renderUI({
      current_source <- app_state$current_source()$weights
      lines <- list(
        shiny::tags$p(
          shiny::tags$strong("Current source: "),
          current_source$label %||% "None loaded"
        )
      )

      err <- last_apply_error_rv()
      if (!is.null(err)) {
        lines <- c(lines, list(
          shiny::div(class = "issue-block issue-block-error", err)
        ))
      } else if (isTRUE(dirty_rv())) {
        lines <- c(lines, list(
          shiny::div(class = "issue-block issue-block-warning", "Unapplied changes are waiting in this table.")
        ))
      } else if (identical(current_source$kind, "entry")) {
        lines <- c(lines, list(
          shiny::div(class = "issue-block issue-block-ok", "This table is currently driving the analysis.")
        ))
      }

      shiny::tagList(lines)
    })

    output$table_summary_badge <- shiny::renderUI({
      config <- app_state$direct_entry_settings()
      sheet <- sheet_rv()
      n_days <- sum(!is.na(direct_entry_parse_dpi_values(sheet$dpi)))
      shiny::span(
        style = "font-size: 0.8em; color: #6c757d; font-weight: normal; margin-left: 0.75em;",
        sprintf(
          "%d group%s \u00b7 %d animal%s/group \u00b7 %d day%s",
          config$n_groups,
          if (config$n_groups == 1L) "" else "s",
          config$mice_per_group,
          if (config$mice_per_group == 1L) "" else "s",
          n_days,
          if (n_days == 1L) "" else "s"
        )
      )
    })

    output$sheet_status <- shiny::renderUI({
      current_source <- app_state$current_source()$weights
      if (!identical(current_source$kind, "entry")) {
        return(shiny::div(
          class = "issue-block issue-block-warning",
          "Click 'Use for analysis' to use this table."
        ))
      }

      if (isTRUE(dirty_rv())) {
        return(shiny::div(
          class = "issue-block issue-block-warning",
          "This table has unapplied changes. Plots still use the last applied table."
        ))
      }

      issues <- app_state$validation()
      blocks <- list(
        format_issue_list("Hard errors", issues$hard_errors, class = "issue-block issue-block-error"),
        format_issue_list("Survival errors", issues$survival_hard_errors, class = "issue-block issue-block-error"),
        format_issue_list("Warnings", issues$warnings, class = "issue-block issue-block-warning")
      )
      blocks <- Filter(Negate(is.null), blocks)

      if (!length(blocks)) {
        return(shiny::div(class = "issue-block issue-block-ok", "Table data are ready to plot."))
      }

      shiny::tagList(blocks)
    })
  })
}
