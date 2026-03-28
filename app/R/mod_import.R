mod_import_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      class = "full-height-card",
      bslib::card_header("Files"),
      shiny::fileInput(ns("weights_file"), "Study Data File (CSV or Excel)",
                       accept = c(".csv", ".xlsx", ".xls")),
      shiny::checkboxInput(ns("infer_survival"), "Infer death events from missing future weights", value = TRUE),
      shiny::helpText(
        "When checked, animals missing from later weigh-ins are treated as deaths on their last recorded day",
        "unless their cage is marked as scheduled sampling."
      ),
      shiny::checkboxInput(ns("detect_scheduled_sampling"), "Auto-detect scheduled sampling cages", value = TRUE),
      shiny::helpText(
        "Conservative rule: cages are suggested as scheduled sampling only when an entire cage drops out on the same DPI."
      ),
      shiny::actionButton(ns("load_example"), "Load example data", class = "btn-primary"),
      shiny::hr(),
      shiny::uiOutput(ns("loaded_files")),
      shiny::uiOutput(ns("import_restore_ui"))
    ),
    shiny::tagList(
      bslib::card(
        bslib::card_header("Set Up Study Data"),
        bslib::accordion(
          id = ns("mapping_accordion"),
          open = "Start Here",
          bslib::accordion_panel(
            "Start Here",
            shiny::helpText("Set the study title and rename groups so plots and summaries read clearly."),
            shiny::textInput(ns("study_title"), "Study name"),
            shiny::tags$hr(),
            shiny::tags$h6("Group names"),
            shiny::helpText("Click Display Name cells to rename groups."),
            DT::DTOutput(ns("group_editor"))
          ),
          bslib::accordion_panel(
            "Match Study Data Columns",
            shiny::helpText("Choose which uploaded columns contain each required study-data field."),
            shiny::uiOutput(ns("weights_mapping_ui")),
            shiny::tags$hr(),
            shiny::tags$h6("Study days (DPI)"),
            DT::DTOutput(ns("day_mapping_table"))
          ),
          bslib::accordion_panel(
            "Review Scheduled Sampling",
            shiny::helpText("Review or correct cage-level scheduled sampling assignments for imported files."),
            shiny::tags$hr(),
            DT::DTOutput(ns("scheduled_sampling_table"))
          ),
          bslib::accordion_panel(
            "More Study Details",
            shiny::textInput(ns("study_id"), "Study ID"),
            shiny::textInput(ns("species"), "Species"),
            shiny::dateInput(ns("challenge_date"), "Challenge date"),
            shiny::textInput(ns("weight_units"), "Weight units", value = "g"),
            shiny::textInput(ns("subtitle"), "Subtitle")
          )
        )
      ),
      shiny::uiOutput(ns("validation_banner")),
      bslib::accordion(
        id = ns("preview_accordion"),
        open = FALSE,
        bslib::accordion_panel(
          "Data Preview",
          DT::DTOutput(ns("unified_preview"))
        )
      )
    )
  )
}

mod_import_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    initial_state <- load_app_state()

    weights_imported <- shiny::reactiveVal(NULL)
    weights_mapping_rv <- shiny::reactiveVal(guess_field_mapping(NULL, weights_field_spec()))
    day_mapping_rv <- shiny::reactiveVal(default_day_mapping(NULL))
    score_day_mapping_rv <- shiny::reactiveVal(default_score_day_mapping(NULL))
    metadata_rv <- shiny::reactiveVal(initial_state$metadata %||% list())
    infer_death_events_rv <- shiny::reactiveVal(normalize_infer_death_events(initial_state$infer_death_events %||% initial_state$infer_survival))
    detect_scheduled_sampling_rv <- shiny::reactiveVal(normalize_detect_scheduled_sampling(initial_state$detect_scheduled_sampling))
    scheduled_sampling_cages_rv <- shiny::reactiveVal(normalize_scheduled_sampling_cages(initial_state$scheduled_sampling_cages))
    direct_entry_settings_rv <- shiny::reactiveVal(normalize_direct_entry_settings(initial_state$direct_entry))
    group_overrides_rv <- shiny::reactiveVal(normalize_group_overrides(initial_state$group_overrides))
    shared_style_rv <- shiny::reactiveVal(normalize_shared_style_settings(initial_state$shared_style))
    stats_settings_rv <- shiny::reactiveVal(normalize_stats_settings(initial_state$stats))
    survival_analysis_settings_rv <- shiny::reactiveVal(normalize_survival_analysis_settings(initial_state$survival_analysis))
    weight_plot_settings_rv <- shiny::reactiveVal(normalize_weight_plot_settings(initial_state$weight_plot))
    score_plot_settings_rv <- shiny::reactiveVal(normalize_score_plot_settings(initial_state$score_plot))
    survival_plot_settings_rv <- shiny::reactiveVal(normalize_survival_plot_settings(initial_state$survival_plot))
    weights_source_rv <- shiny::reactiveVal(list(kind = "none", label = "None loaded"))
    cached_import_weights_rv <- shiny::reactiveVal(NULL)
    has_attempted_load_rv <- shiny::reactiveVal(FALSE)

    set_metadata <- function(value) {
      normalized <- value %||% list()
      if (!identical(normalized, metadata_rv())) {
        metadata_rv(normalized)
      }
      invisible(normalized)
    }

    set_infer_death_events <- function(value) {
      normalized <- normalize_infer_death_events(value)
      if (!identical(normalized, infer_death_events_rv())) {
        infer_death_events_rv(normalized)
      }
      invisible(normalized)
    }

    set_detect_scheduled_sampling <- function(value) {
      normalized <- normalize_detect_scheduled_sampling(value)
      if (!identical(normalized, detect_scheduled_sampling_rv())) {
        detect_scheduled_sampling_rv(normalized)
      }
      invisible(normalized)
    }

    set_scheduled_sampling_cages <- function(value) {
      normalized <- normalize_scheduled_sampling_cages(value)
      if (!identical(normalized, scheduled_sampling_cages_rv())) {
        scheduled_sampling_cages_rv(normalized)
      }
      invisible(normalized)
    }

    set_direct_entry_settings <- function(value) {
      normalized <- normalize_direct_entry_settings(value)
      if (!identical(normalized, direct_entry_settings_rv())) {
        direct_entry_settings_rv(normalized)
      }
      invisible(normalized)
    }

    set_group_overrides <- function(value) {
      normalized <- normalize_group_overrides(value)
      if (!identical(normalized, group_overrides_rv())) {
        group_overrides_rv(normalized)
      }
      invisible(normalized)
    }

    set_shared_style <- function(value) {
      normalized <- normalize_shared_style_settings(value)
      if (!identical(normalized, shared_style_rv())) {
        shared_style_rv(normalized)
      }
      invisible(normalized)
    }

    set_stats_settings <- function(value) {
      normalized <- normalize_stats_settings(value)
      if (!identical(normalized, stats_settings_rv())) {
        stats_settings_rv(normalized)
      }
      invisible(normalized)
    }

    set_survival_analysis_settings <- function(value) {
      normalized <- normalize_survival_analysis_settings(value)
      if (!identical(normalized, survival_analysis_settings_rv())) {
        survival_analysis_settings_rv(normalized)
      }
      invisible(normalized)
    }

    set_weight_plot_settings <- function(value) {
      normalized <- normalize_weight_plot_settings(value)
      if (!identical(normalized, weight_plot_settings_rv())) {
        weight_plot_settings_rv(normalized)
      }
      invisible(normalized)
    }

    set_score_plot_settings <- function(value) {
      normalized <- normalize_score_plot_settings(value)
      if (!identical(normalized, score_plot_settings_rv())) {
        score_plot_settings_rv(normalized)
      }
      invisible(normalized)
    }

    set_survival_plot_settings <- function(value) {
      normalized <- normalize_survival_plot_settings(value)
      if (!identical(normalized, survival_plot_settings_rv())) {
        survival_plot_settings_rv(normalized)
      }
      invisible(normalized)
    }

    apply_metadata_to_inputs <- function(metadata_values = list()) {
      shiny::updateTextInput(session, "study_title", value = metadata_values$study_title %||% "")
      shiny::updateTextInput(session, "study_id", value = metadata_values$study_id %||% "")
      shiny::updateTextInput(session, "species", value = metadata_values$species %||% "")
      shiny::updateTextInput(session, "weight_units", value = metadata_values$weight_units %||% "g")
      shiny::updateTextInput(session, "subtitle", value = metadata_values$subtitle %||% "")
      shiny::updateDateInput(
        session,
        "challenge_date",
        value = if (nzchar(metadata_values$challenge_date %||% "")) {
          tryCatch(as.Date(metadata_values$challenge_date), warning = function(w) NULL, error = function(e) NULL)
        } else NULL
      )
    }

    validation <- shiny::reactive({
      is_import_source <- identical(weights_source_rv()$kind, "import")
      validate_study_data(
        raw_weights = weights_imported(),
        raw_survival = NULL,
        mapping = build_mapping_bundle(
          weights          = weights_mapping(),
          day_map          = day_mapping_rv(),
          score_day_map    = score_day_mapping_rv()
        ),
        group_overrides = group_overrides_rv(),
        infer_death_events = infer_death_events_rv(),
        detect_scheduled_sampling = is_import_source && detect_scheduled_sampling_rv(),
        scheduled_sampling_cages = if (is_import_source) scheduled_sampling_cages_rv() else empty_scheduled_sampling_cages()
      )
    })

    resolved_group_meta <- shiny::reactive({
      resolve_group_colors(validation()$group_meta, shared_style_rv())
    })

    current_group_override_table <- function(group_meta = validation()$group_meta) {
      if (is.null(group_meta) || !nrow(group_meta)) {
        return(empty_group_overrides())
      }

      group_meta |>
        dplyr::select("group_id", "display_name", "plot_order", "custom_color")
    }

    invalid_custom_color_warnings <- shiny::reactive({
      gm <- validation()$group_meta
      if (!nrow(gm) || !"custom_color" %in% names(gm)) return(character())
      invalid_groups <- gm |>
        dplyr::filter(!is.na(.data$custom_color), nzchar(.data$custom_color), is.na(normalize_hex_color(.data$custom_color)))

      if (!nrow(invalid_groups) || !identical(shared_style_rv()$color_mode, "custom")) {
        return(character())
      }

      sprintf(
        "Invalid custom colors were ignored for: %s.",
        paste(invalid_groups$display_name, collapse = ", ")
      )
    })

    set_weights_source <- function(imported,
                                   mapping = NULL,
                                   day_map = NULL,
                                   score_day_map = NULL,
                                   source_name = imported$source_name %||% "study-data.csv",
                                   apply_recent_preset = FALSE,
                                   source_kind = "import") {
      has_attempted_load_rv(TRUE)
      if (!is.null(imported)) {
        imported$source_name <- source_name
      }
      weights_imported(imported)
      weights_mapping_rv(mapping %||% guess_field_mapping(imported, weights_field_spec()))
      day_mapping_rv(day_map %||% default_day_mapping(imported))
      score_day_mapping_rv(score_day_map %||% default_score_day_mapping(imported))
      weights_source_rv(list(kind = source_kind, label = source_name %||% "Study data"))
    }

    set_survival_source <- function(imported,
                                    mapping = NULL,
                                    survival_day_map = NULL,
                                    source_name = "unused",
                                    apply_recent_preset = FALSE,
                                    source_kind = "import") {
      invisible(NULL)
    }

    clear_survival_source <- function() {
      invisible(NULL)
    }

    restore_cached_import_sources <- function() {
      cached_weights <- cached_import_weights_rv()

      if (is.null(cached_weights)) {
        return(invisible(FALSE))
      }

      has_attempted_load_rv(TRUE)
      weights_imported(cached_weights$imported)
      weights_mapping_rv(cached_weights$mapping)
      day_mapping_rv(cached_weights$day_map)
      score_day_mapping_rv(cached_weights$score_day_map %||% default_score_day_mapping(cached_weights$imported))
      weights_source_rv(list(
        kind = "import",
        label = cached_weights$label %||% cached_weights$imported$source_name %||% "study-data.csv"
      ))

      invisible(TRUE)
    }

    load_weights_import <- function(imported) {
      set_weights_source(
        imported = imported,
        source_name = imported$source_name %||% "study-data.csv",
        apply_recent_preset = TRUE,
        source_kind = "import"
      )
    }

    is_excel_file <- function(name) {
      grepl("\\.(xlsx|xls)$", name, ignore.case = TRUE)
    }

    shiny::observeEvent(input$weights_file, {
      req <- input$weights_file
      if (is.null(req)) {
        return(invisible(NULL))
      }

      imported <- if (is_excel_file(req$name)) {
        read_weights_excel_import(req$datapath, req$name)
      } else {
        read_weights_import(req$datapath, req$name)
      }
      load_weights_import(imported)
    })

    shiny::observeEvent(input$load_example, {
      tryCatch({
        example_weights <- load_example_import("weights")
        load_weights_import(example_weights)
        shiny::showNotification("Loaded packaged example data.", type = "message")
      }, error = function(e) {
        shiny::showNotification(
          paste("Failed to load packaged example data:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
      })
    })

    shiny::observe({
      if (!identical(weights_source_rv()$kind, "import") || is.null(weights_imported())) {
        return(invisible(NULL))
      }

      cached_import_weights_rv(list(
        imported = weights_imported(),
        mapping = weights_mapping_rv(),
        day_map = day_mapping_rv(),
        score_day_map = score_day_mapping_rv(),
        label = weights_source_rv()$label
      ))
    })

    shiny::observeEvent(metadata_rv(), {
      apply_metadata_to_inputs(metadata_rv())
    }, ignoreInit = FALSE)

    shiny::observeEvent(infer_death_events_rv(), {
      shiny::updateCheckboxInput(session, "infer_survival", value = infer_death_events_rv())
    }, ignoreInit = FALSE)

    shiny::observeEvent(detect_scheduled_sampling_rv(), {
      shiny::updateCheckboxInput(session, "detect_scheduled_sampling", value = detect_scheduled_sampling_rv())
    }, ignoreInit = FALSE)

    shiny::observeEvent(
      list(input$study_title, input$study_id, input$species, input$challenge_date, input$weight_units, input$subtitle),
      {
        set_metadata(list(
          study_title = input$study_title %||% "",
          study_id = input$study_id %||% "",
          species = input$species %||% "",
          challenge_date = as.character(input$challenge_date %||% ""),
          weight_units = input$weight_units %||% "g",
          subtitle = input$subtitle %||% ""
        ))
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$infer_survival, {
      set_infer_death_events(input$infer_survival)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$detect_scheduled_sampling, {
      set_detect_scheduled_sampling(input$detect_scheduled_sampling)
    }, ignoreInit = TRUE)

    shiny::observeEvent(shared_style_rv(), {
      shiny::updateSelectInput(session, "color_mode", selected = shared_style_rv()$color_mode)
      shiny::updateSelectInput(session, "palette_name", selected = shared_style_rv()$palette_name)
    }, ignoreInit = FALSE)

    shiny::observeEvent(list(input$color_mode, input$palette_name), {
      set_shared_style(list(color_mode = input$color_mode, palette_name = input$palette_name))
    }, ignoreInit = TRUE)

    for (field in weights_field_spec()$field) {
      local({
        field_name <- field
        shiny::observeEvent(input[[paste0("weights_map_", field_name)]], {
          current <- weights_mapping_rv()
          current[[field_name]] <- input[[paste0("weights_map_", field_name)]]
          weights_mapping_rv(current)
        }, ignoreInit = TRUE)
      })
    }

    weights_mapping <- shiny::reactive(weights_mapping_rv())
    metadata <- shiny::reactive(metadata_rv())

    custom_color_inputs <- shiny::reactive({
      group_meta <- resolved_group_meta()
      if (!nrow(group_meta) || !identical(shared_style_rv()$color_mode, "custom")) {
        return(list())
      }

      input_ids <- paste0("group_color_", group_meta$group_id)
      values <- lapply(input_ids, function(id) input[[id]])
      stats::setNames(values, group_meta$group_id)
    })

    shiny::observeEvent(custom_color_inputs(), {
      group_meta <- validation()$group_meta
      if (!nrow(group_meta) || !identical(shared_style_rv()$color_mode, "custom")) {
        return(invisible(NULL))
      }

      overrides <- current_group_override_table(group_meta)
      changed <- FALSE
      for (group_id in names(custom_color_inputs())) {
        input_value <- custom_color_inputs()[[group_id]]
        if (is.null(input_value) || !nzchar(input_value)) {
          next
        }

        row_id <- which(overrides$group_id == group_id)
        if (!length(row_id)) {
          next
        }

        if (!identical(overrides$custom_color[row_id], input_value)) {
          overrides$custom_color[row_id] <- input_value
          changed <- TRUE
        }
      }

      if (changed) {
        set_group_overrides(overrides)
      }
    }, ignoreInit = TRUE)

    state_snapshot <- shiny::reactive({
      list(
        metadata = metadata_rv(),
        infer_death_events = infer_death_events_rv(),
        detect_scheduled_sampling = detect_scheduled_sampling_rv(),
        scheduled_sampling_cages = scheduled_sampling_cages_rv(),
        direct_entry = direct_entry_settings_rv(),
        weights_name = weights_imported()$source_name %||% "",
        survival_name = weights_imported()$source_name %||% "",
        group_overrides = group_overrides_rv(),
        shared_style = shared_style_rv(),
        stats = stats_settings_rv(),
        survival_analysis = survival_analysis_settings_rv(),
        weight_plot = weight_plot_settings_rv(),
        score_plot = score_plot_settings_rv(),
        survival_plot = survival_plot_settings_rv()
      )
    })

    state_snapshot_debounced <- shiny::debounce(state_snapshot, 2000)

    shiny::observeEvent(state_snapshot_debounced(), {
      save_app_state(state_snapshot_debounced())
    }, ignoreInit = TRUE)

    output$weights_mapping_ui <- shiny::renderUI({
      if (is.null(weights_imported())) {
        return(shiny::helpText("Upload a study data file (or load example data), then match each field below."))
      }

      choices <- c("None" = "", label_choices(weights_imported()))
      current <- weights_mapping()

      shiny::tagList(lapply(seq_len(nrow(weights_field_spec())), function(i) {
        field <- weights_field_spec()$field[[i]]
        label <- weights_field_spec()$label[[i]]
        required <- weights_field_spec()$required[[i]]
        shiny::selectInput(
          session$ns(paste0("weights_map_", field)),
          label = if (required) paste0(label, " *") else label,
          choices = choices,
          selected = current[[field]] %||% ""
        )
      }))
    })

    output$day_mapping_table <- DT::renderDT({
      day_map <- day_mapping_rv()
      DT::datatable(
        day_map,
        rownames = FALSE,
        editable = TRUE,
        selection = "none",
        options = list(
          dom = "t",
          pageLength = max(10, nrow(day_map)),
          scrollX = TRUE
        )
      )
    }, server = FALSE)

    shiny::observeEvent(input$day_mapping_table_cell_edit, {
      info <- input$day_mapping_table_cell_edit
      day_map <- day_mapping_rv()
      if (!nrow(day_map)) {
        return(invisible(NULL))
      }

      col_name <- names(day_map)[info$col + 1]
      row_id <- info$row

      if (!col_name %in% c("day", "include")) {
        return(invisible(NULL))
      }

      if (identical(col_name, "day")) {
        new_day <- coerce_integer(info$value)
        if (is.na(new_day)) {
          shiny::showNotification("Day must be a whole number (e.g. 0, 1, 7).", type = "warning")
          return(invisible(NULL))
        }
        day_map[row_id, col_name] <- new_day
      } else {
        day_map[row_id, col_name] <- tolower(trimws(as.character(info$value))) %in% c("true", "t", "1", "yes", "y")
      }

      day_mapping_rv(day_map)
    })

    output$scheduled_sampling_table <- DT::renderDT({
      review <- validation()$scheduled_sampling_review
      is_import_source <- identical(weights_source_rv()$kind, "import")
      if (!is_import_source || !nrow(review)) {
        review <- tibble::tibble(
          Group = character(),
          `Cage Card` = character(),
          `N mice` = integer(),
          `Observed last-day pattern` = character(),
          `Suggested role` = character(),
          Role = character(),
          `Censor DPI` = integer()
        )
      } else {
        review <- review |>
          dplyr::transmute(
            Group = .data$group_label,
            `Cage Card` = .data$cage_card,
            `N mice` = .data$n_mice,
            `Observed last-day pattern` = .data$observed_last_day_pattern,
            `Suggested role` = scheduled_sampling_role_label(.data$suggested_role),
            Role = scheduled_sampling_role_label(.data$role),
            `Censor DPI` = .data$censor_day
          )
      }

      DT::datatable(
        review,
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4))),
        selection = "none",
        options = list(
          dom = "t",
          pageLength = max(6, nrow(review)),
          scrollX = TRUE
        )
      )
    }, server = FALSE)

    shiny::observeEvent(input$scheduled_sampling_table_cell_edit, {
      info <- input$scheduled_sampling_table_cell_edit
      review <- validation()$scheduled_sampling_review
      if (!identical(weights_source_rv()$kind, "import") || !nrow(review)) {
        return(invisible(NULL))
      }

      editable_cols <- c("group_label", "cage_card", "n_mice", "observed_last_day_pattern", "suggested_role", "role", "censor_day")
      col_name <- editable_cols[[info$col + 1]]
      row_id <- info$row

      if (!col_name %in% c("role", "censor_day")) {
        return(invisible(NULL))
      }

      cage_card <- review$cage_card[[row_id]]
      overrides <- scheduled_sampling_cages_rv()
      existing_row <- which(overrides$cage_card == cage_card)
      base_role <- review$role[[row_id]] %||% "survival"
      base_censor_day <- review$censor_day[[row_id]]

      if (!length(existing_row)) {
        overrides <- dplyr::bind_rows(
          overrides,
          tibble::tibble(cage_card = cage_card, role = base_role, censor_day = base_censor_day)
        )
        existing_row <- nrow(overrides)
      }

      if (identical(col_name, "role")) {
        normalized_role <- scheduled_sampling_role_value(
          info$value,
          default = NA_character_,
          strict = TRUE
        )
        if (is.na(normalized_role)) {
          shiny::showNotification(
            "Role must be either Survival or Scheduled sampling.",
            type = "warning"
          )
          return(invisible(NULL))
        }
        overrides$role[[existing_row]] <- normalized_role
        if (identical(normalized_role, "survival")) {
          overrides$censor_day[[existing_row]] <- NA_integer_
        } else if (is.na(overrides$censor_day[[existing_row]])) {
          overrides$censor_day[[existing_row]] <- base_censor_day
        }
      } else {
        new_day <- coerce_integer(info$value)
        if (is.na(new_day)) {
          shiny::showNotification("Censor DPI must be a whole number.", type = "warning")
          return(invisible(NULL))
        }
        overrides$censor_day[[existing_row]] <- new_day
        overrides$role[[existing_row]] <- "scheduled_sampling"
      }

      set_scheduled_sampling_cages(overrides)
    })

    output$group_editor <- DT::renderDT({
      group_id_for_display <- function(x) {
        x <- as.character(x %||% "")
        ifelse(grepl("^x[0-9]+$", x), sub("^x", "", x), x)
      }

      group_meta <- validation()$group_meta
      if (!nrow(group_meta) || !"group_id" %in% names(group_meta)) {
        return(DT::datatable(
          tibble::tibble(
            `Current Group Name` = character(),
            `Display Name` = character(),
            `Plot Order` = integer()
          ),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }

      group_meta <- group_meta |>
        dplyr::select("group_id", "display_name", "plot_order") |>
        dplyr::mutate(
          group_name_display = group_id_for_display(.data$group_id),
          display_name_input = as.character(.data$display_name)
        ) |>
        dplyr::transmute(
          `Current Group Name` = .data$group_name_display,
          `Display Name` = .data$display_name_input,
          `Plot Order` = .data$plot_order
        )

      DT::datatable(
        group_meta,
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = c(0))),
        selection = "none",
        options = list(
          dom = "t",
          scrollX = TRUE
        )
      )
    }, server = FALSE)

    shiny::observeEvent(input$group_editor_cell_edit, {
      info <- input$group_editor_cell_edit
      group_meta <- current_group_override_table()
      if (!nrow(group_meta)) {
        return(invisible(NULL))
      }

      editable_cols <- c("group_id", "display_name", "plot_order")
      col_name <- editable_cols[[info$col + 1]]
      if (!col_name %in% c("display_name", "plot_order")) {
        return(invisible(NULL))
      }

      if (identical(col_name, "plot_order")) {
        new_order <- coerce_integer(info$value)
        if (is.na(new_order)) {
          shiny::showNotification("Plot order must be a whole number.", type = "warning")
          return(invisible(NULL))
        }
        group_meta[info$row, col_name] <- new_order
      } else {
        group_meta[info$row, col_name] <- as.character(info$value)
      }

      set_group_overrides(group_meta)
    })

    output$shared_style_warning <- shiny::renderUI({
      warnings <- invalid_custom_color_warnings()
      format_issue_list(
        "Style warnings",
        warnings,
        class = "issue-block issue-block-warning"
      )
    })

    output$group_color_pickers <- shiny::renderUI({
      group_meta <- resolved_group_meta()

      if (!nrow(group_meta)) {
        return(shiny::helpText("Load data to edit shared group colors."))
      }

      if (!identical(shared_style_rv()$color_mode, "custom")) {
        return(shiny::helpText("Switch to Custom per group to edit group colors."))
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

    output$validation_report <- shiny::renderUI({
      issues <- validation()
      blocks <- list(
        format_issue_list("Hard errors", issues$hard_errors, class = "issue-block issue-block-error"),
        format_issue_list("Survival-only hard errors", issues$survival_hard_errors, class = "issue-block issue-block-error"),
        format_issue_list("Warnings", issues$warnings, class = "issue-block issue-block-warning")
      )

      if (!length(compact_chr(unlist(c(issues$hard_errors, issues$survival_hard_errors, issues$warnings))))) {
        return(shiny::div(class = "issue-block issue-block-ok", shiny::tags$strong("No issues detected.")))
      }

      shiny::tagList(blocks)
    })

    output$weights_preview <- DT::renderDT({
      DT::datatable(
        preview_import_data(weights_imported()),
        rownames = FALSE,
        options = list(pageLength = 6, scrollX = TRUE)
      )
    })

    output$validation_banner <- shiny::renderUI({
      issues <- validation()
      if (!should_render_validation_banner(has_attempted_load_rv(), issues)) {
        return(NULL)
      }

      blocks <- list(
        format_issue_list("Hard errors", issues$hard_errors, class = "issue-block issue-block-error"),
        format_issue_list("Survival errors", issues$survival_hard_errors, class = "issue-block issue-block-error"),
        format_issue_list("Warnings", issues$warnings, class = "issue-block issue-block-warning")
      )
      shiny::div(class = "validation-banner", shiny::tagList(blocks))
    })

    output$unified_preview <- DT::renderDT({
      data <- preview_import_data(weights_imported())
      DT::datatable(data, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })

    output$loaded_files <- shiny::renderUI({
      shiny::tags$p(shiny::tags$strong("Study data:"), weights_source_rv()$label %||% "None loaded")
    })

    output$import_restore_ui <- shiny::renderUI({
      cached_weights <- cached_import_weights_rv()
      needs_restore <- !is.null(cached_weights) && !identical(weights_source_rv()$kind, "import")

      if (!needs_restore) {
        return(NULL)
      }

      shiny::actionButton(session$ns("use_imported_files"), "Use imported files")
    })

    shiny::observeEvent(input$use_imported_files, {
      if (restore_cached_import_sources()) {
        shiny::showNotification("Restored the last imported file source.", type = "message")
      }
    }, ignoreInit = TRUE)

    list(
      validation = validation,
      metadata = metadata,
      set_metadata = set_metadata,
      infer_death_events = shiny::reactive(infer_death_events_rv()),
      set_infer_death_events = set_infer_death_events,
      infer_survival = shiny::reactive(infer_death_events_rv()),
      set_infer_survival = set_infer_death_events,
      detect_scheduled_sampling = shiny::reactive(detect_scheduled_sampling_rv()),
      set_detect_scheduled_sampling = set_detect_scheduled_sampling,
      scheduled_sampling_cages = shiny::reactive(scheduled_sampling_cages_rv()),
      set_scheduled_sampling_cages = set_scheduled_sampling_cages,
      direct_entry_settings = shiny::reactive(direct_entry_settings_rv()),
      set_direct_entry_settings = set_direct_entry_settings,
      set_weights_source = set_weights_source,
      set_survival_source = set_survival_source,
      clear_survival_source = clear_survival_source,
      restore_cached_import_sources = restore_cached_import_sources,
      current_source = shiny::reactive(list(weights = weights_source_rv(), survival = list(kind = "none", label = "Not used"))),
      weights_name = shiny::reactive(weights_imported()$source_name %||% "study-data.csv"),
      survival_name = shiny::reactive(weights_imported()$source_name %||% "study-data.csv"),
      shared_style = shiny::reactive(shared_style_rv()),
      set_shared_style = set_shared_style,
      stats_settings = shiny::reactive(stats_settings_rv()),
      set_stats_settings = set_stats_settings,
      survival_analysis_settings = shiny::reactive(survival_analysis_settings_rv()),
      set_survival_analysis_settings = set_survival_analysis_settings,
      set_group_overrides = set_group_overrides,
      weight_plot_settings = shiny::reactive(weight_plot_settings_rv()),
      set_weight_plot_settings = set_weight_plot_settings,
      score_plot_settings = shiny::reactive(score_plot_settings_rv()),
      set_score_plot_settings = set_score_plot_settings,
      survival_plot_settings = shiny::reactive(survival_plot_settings_rv()),
      set_survival_plot_settings = set_survival_plot_settings,
      resolved_group_meta = resolved_group_meta
    )
  })
}
