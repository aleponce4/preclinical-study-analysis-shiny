group_palette_catalog <- function() {
  list(
    okabe_ito = c(
      "#0072B2",
      "#D55E00",
      "#009E73",
      "#CC79A7",
      "#56B4E9",
      "#E69F00",
      "#F0E442",
      "#999999"
    ),
    tableau_10 = c(
      "#4E79A7",
      "#F28E2B",
      "#E15759",
      "#76B7B2",
      "#59A14F",
      "#EDC948",
      "#B07AA1",
      "#FF9DA7",
      "#9C755F",
      "#BAB0AC"
    ),
    viridis = substr(toupper(viridisLite::viridis(8)), 1, 7),
    brewer_set2 = c(
      "#66C2A5",
      "#FC8D62",
      "#8DA0CB",
      "#E78AC3",
      "#A6D854",
      "#FFD92F",
      "#E5C494",
      "#B3B3B3"
    ),
    grayscale = c(
      "#111111",
      "#333333",
      "#555555",
      "#777777",
      "#999999",
      "#BBBBBB",
      "#D0D0D0",
      "#E5E5E5"
    ),
    graphpad_colorblind = c(
      "#000000",
      "#FF0166",
      "#117F80",
      "#40007F",
      "#AA66FF",
      "#66CCFE"
    ),
    graphpad_colors = c(
      "#0534FF",
      "#FF2701",
      "#01C701",
      "#BD38E9",
      "#FF9301",
      "#010101"
    )
  )
}

group_palette_choices <- function() {
  c(
    "Okabe-Ito" = "okabe_ito",
    "Viridis" = "viridis",
    "Tableau 10" = "tableau_10",
    "ColorBrewer Set2" = "brewer_set2",
    "Grayscale" = "grayscale",
    "GraphPad Colorblind" = "graphpad_colorblind",
    "GraphPad Colors" = "graphpad_colors"
  )
}

resolve_named_palette <- function(palette_name, n) {
  palette_name <- tolower(as.character(palette_name %||% default_shared_style_settings()$palette_name))
  if (identical(palette_name, "viridis")) {
    return(substr(toupper(viridisLite::viridis(max(1L, n))), 1, 7))
  }
  palette <- group_palette_catalog()[[palette_name]] %||% group_palette_catalog()[[default_shared_style_settings()$palette_name]]
  rep_len(palette, n)
}

default_group_palette <- function(n) {
  resolve_named_palette(default_shared_style_settings()$palette_name, n)
}

grayscale_group_palette <- function(n) {
  resolve_named_palette("grayscale", n)
}

palette_preview_ui <- function(palette_name, chips = 6, class = "palette-preview-row palette-preview") {
  colors <- resolve_named_palette(palette_name, chips)
  shiny::tags$span(
    class = class,
    lapply(colors, function(color) {
      shiny::tags$span(
        class = "palette-preview-chip",
        style = sprintf("background:%s;", color)
      )
    })
  )
}

palette_choice_label <- function(palette_name, label = NULL, chips = 6) {
  display_label <- label %||% names(group_palette_choices())[match(palette_name, unname(group_palette_choices()))]
  shiny::tags$span(
    class = "palette-choice-label",
    shiny::tags$span(class = "palette-choice-name", display_label %||% palette_name),
    palette_preview_ui(palette_name, chips = chips)
  )
}

palette_gallery_spec <- function(palette_names = unname(group_palette_choices()), chips = 6) {
  choices <- group_palette_choices()
  values <- palette_names[palette_names %in% choices]
  labels <- names(choices)[match(values, unname(choices))]

  list(
    choiceNames = lapply(seq_along(values), function(i) {
      palette_choice_label(values[[i]], labels[[i]], chips = chips)
    }),
    choiceValues = unname(values)
  )
}

resolve_group_colors <- function(group_meta, shared_style = default_shared_style_settings()) {
  if (is.null(group_meta) || !nrow(group_meta)) {
    return(tibble::tibble(
      group_id = character(),
      display_name = character(),
      plot_order = integer(),
      custom_color = character(),
      color = character()
    ))
  }

  shared_style <- normalize_shared_style_settings(shared_style)

  ordered_meta <- tibble::as_tibble(group_meta) |>
    dplyr::arrange(.data$plot_order, .data$display_name)

  preset_colors <- resolve_named_palette(shared_style$palette_name, nrow(ordered_meta))
  seeded_custom <- normalize_hex_color(ordered_meta$custom_color)
  seeded_custom[is.na(seeded_custom)] <- default_group_palette(nrow(ordered_meta))[is.na(seeded_custom)]

  resolved_colors <- switch(
    shared_style$color_mode,
    custom = seeded_custom,
    preset_colors
  )

  ordered_meta |>
    dplyr::mutate(
      custom_color = seeded_custom,
      color = resolved_colors
    )
}
