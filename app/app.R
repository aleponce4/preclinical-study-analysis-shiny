app_dir_candidates <- unique(c(
  getwd(),
  file.path(getwd(), "app"),
  tryCatch(
    dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = FALSE)),
    error = function(e) character(0)
  )
))

app_dir <- app_dir_candidates[vapply(app_dir_candidates, function(path) {
  file.exists(file.path(path, "R", "utils.R"))
}, logical(1))][1]

if (is.na(app_dir) || !nzchar(app_dir)) {
  stop("Unable to locate the app directory.", call. = FALSE)
}

options(labweight.app_root = normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = FALSE))
options(labweight.app_dir  = normalizePath(app_dir, winslash = "/", mustWork = FALSE))

creds_file <- file.path(app_dir, "credentials.R")
load_credentials_file <- {
  raw <- tolower(trimws(Sys.getenv("LABAPP_LOAD_CREDENTIALS_FILE", unset = "false")))
  raw %in% c("1", "true", "t", "yes", "y", "on")
}
if (isTRUE(load_credentials_file) && file.exists(creds_file)) source(creds_file)

source_files <- c(
  "utils.R",
  "runtime_config.R",
  "palettes.R",
  "import_csv.R",
  "mapping.R",
  "settings.R",
  "direct_entry.R",
  "transform_weights.R",
  "transform_scores.R",
  "transform_survival.R",
  "validate.R",
  "plots_weights.R",
  "plots_scores.R",
  "plots_survival.R",
  "downloads.R",
  "mod_import.R",
  "mod_entry.R",
  "mod_weights.R",
  "mod_scores.R",
  "mod_survival.R"
)

invisible(lapply(source_files, function(file) {
  source(file.path(app_dir, "R", file), chdir = TRUE)
}))

shiny::addResourcePath("www", file.path(app_dir, "www"))
options(shiny.maxRequestSize = 25 * 1024^2)

runtime_config <- get_runtime_config()
validate_runtime_config(runtime_config)
options(shiny.launch.browser = if (isTRUE(runtime_config$cloud_mode)) FALSE else TRUE)

ui_core <- bslib::page_navbar(
  title = shiny::div(
    class = "app-title",
    shiny::span("Preclinical Study Analysis")
  ),
  id = "main_nav",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", base_font = bslib::font_collection("Source Sans Pro", "sans-serif")),
  header = shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", href = "www/app.css"),
    shiny::tags$script(shiny::HTML(
      "$(document).on('shown.bs.tab', '[data-bs-toggle=\"tab\"]', function () {
         setTimeout(function () { $(window).trigger('resize'); }, 50);
       });"
    ))
  ),
  bslib::nav_panel("Import", mod_import_ui("import")),
  bslib::nav_panel("Manual Data Entry", mod_entry_ui("entry")),
  bslib::nav_panel("Weights", mod_weights_ui("weights")),
  bslib::nav_panel("Survival", mod_survival_ui("survival")),
  bslib::nav_panel("Clinical Scores", mod_scores_ui("scores"))
)

ui <- if (runtime_auth_enabled(runtime_config)) {
  if (!requireNamespace("shinymanager", quietly = TRUE)) {
    stop("shinymanager package is required when LABAPP_AUTH_MODE=shared_password.", call. = FALSE)
  }

  app_version <- read_version()
  login_header <- shiny::tags$div(
    style = "text-align:center; margin-bottom: 0.75rem;",
    shiny::tags$h3("Preclinical Study Analysis", style = "margin:0; font-weight:700;"),
    shiny::tags$p(
      "Configured authentication is enabled for this deployment.",
      style = "margin: 0.25rem 0 0 0; color:#495057;"
    )
  )
  login_footer <- shiny::tags$div(
    style = "text-align:center; margin-top: 1rem; font-size: 0.9rem; color:#6c757d;",
    shiny::tags$div(sprintf("Version %s", app_version))
  )

  shinymanager::secure_app(
    ui_core,
    enable_admin = FALSE,
    choose_language = FALSE,
    tags_top = login_header,
    tags_bottom = login_footer
  )
} else {
  ui_core
}

server <- function(input, output, session) {
  if (!isTRUE(runtime_config$cloud_mode)) {
    session$onSessionEnded(function() {
      stopApp()
    })
  }

  if (runtime_auth_enabled(runtime_config)) {
    shinymanager::secure_server(
      check_credentials = make_shared_password_checker(runtime_config)
    )
  }

  app_state <- mod_import_server("import")
  mod_entry_server("entry", app_state)
  mod_weights_server("weights", app_state)
  mod_scores_server("scores", app_state)
  mod_survival_server("survival", app_state)
}

shiny::shinyApp(ui, server)
