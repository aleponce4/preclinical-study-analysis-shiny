if (!exists("project_path")) {
  source(file.path("tests", "testthat", "helper-load_app.R"), chdir = TRUE)
}

source(file.path(getOption("labweight.app_root"), "automation", "R", "bootstrap.R"), chdir = TRUE)
automation_source_modules(getOption("labweight.app_root"))
