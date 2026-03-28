required_versions <- c(
  shiny = "1.10.0",
  bslib = "0.9.0",
  DT = "0.33",
  readr = "2.1.5",
  readxl = "1.4.5",
  dplyr = "1.1.4",
  tidyr = "1.3.1",
  ggplot2 = "3.5.2",
  survival = "3.8-3",
  janitor = "2.2.1",
  stringr = "1.6.0",
  rappdirs = "0.3.4",
  scales = "1.4.0",
  colourpicker = "1.3.0",
  rhandsontable = "0.3.8",
  jsonlite = "2.0.0",
  shinyjs = "2.1.0",
  digest = "0.6.37",
  testthat = "3.2.3",
  rlang = "1.1.7",
  cli = "3.6.5",
  processx = "3.8.6",
  ps = "1.9.1"
)

needs_install <- vapply(names(required_versions), function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(TRUE)
  }

  installed <- as.character(utils::packageVersion(pkg))
  utils::compareVersion(installed, required_versions[[pkg]]) < 0
}, logical(1))

targets <- names(required_versions)[needs_install]

if (!length(targets)) {
  cat("Required public-release packages are already installed.\n")
  quit(save = "no", status = 0)
}

cat("Installing or updating packages:\n")
cat(paste0("- ", targets), sep = "\n")
cat("\n")

install.packages(targets, repos = "https://cloud.r-project.org")
