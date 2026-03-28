packages <- c(
  "shiny", "bslib", "DT", "readr", "readxl", "dplyr", "tidyr",
  "ggplot2", "survival", "janitor", "stringr", "rappdirs",
  "scales", "colourpicker", "rhandsontable", "jsonlite",
  "shinyjs", "digest", "testthat"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (!length(missing)) {
  cat("Required public-release packages are already installed.\n")
  quit(save = "no", status = 0)
}

install.packages(missing, repos = "https://cloud.r-project.org")
