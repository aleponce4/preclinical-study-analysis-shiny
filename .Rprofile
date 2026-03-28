local({
  activate <- file.path("renv", "activate.R")
  required_pkgs <- c("R6", "later", "httpuv", "shiny", "bslib", "ggplot2", "dplyr", "readr", "tidyr", "DT")
  pkg_desc <- vapply(required_pkgs, function(pkg) {
    any(file.exists(Sys.glob(file.path("renv", "library", "*", "R-*", "*", pkg, "DESCRIPTION"))))
  }, logical(1))

  if (file.exists(activate) && all(pkg_desc)) {
    source(activate)
  }
})
