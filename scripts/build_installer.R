`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "") || all(is.na(x))) y else x
}

find_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)

  repeat {
    if (file.exists(file.path(current, "VERSION")) && dir.exists(file.path(current, "app", "R"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Project root not found.", call. = FALSE)
    }
    current <- parent
  }
}

app_build_packages <- function() {
  c(
    "shiny",
    "bslib",
    "DT",
    "readr",
    "dplyr",
    "tidyr",
    "ggplot2",
    "survival",
    "janitor",
    "stringr",
    "rappdirs",
    "scales",
    "colourpicker",
    "rhandsontable",
    "jsonlite"
  )
}

if (.Platform$OS.type != "windows") {
  stop("Run scripts/build_installer.R from native Windows R.", call. = FALSE)
}

root <- find_root()
setwd(root)

if (!requireNamespace("RInno", quietly = TRUE)) {
  stop(
    paste(
      "RInno is required to build the installer.",
      "Install it first with remotes::install_github(\"ficonsulting/RInno\")."
    ),
    call. = FALSE
  )
}

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

version <- trimws(readLines(file.path(root, "VERSION"), warn = FALSE, n = 1))

# Build to a temp dir outside synced folders to prevent background file locks on the
# output .exe while Inno Setup is writing it. The final .exe is copied back.
installer_dir_final <- file.path(root, "installer")
installer_dir       <- file.path(tempdir(), "PreclinicalStudyAnalysisInstaller")
dir.create(installer_dir_final, recursive = TRUE, showWarnings = FALSE)
dir.create(installer_dir,       recursive = TRUE, showWarnings = FALSE)

pkg_dependencies <- app_build_packages()

# Copy inst/templates into the app tree so RInno bundles them
templates_src <- file.path(root, "inst", "templates")
app_templates_dst <- file.path(root, "app", "inst", "templates")
if (dir.exists(templates_src)) {
  dir.create(app_templates_dst, recursive = TRUE, showWarnings = FALSE)
  template_files <- list.files(templates_src, full.names = TRUE)
  file.copy(template_files, app_templates_dst, overwrite = TRUE)
  message(sprintf("Copied %d template file(s) into app/inst/templates/.", length(template_files)))
} else {
  warning("inst/templates/ not found; example/template CSVs will be missing from the installer.")
}

# RInno's get_R uses the regex [1-3]\.[0-9]+\.[0-9]+ which misses R 4.x,
# causing latest_R_version to be character(0) and crashing the if() check.
# Monkey-patch the function to handle this gracefully.
local({
  patched_get_R <- function(app_dir = getwd(),
                            R_version = paste0(">=", R.version$major, ".", R.version$minor)) {
    if (!dir.exists(app_dir))
      stop(glue::glue("{app_dir} does not exist."), call. = FALSE)

    R_version <- RInno:::sanitize_R_version(R_version, clean = TRUE)
    filename  <- file.path(app_dir, glue::glue("R-{R_version}-win.exe"))

    if (file.exists(filename)) {
      cat("Using the copy of R already included:\n", filename, "\n")
      return(invisible(NULL))
    }

    # Fetch latest version list; regex updated to cover R 4.x+
    fetch_versions <- function(url) {
      tryCatch(
        readLines(url, warn = FALSE) |>
          stringr::str_extract("[1-9]\\.[0-9]+\\.[0-9]+") |>
          stats::na.omit() |>
          unique(),
        error = function(e) character(0)
      )
    }

    latest_R_version <- fetch_versions("https://cran.rstudio.com/bin/windows/base/")
    old_R_versions   <- fetch_versions("https://cran.rstudio.com/bin/windows/base/old/")

    if (length(latest_R_version) > 0 && latest_R_version[[1]] == R_version) {
      base_url <- glue::glue("https://cran.r-project.org/bin/windows/base/R-{R_version}-win.exe")
    } else {
      base_url <- glue::glue("https://cran.r-project.org/bin/windows/base/old/{R_version}/R-{R_version}-win.exe")
    }

    cat(glue::glue("Downloading R-{R_version} ...\n"))
    tryCatch(
      curl::curl_download(base_url, filename),
      error = function(e) message("Download error: ", conditionMessage(e))
    )
    if (!file.exists(filename))
      stop(glue::glue("{filename} failed to download."), call. = FALSE)
  }

  utils::assignInNamespace("get_R", patched_get_R, ns = "RInno")
})

RInno::create_app(
  app_name = "Preclinical Study Analysis",
  app_dir = file.path(root, "app"),
  pkgs = pkg_dependencies,
  include_R = TRUE,
  R_version = "4.4.2",
  user_browser = FALSE,
  dir_out = installer_dir
)

find_inno_setup <- function() {
  iscc_path <- Sys.which("iscc")
  if (nzchar(iscc_path)) return(iscc_path)
  candidates <- c(
    "C:/Program Files (x86)/Inno Setup 6/ISCC.exe",
    "C:/Program Files/Inno Setup 6/ISCC.exe",
    "C:/Program Files (x86)/Inno Setup 5/ISCC.exe",
    "C:/Program Files/Inno Setup 5/ISCC.exe"
  )
  found <- candidates[file.exists(candidates)]
  if (length(found)) found[[1]] else NULL
}

if ("compile_iss" %in% getNamespaceExports("RInno")) {
  iscc_path <- find_inno_setup()
  if (is.null(iscc_path)) {
    stop(
      paste(
        "Inno Setup (ISCC.exe) was not found on this machine.",
        "Download and install Inno Setup 6 from https://jrsoftware.org/isdownload.php,",
        "then re-run this script."
      ),
      call. = FALSE
    )
  }
  message(sprintf("Using Inno Setup at: %s", iscc_path))
  RInno::compile_iss()
}

exe_candidates <- list.files(installer_dir, pattern = "\\.exe$", recursive = TRUE, full.names = TRUE)
if (length(exe_candidates)) {
  newest <- exe_candidates[which.max(file.info(exe_candidates)$mtime)]
  target <- file.path(installer_dir_final, sprintf("PreclinicalStudyAnalysis-%s.exe", version))
  file.copy(newest, target, overwrite = TRUE)
  message(sprintf("Installer available at %s", target))
} else {
  warning("RInno completed, but no .exe file was found in the installer directory.")
}
