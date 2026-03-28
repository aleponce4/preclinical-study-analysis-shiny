test_that("url shortcut resolves to sharepoint target and source format", {
  shortcut <- tempfile(fileext = ".url")
  writeLines(c(
    "[InternetShortcut]",
    "URL=https://example.sharepoint.com/sites/example/Shared%20Documents/study.xlsx?web=1"
  ), shortcut)

  expect_true(automation_is_url_shortcut(shortcut))
  expect_equal(
    automation_read_url_shortcut(shortcut),
    "https://example.sharepoint.com/sites/example/Shared%20Documents/study.xlsx?web=1"
  )
  expect_equal(
    automation_resolve_source_reference(shortcut),
    "https://example.sharepoint.com/sites/example/Shared%20Documents/study.xlsx?web=1"
  )
  expect_equal(automation_detect_source_format(shortcut), "excel")
})

test_that("download_workbook uses graph download for sharepoint shortcut when configured", {
  shortcut <- tempfile(fileext = ".url")
  writeLines(c(
    "[InternetShortcut]",
    "URL=https://example.sharepoint.com/sites/example/Shared%20Documents/study.xlsx?web=1"
  ), shortcut)

  root <- tempfile("automation-root-")
  dir.create(file.path(root, "automation"), recursive = TRUE)
  ctx <- automation_create_run_context(root, verbose = FALSE)

  study <- list(
    study_id = "shortcut_study",
    source_url = shortcut
  )

  original_graph_config <- automation_graph_config_from_env
  original_graph_download <- automation_graph_download_shared_file
  on.exit({
    automation_graph_config_from_env <<- original_graph_config
    automation_graph_download_shared_file <<- original_graph_download
  }, add = TRUE)

  automation_graph_config_from_env <<- function(root = getOption("labweight.app_root", getwd())) {
    list(
      client_id = "client-id",
      tenant = "organizations",
      scopes = "Files.ReadWrite offline_access openid profile",
      token_cache = tempfile(fileext = ".json")
    )
  }

  automation_graph_download_shared_file <<- function(source_url, destfile, config = automation_graph_config_from_env()) {
    writeBin(charToRaw("fake-xlsx"), destfile)
    invisible(destfile)
  }

  downloaded <- automation_download_workbook(study, ctx)

  expect_true(file.exists(downloaded))
  expect_match(downloaded, "\\.xlsx$")
  expect_equal(readBin(downloaded, what = "raw", n = 9L), charToRaw("fake-xlsx"))
})
