test_that("prune keeps .gitkeep but removes stale regular files", {
  dir <- tempfile("automation-prune-")
  dir.create(dir, recursive = TRUE)

  gitkeep <- file.path(dir, ".gitkeep")
  stale <- file.path(dir, "old.log")
  fresh <- file.path(dir, "fresh.log")

  writeLines("", gitkeep)
  writeLines("old", stale)
  writeLines("fresh", fresh)

  old_time <- Sys.time() - as.difftime(40, units = "days")
  Sys.setFileTime(gitkeep, old_time)
  Sys.setFileTime(stale, old_time)

  deleted <- automation_prune_files(dir, older_than_days = 30)

  expect_true(file.exists(gitkeep))
  expect_false(file.exists(stale))
  expect_true(file.exists(fresh))
  expect_true(any(grepl("old\\.log$", deleted)))
})
