testthat::test_that("runtime config defaults are local-safe", {
  withr::local_envvar(c(
    LABAPP_CLOUD_MODE = NA_character_,
    LABAPP_AUTH_MODE = NA_character_,
    LABAPP_STORAGE_MODE = NA_character_,
    LABAPP_SHARED_USER = NA_character_,
    LABAPP_SHARED_PASSWORD = NA_character_,
    LABAPP_SHARED_PASSWORD_HASH = NA_character_
  ))

  cfg <- get_runtime_config()
  testthat::expect_false(cfg$cloud_mode)
  testthat::expect_equal(cfg$auth_mode, "none")
  testthat::expect_equal(cfg$storage_mode, "local_disk")
  testthat::expect_false(runtime_auth_enabled(cfg))
  testthat::expect_false(runtime_session_storage(cfg))
})

testthat::test_that("cloud mode defaults storage to session", {
  withr::local_envvar(c(
    LABAPP_CLOUD_MODE = "true",
    LABAPP_AUTH_MODE = NA_character_,
    LABAPP_STORAGE_MODE = NA_character_
  ))

  cfg <- get_runtime_config()
  testthat::expect_true(cfg$cloud_mode)
  testthat::expect_equal(cfg$storage_mode, "session")
  testthat::expect_true(runtime_session_storage(cfg))
})

testthat::test_that("shared-password checker supports plain and sha256 hash", {
  checker_plain <- make_shared_password_checker(list(
    cloud_mode = TRUE,
    auth_mode = "shared_password",
    shared_user = "lab_user",
    shared_password = "pw123",
    shared_password_hash = "",
    storage_mode = "session"
  ))

  ok_plain <- checker_plain("lab_user", "pw123")
  bad_plain <- checker_plain("lab_user", "wrong")
  testthat::expect_true(ok_plain$result)
  testthat::expect_false(bad_plain$result)

  checker_hash <- make_shared_password_checker(list(
    cloud_mode = TRUE,
    auth_mode = "shared_password",
    shared_user = "lab_user",
    shared_password = "",
    shared_password_hash = tolower(digest::digest("pw123", algo = "sha256", serialize = FALSE)),
    storage_mode = "session"
  ))

  ok_hash <- checker_hash("lab_user", "pw123")
  bad_hash <- checker_hash("lab_user", "bad")
  testthat::expect_true(ok_hash$result)
  testthat::expect_false(bad_hash$result)
})
