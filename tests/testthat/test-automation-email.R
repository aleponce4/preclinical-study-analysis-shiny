test_that("dry-run study email returns metadata without sending", {
  tmp_pdf <- tempfile(fileext = ".pdf")
  writeBin(charToRaw("fake-pdf"), tmp_pdf)

  study <- list(
    study_id = "s1",
    report_title = "Study 1",
    email_to = c("to@example.org"),
    email_cc = c("cc@example.org")
  )

  analysis <- list(
    generated_at = as.POSIXct("2026-03-09 09:00:00", tz = "America/Chicago"),
    source_path = "/tmp/source.xlsx",
    source_file = "source.xlsx",
    warnings = c("warning a")
  )

  smtp <- list(
    smtp_server = "smtp://example.org",
    username = "",
    password = "",
    from = "sender@example.org",
    reply_to = "",
    subject_prefix = "[TEST]",
    use_ssl = "try"
  )

  res <- automation_send_study_report_email(
    study = study,
    analysis = analysis,
    report_path = tmp_pdf,
    smtp_config = smtp,
    dry_run = TRUE,
    verbose = FALSE
  )

  expect_false(res$sent)
  expect_true(res$dry_run)
  expect_match(res$subject, "\\[TEST\\]")
  expect_match(res$body, "Source: source.xlsx", fixed = TRUE)
  expect_false(grepl("/tmp/source.xlsx", res$body, fixed = TRUE))
})

test_that("failure digest dry-run composes recipients", {
  smtp <- list(
    smtp_server = "smtp://example.org",
    username = "",
    password = "",
    from = "sender@example.org",
    reply_to = "",
    subject_prefix = "[TEST]",
    use_ssl = "try"
  )

  res <- automation_send_failure_digest(
    failures = list(
      list(study_id = "s1", error = "boom"),
      list(study_id = "s2", error = "bad parse")
    ),
    admin_email = list(to = c("admin@example.org"), cc = character()),
    smtp_config = smtp,
    dry_run = TRUE,
    verbose = FALSE
  )

  expect_false(res$sent)
  expect_true(res$dry_run)
  expect_equal(res$to, "admin@example.org")
})

test_that("mime message includes attachment metadata", {
  tmp_pdf <- tempfile(fileext = ".pdf")
  writeBin(charToRaw("fake-pdf"), tmp_pdf)

  msg <- automation_compose_mime_message(
    from = "sender@example.org",
    to = c("to@example.org"),
    cc = character(),
    subject = "Subject",
    text_body = "Body",
    attachments = list(list(path = tmp_pdf, filename = "report.pdf", mime_type = "application/pdf"))
  )

  expect_match(msg, "Content-Type: multipart/mixed", fixed = TRUE)
  expect_match(msg, "report.pdf", fixed = TRUE)
  expect_match(msg, "Content-Transfer-Encoding: base64", fixed = TRUE)
})
