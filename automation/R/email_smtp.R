automation_smtp_config_from_env <- function() {
  smtp_server <- trimws(Sys.getenv("LABAPP_SMTP_SERVER", unset = ""))
  email_from <- trimws(Sys.getenv("LABAPP_EMAIL_FROM", unset = ""))

  if (!nzchar(smtp_server)) {
    stop("LABAPP_SMTP_SERVER must be set for email delivery.", call. = FALSE)
  }

  if (!nzchar(email_from)) {
    stop("LABAPP_EMAIL_FROM must be set for email delivery.", call. = FALSE)
  }

  list(
    smtp_server = smtp_server,
    username = trimws(Sys.getenv("LABAPP_SMTP_USERNAME", unset = "")),
    password = Sys.getenv("LABAPP_SMTP_PASSWORD", unset = ""),
    from = email_from,
    reply_to = trimws(Sys.getenv("LABAPP_EMAIL_REPLY_TO", unset = "")),
    subject_prefix = trimws(Sys.getenv("LABAPP_EMAIL_SUBJECT_PREFIX", unset = "")),
    use_ssl = trimws(Sys.getenv("LABAPP_SMTP_USE_SSL", unset = "try"))
  )
}

automation_header_safe <- function(x) {
  gsub("[\r\n]+", " ", as.character(x %||% ""), perl = TRUE)
}

automation_wrap_base64 <- function(path, width = 76L) {
  encoded <- base64enc::base64encode(path)
  n <- nchar(encoded)
  starts <- seq.int(1L, n, by = width)
  substring(encoded, starts, pmin(starts + width - 1L, n))
}

automation_compose_mime_message <- function(from,
                                            to,
                                            cc = character(),
                                            subject,
                                            text_body,
                                            attachments = list(),
                                            reply_to = "") {
  boundary <- sprintf("----=labapp-boundary-%s", format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"))

  lines <- c(
    sprintf("From: %s", automation_header_safe(from)),
    sprintf("To: %s", paste(automation_header_safe(to), collapse = ", ")),
    if (length(cc)) sprintf("Cc: %s", paste(automation_header_safe(cc), collapse = ", ")),
    sprintf("Subject: %s", automation_header_safe(subject)),
    if (nzchar(reply_to)) sprintf("Reply-To: %s", automation_header_safe(reply_to)),
    sprintf("Date: %s", format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z", tz = "UTC")),
    "MIME-Version: 1.0",
    sprintf("Content-Type: multipart/mixed; boundary=\"%s\"", boundary),
    "",
    sprintf("--%s", boundary),
    "Content-Type: text/plain; charset=\"UTF-8\"",
    "Content-Transfer-Encoding: 7bit",
    "",
    as.character(text_body %||% "")
  )

  for (attachment in attachments) {
    file_path <- attachment$path
    if (is.null(file_path) || !file.exists(file_path)) {
      stop(sprintf("Attachment does not exist: %s", as.character(file_path)), call. = FALSE)
    }

    filename <- attachment$filename %||% basename(file_path)
    mime_type <- attachment$mime_type %||% "application/octet-stream"

    lines <- c(
      lines,
      "",
      sprintf("--%s", boundary),
      sprintf("Content-Type: %s; name=\"%s\"", mime_type, automation_header_safe(filename)),
      "Content-Transfer-Encoding: base64",
      sprintf("Content-Disposition: attachment; filename=\"%s\"", automation_header_safe(filename)),
      "",
      automation_wrap_base64(file_path)
    )
  }

  lines <- c(lines, "", sprintf("--%s--", boundary), "")
  paste(lines, collapse = "\r\n")
}

automation_send_smtp_email <- function(smtp_config,
                                       to,
                                       cc = character(),
                                       subject,
                                       text_body,
                                       attachments = list(),
                                       verbose = FALSE) {
  to <- automation_chr_vec(to)
  cc <- automation_chr_vec(cc)
  recipients <- unique(c(to, cc))
  if (!length(recipients)) {
    stop("Email send requested with no recipients.", call. = FALSE)
  }

  message <- automation_compose_mime_message(
    from = smtp_config$from,
    to = to,
    cc = cc,
    subject = subject,
    text_body = text_body,
    attachments = attachments,
    reply_to = smtp_config$reply_to
  )

  handle_args <- list(
    mail_from = smtp_config$from,
    mail_rcpt = recipients,
    message = message,
    smtp_server = smtp_config$smtp_server,
    use_ssl = smtp_config$use_ssl,
    verbose = isTRUE(verbose)
  )

  if (nzchar(smtp_config$username)) {
    handle_args$username <- smtp_config$username
    handle_args$password <- smtp_config$password
  }

  do.call(curl::send_mail, handle_args)
}

automation_prefixed_subject <- function(smtp_config, subject) {
  prefix <- trimws(smtp_config$subject_prefix %||% "")
  if (!nzchar(prefix)) {
    return(subject)
  }
  sprintf("%s %s", prefix, subject)
}

automation_send_study_report_email <- function(study,
                                                analysis,
                                                report_path,
                                                smtp_config,
                                                dry_run = FALSE,
                                                verbose = FALSE) {
  subject <- automation_prefixed_subject(
    smtp_config,
    sprintf("%s report - %s", study$report_title %||% study$study_id, format(analysis$generated_at, "%Y-%m-%d"))
  )

  warning_lines <- if (length(analysis$warnings)) {
    paste(sprintf("- %s", analysis$warnings), collapse = "\n")
  } else {
    "- None"
  }

  body <- paste(
    sprintf("Automated report for study '%s' is attached.", study$study_id),
    "",
    sprintf("Generated: %s", format(analysis$generated_at, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Source: %s", analysis$source_file %||% basename(analysis$source_path %||% "")),
    "",
    "Validation warnings:",
    warning_lines,
    sep = "\n"
  )

  if (isTRUE(dry_run)) {
    return(list(sent = FALSE, dry_run = TRUE, subject = subject, to = study$email_to, cc = study$email_cc, body = body))
  }

  automation_send_smtp_email(
    smtp_config = smtp_config,
    to = study$email_to,
    cc = study$email_cc,
    subject = subject,
    text_body = body,
    attachments = list(list(path = report_path, filename = basename(report_path), mime_type = "application/pdf")),
    verbose = verbose
  )

  list(sent = TRUE, dry_run = FALSE, subject = subject, to = study$email_to, cc = study$email_cc, body = body)
}

automation_send_failure_digest <- function(failures,
                                           admin_email,
                                           smtp_config,
                                           dry_run = FALSE,
                                           verbose = FALSE) {
  if (!length(failures)) {
    return(invisible(NULL))
  }

  to <- automation_chr_vec(admin_email$to)
  cc <- automation_chr_vec(admin_email$cc)
  if (!length(to)) {
    return(invisible(NULL))
  }

  summary_lines <- vapply(failures, function(item) {
    sprintf("- %s: %s", item$study_id, item$error)
  }, character(1))

  subject <- automation_prefixed_subject(
    smtp_config,
    sprintf("Automated report failures (%d)", length(failures))
  )

  body <- paste(
    "One or more study reports failed during the latest automation run.",
    "",
    "Failures:",
    paste(summary_lines, collapse = "\n"),
    "",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sep = "\n"
  )

  if (isTRUE(dry_run)) {
    return(list(sent = FALSE, dry_run = TRUE, subject = subject, to = to, cc = cc))
  }

  automation_send_smtp_email(
    smtp_config = smtp_config,
    to = to,
    cc = cc,
    subject = subject,
    text_body = body,
    attachments = list(),
    verbose = verbose
  )

  list(sent = TRUE, dry_run = FALSE, subject = subject, to = to, cc = cc)
}
