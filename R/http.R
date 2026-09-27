# HTTP transport (design.md sections 4.1 and 4.6, slice S2a).
#
# Every request qesR makes goes through .qes_request(), and the network itself
# is reached only through .qes_transport(), a thin wrapper around curl. Tests
# replace .qes_transport() (testthat::local_mocked_bindings) with canned
# responses, so the retry, politeness and error rules below run offline.
#
# The rules:
#   - one plain GET, carrying only the User-Agent "qesR/<version> R/<version>":
#     no other header, cookie, query parameter or token about the user;
#   - redirects followed (at most 5; Borealis answers with a 303 to S3);
#     30 s to connect, and a transfer slower than 1 KB/s for
#     getOption("qesR.stall_timeout", 60) seconds is abandoned;
#   - TLS settings are never changed, and a TLS failure is never retried;
#   - requests to one host are sequential and at least one second apart;
#   - 408, 429, 500, 502, 503, 504 and transient transport errors are retried,
#     up to getOption("qesR.max_tries", 4) attempts, waiting as long as the
#     server's Retry-After asks (at most 120 s; longer is an error) or else
#     min(60, 2^k) seconds times a jitter between 0.5 and 1.5;
#   - a Harvard WAF challenge (202 with x-amzn-waf-action) or a 403 is never
#     retried or worked around: the error explains the manual route;
#   - a file is written to a ".part" file next to its destination, checked,
#     and only then renamed, so a partial download never gets the final name.

# Exactly "qesR/<version> R/<version>": no e-mail, URL or other identifier.
.qes_user_agent <- function() {
  sprintf("qesR/%s R/%s", as.character(utils::packageVersion("qesR")), as.character(getRversion()))
}

# ---- options -----------------------------------------------------------------

# A positive whole-number option, or an input error naming the option.
.qes_count_option <- function(name, default) {
  value <- getOption(name, default)
  ok <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    value >= 1 && value == round(value)
  if (!ok) {
    .qes_abort(
      "input_option_count",
      class = "qesR_error_input",
      args = list(name),
      data = list(arg = name, value = value)
    )
  }
  as.integer(value)
}

.qes_max_tries <- function() {
  .qes_count_option("qesR.max_tries", 4L)
}

# ---- the curl handle and the network seam ------------------------------------

# Options of the curl handle of every request (a list, so tests can read it).
.qes_handle_options <- function() {
  list(
    useragent = .qes_user_agent(),
    followlocation = TRUE,
    maxredirs = 5L,
    connecttimeout = 30L,
    low_speed_limit = 1024L,
    low_speed_time = .qes_count_option("qesR.stall_timeout", 60L)
  )
}

.qes_handle <- function() {
  do.call(curl::new_handle, .qes_handle_options())
}

# The only function that reaches the network. With `dest`, the body is written
# to that file; otherwise it is returned as a raw vector in `content`.
# Returns list(url, status, headers, content): `url` after redirects, `status`
# the final HTTP status, `headers` a named list with lower-case names.
.qes_transport <- function(url, dest = NULL, handle = .qes_handle()) {
  res <- if (is.null(dest)) {
    curl::curl_fetch_memory(url, handle = handle)
  } else {
    curl::curl_fetch_disk(url, dest, handle = handle)
  }
  list(
    url = res$url,
    status = as.integer(res$status_code),
    headers = curl::parse_headers_list(res$headers),
    content = res$content
  )
}

# ---- politeness ----------------------------------------------------------------

.qes_http_state <- new.env(parent = emptyenv())

.qes_url_host <- function(url) {
  tolower(sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/?#:]+).*$", "\\1", url))
}

.qes_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

# Clock seam, replaced in tests so that waits do not depend on how fast the
# machine is.
.qes_now <- function() {
  Sys.time()
}

# Wait until at least `gap` seconds have passed since the last request to the
# same host ended, then record this request.
.qes_polite_wait <- function(url, gap = 1) {
  host <- .qes_url_host(url)
  last <- .qes_http_state[[host]]
  if (!is.null(last)) {
    elapsed <- as.numeric(difftime(.qes_now(), last, units = "secs"))
    if (is.finite(elapsed) && elapsed < gap) {
      .qes_sleep(gap - elapsed)
    }
  }
  .qes_http_state[[host]] <- .qes_now()
  invisible(host)
}

.qes_polite_done <- function(url) {
  .qes_http_state[[.qes_url_host(url)]] <- .qes_now()
  invisible()
}

# ---- retry rules -------------------------------------------------------------------

.qes_retry_status <- c(408L, 429L, 500L, 502L, 503L, 504L)

# Longest Retry-After qesR waits for; a longer one is an error that says so.
.qes_retry_after_cap <- 120

# Exponential backoff before attempt k + 1: min(60, 2^k) seconds times a jitter
# in [0.5, 1.5). The jitter comes from the clock, not from the random number
# generator, so a retry never changes the user's random number stream.
.qes_backoff <- function(k) {
  jitter <- 0.5 + (as.numeric(.qes_now()) * 1000) %% 1000 / 1000
  min(60, 2^k) * jitter
}

# Seconds asked for by a Retry-After header (delay-seconds or HTTP-date), or
# NA when there is none or it cannot be read.
.qes_retry_after <- function(headers) {
  value <- headers[["retry-after"]]
  if (is.null(value) || length(value) == 0L) {
    return(NA_real_)
  }
  value <- trimws(as.character(value[[1]]))
  if (grepl("^[0-9]+$", value)) {
    return(as.numeric(value))
  }
  when <- tryCatch(curl::parse_date(value), error = function(e) NA)
  if (length(when) != 1L || is.na(when)) {
    return(NA_real_)
  }
  max(0, as.numeric(difftime(when, .qes_now(), units = "secs")))
}

# curl error classes (curl >= 6.0.0) that no retry can fix.
.qes_curl_permanent <- c(
  "curl_error_url_malformat", "curl_error_unsupported_protocol",
  "curl_error_too_many_redirects", "curl_error_write_error",
  "curl_error_aborted_by_callback", "curl_error_out_of_memory",
  "curl_error_filesize_exceeded", "curl_error_login_denied",
  "curl_error_remote_access_denied", "curl_error_bad_function_argument"
)

.qes_is_tls_error <- function(e) {
  any(grepl("^curl_error_(ssl|peer_failed_verification)", class(e)))
}

.qes_is_offline_error <- function(e) {
  inherits(e, c("curl_error_couldnt_resolve_host", "curl_error_couldnt_resolve_proxy"))
}

.qes_is_transient_error <- function(e) {
  inherits(e, "curl_error") && !inherits(e, .qes_curl_permanent)
}

# The server's own explanation of an error response, if it gave one: the
# "message" field of a Dataverse JSON error, or the start of a plain-text body.
.qes_server_message <- function(res) {
  body <- res$content
  if (!is.raw(body) || length(body) == 0L) {
    return(NA_character_)
  }
  text <- tryCatch(rawToChar(utils::head(body, 4096)), error = function(e) "")
  text <- if (validUTF8(text)) trimws(text) else ""
  if (startsWith(text, "{")) {
    msg <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE)$message, error = function(e) NULL)
    if (is.character(msg) && length(msg) == 1L && nzchar(msg)) {
      return(substr(msg, 1L, 300L))
    }
    return(NA_character_)
  }
  type <- tolower(as.character(res$headers[["content-type"]] %||% ""))
  if (startsWith(type, "text/plain") && nzchar(text)) {
    return(substr(text, 1L, 300L))
  }
  NA_character_
}

# ---- errors ----------------------------------------------------------------------

.qes_http_abort <- function(url, res, attempts, retry_after = NA_real_) {
  status <- res$status
  server_message <- .qes_server_message(res)
  data <- list(
    url = url, attempts = attempts, status = status,
    retry_after = retry_after, server_message = server_message
  )
  details <- if (is.na(server_message)) NULL else server_message
  if (!is.na(retry_after)) {
    .qes_abort(
      "http_retry_after_long",
      class = "qesR_error_http",
      args = list(.qes_q(url), round(retry_after)),
      data = data, details = details
    )
  }
  if (identical(status, 404L)) {
    .qes_abort("http_not_found", class = "qesR_error_http", args = list(.qes_q(url)),
      data = data, details = details)
  }
  .qes_abort("http_status", class = "qesR_error_http", args = list(.qes_q(url), status, attempts),
    data = data, details = details)
}

.qes_refused_abort <- function(url, res, attempts, manual_path) {
  data <- list(
    url = url, attempts = attempts, status = res$status, retry_after = NA_real_,
    server_message = NA_character_, waf_action = res$headers[["x-amzn-waf-action"]] %||% NA_character_,
    manual_path = manual_path %||% NA_character_
  )
  if (is.null(manual_path)) {
    .qes_abort("http_refused", class = "qesR_error_http_refused",
      args = list(.qes_q(url), res$status), data = data)
  }
  # a cache path carries its place relative to a qesR.cache_dir folder
  relative <- attr(manual_path, "relative")
  data$manual_path <- as.character(manual_path)
  if (is.null(relative)) {
    .qes_abort("http_refused_save", class = "qesR_error_http_refused",
      args = list(.qes_q(url), res$status, .qes_q(manual_path)), data = data)
  }
  .qes_abort("http_refused_manual", class = "qesR_error_http_refused",
    args = list(.qes_q(url), res$status, .qes_q(as.character(manual_path)), relative), data = data)
}

.qes_transport_abort <- function(url, e, attempts) {
  data <- list(url = url, attempts = attempts)
  if (.qes_is_tls_error(e)) {
    .qes_abort("tls", class = "qesR_error_tls", args = list(.qes_q(.qes_url_host(url))),
      data = data, parent = e)
  }
  if (.qes_is_offline_error(e)) {
    .qes_abort("offline", class = "qesR_error_offline", args = list(.qes_q(.qes_url_host(url))),
      data = data, parent = e)
  }
  .qes_abort("network_attempts", class = "qesR_error_network", args = list(.qes_q(url), attempts),
    data = data, parent = e)
}

# ---- the request loop ----------------------------------------------------------

# One GET with the rules above.
#   dest         file to write the body to (NULL: return it in memory);
#   verify       function(part_path) called on the complete download before it
#                takes its final name; it signals an error to reject the file;
#   manual_path  where a user may put the file by hand (named when the server
#                refuses the automated request);
#   max_tries    attempts before giving up.
# Returns the transport response; with `dest`, `content` is `dest`.
.qes_request <- function(url, dest = NULL, verify = NULL, manual_path = NULL,
                         max_tries = .qes_max_tries()) {
  force(max_tries)
  # built outside the error handler below, so that an invalid option is an
  # input error and not a failed transfer
  handle <- .qes_handle()
  # the part file of the current attempt; removed on every way out (an error,
  # but also a user interrupt), unless it has been given its final name
  part <- NULL
  on.exit(if (!is.null(part)) unlink(part), add = TRUE)
  attempts <- 0L
  repeat {
    attempts <- attempts + 1L
    part <- if (is.null(dest)) {
      NULL
    } else {
      tempfile(pattern = "qesR-", tmpdir = dirname(dest), fileext = ".part")
    }
    .qes_polite_wait(url)
    res <- tryCatch(.qes_transport(url, part, handle), error = function(e) e)
    .qes_polite_done(url)

    if (inherits(res, "error")) {
      if (!is.null(part)) unlink(part)
      if (.qes_is_tls_error(res) || .qes_is_offline_error(res) ||
        !.qes_is_transient_error(res) || attempts >= max_tries) {
        .qes_transport_abort(url, res, attempts)
      }
      .qes_sleep(.qes_backoff(attempts))
      next
    }

    status <- res$status
    waf <- !is.null(res$headers[["x-amzn-waf-action"]])
    if (identical(status, 403L) || (identical(status, 202L) && waf)) {
      res <- .qes_detach(res, part)
      .qes_refused_abort(url, res, attempts, manual_path)
    }

    if (status >= 200L && status < 300L) {
      if (is.null(part)) {
        return(res)
      }
      .qes_finish_part(part, dest, verify)
      part <- NULL
      res$content <- dest
      return(res)
    }

    res <- .qes_detach(res, part)
    if (status %in% .qes_retry_status && attempts < max_tries) {
      wait <- .qes_retry_after(res$headers)
      if (!is.na(wait) && wait > .qes_retry_after_cap) {
        .qes_http_abort(url, res, attempts, retry_after = wait)
      }
      .qes_sleep(if (is.na(wait)) .qes_backoff(attempts) else max(1, wait))
      next
    }
    .qes_http_abort(url, res, attempts)
  }
}

# Keep the start of an error response's body in memory and delete its part
# file, so no partial download survives an error.
.qes_detach <- function(res, part) {
  if (!is.null(part)) {
    size <- file.size(part)
    res$content <- if (is.na(size) || size == 0) raw(0) else readBin(part, "raw", min(size, 4096))
    unlink(part)
  }
  res
}

# Check a complete ".part" download and give it its final name. The part file
# never survives: it is renamed, or deleted when the check fails.
.qes_finish_part <- function(part, dest, verify = NULL) {
  ok <- FALSE
  on.exit(if (!ok) unlink(part), add = TRUE)
  if (!is.null(verify)) {
    verify(part)
  }
  if (!file.rename(part, dest)) {
    # a rename can fail where the destination already exists on some
    # platforms, or across volumes: copy, then remove the part file
    if (!file.copy(part, dest, overwrite = TRUE)) {
      .qes_abort(
        "cache_write",
        class = "qesR_error_cache",
        args = list(.qes_q(dest)),
        data = list(path = dest, reason = "write")
      )
    }
    unlink(part)
  }
  ok <- TRUE
  invisible(dest)
}

# ---- convenience wrappers ------------------------------------------------------------

# Download `url` to `dest`; `what` names the file in the progress message.
.qes_fetch <- function(url, dest, quiet = TRUE, what = NULL, verify = NULL,
                       manual_path = NULL, max_tries = .qes_max_tries()) {
  if (!is.null(what)) {
    .qes_inform(
      "download_file",
      class = "qesR_message_download",
      args = list(.qes_q(what), .qes_url_host(url)),
      data = list(url = url),
      quiet = quiet
    )
  }
  .qes_request(url, dest = dest, verify = verify, manual_path = manual_path, max_tries = max_tries)
  invisible(dest)
}

# GET a JSON document and parse it (lists, no simplification).
.qes_fetch_json <- function(url, max_tries = .qes_max_tries()) {
  res <- .qes_request(url, max_tries = max_tries)
  text <- rawToChar(res$content)
  Encoding(text) <- "UTF-8"
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}
