# Contract: opt-in assignment (design.md sections 2.3 and 8.1, "Assignment").
# With assign_global = TRUE the object lands in the frame of whoever called the
# exported name, at top level and inside a user function, directly and through
# each legacy wrapper, and the assigned object is identical to the returned one.

assign_cases <- function() {
  list(
    get_qes = list(
      call = quote(get_qes("qes2018", assign_global = TRUE, quiet = TRUE)),
      name = "qes2018"
    ),
    get_qes_master = list(
      call = quote(get_qes_master(surveys = "qes2018", assign_global = TRUE, quiet = TRUE)),
      name = "qes_master"
    ),
    get_decon = list(
      call = quote(get_decon("qes2018", assign_global = TRUE, quiet = TRUE)),
      name = "decon"
    ),
    get_codebook = list(
      call = quote(get_codebook("qes2018", assign_global = TRUE, quiet = TRUE)),
      name = "qes2018_codebook"
    ),
    get_qes_codebook = list(
      call = quote(get_qes_codebook("qes2018", assign_global = TRUE, quiet = TRUE)),
      name = "qes2018_codebook"
    ),
    qes_codebook = list(
      call = quote(qes_codebook("qes2018", assign_global = TRUE, quiet = TRUE)),
      name = "qes2018_codebook"
    )
  )
}

# Evaluate `call` in a fresh frame standing for the caller; report what landed there.
run_in_caller_frame <- function(call, name, nested) {
  caller <- new.env(parent = globalenv())
  if (nested) {
    # a user function that calls the export from its own body
    user_fn <- eval(bquote(function() {
      value <- .(call)
      list(value = value, frame = environment())
    }), envir = caller)
    out <- suppressMessages(user_fn())
    frame <- out$frame
    value <- out$value
  } else {
    value <- suppressMessages(eval(call, envir = caller))
    frame <- caller
  }
  list(
    value = value,
    landed = exists(name, envir = frame, inherits = FALSE),
    assigned = if (exists(name, envir = frame, inherits = FALSE)) get(name, envir = frame) else NULL
  )
}

expect_assignment_lands <- function(case, nested) {
  res <- run_in_caller_frame(case$call, case$name, nested = nested)
  expect_true(res$landed, info = deparse(case$call))
  expect_identical(res$assigned, res$value, info = deparse(case$call))
}

test_that("the assignment table covers every export with assign_global", {
  has_arg <- vapply(v044_exports, function(f) {
    "assign_global" %in% names(formals(getExportedValue("qesR", f)))
  }, logical(1))
  expect_setequal(names(assign_cases()), v044_exports[has_arg])
  expect_setequal(names(assign_cases()), assigning_exports)
})

test_that("default calls assign nothing into the caller's frame", {
  local_fake_dataverse()
  local_qes_once()
  for (case in assign_cases()) {
    call <- case$call
    call$assign_global <- NULL
    caller <- new.env(parent = globalenv())
    suppressMessages(eval(call, envir = caller))
    expect_identical(ls(caller, all.names = TRUE), character(0), info = deparse(call))
  }
})

test_that("opt-in assignment lands in the caller's frame: direct canonical calls", {
  local_fake_dataverse()
  local_qes_once()
  for (f in c("get_qes", "get_qes_master", "get_decon", "get_codebook")) {
    case <- assign_cases()[[f]]
    expect_assignment_lands(case, nested = FALSE)
    expect_assignment_lands(case, nested = TRUE)
  }
})

test_that("opt-in assignment lands in the caller's frame through wrappers", {
  local_fake_dataverse()
  local_qes_once()
  for (f in c("get_qes_codebook", "qes_codebook")) {
    case <- assign_cases()[[f]]
    expect_assignment_lands(case, nested = FALSE)
    expect_assignment_lands(case, nested = TRUE)
  }
})

test_that("get_qes(assign_global = TRUE) also assigns <code>_codebook", {
  local_fake_dataverse()
  local_qes_once()
  caller <- new.env(parent = globalenv())
  value <- suppressMessages(eval(quote(get_qes("qes2018", assign_global = TRUE, quiet = TRUE)), envir = caller))
  expect_true(exists("qes2018_codebook", envir = caller, inherits = FALSE))
  expect_identical(
    get("qes2018_codebook", envir = caller),
    attr(value, "qes_codebook", exact = TRUE)
  )
})

test_that("get_qes() uses the canonical code for assignment and qes_survey_code", {
  local_fake_dataverse()
  local_qes_once()
  caller <- new.env(parent = globalenv())
  value <- suppressMessages(eval(quote(get_qes(" QES2018 ", assign_global = TRUE, quiet = TRUE)), envir = caller))
  expect_setequal(ls(caller), c("qes2018", "qes2018_codebook"))
  expect_identical(attr(value, "qes_survey_code"), "qes2018")
})

test_that("get_qes_master(save_path =) sets saved_to before assigning", {
  local_fake_dataverse()
  local_qes_once()
  path <- withr::local_tempfile(fileext = ".csv")
  caller <- new.env(parent = globalenv())
  value <- suppressMessages(eval(bquote(get_qes_master(
    surveys = "qes2018", assign_global = TRUE, quiet = TRUE, save_path = .(path)
  )), envir = caller))
  expect_identical(get("qes_master", envir = caller), value)
})

# Calls for the 11 legacy wrappers, with `quiet = TRUE` wherever the formals
# allow it (to show that quiet does not silence the notice), and the legacy
# column names each must return (NULL: an unnamed character(1); with
# `prefix = TRUE`, the first columns, since slice S3 appends columns to the
# codebook after those of v0.4.4, design.md section 2.3). Inputs that
# are themselves codebooks are built with the canonical qes_codebook(), so
# only the wrapper under test can emit a deprecation notice.
legacy_calls <- function() {
  list(
    get_codebook = list(
      call = quote(get_codebook("qes2018", quiet = TRUE)),
      names = v044_codebook_cols$compact,
      prefix = TRUE
    ),
    get_qes_codebook = list(
      call = quote(get_qes_codebook("qes2018", quiet = TRUE)),
      names = v044_codebook_cols$compact,
      prefix = TRUE
    ),
    format_codebook = list(
      call = quote(format_codebook(qes_codebook("qes2018", quiet = TRUE), layout = "long")),
      names = v044_codebook_cols$long,
      prefix = TRUE
    ),
    get_value_labels = list(
      call = quote(get_value_labels(qes_codebook("qes2018", quiet = TRUE), long = TRUE)),
      names = v044_value_labels_long_cols
    ),
    get_question = list(
      call = quote(get_question(get_qes("qes2018", quiet = TRUE), "q1")),
      names = NULL
    ),
    get_codebook_files = list(
      call = quote(get_codebook_files("qes2018", quiet = TRUE)),
      names = v044_codebook_files_cols
    ),
    get_qes_codebook_files = list(
      call = quote(get_qes_codebook_files("qes2018", quiet = TRUE)),
      names = v044_codebook_files_cols
    ),
    download_codebook = list(
      call = quote(download_codebook("qes2018", quiet = TRUE)),
      names = v044_download_codebook_cols
    ),
    get_preview = list(
      call = quote(get_preview("qes2018")),
      names = names(fake_study_data("qes2018"))
    ),
    get_decon = list(
      call = quote(get_decon("qes2018", quiet = TRUE)),
      names = v044_decon_cols
    ),
    get_qescodes = list(
      call = quote(get_qescodes()),
      names = v044_qescodes_cols
    )
  )
}

# Evaluate `call`, return every qesR_message_deprecated it emits, and muffle
# all its messages (e.g. the assign_global note of an inner get_qes()).
capture_deprecated <- function(call) {
  msgs <- list()
  value <- withCallingHandlers(
    eval(call, envir = new.env(parent = globalenv())),
    message = function(m) {
      if (inherits(m, "qesR_message_deprecated")) {
        msgs[[length(msgs) + 1L]] <<- m
      }
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, messages = msgs)
}

test_that("the legacy call table covers the 11 legacy wrappers", {
  expect_setequal(names(legacy_calls()), legacy_exports)
})

test_that("legacy wrappers emit qesR_message_deprecated once per session", {
  local_tempdir_cleanup()
  registry <- qesR:::.qes_deprecated
  expect_setequal(registry$name, legacy_exports)
  # Only wrappers whose replacement has shipped announce anything (design.md
  # section 0, item 10); `shipped` is the registry's logical column for that.
  announcing <- registry$name[registry$shipped]

  for (f in legacy_exports) {
    case <- legacy_calls()[[f]]
    local_fake_dataverse()
    local_qes_once()
    withr::local_options(qesR.quiet_deprecated = NULL)

    # first call: exactly one notice naming the wrapper, even with quiet = TRUE,
    # and the legacy shape is returned
    first <- capture_deprecated(case$call)
    if (f %in% announcing) {
      expect_identical(length(first$messages), 1L, info = f)
      expect_identical(first$messages[[1]]$fn, f, info = f)
      expect_identical(
        first$messages[[1]]$replacement,
        registry$replacement[registry$name == f],
        info = f
      )
    } else {
      expect_identical(length(first$messages), 0L, info = f)
    }
    if (is.null(case$names)) {
      expect_true(is.character(first$value) && length(first$value) == 1L, info = f)
    } else if (isTRUE(case$prefix)) {
      expect_identical(names(first$value)[seq_along(case$names)], case$names, info = f)
    } else {
      expect_identical(names(first$value), case$names, info = f)
    }

    # second call in the same session: no notice
    second <- capture_deprecated(case$call)
    expect_identical(length(second$messages), 0L, info = f)

    # after a reset, only options(qesR.quiet_deprecated = TRUE) silences it
    local_qes_once()
    withr::with_options(list(qesR.quiet_deprecated = TRUE), {
      silenced <- capture_deprecated(case$call)
    })
    expect_identical(length(silenced$messages), 0L, info = f)
  }
})
