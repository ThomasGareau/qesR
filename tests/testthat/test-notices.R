# Once-per-session notices (design.md sections 2.3 and 7, slice S0c):
# the assignment-default note and the legacy deprecation registry.

test_that("the deprecation registry lists the 11 legacy wrappers", {
  reg <- qesR:::.qes_deprecated
  expect_identical(names(reg), c("name", "replacement", "since", "slice", "shipped"))
  expect_setequal(reg$name, legacy_exports)
  expect_false(anyDuplicated(reg$name) > 0L)
  expect_type(reg$shipped, "logical")
  expect_false(anyNA(reg$shipped))
  # get_decon() is soft-deprecated from the engine-based legacy switch (0.7.0)
  expect_true(reg$shipped[reg$name == "get_decon"])
  expect_identical(reg$since[reg$name == "get_decon"], "0.7.0")
  expect_identical(reg$replacement[reg$name == "get_decon"], "qes_harmonize(srvy, targets = \"decon\")")
  # a shipped replacement must name a function that exists now
  for (i in which(reg$shipped)) {
    fn <- sub("\\(.*$", "", sub("^head\\(", "", reg$replacement[i]))
    expect_true(fn %in% getNamespaceExports("qesR"), info = reg$name[i])
  }
})

test_that("canonical exports never announce a deprecation", {
  local_fake_legacy()
  local_qes_once()
  withr::local_options(qesR.quiet_deprecated = NULL)
  expect_identical(count_class(qes_codebook("qes2018", quiet = TRUE), "qesR_message_deprecated"), 0L)
  expect_identical(count_class(get_qes("qes2018", quiet = TRUE), "qesR_message_deprecated"), 0L)
  expect_identical(
    count_class(get_qes_master(surveys = "qes2018", quiet = TRUE), "qesR_message_deprecated"),
    0L
  )
})

test_that("the deprecation notice is shown in French with qesR.lang = 'fr'", {
  local_fake_dataverse()
  local_qes_once()
  withr::local_options(qesR.quiet_deprecated = NULL, qesR.lang = "fr")
  msg <- NULL
  withCallingHandlers(
    get_codebook("qes2018", quiet = TRUE),
    qesR_message_deprecated = function(m) {
      msg <<- m
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(msg$lang, "fr")
  expect_identical(msg$fn, "get_codebook")
  expect_identical(msg$replacement, "qes_codebook()")
})

test_that("the assignment-default note fires once, only when assign_global is unset", {
  local_fake_legacy()
  local_qes_once()

  # supplying assign_global (either value) never triggers it
  expect_identical(
    count_class(get_qes("qes2018", assign_global = FALSE, quiet = TRUE), "qesR_message_assign_default"),
    0L
  )
  caller <- new.env(parent = globalenv())
  expect_identical(
    count_class(
      eval(quote(get_qes("qes2018", assign_global = TRUE, quiet = TRUE)), caller),
      "qesR_message_assign_default"
    ),
    0L
  )

  # unset: once per session per function, even with quiet = TRUE
  expect_identical(count_class(get_qes("qes2018", quiet = TRUE), "qesR_message_assign_default"), 1L)
  expect_identical(count_class(get_qes("qes2018", quiet = TRUE), "qesR_message_assign_default"), 0L)
  expect_identical(
    count_class(get_decon("qes2018", quiet = TRUE), "qesR_message_assign_default"),
    1L
  )
  expect_identical(
    count_class(get_qes_master(surveys = "qes2018", quiet = TRUE), "qesR_message_assign_default"),
    1L
  )

  # internal calls (get_preview, get_decon, get_qes_master -> get_qes) never
  # announce get_qes()'s note on their own
  local_qes_once()
  withr::local_options(qesR.quiet_deprecated = TRUE)
  n <- count_class(
    {
      get_preview("qes2018")
      get_decon("qes2018", assign_global = FALSE, quiet = TRUE)
      get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE)
    },
    "qesR_message_assign_default"
  )
  expect_identical(n, 0L)
})

test_that("the assignment-default note carries the function and object names", {
  local_fake_dataverse()
  local_qes_once()
  msg <- NULL
  withCallingHandlers(
    get_qes(" QES2018 ", quiet = TRUE),
    qesR_message_assign_default = function(m) {
      msg <<- m
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(msg$fn, "get_qes")
  expect_identical(msg$object_name, "qes2018")
})

test_that(".qes_reset_once() clears every flag", {
  once <- qesR:::.qes_once_first
  local_qes_once()
  expect_true(once("test:key"))
  expect_false(once("test:key"))
  qesR:::.qes_reset_once()
  expect_true(once("test:key"))
})

test_that("get_codebook() assigns what it returns, in every layout", {
  local_fake_dataverse()
  withr::local_options(qesR.quiet_deprecated = TRUE)
  for (layout in c("compact", "wide", "long")) {
    caller <- new.env(parent = globalenv())
    value <- eval(
      bquote(get_codebook(" QES2018 ", assign_global = TRUE, quiet = TRUE, layout = .(layout))),
      caller
    )
    expect_identical(ls(caller), "qes2018_codebook", info = layout)
    expect_identical(get("qes2018_codebook", envir = caller), value, info = layout)
  }
})
