# Harmonization primitives (design.md sections 5.4, 5.6 and 8.1, slice HZ1).

test_that(".canon() keeps SPSS user-missing codes (unclass before any NA test)", {
  x <- haven::labelled_spss(
    c(1, 6, 7, NA, 10),
    labels = c(yes = 1, spoiled = 7, `not reached` = 10),
    na_range = c(6, 10)
  )
  # haven's is.na() treats declared user-missing values as missing
  expect_true(is.na(x[2]))
  expect_identical(.canon(x), c("1", "6", "7", NA, "10"))
  y <- haven::labelled_spss(c(1, 8, 9), labels = c(dk = 8, refused = 9), na_values = c(8, 9))
  expect_identical(.canon(y), c("1", "8", "9"))
})

test_that(".canon() writes numbers without exponent or trailing decimals", {
  expect_identical(.canon(1e5), "100000")
  expect_identical(.canon(c(-99, 0, 98, 9999)), c("-99", "0", "98", "9999"))
  expect_identical(.canon(c(1.5, 0.25)), c("1.5", "0.25"))
  expect_identical(.canon(-0), "0")
  expect_identical(.canon(haven::labelled(c(1, 2), labels = c(a = 1))), c("1", "2"))
  expect_identical(.canon(c(1L, NA)), c("1", NA))
  expect_identical(.canon(NA), NA_character_)
})

test_that(".canon() trims text and marks it UTF-8", {
  expect_identical(.canon(c(" EN", "FR ", NA)), c("EN", "FR", NA))
  e <- .canon(paste0(" Qu", intToUtf8(0xE9), "bec "))
  expect_identical(e, paste0("Qu", intToUtf8(0xE9), "bec"))
  expect_true(all(Encoding(e) %in% c("UTF-8", "unknown")))
  expect_identical(.canon(factor(c("b", "a"))), c("b", "a"))
})

test_that("canonical code text is recognized", {
  expect_identical(
    .qes_is_canon_code(c("1", "-99", "98", "NA", "EN", "C", "01", "1.0", "1e5", " 1", "", "0.5")),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("the affine grammar is parsed, never evaluated", {
  expect_identical(.qes_affine("x"), c(a = 1, b = 0))
  expect_identical(.qes_affine("x-1"), c(a = 1, b = -1))
  expect_identical(.qes_affine("0.5*x+2"), c(a = 0.5, b = 2))
  expect_identical(.qes_affine("-1*x+10"), c(a = -1, b = 10))
  for (bad in c("x^2", "exp(x)", "system()", "system('ls')", "1.2.3*x", "x+", "2*y", "x;x", "", NA)) {
    expect_null(.qes_affine(bad), label = bad)
  }
})

test_that("key=value cells are parsed strictly", {
  expect_identical(.qes_parse_kv("min=0;max=10"), c(min = "0", max = "10"))
  expect_identical(.qes_parse_kv(NA), stats::setNames(character(0), character(0)))
  expect_identical(.qes_parse_kv("98=dk;-99=no_answer;NA=inapplicable"),
                   c(`98` = "dk", `-99` = "no_answer", `NA` = "inapplicable"))
  expect_null(.qes_parse_kv("min=0;max"))
  expect_null(.qes_parse_kv("min=0;min=1"))
  expect_null(.qes_parse_kv("=1"))
})

test_that("labels are normalized the same way in any locale", {
  e_acute <- intToUtf8(0xE9)
  plain <- .qes_norm_label(c(
    paste0("Tr", intToUtf8(0xE8), "s int", e_acute, "ress", e_acute, "(e)"),
    paste0("J", intToUtf8(0x2019), "ai annul", e_acute, "  mon vote "),
    paste0("Qu", "e", intToUtf8(0x301), "bec solidaire"),
    NA
  ))
  expect_identical(plain, c("tres interesse(e)", "j'ai annule mon vote", "quebec solidaire", NA))
  withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), {
    expect_identical(.qes_norm_label(c(
      paste0("Tr", intToUtf8(0xE8), "s int", e_acute, "ress", e_acute, "(e)"),
      paste0("J", intToUtf8(0x2019), "ai annul", e_acute, "  mon vote "),
      paste0("Qu", "e", intToUtf8(0x301), "bec solidaire"),
      NA
    )), plain)
  })
})

test_that("text md5 is the md5 of the UTF-8 bytes", {
  expect_identical(.qes_md5_text(c("abc", NA, "")), c("900150983cd24fb0d6963f7d28e17f72", NA, "d41d8cd98f00b204e9800998ecf8427e"))
  expect_identical(.qes_md5_text(intToUtf8(0xE9)), "66ddcd97cfdeabb2f6fb8a999b4bc76f")
})
