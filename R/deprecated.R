#' Older function names
#'
#' @description
#' Eleven functions from qesR 0.4.4 are kept as **legacy wrappers**. They keep
#' working, with the same arguments, and will not be removed. Each one prints a
#' short notice naming its replacement, once per session.
#'
#' | Legacy function | Replacement | Deprecated since |
#' |---|---|---|
#' | `get_codebook()` | `qes_codebook()` | 0.5.0 |
#' | `get_qes_codebook()` | `qes_codebook()` | 0.5.0 |
#' | `get_preview()` | `head(get_qes(srvy), obs)` | 0.5.0 |
#' | `format_codebook()` | `qes_codebook(codebook, layout = )` | 0.5.0 |
#' | `get_value_labels()` | `qes_codebook(layout = "long")` | 0.5.0 |
#' | `get_question()` | `qes_question()` | 0.5.0 |
#' | `get_codebook_files()` | `qes_docs()` | 0.5.0 |
#' | `get_qes_codebook_files()` | `qes_docs()` | 0.5.0 |
#' | `download_codebook()` | `qes_download(what = "docs")` | 0.5.0 |
#' | `get_qescodes()` | `qes_studies()` | 0.5.0 |
#' | `get_decon()` | `qes_harmonize(srvy, targets = "decon")` | 0.7.0 |
#'
#' @section Notices:
#' The notice is a message of class `qesR_message_deprecated`, not a warning,
#' so it never turns into an error under `options(warn = 2)`. It is shown once
#' per session for each function. `quiet = TRUE` does not hide it; set
#' `options(qesR.quiet_deprecated = TRUE)` to hide all of them. Its language
#' follows `options(qesR.lang =)` (see [qesR-package]); the data returned never
#' depends on the language.
#'
#' @section En français:
#' Onze fonctions de qesR 0.4.4 restent disponibles comme fonctions
#' héritées : elles continuent de fonctionner, avec les mêmes
#' arguments, et ne seront pas retirées. Chacune affiche une fois par session
#' une courte note qui nomme la fonction qui la remplace (depuis 0.5.0, et
#' depuis 0.7.0 pour `get_decon()`). La note est un message de classe
#' `qesR_message_deprecated` ; `quiet = TRUE` ne la masque pas, mais
#' `options(qesR.quiet_deprecated = TRUE)` la masque. Elle s'affiche en
#' français avec `options(qesR.lang = "fr")`.
#'
#' @examples
#' # a legacy function and its replacement give the same rows
#' old_rows <- get_preview("qes_demo", obs = 2)
#' new_rows <- head(get_qes("qes_demo", assign_global = FALSE, quiet = TRUE), 2)
#' identical(dim(old_rows), dim(new_rows))
#'
#' # hide the notices of every legacy function
#' op <- options(qesR.quiet_deprecated = TRUE)
#' head(get_qescodes(), 3)
#' options(op)
#'
#' @name qesR-deprecated
#' @aliases qesR-deprecated
NULL

# Registry of the legacy wrappers (design.md sections 1.2 and 2.3). It drives
# the notices and the contract test. `shipped` says whether the replacement is
# available, which turns the notice on (every replacement has shipped; the
# flag is kept for future wrappers); `slice` is where that happens.
.qes_deprecated <- data.frame(
  name = c(
    "get_codebook", "get_qes_codebook", "get_preview",
    "format_codebook", "get_value_labels", "get_question",
    "get_codebook_files", "get_qes_codebook_files", "download_codebook",
    "get_qescodes", "get_decon"
  ),
  replacement = c(
    "qes_codebook()", "qes_codebook()", "head(get_qes(srvy), obs)",
    "qes_codebook(codebook, layout = )", "qes_codebook(layout = \"long\")",
    "qes_question()",
    "qes_docs()", "qes_docs()", "qes_download(what = \"docs\")",
    "qes_studies()", "qes_harmonize(srvy, targets = \"decon\")"
  ),
  since = c(
    "0.5.0", "0.5.0", "0.5.0",
    "0.5.0", "0.5.0", "0.5.0",
    "0.5.0", "0.5.0", "0.5.0",
    "0.5.0", "0.7.0"
  ),
  slice = c(
    "S0c", "S0c", "S0c",
    "S3", "S3", "S3",
    "S1", "S1", "S2c",
    "S1", "HZ6"
  ),
  shipped = c(
    TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE
  ),
  stringsAsFactors = FALSE
)

# Called first by every legacy wrapper. Shows the notice once per session per
# function, only when the replacement has shipped, and only unless
# options(qesR.quiet_deprecated = TRUE). `quiet` arguments never reach here.
.qes_deprecate <- function(name) {
  row <- .qes_deprecated[.qes_deprecated$name == name, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop(sprintf("qesR internal error: '%s' is not in the legacy registry.", name), call. = FALSE)
  }
  if (!isTRUE(row$shipped) || isTRUE(getOption("qesR.quiet_deprecated", FALSE))) {
    return(invisible(FALSE))
  }
  if (!.qes_once_first(paste0("deprecated:", name))) {
    return(invisible(FALSE))
  }
  .qes_inform(
    "deprecated",
    class = "qesR_message_deprecated",
    args = list(name, row$replacement),
    data = list(fn = name, replacement = row$replacement, since = row$since)
  )
  invisible(TRUE)
}
