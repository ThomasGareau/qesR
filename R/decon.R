# get_decon(): the teaching dataset of qesR 0.4.4, rendered from the
# harmonization engine since qesR 0.7.0 (design.md sections 2.3 and 5.12,
# slice HZ6; the renderer is R/legacy.R and the spec's legacy.csv, profile
# "decon"). Soft-deprecated from 0.7.0: its replacement is
# qes_harmonize(srvy, targets = "decon", include_draft = TRUE).

#' Create a Prepared Non-Exhaustive qesR Dataset
#'
#' Builds a small teaching dataset with 19 standardized columns from one
#' Quebec Election Study of qesR 0.4.4.
#'
#' `get_decon()` is soft-deprecated since qesR 0.7.0: it keeps working and
#' will not be removed, and a message names its replacement once per
#' session. The same variables, with a reason for every missing value and a
#' grade for every study's question, come from
#' `qes_harmonize(srvy, targets = "decon", include_draft = TRUE)`
#' (`include_draft = TRUE` applies the crosswalk rows still in review, as
#' `get_decon()` does; without it the columns are `NA` until those rows are
#' signed off).
#'
#' `get_decon()` returns the data and assigns nothing unless
#' `assign_global = TRUE`: write `decon <- get_decon("qes2022")`. The first
#' call in a session that leaves `assign_global` unset prints a one-time note
#' about this change from qesR 0.4.4.
#'
#' @section Columns:
#' `qes_code`, then `citizenship`, `yob` (year of birth), `age`, `gender`,
#' `province_territory`, `education`, `political_interest`, `turnout`,
#' `votechoice`, `votechoice_text`, `party_best`, `partylean`, `fed_pid`,
#' `prov_pid`, `ideology`, `income`, `religion` and `born_canada`. Since
#' qesR 0.7.0 each column is rendered from a target of the harmonization
#' engine ([qes_harmonize()]): categorical columns are factors with the
#' target's English levels (the same levels in every study), numbers stay
#' numbers, and `income` and `religion` are each study's own categories as
#' text (the `qes2022` income amount stays a number). `turnout` and
#' `votechoice` are the reported turnout and vote, asked after the
#' election, except for `qes2022`, where they are the
#' likelihood of voting and the vote intention asked during the campaign,
#' as in qesR 0.4.4; `attr(, "timing")` says which (`"post"` or `"pre"`).
#'
#' @section What changed in 0.7.0:
#' The columns hold the targets of the harmonization spec (see
#' `attr(, "source_map")`): the factor levels are the targets' English
#' labels rather than each file's labels (`"Man"` rather than `"A man"`),
#' `education` has four groups, `province_territory` is `"Quebec"`,
#' `yob` is a number, `income` and `religion` are text (the `qes2022`
#' income amount stays a number), and codes the spec
#' does not map (such as the `-99` of `qes2022`) are `NA`. `turnout` and
#' `votechoice` are filled for every study that asked the reported vote
#' (`qes2018` from `q5` and `q6`), and `get_decon("qes1998")` returns real
#' rows. `party_best` and `partylean` are `NA` everywhere (their 0.4.4
#' sources were other questions in each study).
#'
#' @section En français:
#' `get_decon()` construit un petit jeu de données d'enseignement de 19
#' colonnes à partir d'une étude de qesR 0.4.4. Depuis qesR 0.7.0, elle est
#' obsolète (dépréciation douce) : elle continue de fonctionner et ne sera
#' pas retirée, et un message nomme son remplacement une fois par session,
#' `qes_harmonize(srvy, targets = "decon", include_draft = TRUE)`
#' (`include_draft = TRUE` applique les lignes de correspondance encore en
#' révision, comme `get_decon()`). Chaque colonne est rendue à partir d'une cible du moteur d'harmonisation : facteurs aux niveaux
#' anglais des cibles, nombres, et texte pour `income` et `religion` (le
#' montant du revenu de `qes2022` reste un nombre).
#' `turnout` et `votechoice` sont la participation et le vote déclarés
#' après l'élection, sauf pour `qes2022`, où ce sont la probabilité de voter
#' et l'intention de vote pendant la campagne, comme dans qesR 0.4.4 ;
#' `attr(, "timing")` l'indique (`"post"` ou `"pre"`).
#'
#' @param srvy A qesR survey code. Defaults to `"qes2022"`. Codes are trimmed
#'   and case-insensitive. The 11 studies of qesR 0.4.4 are available, and
#'   `"qes_demo"`, the synthetic demonstration study; the 1998 firms' own
#'   files (`qes1998_crop`, `qes1998_createc`) raise an error of class
#'   `qesR_error_input` (their respondents are in `qes1998`).
#' @param assign_global If TRUE, also assign the result as `decon` into the
#'   environment `get_decon()` was called from (the global environment only
#'   when called at top level). Defaults to FALSE.
#' @param quiet If TRUE, suppress informational output while downloading.
#'
#' @return A data frame with the 19 columns above, returned visibly, with
#'   the attributes `timing` (what `turnout`, `votechoice` and
#'   `votechoice_text` hold: `"pre"`, `"post"` or `NA` where the column has
#'   no value), `source_map` (`qes_code`, `column`, `source_variable`,
#'   `target`, `grade`), `legacy_na_columns` and `qes_provenance` (the file
#'   read; see [qes_provenance()]).
#' @family legacy
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' # the synthetic demonstration study, offline
#' decon <- get_decon("qes_demo", quiet = TRUE)
#' head(decon)
#' attr(decon, "source_map")[, c("column", "source_variable", "target")]
#'
#' # the replacement
#' h <- qes_harmonize("qes_demo", targets = "decon", include_draft = TRUE, quiet = TRUE)
#' names(h)
#' @export
get_decon <- function(srvy = "qes2022", assign_global = FALSE, quiet = FALSE) {
  .qes_deprecate("get_decon")
  .get_decon_impl(
    srvy, assign_global = assign_global, quiet = quiet,
    envir = parent.frame(),
    assign_missing = missing(assign_global)
  )
}

.get_decon_impl <- function(srvy = "qes2022", assign_global = FALSE, quiet = FALSE,
                            envir = NULL, assign_missing = FALSE) {
  .qes_check_flag(assign_global, "assign_global")
  .qes_check_flag(quiet, "quiet")
  code <- .get_qes_study(srvy, demo = TRUE)$qes_survey_code
  if (length(code) != 1L || !(code %in% c(.qes_legacy_codes, "qes_demo"))) {
    .qes_abort(
      "input_decon_study",
      class = "qesR_error_input",
      args = list(.qes_q(srvy)),
      data = list(arg = "srvy", value = srvy)
    )
  }
  .qes_legacy_notice("get_decon")
  # one study: its failure is the error, with no "skipping" message
  built <- .qes_legacy_build("decon", code, quiet, skip = FALSE)
  decon <- built$data
  filled <- built$filled[[code]]
  tg <- .qes_spec_get(NULL, "error")$tables$targets
  timing_of <- function(col) {
    t <- .qes_split_list(filled[[col]])
    if (length(t) == 0L) NA_character_ else tg$target_timing[match(t[1], tg$target)]
  }
  attr(decon, "timing") <- c(
    turnout = timing_of("turnout"),
    votechoice = timing_of("votechoice"),
    votechoice_text = timing_of("votechoice_text")
  )
  sm <- built$source_map
  attr(decon, "source_map") <- data.frame(
    qes_code = sm$qes_code, column = sm$harmonized_variable, source_variable = sm$source_variable,
    target = sm$target, grade = sm$grade, stringsAsFactors = FALSE
  )
  na_cols <- built$na_rows
  if (is.null(na_cols)) {
    na_cols <- data.frame(column = character(0), study = character(0), reason = character(0),
                          n_cells = integer(0), cause = character(0), basis = character(0),
                          stringsAsFactors = FALSE)
  }
  attr(decon, "legacy_na_columns") <- na_cols
  attr(decon, "qes_provenance") <- built$provenance

  if (isTRUE(assign_global)) {
    .qes_assign("decon", decon, envir)
  } else if (isTRUE(assign_missing)) {
    .qes_assign_default_notice("get_decon", "decon")
  }

  decon
}
