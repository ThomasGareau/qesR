# qes_design(): harmonized data as a survey design (design.md sections 2.2
# and 5.3, slice HZ4). The survey and srvyr packages are suggested, not
# imported: they are loaded only here, when asked for.

#' Harmonized data as a survey design (experimental)
#'
#' `qes_design()` turns the result of [qes_harmonize()] into a survey design
#' object of the \pkg{survey} package (or a `tbl_svy` of \pkg{srvyr}), with
#' the weight column that fits the targets, one stratum per study, and, in
#' the long layout, the respondent as the sampling unit.
#'
#' @section Choosing a weight:
#' Each wave of a study has at most one recommended weight (see
#' `qes_spec("spec")$tables$weights`). [qes_harmonize()] returns it as
#' `weight_pre` (the pre-election wave) and `weight_post` (the post-election
#' wave), or as `weight` in the long layout, normalized to mean 1 in each
#' study and wave by default. A pre-election question such as vote
#' intention is weighted with `weight_pre`, a post-election one such as
#' reported vote with `weight_post`.
#'
#' With `weight = NULL`, `qes_design()` looks at the wave each target of
#' `x` was taken from, as `attr(x, "qes_weight_guide")` records it (targets
#' that do not depend on the moment of the interview, such as the year of
#' birth, do not count; a question that can be asked in any wave, such as
#' sovereignty, counts through the wave that asked it): if all these waves
#' call for the same column, it uses that column; if they call for both
#' (the default `targets = "core"` does), it stops and asks you to choose.
#' With no such target, it uses the one weight column that has values, and
#' asks you to choose when both have values.
#' In the long layout the weight is the `weight` column.
#'
#' Rows without a value of the weight (respondents outside the waves that
#' have it, or waves whose weight is not documented yet) are left out of
#' the design, with a message that counts them by study.
#'
#' @section Pooling studies:
#' Normalized weights have mean 1 in each study and wave, so a pooled
#' estimate gives each study a share proportional to its number of
#' respondents. `pool = "equal"` rescales the weights so that each study
#' (each study and wave in the long layout; the polls of pooled polls count
#' as one) has the same total; it is the
#' only place where qesR changes the relative size of studies. Whether
#' either is meaningful depends on the question: studies differ in
#' population, mode, wording and sampling, and the grades of
#' [qes_spec()] say how comparable each study's question is.
#'
#' @section Design:
#' Studies are independent samples, so each study is a stratum. A study
#' made of several independent samples is divided further, by its `stratum`
#' column: the 1998 panel by polling firm (`stratum` 1 = CREATEC, 2 =
#' CROP), the pooled CROP polls of 2007-2010 by poll (`stratum` is the
#' poll's wave name, as in `waves`). In the
#' respondent layout each row is its own sampling unit (`ids = ~1`). In the
#' long layout a respondent interviewed in two waves appears in two rows of
#' one study, so the respondent (`qes_id`) is the sampling unit and the
#' variance accounts for the two answers of a panel respondent being
#' related. The studies are opt-in online panels or telephone samples whose
#' weights adjust to census margins; standard errors computed from these
#' designs treat the weighted sample as a probability sample and are only
#' as good as that assumption.
#'
#' @section En français:
#' `qes_design()` (expérimental) transforme le résultat de
#' [qes_harmonize()] en plan de sondage du package \pkg{survey} (ou en
#' `tbl_svy` de \pkg{srvyr}). Chaque étude est une strate ; en disposition
#' longue, la personne (`qes_id`) est l'unité d'échantillonnage. Une étude
#' formée de plusieurs échantillons indépendants est divisée selon sa
#' colonne `stratum` : le panel de 1998 par firme de sondage (1 = CREATEC,
#' 2 = CROP), les sondages CROP de 2007-2010 par sondage (`stratum` est le
#' nom de la vague du sondage, comme dans `waves`). Avec
#' `weight = NULL`, la pondération est choisie d'après la vague d'où vient
#' chaque cible (`attr(x, "qes_weight_guide")`) : `weight_pre` pour des
#' cibles de vagues préélectorales, `weight_post` pour des cibles de vagues
#' postélectorales ; les cibles fixes, comme l'année de naissance, ne
#' comptent pas ; si `x` mêle les deux, la fonction demande de choisir.
#' Chaque vague a au plus une pondération recommandée.
#' Les lignes sans valeur de pondération sont laissées hors du plan, avec un
#' message. `pool = "equal"` donne le même total à chaque étude (et vague en
#' disposition longue ; les sondages regroupés comptent pour une seule
#' étude).
#'
#' @param x Harmonized data returned by [qes_harmonize()], with its weight
#'   columns and attributes.
#' @param weight The weight column: `NULL` (default, chosen from the waves
#'   of the targets, see *Choosing a weight*), `"weight_pre"` or
#'   `"weight_post"` (respondent layout) or `"weight"` (long layout).
#' @param engine `"survey"` (default) returns a `survey.design2` object of
#'   the \pkg{survey} package; `"srvyr"` a `tbl_svy` of the \pkg{srvyr}
#'   package. The package must be installed.
#' @param pool `"as_is"` (default) keeps the weights of `x`; `"equal"`
#'   rescales them so that every study (every study and wave in the long
#'   layout; the polls of pooled polls count as one) has the same total.
#'
#' @return A `survey.design2` (engine `"survey"`) or `tbl_svy` (engine
#'   `"srvyr"`) object whose data are the rows of `x` with a value of the
#'   weight, as a plain data frame, with the added column `qes_stratum`
#'   (the study, or `<study>:<stratum>` where the `stratum` column is set).
#'   The weight is the column named by `weight`.
#'
#' @family harmonization
#' @seealso [qes_harmonize()] for the weight columns and
#'   `attr(, "qes_weight_guide")`, which says which weight fits each target.
#' @examples
#' h <- qes_harmonize("qes_demo", targets = c("sov_indep", "vote_prov_recall"),
#'                    include_draft = TRUE, quiet = TRUE)
#' # every target of the demonstration study was asked after the election
#' attr(h, "qes_weight_guide")
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   d <- qes_design(h, weight = "weight_post")
#'   survey::svymean(~sov_indep, d, na.rm = TRUE)
#' }
#' @export
qes_design <- function(x, weight = NULL, engine = c("survey", "srvyr"), pool = c("as_is", "equal")) {
  engine <- .qes_check_one(engine, "engine", c("survey", "srvyr"))
  pool <- .qes_check_one(pool, "pool", c("as_is", "equal"))
  long <- is.data.frame(x) && all(c("wave", "weight") %in% names(x))
  cols <- if (long) "weight" else intersect(c("weight_pre", "weight_post"), names(x))
  if (!inherits(x, "qes_harmonized") || !is.data.frame(x) || length(cols) == 0L) {
    .qes_abort("input_design_x", class = "qesR_error_input", data = list(arg = "x", value = NULL))
  }
  if (is.null(weight)) {
    weight <- if (long) "weight" else .qes_design_pick_weight(x, cols)
  } else if (!is.character(weight) || length(weight) != 1L || is.na(weight) || !weight %in% cols) {
    .qes_abort("input_design_weight", class = "qesR_error_input", args = list(.qes_q(cols)),
               data = list(arg = "weight", value = weight))
  }
  pkgs <- if (identical(engine, "srvyr")) c("survey", "srvyr") else "survey"
  for (p in pkgs) {
    if (!.qes_has_package(p)) {
      .qes_abort("design_dependency", class = "qesR_error_dependency", args = list(engine, p),
                 data = list(package = p))
    }
  }
  df <- x
  class(df) <- "data.frame"
  for (a in c("qes_spec", "qes_provenance", "qes_weight_guide", "failed_studies")) attr(df, a) <- NULL
  w <- df[[weight]]
  keep <- !is.na(w) & w > 0
  if (!any(keep)) {
    .qes_abort("input_design_no_weight", class = "qesR_error_input", args = list(.qes_q(weight)),
               data = list(arg = "weight", value = weight))
  }
  if (any(!keep)) {
    tab <- table(df$study[!keep])
    .qes_inform("design_dropped", class = "qesR_message_design_dropped",
                args = list(sum(!keep), .qes_q(weight), paste(sprintf("%s %d", names(tab), as.integer(tab)), collapse = ", ")),
                data = list(study = names(tab), n = as.integer(tab)))
  }
  df <- df[keep, , drop = FALSE]
  rownames(df) <- NULL
  # studies are independent samples; within a study, the independent samples
  # that its waves declare (strata_var: the firms of the 1998 panel, the
  # polls of pooled polls) are strata too
  stratum <- if ("stratum" %in% names(df)) df$stratum else rep(NA_character_, nrow(df))
  df$qes_stratum <- ifelse(is.na(stratum), df$study, paste(df$study, stratum, sep = ":"))
  if (identical(pool, "equal")) {
    # the polls of pooled polls are one study: they share one total, or
    # 24 polls would each get a study's share
    group <- if (long) {
      ifelse(df$wave_design %in% "poll_wave", df$study, paste(df$study, df$wave, sep = ":"))
    } else {
      df$study
    }
    totals <- tapply(df[[weight]], group, sum)
    df[[weight]] <- df[[weight]] * (nrow(df) / length(totals)) / unname(totals[group])
  }
  ids <- if (long) stats::as.formula("~qes_id") else stats::as.formula("~1")
  design <- survey::svydesign(ids = ids, strata = stats::as.formula("~qes_stratum"),
                              weights = stats::as.formula(paste0("~", weight)), data = df, nest = TRUE)
  # printed designs show the call that made them
  design$call <- match.call()
  if (identical(engine, "srvyr")) {
    return(srvyr::as_survey_design(design))
  }
  design
}

# The weight column that fits the targets of respondent-layout data `x`:
# the weight column of the wave each applied target came from, from the
# weight guide (static targets such as the year of birth do not count; a
# target whose timing is "any" counts through the wave that asked it). With
# no such target, the one weight column that has values. Otherwise an error
# that asks the user to choose.
.qes_design_pick_weight <- function(x, cols) {
  guide <- attr(x, "qes_weight_guide", exact = TRUE)
  chosen <- character(0)
  if (is.data.frame(guide) && nrow(guide) > 0L) {
    g <- guide[guide$target %in% names(x) & !is.na(guide$weight_column) &
                 !guide$target_timing %in% "static", , drop = FALSE]
    chosen <- unique(g$weight_column)
  }
  if (length(chosen) == 1L) {
    return(chosen)
  }
  if (length(chosen) > 1L) {
    .qes_abort("input_design_weight_choose", class = "qesR_error_input", args = list(.qes_q(chosen)),
               data = list(arg = "weight", value = NULL))
  }
  has_values <- cols[vapply(cols, function(k) any(!is.na(x[[k]])), logical(1))]
  if (length(has_values) == 1L) {
    return(has_values)
  }
  .qes_abort("input_design_weight_untimed", class = "qesR_error_input", args = list(.qes_q(cols)),
             data = list(arg = "weight", value = NULL))
}

# Is suggested package `p` installed? (A seam for the tests.)
.qes_has_package <- function(p) {
  requireNamespace(p, quietly = TRUE)
}
