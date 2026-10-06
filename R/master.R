# get_qes_master(): the legacy stacked master of qesR 0.4.4, rendered from
# the harmonization engine since qesR 0.7.0 (design.md section 5.12, slice
# HZ6; the renderer is R/legacy.R and the spec's legacy.csv).

# The legacy master builds the 11 qesR 0.4.4 studies, by default and with
# "all", plus the synthetic `qes_demo` when it is named. The 1998 firms'
# own files (qes1998_crop, qes1998_createc) are refused: the qes1998 panel
# file holds their respondents, so naming them with qes1998 would count the
# same people twice, and the spec does not harmonize them.
.validate_master_surveys <- function(surveys) {
  if (is.null(surveys)) {
    return(.qes_legacy_codes)
  }
  if (is.character(surveys) && length(surveys) == 1L && identical(.qes_canon_code(surveys), "all")) {
    return(.qes_legacy_codes)
  }
  codes <- .qes_resolve_codes(surveys, "surveys", demo = TRUE)
  new_codes <- setdiff(codes, c(.qes_legacy_codes, "qes_demo"))
  if (length(new_codes) > 0L) {
    .qes_abort(
      "input_master_study",
      class = "qesR_error_input",
      args = list(.qes_q(new_codes)),
      data = list(arg = "surveys", value = new_codes)
    )
  }
  codes
}

#' Build the merged file
#'
#' Reads 11 Quebec Election Studies, from 1998 to 2022, and stacks them in
#' one data frame: one row per respondent of each study, and the same 30
#' harmonized columns for every study.
#'
#' `get_qes_master()` has a fixed layout: its arguments, its first 30
#' columns (in their order) and their types do not change, so that code
#' written against it keeps working; columns added later come after the 30
#' and are never removed. It is a quick, flat overview. For an analysis you
#' will publish, prefer [qes_harmonize()] (experimental), which keeps
#' intention and recall, the sovereignty wordings and the four-point and 0-10
#' interest scales apart, gives every missing value a reason and every cell
#' a comparability grade, or the study files themselves ([get_qes()]).
#' `vignette("migrating-0.7", package = "qesR")` says how its values compare
#' with those of earlier versions of qesR, and how to reproduce those.
#'
#' `get_qes_master()` returns the data and assigns nothing unless
#' `assign_global = TRUE`: write `master <- get_qes_master()`. The first
#' top-level call in a session (console, `Rscript` or `source()`) prints a
#' one-time note saying so, unless `quiet = TRUE` or `assign_global` is passed. The first call in a session also prints one-time notes on how
#' its values and columns compare with those of earlier versions (classes
#' `qesR_message_values_changed` and `qesR_message_legacy_columns`).
#'
#' @section How it is built:
#' Each study is harmonized by [qes_harmonize()] from its pinned original
#' file (checked by md5), and each column is rendered from the
#' targets it needs, as the renderer table of the spec says
#' (`qes_spec("spec")$tables$legacy`): for example `vote_choice` is the
#' reported vote (target `vote_prov_recall`) with the master's own party
#' labels, `sovereignty_support` is 1 or 0 for the referendum on an
#' independent country (`sov_indep`), and `political_interest` puts the
#' four-point interest items on 0-10 as 10, 7, 3 and 0. A code the spec does
#' not map is `NA`, never passed through. Every row of every file is kept:
#' there is no de-duplication and no removal of empty rows, so each study
#' contributes exactly its number of respondents (`qes2007_panel`: 2,442
#' rows). Variables that share a name across studies are not stacked: they
#' often hold different questions; read them from each study with
#' [get_qes()].
#'
#' The engine applies only the crosswalk rows signed off by a reviewer
#' (status `stable`), as [qes_harmonize()] does by default. The rows were
#' signed off after an automated double review against the original files
#' and documents (not a human review); a row that nobody has reviewed yet
#' is never read. A row still in review would not be applied, and the
#' columns it would fill would be `NA`: `attr(, "legacy_na_columns")` lists
#' such columns with the reason `not_reviewed` and says why each row is
#' held, and a message names them. The recommended weights that still need
#' review (those of `qes1998`, `qes2007_panel`, `qes2012_panel` and the CROP
#' polls, registered but not accepted yet: `qes_spec("spec")$tables$weights`
#' says what is known of each) are not applied: `weight_pre` and
#' `weight_post` are `NA` there, with the reason `not_reviewed` and the cause
#' `weight_needs_review`. `survey_weight` is each study's own weight, on its
#' own scale, and is not reviewed: in some studies it is a weight that needs
#' review (`qes2012_panel`, the CROP polls) or one calibrated on the vote
#' (`qes2008`), and `qes_design()` never uses it; the note of each study's
#' row of `qes_spec("spec")$tables$legacy` says which weight it is. Do not
#' pool weighted estimates across studies with it. `attr(, "source_map")`
#' gives the grade and review status of each column's question in each
#' study, and `attr(, "legacy_column_map")` says what each column holds.
#'
#' @section En français:
#' `get_qes_master()` empile 11 Études électorales québécoises, de 1998 à
#' 2022, dans un seul tableau : une ligne par répondant de chaque étude, et
#' les mêmes 30 colonnes harmonisées pour toutes les études. Son format est
#' fixe : mêmes arguments, mêmes 30 premières colonnes dans le même ordre,
#' mêmes types ; les colonnes ajoutées par la suite suivent les 30 et ne
#' seront jamais retirées. C'est une vue d'ensemble rapide et à plat ; pour
#' une analyse que vous publierez, préférez [qes_harmonize()], qui garde
#' séparés l'intention et le vote déclaré, les formulations de la
#' souveraineté et les échelles d'intérêt, et donne un motif à chaque
#' valeur manquante et un niveau de comparabilité à chaque cellule.
#' `vignette("fr-migrer-0.7", package = "qesR")` compare ses valeurs à
#' celles des versions antérieures de qesR et dit comment reproduire
#' celles-ci.
#'
#' Chaque colonne est rendue à partir des cibles de la spécification
#' d'harmonisation, selon la table `legacy` de la spécification
#' (`qes_spec("spec")$tables$legacy`). `vote_choice` et `turnout` sont le
#' vote et la participation déclarés après l'élection dans toutes les
#' études qui les ont demandés (les sondages CROP n'ont demandé que
#' l'intention de vote : `NA`) ; `sovereignty_support` vaut 1 ou 0 pour le
#' référendum sur un pays indépendant ; `political_interest` place les
#' échelles d'intérêt en quatre points sur 0-10 (10, 7, 3 et 0). Un code que
#' la spécification n'apparie pas vaut `NA`. Aucune ligne n'est retirée
#' (`qes2007_panel` : 2 442 lignes), et les variables de même nom d'une
#' étude à l'autre ne sont pas empilées. `attr(, "legacy_column_map")`
#' décrit chaque colonne et `attr(, "source_map")` donne la question de
#' chaque colonne dans chaque étude, avec son niveau de comparabilité et son
#' statut de révision. Seules les lignes de correspondance approuvées par un
#' réviseur (statut `stable`) sont appliquées, comme le fait
#' [qes_harmonize()] par défaut ; elles ont été approuvées après une double
#' révision automatisée sur les fichiers et documents originaux (et non une
#' révision humaine) ; une ligne que personne n'a encore révisée n'est
#' jamais lue. Les colonnes d'une ligne encore en révision vaudraient `NA` ;
#' `attr(, "legacy_na_columns")` les énumérerait avec le motif
#' `not_reviewed` en disant pourquoi la ligne est retenue, et un message les
#' nommerait. Les pondérations recommandées encore à réviser (celles de
#' `qes1998`, `qes2007_panel`, `qes2012_panel` et des sondages CROP,
#' enregistrées mais pas encore acceptées) ne sont pas appliquées :
#' `weight_pre` et `weight_post` y valent `NA`, avec le motif `not_reviewed`
#' et la cause `weight_needs_review`. `survey_weight` est la pondération
#' propre à chaque étude, sur sa propre échelle, et n'est pas révisée : dans
#' certaines études, c'est une pondération à réviser (`qes2012_panel`,
#' sondages CROP) ou calée sur le vote (`qes2008`), et `qes_design()` ne
#' l'utilise jamais ; la note de la ligne de chaque étude dans
#' `qes_spec("spec")$tables$legacy` dit laquelle. Ne combinez pas
#' d'estimations pondérées entre études avec elle.
#'
#' @param surveys Character vector of qesR survey codes (see [qes_studies()]).
#'   Defaults to the 11 studies of the merged file (`qes2022`, `qes2018`,
#'   `qes2018_panel`, `qes2014`, `qes2012`, `qes2012_panel`,
#'   `qes_crop_2007_2010`, `qes2008`, `qes2007`, `qes2007_panel` and
#'   `qes1998`); `"all"` on its own means the same 11. The 1998 firms' own files (`qes1998_crop`, `qes1998_createc`)
#'   raise an error: their respondents are in `qes1998`. Codes are trimmed
#'   and case-insensitive. `"qes_demo"` builds the master of the synthetic
#'   demonstration study, offline.
#' @param assign_global If TRUE, also assign the result as `object_name` into
#'   the environment `get_qes_master()` was called from (the global environment
#'   only when called at top level), after `saved_to` is set. Defaults to FALSE.
#' @param object_name Object name used when `assign_global = TRUE`. Defaults to
#'   `"qes_master"`.
#' @param quiet If TRUE, suppress informational output while downloading.
#' @param strict If TRUE, stop when any study fails. If FALSE, return partial results and
#'   record failures in attributes.
#' @param save_path Optional output path for writing the master file: `.rds`
#'   writes an RDS file, any other extension a UTF-8 CSV file. The
#'   provenance of every study read is written next to it, as
#'   `<stem>_provenance.csv`.
#'
#' @return A data frame, returned visibly: the 30 documented columns, in a
#'   fixed order and type, then the appended columns
#'   `vote_choice_timing`, `sovereignty_item`, `family`,
#'   `study_design`, `waves`, `subsample`, `source_row` (to join raw
#'   variables from [get_qes()]), `weight_pre` and `weight_post` (the
#'   spec's recommended weights, as deposited), `vote_intent`,
#'   `turnout_intent` and `sov_partnership_1995`. Attributes:
#'   * `source_map`: the source of every column of every study
#'     (`qes_code`, `qes_year`, `qes_name_en`, `harmonized_variable`,
#'     `source_variable`, `target`, `map_id`, `grade`, `status` (of the
#'     crosswalk row: `stable`, or `review` for a row held in review),
#'     `render`, `file_md5`, `spec_version`);
#'   * `loaded_surveys`, `failed_surveys`: the studies read, and one line per
#'     failed study, `"<code>: <reason>"`. qesR's own part of the reason is
#'     always in English, whatever the message language; a root cause raised
#'     by R itself (for example a download error) keeps the text R reported.
#'     The full conditions are in the `failures` field of the
#'     `strict = TRUE` error;
#'   * `duplicates_removed` and `empty_rows_removed`: always `0L`;
#'   * `harmonized_variables`: the columns after `qes_code`, `qes_year` and
#'     `qes_name_en`;
#'   * `crossstudy_variables_added` (always empty) and `variable_name_map`
#'     (no rows): kept so that code reading them keeps working;
#'   * `legacy_na_columns`: one row per column and study whose cells are all
#'     `NA` (`column`, `study`, `reason`, `n_cells`, `cause`, `basis`);
#'     `reason` is `"no_source"` (the spec has no applied question for it
#'     in the study, the file lacks the variable, or the study has no
#'     registered weight), `"not_reviewed"` (the study has the question, but its
#'     crosswalk row is not signed off by a reviewer yet: `cause` is then
#'     `"not_signed_off"` and `basis` says why the row is held; or, for
#'     `weight_pre` and `weight_post`, the study's recommended weight is
#'     registered but still needs review: `cause` is then
#'     `"weight_needs_review"` and `basis` names the weight),
#'     `"na_column"` (no valid source in any study) or
#'     `"all_missing"` (the question's every answer is a missing value);
#'     `cause` names the rule behind it where there is one:
#'     `"reported_vote_only"` (`vote_choice` and `turnout` hold only the
#'     reported vote and turnout, which the CROP polls did not ask),
#'     `"independence_question_only"` (`sovereignty_support` and
#'     `sovereignty` hold only the referendum question on an independent
#'     country, which the study did not ask), `"no_valid_source"` (no study
#'     has a valid source for the column), `"invalid_044_source"` (the
#'     source the first versions of qesR read for the column was verified
#'     wrong, and the spec has no row for it in the study),
#'     `"not_comparable_source"`
#'     (the study's only source is graded `not_comparable`),
#'     `"not_harmonized_yet"` or `"legacy_frozen"` (reason `"no_source"`:
#'     the study's question is now harmonized in [qes_harmonize()], but
#'     the frozen column keeps its `NA`: `federal_pid` of `qes2012` and
#'     `language` of `qes2022`);
#'     `basis`
#'     says in words why the column is `NA` in the study. No value is
#'     blanked after it is read;
#'   * `legacy_column_map`: what each column means (`column`, `target`,
#'     `definition`, `studies_changed`, `flag`, `note`, `render`);
#'     `studies_changed` lists the studies whose values differ from those
#'     of the first published version of the merged file (see
#'     `vignette("migrating-0.7", package = "qesR")`), and `flag` is
#'     `"approximate"` for columns that mix instruments;
#'   * `removed_columns`: the names of the 70 columns that stacked
#'     same-named variables across studies, which are not built;
#'   * `qes_provenance`: the file read for each study, with the cell and
#'     spec levels (see [qes_provenance()]);
#'   * `qes_spec`: the spec version and content hash that built the data;
#'   * `saved_to`: the output path when `save_path` is given.
#' @family data
#' @seealso [qes_harmonize()] for harmonized data with grades and reasons,
#'   [get_qes()] for the study files, [qes_provenance()] and [qes_cite()] to
#'   record and cite the files read.
#' @examples
#' # the synthetic demonstration study, offline
#' demo_master <- get_qes_master(surveys = "qes_demo", quiet = TRUE)
#' head(demo_master[, c("qes_code", "gender", "turnout", "vote_choice")])
#' attr(demo_master, "legacy_column_map")[1:5, c("column", "target", "definition")]
#' @export
get_qes_master <- function(
  surveys = NULL,
  assign_global = FALSE,
  object_name = "qes_master",
  quiet = FALSE,
  strict = FALSE,
  save_path = NULL
) {
  .get_qes_master_impl(
    surveys = surveys, assign_global = assign_global, object_name = object_name,
    quiet = quiet, strict = strict, save_path = save_path,
    envir = parent.frame(),
    assign_missing = missing(assign_global)
  )
}

.get_qes_master_impl <- function(
  surveys = NULL,
  assign_global = FALSE,
  object_name = "qes_master",
  quiet = FALSE,
  strict = FALSE,
  save_path = NULL,
  envir = NULL,
  assign_missing = FALSE
) {
  .qes_check_flag(assign_global, "assign_global")
  .qes_check_flag(quiet, "quiet")
  .qes_check_flag(strict, "strict")
  surveys <- .validate_master_surveys(surveys)
  .assert_single_string(object_name, "object_name")
  if (!is.null(save_path)) {
    .assert_single_string(save_path, "save_path")
    out_dir <- dirname(save_path)
    if (!dir.exists(out_dir)) {
      .qes_abort(
        "input_save_dir",
        class = "qesR_error_input",
        args = list(.qes_q(out_dir)),
        data = list(arg = "save_path", value = save_path)
      )
    }
  }
  .qes_legacy_notice("get_qes_master")

  built <- .qes_legacy_build("master", surveys, quiet)
  failed <- built$failed
  failed_conditions <- built$failed_conditions

  if (is.null(built$data)) {
    .qes_abort(
      "master_none",
      class = "qesR_error_source",
      data = list(
        study = names(failed_conditions),
        file_id = NA_character_,
        failures = failed_conditions
      ),
      parent = if (length(failed_conditions) > 0L) failed_conditions[[1]] else NULL
    )
  }

  if (isTRUE(strict) && length(failed) > 0L) {
    .qes_abort(
      "master_strict",
      class = "qesR_error_source",
      args = list(length(failed), .qes_q(names(failed_conditions))),
      data = list(
        study = names(failed_conditions),
        file_id = NA_character_,
        failures = failed_conditions
      ),
      parent = failed_conditions[[1]]
    )
  }

  master <- built$data
  prov <- built$provenance
  na_cols <- built$na_rows
  if (is.null(na_cols)) {
    na_cols <- data.frame(column = character(0), study = character(0), reason = character(0),
                          n_cells = integer(0), cause = character(0), basis = character(0),
                          stringsAsFactors = FALSE)
  }

  attr(master, "source_map") <- built$source_map
  attr(master, "loaded_surveys") <- built$loaded
  attr(master, "failed_surveys") <- failed
  attr(master, "duplicates_removed") <- 0L
  attr(master, "empty_rows_removed") <- 0L
  attr(master, "harmonized_variables") <- setdiff(names(master), c("qes_code", "qes_year", "qes_name_en"))
  attr(master, "crossstudy_variables_added") <- character(0)
  attr(master, "variable_name_map") <- data.frame(
    legacy_variable = character(0),
    master_variable = character(0),
    label_hint = character(0),
    stringsAsFactors = FALSE
  )
  attr(master, "legacy_na_columns") <- na_cols
  attr(master, "legacy_column_map") <- .qes_legacy_column_map("master")
  attr(master, "removed_columns") <- .qes_legacy_removed()$column
  attr(master, "qes_provenance") <- prov
  attr(master, "qes_spec") <- .qes_legacy_spec(built$spec)

  if (!is.null(save_path)) {
    ext <- tolower(tools::file_ext(save_path))
    if (ext == "rds") {
      saveRDS(master, save_path)
    } else {
      .qes_write_csv(master, save_path)
    }
    if (!is.null(prov)) {
      stem <- tools::file_path_sans_ext(save_path)
      p <- as.data.frame(prov)
      attributes(p)[c("cell", "spec")] <- NULL
      .qes_write_csv(p, paste0(stem, "_provenance.csv"))
    }
    attr(master, "saved_to") <- save_path
  }

  # Attributes are final before opt-in assignment, so the assigned object is
  # identical to the returned one.
  if (isTRUE(assign_global)) {
    .qes_assign(object_name, master, envir)
  } else if (isTRUE(assign_missing)) {
    .qes_assign_default_notice("get_qes_master", object_name, quiet = quiet, envir = envir)
  }

  if (!quiet) {
    .qes_inform("master_n_rows", class = "qesR_message_download", args = list(nrow(master)))
    .qes_inform("master_n_loaded", class = "qesR_message_download", args = list(length(built$loaded)))
    if (length(failed) > 0L) {
      .qes_inform("master_n_skipped", class = "qesR_message_download", args = list(length(failed)))
    }
    if (!is.null(save_path)) {
      .qes_inform("master_saved", class = "qesR_message_download", args = list(save_path))
    }
  }

  master
}
