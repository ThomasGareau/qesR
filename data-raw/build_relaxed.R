#!/usr/bin/env Rscript

# The relaxed rows of the harmonization spec (design.md section 5.14, spec
# 4.4.0): inst/extdata/harmonize/relaxed_maps.csv and the rx_ value maps of
# valuemaps.csv, and, on the pinned files, the recorded results of
# qes_decon() (expected/relaxed_marginals.csv and expected/relaxed_hashes.csv,
# checked live by V-R11).
#
# Usage (from the package root):
#   Rscript data-raw/build_relaxed.R              # relaxed_maps.csv, rx_ maps
#   QESR_CACHE_DIR=<dir> Rscript data-raw/build_relaxed.R --expected
#                                                 # also the recorded results
#   Rscript data-raw/build_relaxed.R --check      # write nothing; fail when a
#                                                 # file would change
#
# The relaxed columns themselves (relaxed.csv) and their level sets
# (levels.csv, prefix rx_) are edited by hand. Each relaxed row is declared
# below in a compact form: study, wave, column, source variable, rule and the
# map of codes to levels ("1,2,3" = "no_diploma"; "NA:<reason>" for a missing
# value). The script reads the shipped dictionary for each code's label
# (quoted in valuemaps.csv with its origin, so that V-D3 compares it with the
# file), the question wording and its document reference, and the counts of
# the evidence. A row starts in review (status "review"); the rows listed in
# `signed_off` below are stable, and qes_decon() applies stable rows only.
# After a change: bump SPEC (section 5.11), add a CHANGES.csv row and run
# data-raw/spec_check.R --write-hash.
#
# The groups follow the decisions of design.md section 12.5 (OD-R1 to
# OD-R16): education in four groups (vocational diplomas with college,
# OD-R10; years of schooling read as diplomas, the 2007 panel and the CROP
# polls); income in thirds of each study's respondents by the midpoint rule
# on the unweighted counts (OD-R11; V-R9 checks it); two employment statuses
# take the one that is not work (OD-R9); several mother tongues: French, then
# English (OD-R5); qes1998: education and employment are left out (OD-R1,
# OD-R2), the CREATEC sample is francophone by design (OD-R3).

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
with_expected <- "--expected" %in% args
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
cache_dir <- Sys.getenv("QESR_CACHE_DIR")
if (nzchar(cache_dir)) {
  options(qesR.cache_dir = cache_dir, qesR.cache = "disk")
}

dir <- normalizePath(file.path("inst", "extdata", "harmonize"), mustWork = TRUE)
dict <- .qes_dict_shipped()
cat_ <- .qes_catalog()
levels_tab <- .qes_read_csv(file.path(dir, "levels.csv"), "spec_levels")
relaxed <- .qes_read_csv(file.path(dir, "relaxed.csv"), "spec_relaxed")
vm_old <- .qes_read_csv(file.path(dir, "valuemaps.csv"), "spec_valuemaps")

# ---- the relaxed rows ----------------------------------------------------------------

rows <- list()
# One relaxed row. `map`: named list, "<codes>" -> level or "NA:<reason>";
# `then`: list(var, map) for rules coalesce and fn:amount_bands; `gate`:
# list(var, to = c(code = outcome)).
# `wording_en`, `wording_fr`: the question's wording when the dictionary
# lacks it (it is used instead of the dictionary's); `code_notes`: named list,
# code -> note of that code's row in the value map (instead of the note
# copied from a strict map); `count_notes`: named list, codes (as in `map`)
# -> what the evidence adds after that group's count.
rx_row <- function(study, wave, column, var, map = NULL, rule = "map", args = NA_character_,
                   na_codes = NA_character_, gate = NULL, override = FALSE, then = NULL,
                   notes_en = NA_character_, notes_fr = NA_character_, wording_ref = NA_character_,
                   wording_en = NA_character_, wording_fr = NA_character_, code_notes = NULL,
                   count_notes = NULL) {
  rows[[length(rows) + 1L]] <<- list(
    study = study, wave = wave, column = column, var = var, map = map, rule = rule, args = args,
    na_codes = na_codes, gate = gate, override = override, then = then, notes_en = notes_en,
    notes_fr = notes_fr, wording_ref = wording_ref, wording_en = wording_en, wording_fr = wording_fr,
    code_notes = code_notes, count_notes = count_notes
  )
}

# -- citizenship
rx_row("qes2018_panel", "pre", "citizenship", "qa",
       list("1" = "citizen", "2" = "NA:not_mappable", "3" = "NA:dk_refused"),
       notes_en = "OD-R12: the screening question asks whether the respondent may vote in the coming Quebec election, which requires Canadian citizenship; every respondent answered yes, so the column is constant in this study.",
       notes_fr = "OD-R12 : la question de sélection demande si la personne peut voter à la prochaine élection québécoise, ce qui exige la citoyenneté canadienne ; tous les répondants ont répondu oui, si bien que la colonne est constante dans cette étude.")

# -- education
edu_note_en <- "Vocational diplomas go with college (OD-R10)."
edu_note_fr <- "Les diplômes professionnels vont avec le collégial (OD-R10)."
rx_row("qes2007", "post", "education", "q77",
       list("1,2,3,4" = "no_diploma", "5" = "high_school", "6,7" = "college", "8,9,10,11" = "university",
            "98" = "NA:dk", "99" = "NA:refused"))
rx_row("qes2008", "post", "education", "q77",
       list("1,2,3,4" = "no_diploma", "5" = "high_school", "6,7" = "college", "8,9,10,11" = "university",
            "98" = "NA:dk", "99" = "NA:refused"),
       wording_en = "What is the highest level of education that you have completed?",
       wording_ref = "197296:Q77;196358:Q77")
rx_row("qes2012", "post", "education", "scol",
       list("1,2,3,4" = "no_diploma", "5" = "high_school", "6,7,8" = "college", "9,10,11,12" = "university",
            "98" = "NA:dk", "99" = "NA:refused"),
       notes_en = "Code 8 (post-secondary, not higher education; the French questionnaire's technical course) is college; code 10 (certificate and diploma), in neither questionnaire, is university with code 9 (some higher education).",
       notes_fr = "Le code 8 (postsecondaire non universitaire ; le cours technique du questionnaire français) est collégial ; le code 10 (certificat et diplôme), absent des deux questionnaires, est universitaire, comme le code 9 (études supérieures non terminées).")
rx_row("qes2014", "post", "education", "QSCOL",
       list("1,2,3,4" = "no_diploma", "5" = "high_school", "6,7,8" = "college", "9,10,11" = "university",
            "99" = "NA:refused"))
rx_row("qes2018", "post", "education", "qscol",
       list("1,2,3,4,5,6,7" = "no_diploma", "8" = "high_school", "9,10,11,12" = "college",
            "13,14,15" = "university", "99" = "NA:refused"),
       notes_en = "Code 9 (secondary 5 with a vocational diploma, DEP) is college (OD-R10); the strict education4 puts it with secondary.",
       notes_fr = "Le code 9 (secondaire 5 avec un diplôme d'études professionnelles, DEP) est collégial (OD-R10) ; la cible stricte education4 le classe au secondaire.")
rx_row("qes2022", "cps", "education", "cps_edu",
       list("1,2,3,4" = "no_diploma", "5" = "high_school", "6,7" = "college", "8,9,10,11" = "university"),
       na_codes = "-99=no_answer")
rx_row("qes2018_panel", "pre", "education", "d3",
       list("1,2" = "no_diploma", "3" = "high_school", "4,5" = "college", "6,7,8" = "university",
            "9" = "NA:dk_refused"),
       notes_en = "Code 4 (registered apprenticeship or other trades certificate) is college (OD-R10); code 6 (university certificate below the bachelor's) is university.",
       notes_fr = "Le code 4 (apprentissage enregistré ou autre certificat d'une école de métiers) est collégial (OD-R10) ; le code 6 (certificat universitaire inférieur au baccalauréat) est universitaire.")
rx_row("qes2007_panel", "*", "education", "scol",
       list("1" = "no_diploma", "2" = "high_school", "3" = "college", "4" = "university", "9" = "NA:refused"),
       notes_en = "Years of schooling, in bands named after a level: 7 years or less (primary) is no diploma, 8 to 12 years (secondary) is high school, 13 to 15 years (CEGEP, technical school) is college and 16 years or more is university. The high school group therefore also holds those who left secondary school without a diploma, and the college group may hold respondents with some university but no degree, whom other studies count as university.",
       notes_fr = "Années d'études, en tranches nommées d'après un ordre d'enseignement : 7 ans ou moins (primaire) est sans diplôme, 8 à 12 ans (secondaire) est secondaire, 13 à 15 ans (cégep, école technique) est collégial et 16 ans ou plus est universitaire. Le groupe secondaire compte donc aussi ceux qui ont quitté le secondaire sans diplôme, et le groupe collégial peut compter des personnes ayant fait des études universitaires sans diplôme, que les autres études classent à l'universitaire.")
rx_row("qes_crop_2007_2010", "*", "education", "scol",
       list("1" = "no_diploma", "2" = "high_school", "3" = "college", "4" = "university", "9" = "NA:refused"),
       notes_en = "Years of schooling in CROP's four ranges (7 or fewer, primary; 8 to 12, secondary; 13 to 15, CEGEP or technical school; 16 or more, university): 7 or fewer is no diploma and 8 to 12 is high school, so that group also holds those who left secondary school without a diploma; grouping by years, not by highest level, can also put some university (14 or 15 years) in college.",
       notes_fr = "Années d'études selon les quatre intervalles de CROP (7 ou moins, primaire ; 8 à 12, secondaire ; 13 à 15, cégep ou école technique ; 16 ou plus, université) : 7 ou moins est sans diplôme et 8 à 12 est secondaire, si bien que ce groupe compte aussi ceux qui ont quitté le secondaire sans diplôme ; le classement par années, et non par plus haut niveau atteint, peut aussi placer des études universitaires non terminées (14 ou 15 ans) au collégial.")
rx_row("qes1998", "pre", "education", "scol",
       list("1,2,3" = "NA:not_mappable", "9" = "NA:dk_refused"),
       notes_en = "OD-R1: the pooled file groups years of schooling as 1-9, 10-15 and university; 10-15 years spans high school and college, and mapping only the other two groups would bias every share, so the study is left out.",
       notes_fr = "OD-R1 : le fichier regroupé classe les années d'études en 1-9, 10-15 et université ; 10 à 15 ans chevauche le secondaire et le collégial, et n'apparier que les deux autres groupes fausserait toutes les proportions, si bien que l'étude est laissée de côté.")

# -- income (thirds by the midpoint rule on the unweighted counts)
inc_en <- "Thirds by the midpoint rule on the unweighted counts of the brackets (OD-R11)."
inc_fr <- "Tiers selon la règle du point milieu sur les effectifs non pondérés des tranches (OD-R11)."
rx_row("qes2007", "post", "income_cat", "q78",
       list("1,2,3" = "low", "4,5,6" = "middle", "7,8,9,10" = "high", "98" = "NA:dk", "99" = "NA:refused"),
       notes_en = paste(inc_en, "Low is under $40,000, high $70,000 or more (2006 income)."),
       notes_fr = paste(inc_fr, "Faible est sous 40 000 $, élevé 70 000 $ ou plus (revenu de 2006)."))
rx_row("qes2008", "post", "income_cat", "q78",
       list("1,2,3" = "low", "4,5,6" = "middle", "7,8,9,10" = "high", "98" = "NA:dk", "99" = "NA:refused"),
       notes_en = paste(inc_en, "Low is under $40,000, high $70,000 or more (2007 income)."),
       notes_fr = paste(inc_fr, "Faible est sous 40 000 $, élevé 70 000 $ ou plus (revenu de 2007)."))
rx_row("qes2012", "post", "income_cat", "reven",
       list("1,2,3,4" = "low", "5,6,7" = "middle", "8,9" = "high", "98" = "NA:dk", "99" = "NA:refused"),
       notes_en = paste(inc_en, "Low is under $40,000, high $88,000 or more (2011 income)."),
       notes_fr = paste(inc_fr, "Faible est sous 40 000 $, élevé 88 000 $ ou plus (revenu de 2011)."))
rx_row("qes2014", "post", "income_cat", "Q57",
       list("1,2,3,4" = "low", "5,6" = "middle", "7,8,9" = "high", "99" = "NA:refused"),
       notes_en = paste(inc_en, "Low is under $40,000, high $72,000 or more (2013 income)."),
       notes_fr = paste(inc_fr, "Faible est sous 40 000 $, élevé 72 000 $ ou plus (revenu de 2013)."))
rx_row("qes2018", "post", "income_cat", "q61",
       list("1,2,3,4" = "low", "5,6" = "middle", "7,8,9" = "high", "99" = "NA:refused"),
       notes_en = paste(inc_en, "Low is under $40,000, high $72,000 or more (2017 income)."),
       notes_fr = paste(inc_fr, "Faible est sous 40 000 $, élevé 72 000 $ ou plus (revenu de 2017)."))
rx_row("qes2007_panel", "*", "income_cat", "revenu",
       list("1,2" = "low", "3" = "middle", "4,5" = "high", "9" = "NA:dk_refused"),
       notes_en = paste(inc_en, "Low is under $40,000, high $60,000 or more."),
       notes_fr = paste(inc_fr, "Faible est sous 40 000 $, élevé 60 000 $ ou plus."))
rx_row("qes_crop_2007_2010", "*", "income_cat", "revenu",
       list("1,2" = "low", "3,4" = "middle", "5" = "high", "9" = "NA:dk_refused"),
       notes_en = paste(inc_en, "Low is under $40,000, high $80,000 or more; the thirds pool the 24 polls."),
       notes_fr = paste(inc_fr, "Faible est sous 40 000 $, élevé 80 000 $ ou plus ; les tiers regroupent les 24 sondages."))
rx_row("qes2018_panel", "pre", "income_cat", "d5",
       list("1,2" = "low", "3,4" = "middle", "5,6,7" = "high", "8" = "NA:dk_refused"),
       notes_en = paste(inc_en, "Low is under $40,000, high $80,000 or more."),
       notes_fr = paste(inc_fr, "Faible est sous 40 000 $, élevé 80 000 $ ou plus."))
rx_row("qes2022", "cps", "income_cat", "cps_income", rule = "fn:amount_bands",
       args = "breaks=52200,95600;levels=low,middle,high;then=cps_income2:rx_income_cat_qes2022_cps_income2",
       na_codes = "-99=no_answer;0=no_answer",
       then = list(var = "cps_income2",
                   map = list("1,2,3" = "low", "4" = "middle", "5,6,7,8" = "high", "-99" = "NA:no_answer")),
       notes_en = "The amount (2021 income) in thirds of the 1,444 amounts given, unweighted: low under $52,200, high $95,600 or more. An amount of 0 or -99 (no amount) passes to the bracket question cps_income2, whose brackets go in by their midpoint against the same limits.",
       notes_fr = "Le montant (revenu de 2021) en tiers des 1 444 montants donnés, non pondérés : faible sous 52 200 $, élevé 95 600 $ ou plus. Un montant de 0 ou de -99 (aucun montant) passe à la question par tranches cps_income2, dont les tranches sont classées selon leur point milieu, avec les mêmes seuils.")

# -- language (several mother tongues: French, then English; OD-R5)
lang_en <- "OD-R5: a respondent who reported two mother tongues is French when one is French, else English when one is English."
lang_fr <- "OD-R5 : une personne qui a déclaré deux langues maternelles est classée au français si l'une est le français, sinon à l'anglais si l'une est l'anglais."
# the 2007 question (the file has the French wording; the English
# questionnaire gives the English one and the labels of codes 3 to 7)
langu07_en <- "What is the language you first learned at home in your childhood and that you still understand?"
langu07_ref <- "192422:LANGU;192423:LANGU;425921:langu"
rx_row("qes2007", "post", "language", "langu",
       list("1,4,7" = "french", "2,5" = "english", "3,6" = "other", "9" = "NA:dk_refused"),
       override = TRUE, notes_en = lang_en, notes_fr = lang_fr,
       wording_en = langu07_en, wording_ref = langu07_ref,
       code_notes = list("4" = "two first languages including French; assigned to French (OD-R5)",
                         "7" = "two first languages including French; assigned to French (OD-R5)",
                         "5" = "two first languages including English, not French; assigned to English (OD-R5)"))
rx_row("qes2014", "post", "language", "QLANG",
       list("1,4,5" = "french", "2,6" = "english", "3" = "other", "8" = "NA:dk", "9" = "NA:refused"),
       override = TRUE, notes_en = lang_en, notes_fr = lang_fr)
rx_row("qes2022", "cps", "language", "cps_lang_2", rule = "fn:multiselect",
       args = "select=cps_lang_2:french,cps_lang_1:english,cps_lang_3:other;selected=1;first=TRUE",
       override = TRUE, notes_en = lang_en, notes_fr = lang_fr)
rx_row("qes1998", "pre", "language", "firme_post",
       list("1" = "french", "2" = "NA:not_mappable"),
       wording_ref = "332050:Description de la base de données",
       notes_en = "OD-R3: the CREATEC sample (firme_post = 1) holds only respondents whose mother tongue is French (codebook of the CREATEC file); the CROP respondents were screened on another criterion and have no mother tongue.",
       notes_fr = "OD-R3 : l'échantillon de CREATEC (firme_post = 1) ne compte que des personnes dont la langue maternelle est le français (livre de codes du fichier CREATEC) ; les répondants de CROP ont été choisis selon un autre critère et n'ont pas de langue maternelle.")
flag_en <- "Yes when this language is among the mother tongues reported."
flag_fr <- "Oui quand cette langue est parmi les langues maternelles déclarées."
rx_row("qes2007", "post", "language_fr", "langu",
       list("1,4,7" = "yes", "2,3,5,6" = "no", "9" = "NA:dk_refused"), override = TRUE,
       notes_en = flag_en, notes_fr = flag_fr, wording_en = langu07_en, wording_ref = langu07_ref,
       code_notes = list("4" = "two first languages including French; French = yes",
                         "7" = "two first languages including French; French = yes",
                         "5" = "two first languages, neither French; French = no"))
rx_row("qes2007", "post", "language_eng", "langu",
       list("2,5,7" = "yes", "1,3,4,6" = "no", "9" = "NA:dk_refused"), override = TRUE,
       notes_en = flag_en, notes_fr = flag_fr, wording_en = langu07_en, wording_ref = langu07_ref,
       code_notes = list("5" = "two first languages including English; English = yes",
                         "7" = "two first languages including English; English = yes",
                         "4" = "two first languages, neither English; English = no"))
rx_row("qes2014", "post", "language_fr", "QLANG",
       list("1,4,5" = "yes", "2,3,6" = "no", "8" = "NA:dk", "9" = "NA:refused"), override = TRUE,
       notes_en = flag_en, notes_fr = flag_fr)
rx_row("qes2014", "post", "language_eng", "QLANG",
       list("2,4,6" = "yes", "1,3,5" = "no", "8" = "NA:dk", "9" = "NA:refused"), override = TRUE,
       notes_en = flag_en, notes_fr = flag_fr)
rx_row("qes2022", "cps", "language_fr", "cps_lang_2", rule = "fn:multiselect",
       args = "select=cps_lang_2:yes,cps_lang_1:no,cps_lang_3:no;selected=1;first=TRUE", override = TRUE,
       notes_en = flag_en, notes_fr = flag_fr)
rx_row("qes2022", "cps", "language_eng", "cps_lang_1", rule = "fn:multiselect",
       args = "select=cps_lang_1:yes,cps_lang_2:no,cps_lang_3:no;selected=1;first=TRUE", override = TRUE,
       notes_en = flag_en, notes_fr = flag_fr)

# -- religion
rel_en <- "Those who belong to no religion are none; Jewish, Muslim and other non-Christian religions are other."
rel_fr <- "Les personnes sans appartenance religieuse sont aucune ; les religions juive, musulmane et les autres religions non chrétiennes sont autre."
rx_row("qes2012", "post", "religion", "q103",
       list("1" = "catholic", "2" = "protestant", "3" = "other_christian", "4,5,6" = "other", "9" = "NA:refused"),
       gate = list(var = "q102", to = c("2" = "none", "3" = "refused")), notes_en = rel_en, notes_fr = rel_fr)
rx_row("qes2014", "post", "religion", "Q63",
       list("1" = "catholic", "2" = "protestant", "3" = "other_christian", "4,5,6" = "other", "9" = "NA:refused"),
       gate = list(var = "Q62", to = c("2" = "none", "9" = "refused")), notes_en = rel_en, notes_fr = rel_fr)
rx_row("qes2018", "post", "religion", "q67",
       list("1" = "catholic", "2" = "protestant", "3" = "other_christian", "4,5,96" = "other", "99" = "NA:refused"),
       gate = list(var = "q66", to = c("2" = "none", "9" = "refused")), notes_en = rel_en, notes_fr = rel_fr)
rx_row("qes2022", "cps", "religion", "cps_religion",
       list("1,2" = "none", "10" = "catholic", "8,9,13,15,16,17,18,19,20,21" = "protestant",
            "11,12,14" = "other_christian", "3,4,5,6,7,22" = "other"),
       na_codes = "-99=no_answer",
       notes_en = paste(rel_en, "Agnostic counts as none; the Orthodox churches, Jehovah's Witnesses and Mormons are other Christian; the other Christian denominations listed are Protestant."),
       notes_fr = paste(rel_fr, "L'agnosticisme compte comme aucune ; les Églises orthodoxes, les Témoins de Jéhovah et les mormons sont autre chrétienne ; les autres dénominations chrétiennes de la liste sont protestantes."))

# -- marital status
rx_row("qes2012", "post", "marital", "q109",
       list("1,6" = "married", "2,4" = "separated_divorced", "5" = "widowed", "3" = "never_married", "9" = "NA:refused"),
       notes_en = "OD-R7: the question asks the official civil status, with no common-law option; civil partnership (6) is married. Partners who live together could answer Single or civil partnership: 360 respondents (24%) chose civil partnership, far more than the legal civil unions in Quebec, so common-law partners are split between married and never married.",
       notes_fr = "OD-R7 : la question demande l'état civil officiel, sans union de fait ; l'union civile (6) est marié(e). Les conjoints de fait pouvaient répondre célibataire ou union civile : 360 répondants (24 %) ont choisi l'union civile, bien plus que les unions civiles légales au Québec, si bien que les conjoints de fait se répartissent entre marié(e) et jamais marié(e).")
rx_row("qes2014", "post", "marital", "Q68",
       list("1,6" = "married", "2,4" = "separated_divorced", "5" = "widowed", "3" = "never_married", "9" = "NA:refused"),
       notes_en = "OD-R7: the question asks the official civil status and lists no common-law option. Civil union (6) is married. Code 6 holds 309 of the 1,501 substantive answers, far more than the share of formal civil unions in Quebec, so most respondents living common-law probably chose it; any who answered single instead are never married.",
       notes_fr = "OD-R7 : la question demande l'état civil officiel et n'offre pas l'union de fait. L'union civile (6) est marié(e). Le code 6 regroupe 309 des 1 501 réponses valides, bien plus que la part des unions civiles au Québec : la plupart des conjoints de fait l'ont probablement choisie ; ceux qui ont répondu célibataire sont jamais marié(e)s.")
rx_row("qes2018", "post", "marital", "qstat",
       list("1,6" = "married", "2,4" = "separated_divorced", "5" = "widowed", "3" = "never_married", "98" = "NA:refused"),
       notes_en = "Married or in a civil union (1) and common-law partner (6) are married.",
       notes_fr = "Marié(e) ou en union civile (1) et conjoint(e) de fait (6) sont marié(e).")
rx_row("qes2022", "pes", "marital", "pes_married",
       list("1,2" = "married", "3,4" = "separated_divorced", "5" = "widowed", "6" = "never_married"),
       na_codes = "-99=no_answer",
       notes_en = "Asked after the election: the respondents of the campaign wave only are missing (not in this wave).",
       notes_fr = "Posée après l'élection : les répondants de la seule vague de campagne sont manquants (absents de cette vague).")

# -- employment (two statuses: the one that is not work, OD-R9)
emp_en <- "OD-R9: a respondent who gave two statuses (student and working, retired and working, at home and working) takes the one that is not work; two jobs is working; at home, disabled and other statuses are other."
emp_fr <- "OD-R9 : une personne qui a donné deux situations (étudiante et salariée, retraitée et salariée, au foyer et salariée) prend celle qui n'est pas l'emploi ; deux emplois est en emploi ; au foyer, handicapé et autres situations sont autre."
rx_row("qes2007", "post", "employment", "q79",
       list("1,2,8" = "working", "4" = "unemployed", "3,11" = "retired", "5,9" = "student", "6,7,10,96" = "other",
            "99" = "NA:refused"), notes_en = emp_en, notes_fr = emp_fr)
rx_row("qes2008", "post", "employment", "q79",
       list("1,2" = "working", "4" = "unemployed", "3" = "retired", "5" = "student", "6,7,96" = "other",
            "99" = "NA:refused"),
       notes_en = "One status per respondent (the 2008 questionnaire has no two-status codes): self-employed and working for pay are working; at home, disabled and other (specify) are other.",
       notes_fr = "Une seule situation par personne (le questionnaire de 2008 n'a pas de codes de double situation) : à son compte et salarié sont en emploi ; au foyer, handicapé et autre (préciser) sont autre.")
for (s in c("qes2012", "qes2014", "qes2018")) {
  w12 <- identical(s, "qes2012")
  rx_row(s, "post", "employment", c(qes2012 = "occup", qes2014 = "Q58", qes2018 = "qoccup")[[s]],
         list("1,2,8" = "working", "4" = "unemployed", "3,11" = "retired", "5,9" = "student", "6,7,10,96" = "other",
              "99" = "NA:refused"), notes_en = emp_en, notes_fr = emp_fr,
         # qes2012: the file's label is the English question; the wording is
         # that of the questionnaires (Q101)
         wording_en = if (w12) "Are you currently self-employed, working for pay, retired, unemployed or looking for work, a student, caring for a family, or something else?" else NA_character_,
         wording_fr = if (w12) "Travaillez-vous actuellement à votre compte, êtes-vous salarié(e), avez-vous pris votre retraite, êtes-vous au chômage ou cherchez-vous du travail, êtes-vous étudiant(e), ménager(ère), ou quelque chose d\u2019autre?" else NA_character_,
         wording_ref = if (w12) "196367:Q101;196370:Q101" else NA_character_)
}
rx_row("qes2022", "pes", "employment", "pes_employed",
       list("1,2,3" = "working", "5" = "unemployed", "4,11" = "retired", "6,9" = "student", "7,8,10,12" = "other"),
       na_codes = "-99=no_answer",
       notes_en = paste(emp_en, "Asked after the election: the respondents of the campaign wave only are missing (not in this wave)."),
       notes_fr = paste(emp_fr, "Posée après l'élection : les répondants de la seule vague de campagne sont manquants (absents de cette vague)."))
rx_row("qes2018_panel", "pre", "employment", "d4",
       list("1,2,3" = "working", "4" = "unemployed", "6" = "retired", "5" = "student", "7,8" = "other",
            "9" = "NA:dk_refused"),
       notes_en = "Full time, part time and self-employed are working; outside the labour market (at home) and other are other. The item was asked of the 850 web respondents only: the file codes all 400 telephone respondents (method 1-2) as 9 (don't know), so 400 of the 406 missing values are not asked, not real don't-know answers (6 web respondents chose 9).",
       notes_fr = "Temps plein, temps partiel et autonome sont en emploi ; à l'extérieur du marché du travail (au foyer) et autre sont autre. La question n'a été posée qu'aux 850 répondants web : le fichier code 9 (ne sait pas) pour les 400 répondants téléphoniques (method 1-2), si bien que 400 des 406 valeurs manquantes sont des questions non posées et non de vrais « ne sait pas » (6 répondants web ont choisi 9).",
       count_notes = list("9" = "method 1 = 210, method 2 = 190, method 3 = 6; the telephone rows were not asked"))
rx_row("qes2007_panel", "*", "employment", "occup",
       list("1,2" = "working", "3" = "unemployed", "5" = "retired", "6" = "student", "4" = "other", "9" = "NA:refused"),
       notes_en = "Full time and part time are working; at home full time is other.",
       notes_fr = "Temps plein et temps partiel sont en emploi ; à la maison à temps plein est autre.")
rx_row("qes_crop_2007_2010", "*", "employment", "Occup",
       list("1,2" = "working", "3" = "unemployed", "5" = "retired", "6" = "student", "4" = "other", "9" = "NA:refused"),
       notes_en = "Full time and part time are working; at home full time is other.",
       notes_fr = "Temps plein et temps partiel sont en emploi ; à la maison à temps plein est autre.")
rx_row("qes1998", "pre", "employment", "occup",
       list("1,2,3" = "NA:not_mappable"),
       notes_en = "OD-R2: the pooled file has full time, part time and not working; not working joins the unemployed, the retired, students and those at home, so the study is left out.",
       notes_fr = "OD-R2 : le fichier regroupé a temps plein, temps partiel et ne travaille pas ; ne travaille pas réunit chômeurs, retraités, étudiants et personnes au foyer, si bien que l'étude est laissée de côté.")

# -- region (2007 panel) and administrative region
rx_row("qes2007_panel", "*", "region", "reg2",
       list("12,13,14,15,16" = "mtl_cma", "4,20" = "quebec_cma", "1,2,3,5,6,7,8,9,10,11,17,18,19,21,22" = "rest"),
       notes_en = "Sub-regions: the five parts of the Montreal CMA (in Lanaudière, Laurentides, Laval, Montérégie and Montréal) are the Montreal CMA, and the parts of the Quebec CMA in Capitale-Nationale and Chaudière-Appalaches are the Quebec CMA.",
       notes_fr = "Sous-régions : les cinq parties de la RMR de Montréal (dans Lanaudière, les Laurentides, Laval, la Montérégie et Montréal) sont la RMR de Montréal, et les parties de la RMR de Québec dans la Capitale-Nationale et Chaudière-Appalaches sont la RMR de Québec.")
reg_id <- stats::setNames(as.list(levels_tab$name[levels_tab$levels_id == "rx_region_admin"]), 1:17)
# qes2012 and qes2018: the dictionary has no wording (qes2012's file label is
# the English question); the questionnaires give it. No English questionnaire
# of 2018 has Q0QC, so its row gives the French wording only.
rx_row("qes2012", "post", "region_admin", "q0qc", reg_id,
       wording_en = "In what region in Quebec do you live?",
       wording_fr = "Dans quelle région du Québec habitez-vous?",
       wording_ref = "425918:q0qc;196367:Q110;196370:Q110")
rx_row("qes2014", "post", "region_admin", "QREGION", reg_id)
rx_row("qes2018", "post", "region_admin", "q0qc", reg_id,
       wording_fr = "Dans quelle région du Québec demeurez-vous ?",
       wording_ref = "367181:Q0QC;425914:q0qc")
rx_row("qes2007", "post", "region_admin", "nomx",
       list("1" = reg_id[["1"]], "2" = reg_id[["2"]], "3,33" = reg_id[["3"]], "4" = reg_id[["4"]], "5" = reg_id[["5"]],
            "6" = reg_id[["6"]], "7" = reg_id[["7"]], "8" = reg_id[["8"]], "9" = reg_id[["9"]], "11" = reg_id[["11"]],
            "12,32" = reg_id[["12"]], "13" = reg_id[["13"]], "14,24" = reg_id[["14"]], "15,25" = reg_id[["15"]],
            "16,26" = reg_id[["16"]], "17" = reg_id[["17"]]),
       notes_en = "Sub-regions that split a region (its part in a CMA and the rest) are joined; Nord-du-Québec has no code.",
       notes_fr = "Les sous-régions qui divisent une région (sa partie dans une RMR et le reste) sont réunies ; le Nord-du-Québec n'a pas de code.")
rx_row("qes2007_panel", "*", "region_admin", "reg2",
       list("2" = reg_id[["1"]], "21" = reg_id[["2"]], "19,20" = reg_id[["3"]], "10" = reg_id[["4"]], "6" = reg_id[["5"]],
            "16" = reg_id[["6"]], "18" = reg_id[["7"]], "1" = reg_id[["8"]], "5" = reg_id[["9"]], "17" = reg_id[["10"]],
            "7" = reg_id[["11"]], "3,4" = reg_id[["12"]], "14" = reg_id[["13"]], "8,12" = reg_id[["14"]],
            "9,13" = reg_id[["15"]], "11,15" = reg_id[["16"]], "22" = reg_id[["17"]]),
       notes_en = "Sub-regions that split a region (its part in a CMA and the rest) are joined.",
       notes_fr = "Les sous-régions qui divisent une région (sa partie dans une RMR et le reste) sont réunies.")

# -- vote at the previous provincial election
rx_row("qes2007_panel", "pre", "vote_prev", "voteprec",
       list("1" = "ADQ", "2" = "PLQ", "3" = "PQ", "4" = "other", "6" = "NA:not_voted", "8" = "NA:dk", "9" = "NA:refused"),
       notes_en = "The election of April 2003 (QC2003); asked in the second pre-election poll only, so the other respondents are system missing.",
       notes_fr = "L'élection d'avril 2003 (QC2003) ; posée au deuxième sondage préélectoral seulement, si bien que les autres répondants sont des valeurs manquantes système.")
rx_row("qes_crop_2007_2010", "*", "vote_prev", "QP4",
       list("1" = "ADQ", "2" = "PLQ", "3" = "PQ", "4" = "QS", "5" = "PVQ", "6" = "other", "7" = "NA:not_voted",
            "9" = "NA:dk_refused"),
       notes_en = "The last Quebec election before each poll: QC2007 for the polls of June 2007 to November 2008, QC2008 from January 2009. The original question (QP4); code 7 joins not voted and spoiled.",
       notes_fr = "La dernière élection québécoise avant chaque sondage : QC2007 pour les sondages de juin 2007 à novembre 2008, QC2008 à partir de janvier 2009. La question originale (QP4) ; le code 7 réunit n'a pas voté et a annulé.")
rx_row("qes1998", "pre", "vote_prev", "vote94",
       list("1" = "PLQ", "2" = "PQ", "3" = "NA:not_voted", "4" = "NA:dk_refused", "9" = "NA:not_mappable"),
       notes_en = "The election of 1994 (QC1994). The pooled file names only the PLQ and the PQ; the unlabelled code 9 holds the other parties (the ADQ among them) and unknown answers, so 1994 ADQ voters are missing.",
       notes_fr = "L'élection de 1994 (QC1994). Le fichier regroupé ne nomme que le PLQ et le PQ ; le code 9, sans étiquette, réunit les autres partis (dont l'ADQ) et des réponses inconnues, si bien que les électeurs de l'ADQ en 1994 sont manquants.")

# -- satisfaction with the government (1998)
rx_row("qes1998", "pre", "gov_satisfaction", "satisf",
       list("1" = "very", "2" = "fairly", "3" = "not_very", "4" = "not_at_all", "9" = "NA:dk_refused"),
       notes_en = "Satisfaction with the Bouchard (PQ) government, in the pooled file's words: very satisfied, rather satisfied, rather dissatisfied, very dissatisfied; code 9 is unlabelled and read as don't know or refused.",
       notes_fr = "La satisfaction envers le gouvernement Bouchard (PQ), dans les mots du fichier regroupé : très satisfait, plutôt satisfait, plutôt insatisfait, très insatisfait ; le code 9, sans étiquette, est lu comme ne sait pas ou refus.")

# -- sovereignty (CROP polls)
rx_row("qes_crop_2007_2010", "*", "sovereignty", "intvoterefa", rule = "coalesce",
       args = "then=intvoterefb:rx_sovereignty_qes_crop_2007_2010_intvoterefb;fallthrough=dk",
       map = list("1" = "yes", "2" = "no", "3" = "NA:not_mappable", "8" = "NA:dk", "9" = "NA:refused"),
       then = list(var = "intvoterefb",
                   map = list("1" = "yes", "2" = "no", "3" = "NA:not_mappable", "8" = "NA:dk", "9" = "NA:refused")),
       notes_en = "OD-R8: the referendum question of the CROP polls. Its label is cut off in the file and the deposited codebook gives no more. CROP's reports give the wording for 19 of the 24 polls: whether Quebec should become a sovereign country (« devienne un pays souverain »). It is not documented for the other five; in April 2009 CROP asked both a sovereign-country and a sovereignty-partnership question, and for May 2009 the press does not say which one gave the published result. For those who did not know, the push (intvoterefb) is used. The push was also asked of those who would not vote or refused, whose first answer is kept. Would not vote is missing.",
       notes_fr = "OD-R8 : la question référendaire des sondages CROP. Le fichier tronque son étiquette et le livre de codes déposé n'en dit pas plus. Les rapports de CROP donnent le libellé pour 19 des 24 sondages : un Québec qui « devienne un pays souverain ». Il n'est pas documenté pour les cinq autres ; en avril 2009, CROP a posé une question sur le pays souverain et une autre sur la souveraineté-partenariat, et pour mai 2009 la presse ne dit pas laquelle a donné le résultat publié. Pour ceux qui ne savaient pas, la relance (intvoterefb) est utilisée. Elle a aussi été posée à ceux qui ne voteraient pas ou refusaient, dont la première réponse est gardée. Ne voterait pas est manquant.")
rx_row("qes_crop_2007_2010", "*", "sovereignty_type", "intvoterefa", rule = "constant",
       args = "value=crop_undocumented",
       notes_en = "OD-R8: every CROP poll respondent; sovereignty says whether they answered.",
       notes_fr = "OD-R8 : chaque répondant des sondages CROP ; sovereignty dit s'il a répondu.")

# ---- review ----------------------------------------------------------------------------------

# The rows signed off (status stable), as column|study|wave|source_var (a
# coalesce or fn: row by its first variable): the automated double review of
# 2026-10-01 against the original files and documents (one pass on the codes
# and the data, one on the wording, adjudicated where they disagreed), with
# the corrections it asked for applied above (spec 4.5.0). It is not a human
# review, and the rows say so. A row not listed stays in review, and
# qes_decon() does not apply it.
review_by <- "automated double review against original files and documents, 2026-10-01 (codes/data + wording; adjudicated where they disagreed); not a human review"
review_on <- as.Date("2026-10-01")
review_note <- "automated double review against original files and documents, 2026-10-01; not a human review"
signed_off <- c(
  "citizenship|qes2018_panel|pre|qa",
  "education|qes2022|cps|cps_edu",
  "education|qes2018|post|qscol",
  "education|qes2014|post|QSCOL",
  "education|qes2012|post|scol",
  "education|qes2018_panel|pre|d3",
  "education|qes2007_panel|*|scol",
  "education|qes2007|post|q77",
  "education|qes2008|post|q77",
  "education|qes1998|pre|scol",
  "education|qes_crop_2007_2010|*|scol",
  "income_cat|qes2022|cps|cps_income",
  "income_cat|qes2018|post|q61",
  "income_cat|qes2014|post|Q57",
  "income_cat|qes2012|post|reven",
  "income_cat|qes2018_panel|pre|d5",
  "income_cat|qes2007_panel|*|revenu",
  "income_cat|qes2007|post|q78",
  "income_cat|qes2008|post|q78",
  "income_cat|qes_crop_2007_2010|*|revenu",
  "language|qes2022|cps|cps_lang_2",
  "language|qes2014|post|QLANG",
  "language|qes2007|post|langu",
  "language|qes1998|pre|firme_post",
  "language_fr|qes2022|cps|cps_lang_2",
  "language_fr|qes2014|post|QLANG",
  "language_fr|qes2007|post|langu",
  "language_eng|qes2022|cps|cps_lang_1",
  "language_eng|qes2014|post|QLANG",
  "language_eng|qes2007|post|langu",
  "religion|qes2022|cps|cps_religion",
  "religion|qes2018|post|q67",
  "religion|qes2014|post|Q63",
  "religion|qes2012|post|q103",
  "marital|qes2022|pes|pes_married",
  "marital|qes2018|post|qstat",
  "marital|qes2014|post|Q68",
  "marital|qes2012|post|q109",
  "employment|qes2022|pes|pes_employed",
  "employment|qes2018|post|qoccup",
  "employment|qes2014|post|Q58",
  "employment|qes2012|post|occup",
  "employment|qes2018_panel|pre|d4",
  "employment|qes2007_panel|*|occup",
  "employment|qes2007|post|q79",
  "employment|qes2008|post|q79",
  "employment|qes1998|pre|occup",
  "employment|qes_crop_2007_2010|*|Occup",
  "region|qes2007_panel|*|reg2",
  "region_admin|qes2018|post|q0qc",
  "region_admin|qes2014|post|QREGION",
  "region_admin|qes2012|post|q0qc",
  "region_admin|qes2007_panel|*|reg2",
  "region_admin|qes2007|post|nomx",
  "vote_prev|qes2007_panel|pre|voteprec",
  "vote_prev|qes1998|pre|vote94",
  "vote_prev|qes_crop_2007_2010|*|QP4",
  "sovereignty|qes_crop_2007_2010|*|intvoterefa",
  "sovereignty_type|qes_crop_2007_2010|*|intvoterefa",
  "gov_satisfaction|qes1998|pre|satisf"
)

# ---- build the tables -----------------------------------------------------------------

short <- function(s) s
level_code <- function(column, name) {
  id <- relaxed$levels_id[match(column, relaxed$column)]
  set <- .qes_spec_levels(levels_tab, id)
  code <- set$code[match(name, set$name)]
  if (is.na(code)) stop(sprintf("level %s is not in the set of %s", name, column))
  code
}
data_file <- function(study) {
  f <- cat_$files
  f$file_id[f$study == study & f$role == "data" & f$is_default %in% TRUE][1]
}
dict_var <- function(study, var) dict$variables[dict$variables$study == study & dict$variables$variable == var, , drop = FALSE]
dict_val <- function(study, var) dict$values[dict$values$study == study & dict$values$variable == var, , drop = FALSE]
strict_label <- function(study, var, code) {
  # the label a strict value map quotes for this variable's code (questionnaire
  # labels of codes the file leaves unlabelled)
  xw <- .qes_read_csv(file.path(dir, "crosswalk.csv"), "spec_crosswalk")
  ids <- unique(xw$map_id[xw$study == study & xw$source_var == var & !is.na(xw$map_id)])
  m <- vm_old[vm_old$map_id %in% ids & vm_old$source_code == code & !is.na(vm_old$source_label), , drop = FALSE]
  if (nrow(m) == 0L) return(NULL)
  m[1, , drop = FALSE]
}
map_rows <- function(id, study, var, column, map, code_notes = NULL) {
  val <- dict_val(study, var)
  out <- list()
  for (codes in names(map)) {
    to <- map[[codes]]
    for (code in strsplit(codes, ",", fixed = TRUE)[[1]]) {
      j <- match(code, val$value)
      lab <- if (is.na(j)) NA_character_ else val$label[j]
      origin <- if (is.na(j)) NA_character_ else val$label_source[j]
      note <- NA_character_
      if (is.na(lab) || origin %in% c("none", NA)) {
        donor <- strict_label(study, var, code)
        if (!is.null(donor)) {
          lab <- donor$source_label
          origin <- donor$source_label_origin
          note <- donor$note
        } else {
          lab <- NA_character_
          origin <- NA_character_
          note <- "unlabelled in the file"
        }
      }
      if (!is.null(code_notes[[code]])) note <- code_notes[[code]]
      na <- startsWith(to, "NA:")
      out[[length(out) + 1L]] <- data.frame(
        map_id = id, source_code = code, source_label = lab, source_label_hash = NA_character_,
        source_label_origin = origin, target_code = if (na) NA_integer_ else level_code(column, to),
        na_reason = if (na) sub("^NA:", "", to) else NA_character_, alias_exception = NA_character_,
        note = note, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}
evidence_of <- function(study, var, map, count_notes = NULL) {
  val <- dict_val(study, var)
  parts <- vapply(names(map), function(codes) {
    n <- sum(val$n[val$value %in% strsplit(codes, ",", fixed = TRUE)[[1]]], na.rm = TRUE)
    extra <- if (is.null(count_notes[[codes]])) "" else paste0(": ", count_notes[[codes]])
    sprintf("%s = %s (%d%s)", gsub(",", ", ", codes), map[[codes]], n, extra)
  }, character(1))
  sprintf("Every code of %s in the dictionary has an outcome (V-R9). Dictionary counts, all rows of file %s: %s.",
          var, data_file(study), paste(parts, collapse = "; "))
}

label_en_studies <- "qes2012"
maps <- list()
out <- list()
for (r in rows) {
  id <- NA_character_
  if (r$rule %in% c("map", "coalesce")) {
    id <- tolower(paste("rx", r$column, r$study, r$var, sep = "_"))
    maps[[length(maps) + 1L]] <- map_rows(id, r$study, r$var, r$column, r$map, r$code_notes)
  }
  if (!is.null(r$then)) {
    tid <- tolower(paste("rx", r$column, r$study, r$then$var, sep = "_"))
    maps[[length(maps) + 1L]] <- map_rows(tid, r$study, r$then$var, r$column, r$then$map)
    if (!grepl(tid, r$args, fixed = TRUE)) stop(sprintf("args of %s %s must name map %s", r$study, r$column, tid))
  }
  v <- dict_var(r$study, r$var)
  if (nrow(v) != 1L) stop(sprintf("%s is not a variable of %s in the dictionary", r$var, r$study))
  # in each language, the row's own wording, else the dictionary's question;
  # when the dictionary has no question, the file's variable label (often the
  # question) goes with the language of the study's labels: English for
  # qes2012, French otherwise, so that an English label never sits in
  # wording_fr
  wording_en <- if (!is.na(r$wording_en)) r$wording_en else v$question_en
  wording_fr <- if (!is.na(r$wording_fr)) r$wording_fr else v$question_fr
  if (is.na(v$question_en) && is.na(v$question_fr) && !is.na(v$label)) {
    if (r$study %in% label_en_studies) {
      if (is.na(wording_en)) wording_en <- v$label
    } else if (is.na(wording_fr)) {
      wording_fr <- v$label
    }
  }
  ref <- r$wording_ref
  if (is.na(ref)) ref <- if (!is.na(v$doc_ref)) v$doc_ref else paste0(data_file(r$study), ":", r$var)
  evidence <- if (identical(r$rule, "map")) evidence_of(r$study, r$var, r$map, r$count_notes) else
    sprintf("Rule %s on %s (file %s); checked on the pinned file by data-raw/build_relaxed.R --expected, counts in expected/relaxed_marginals.csv.",
            r$rule, paste(c(r$var, r$then$var, r$gate$var), collapse = ", "), data_file(r$study))
  stable <- paste(r$column, r$study, r$wave, r$var, sep = "|") %in% signed_off
  out[[length(out) + 1L]] <- data.frame(
    study = r$study, wave = r$wave, column = r$column, rule = r$rule, source_var = r$var, map_id = id,
    args = r$args, na_codes = r$na_codes,
    gate_var = if (is.null(r$gate)) NA_character_ else r$gate$var,
    gate_codes = if (is.null(r$gate)) NA_character_ else paste(names(r$gate$to), collapse = ";"),
    gate_to = if (is.null(r$gate)) NA_character_ else paste(names(r$gate$to), r$gate$to, sep = "=", collapse = ";"),
    override = r$override, wording_en = wording_en, wording_fr = wording_fr, wording_ref = ref,
    notes_en = r$notes_en, notes_fr = r$notes_fr, evidence = evidence,
    reviewed_by = if (stable) review_by else NA_character_, reviewed_on = if (stable) review_on else as.Date(NA),
    review_note = if (stable) review_note else NA_character_, status = if (stable) "stable" else "review",
    added_in = "4.4.0",
    stringsAsFactors = FALSE
  )
}
rm_new <- do.call(rbind, out)
unknown <- setdiff(signed_off, paste(rm_new$column, rm_new$study, rm_new$wave, rm_new$source_var, sep = "|"))
if (length(unknown) > 0L) stop("signed off but not a relaxed row: ", paste(unknown, collapse = ", "))
rm_new <- rm_new[order(match(rm_new$column, relaxed$column), match(rm_new$study, unique(.qes_read_csv(file.path(dir, "waves.csv"), "spec_waves")$study))), , drop = FALSE]
rownames(rm_new) <- NULL
vm_rx <- do.call(rbind, maps)
vm_rx <- vm_rx[!duplicated(paste(vm_rx$map_id, vm_rx$source_code)), , drop = FALSE]

# ---- write ----------------------------------------------------------------------------------

# CSV text as the spec files are written by hand: fields quoted only when
# they need it, LF line endings.
csv_text <- function(x) {
  field <- function(v) {
    out <- if (is.logical(v)) ifelse(v, "TRUE", "FALSE") else if (is.numeric(v)) .qes_code_chr(v) else
      if (inherits(v, "Date")) format(v, "%Y-%m-%d") else enc2utf8(as.character(v))
    out[is.na(v)] <- ""
    q <- grepl("[\",\n]", out)
    out[q] <- paste0("\"", gsub("\"", "\"\"", out[q], fixed = TRUE), "\"")
    out
  }
  body <- do.call(paste, c(lapply(x, field), sep = ","))
  paste0(paste(c(paste(names(x), collapse = ","), body), collapse = "\n"), "\n")
}
write_text <- function(text, path) {
  old <- if (file.exists(path)) paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
  if (identical(paste0(old, "\n"), text)) return(FALSE)
  if (check_only) {
    message("would change: ", path)
    return(TRUE)
  }
  con <- file(path, open = "wb")
  writeBin(charToRaw(enc2utf8(text)), con)
  close(con)
  message("wrote ", path)
  TRUE
}
changed <- write_text(csv_text(rm_new), file.path(dir, "relaxed_maps.csv"))
# valuemaps.csv: the rows of the strict maps stay as they are; the rx_ rows
# are replaced
vm_path <- file.path(dir, "valuemaps.csv")
lines <- readLines(vm_path, encoding = "UTF-8", warn = FALSE)
keep <- lines[!startsWith(lines, "rx_")]
rx_lines <- strsplit(sub("\n$", "", csv_text(vm_rx)), "\n", fixed = TRUE)[[1]][-1]
changed <- write_text(paste0(paste(c(keep, rx_lines), collapse = "\n"), "\n"), vm_path) || changed

# ---- recorded results on the pinned files ---------------------------------------------------
if (with_expected) {
  .qes_spec_reset()
  res <- .qes_decon_compute(studies = NULL, lang = "en", quiet = TRUE, include_review = TRUE)
  x <- res$lead
  rx <- res$rx
  marg <- list()
  hashes <- list()
  for (col in rx$column) {
    static <- rx$timing[match(col, rx$column)] %in% "static"
    v <- res$value[[col]]
    r <- res$reason[[col]]
    for (s in unique(x$study)) {
      k <- which(x$study == s)
      if (static) k <- k[!duplicated(x$qes_id[k])]
      w <- if (static) rep("*", length(k)) else ifelse(is.na(x$wave[k]), "NA", x$wave[k])
      key <- paste(w, ifelse(is.na(v[k]), "", v[k]), ifelse(is.na(v[k]), r[k], ""), sep = "\x1f")
      tab <- table(key)
      p <- strsplit(names(tab), "\x1f", fixed = TRUE)
      marg[[length(marg) + 1L]] <- data.frame(
        column = col, study = s, wave = vapply(p, `[`, "", 1L),
        value = vapply(p, function(z) if (nzchar(z[2])) z[2] else NA_character_, ""),
        na_reason = vapply(p, function(z) if (length(z) > 2L && nzchar(z[3])) z[3] else NA_character_, ""),
        n = as.integer(tab), stringsAsFactors = FALSE
      )
      cell <- ifelse(is.na(v[k]), paste0("NA:", r[k]), v[k])
      hashes[[length(hashes) + 1L]] <- data.frame(column = col, study = s, n = length(k),
                                                  md5 = .qes_md5_text(paste(cell, collapse = "\n")),
                                                  stringsAsFactors = FALSE)
    }
  }
  marg <- do.call(rbind, marg)
  marg <- marg[order(match(marg$column, rx$column), marg$study, marg$wave, is.na(marg$value), marg$value,
                     marg$na_reason, method = "radix"), , drop = FALSE]
  hashes <- do.call(rbind, hashes)
  changed <- write_text(csv_text(marg), file.path(dir, "expected", "relaxed_marginals.csv")) || changed
  changed <- write_text(csv_text(hashes), file.path(dir, "expected", "relaxed_hashes.csv")) || changed
}
if (check_only && changed) quit(status = 1L)
