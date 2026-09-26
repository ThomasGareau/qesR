# Build the synthetic demonstration study `qes_demo` (design.md sections 3.1
# and 8.1).
#
# Usage (from the package root): Rscript data-raw/make_demo.R
#
# Writes
#   inst/extdata/demo/data/qes_demo.sav      60 synthetic respondents;
#   inst/extdata/demo/catalog/studies.csv    one catalog row;
#   inst/extdata/demo/catalog/files.csv      one file row, with the .sav md5.
#
# No row comes from a real respondent: every value is drawn at random with a
# fixed seed. Variable names, variable labels and value labels are a subset of
# the Quebec Election Study 2014 SPSS file (10.5683/SP3/64F7WR, file 425916,
# CC0 1.0), so code written for qes2014 runs on the demo. Non-ASCII text is
# written with \u escapes so this script is ASCII.
#
# The .sav header records when it was written, so every run gives a new md5;
# files.csv is rewritten with it. Commit the three files together.

set.seed(20260926)
n <- 60L

lab <- function(x, label, labels = NULL) {
  if (!is.null(labels)) {
    x <- haven::labelled(x, labels = labels, label = label)
  } else {
    attr(x, "label") <- label
  }
  x
}

no_answer <- "Je pr\u00e9f\u00e8re ne pas r\u00e9pondre"
dont_know <- "Je ne sais pas"
yes_no <- stats::setNames(c(1, 2, 9), c("Oui", "Non", no_answer))
parties <- stats::setNames(
  c(1, 2, 3, 4, 5, 6, 96, 99),
  c(
    "Parti lib\u00e9ral du Qu\u00e9bec", "Parti qu\u00e9b\u00e9cois",
    "Coalition avenir Qu\u00e9bec", "Qu\u00e9bec solidaire",
    "Parti vert du Qu\u00e9bec", "Option nationale", "Un autre parti", no_answer
  )
)

turnout <- sample(c(1, 2, 9), n, replace = TRUE, prob = c(0.85, 0.12, 0.03))
vote <- ifelse(
  turnout == 1,
  sample(c(1, 2, 3, 4, 5, 6, 96, 99), n, replace = TRUE,
         prob = c(0.38, 0.25, 0.22, 0.08, 0.02, 0.01, 0.01, 0.03)),
  NA_real_
)

demo <- data.frame(
  QUEST = seq_len(n),
  stringsAsFactors = FALSE
)
attr(demo$QUEST, "label") <- "Identifiant synth\u00e9tique"
demo$LANG <- lab(
  sample(c("FR", "EN"), n, replace = TRUE, prob = c(0.8, 0.2)),
  paste0(
    "Pr\u00e9f\u00e8reriez-vous r\u00e9pondre \u00e0 ce questionnaire en anglais ou en fran\u00e7ais?  ",
    "Would you prefer to complete the survey in English or French?"
  ),
  c(English = "EN", "Fran\u00e7ais" = "FR")
)
demo$QAGE <- lab(
  as.numeric(sample(1930:1995, n, replace = TRUE)),
  "En quelle ann\u00e9e \u00eates-vous n\u00e9(e)? / Entrez l'ann\u00e9e de naissance",
  stats::setNames(9999, no_answer)
)
demo$QSEXE <- lab(
  as.numeric(sample(1:2, n, replace = TRUE)),
  "Quel est votre sexe?",
  stats::setNames(c(1, 2), c("Masculin", "F\u00e9minin"))
)
demo$QREGION <- lab(
  as.numeric(sample(c(3, 6, 13, 16), n, replace = TRUE)),
  "Dans quelle r\u00e9gion du Qu\u00e9bec habitez-vous?",
  stats::setNames(
    c(3, 6, 13, 16),
    c("Capitale-Nationale", "Montr\u00e9al", "Laval", "Mont\u00e9r\u00e9gie")
  )
)
demo$Q2 <- lab(turnout, "Avez-vous vot\u00e9 \u00e0 cette \u00e9lection provinciale?", yes_no)
demo$Q3 <- lab(vote, "Pour quel parti avez-vous vot\u00e9?", parties)
demo$Q19 <- lab(
  sample(c(1, 2, 8, 9), n, replace = TRUE, prob = c(0.35, 0.55, 0.07, 0.03)),
  paste0(
    "Si un r\u00e9f\u00e9rendum sur l'ind\u00e9pendance avait lieu vous demandant si vous ",
    "voulez que le Qu\u00e9bec devienne un pays ind\u00e9pendant, voteriez-vous OUI ou ",
    "voteriez-vous NON?"
  ),
  stats::setNames(c(1, 2, 8, 9), c("Oui", "Non", dont_know, no_answer))
)
demo$Q28 <- lab(
  sample(c(1, 2, 3, 4, 8, 9), n, replace = TRUE, prob = c(0.2, 0.45, 0.22, 0.08, 0.03, 0.02)),
  "Quel est votre int\u00e9r\u00eat pour la politique en g\u00e9n\u00e9ral? \u00cates-vous:",
  stats::setNames(
    c(1, 2, 3, 4, 8, 9),
    c(
      "Tr\u00e8s int\u00e9ress\u00e9(e)", "Plut\u00f4t int\u00e9ress\u00e9(e)",
      "Pas tr\u00e8s int\u00e9ress\u00e9(e)", "Pas du tout int\u00e9ress\u00e9(e)",
      dont_know, no_answer
    )
  )
)
demo$Q32 <- lab(
  as.numeric(sample(c(0:10, 98, 99), n, replace = TRUE,
                    prob = c(rep(0.9 / 11, 11), 0.07, 0.03))),
  "Et sur la m\u00eame \u00e9chelle, o\u00f9 vous placeriez-vous, de mani\u00e8re g\u00e9n\u00e9rale?",
  stats::setNames(
    c(0, 10, 98, 99),
    c("0: Le plus \u00e0 gauche", "10: Le plus \u00e0 droite", dont_know, no_answer)
  )
)
demo$POND <- round(stats::runif(n, 0.4, 2.2), 4)
demo$POND <- demo$POND / mean(demo$POND)
attr(demo$POND, "label") <- "Pond\u00e9ration"

data_dir <- file.path("inst", "extdata", "demo", "data")
cat_dir <- file.path("inst", "extdata", "demo", "catalog")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cat_dir, recursive = TRUE, showWarnings = FALSE)
sav <- file.path(data_dir, "qes_demo.sav")
haven::write_sav(demo, sav)
md5 <- unname(tools::md5sum(sav))
bytes <- file.info(sav)$size

write_lines <- function(lines, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))), con)
}

write_lines(c(
  paste(
    "study,aliases,family,title_deposit,title_en,title_fr,authors,year,year_end,election_id,",
    "study_design,default_member,target_population_en,target_population_fr,server,doi,",
    "dataset_version,data_file_id,label_file_id,source_lang,licence,licence_url,",
    "metadata_shipped,publisher,citation_year,dataset_unf,notes_en,notes_fr",
    sep = ""
  ),
  paste0(
    "qes_demo,,demo,,qesR demonstration data (synthetic),",
    "Donn\u00e9es de d\u00e9monstration de qesR (synth\u00e9tiques),,2014,2014,QC2014,",
    "post,FALSE,None (synthetic data),Aucune (donn\u00e9es synth\u00e9tiques),,,1.0,0,,fr,",
    "CC0 1.0,http://creativecommons.org/publicdomain/zero/1.0,TRUE,,,,",
    "\"60 synthetic respondents drawn at random. Variable names and labels are a subset of ",
    "the Quebec Election Study 2014 SPSS file (CC0); no value comes from a real respondent.\",",
    "\"60 r\u00e9pondants synth\u00e9tiques tir\u00e9s au hasard. Les noms et \u00e9tiquettes des ",
    "variables sont un sous-ensemble du fichier SPSS de l'\u00c9tude \u00e9lectorale ",
    "qu\u00e9b\u00e9coise 2014 (CC0) ; aucune valeur ne vient d'un vrai r\u00e9pondant.\""
  )
), file.path(cat_dir, "studies.csv"))

write_lines(c(
  paste(
    "study,file_id,role,lang,file_name,original_file_name,format,ingested,bytes,md5,",
    "checksum_type,unf,n_rows,n_cols,encoding,id_vars,is_default,dataset_version",
    sep = ""
  ),
  sprintf(
    "qes_demo,0,data,,qes_demo.sav,qes_demo.sav,sav,FALSE,%s,%s,MD5,,%d,%d,,QUEST,TRUE,1.0",
    format(bytes, scientific = FALSE), md5, nrow(demo), ncol(demo)
  )
), file.path(cat_dir, "files.csv"))

cat(sprintf("qes_demo: %d rows, %d columns, md5 %s\n", nrow(demo), ncol(demo), md5))
