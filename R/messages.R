# Message table (design.md section 7).
#
# One entry per condition or notice: list(key = c(en = "...", fr = "...")).
# Both languages use the same positional sprintf() placeholders (%1$s, %2$s,
# ...), which a test checks. Non-ASCII characters are written as \u escapes so
# the sources stay ASCII. Keys are added in the slice that first uses them.
#
# French typography: guillemets with no-break spaces (\u00ab\u00a0...\u00a0\u00bb)
# and a no-break space before "?", ":" and ";".

.qes_messages <- list(
  # ---- shared fragments ----------------------------------------------------
  caused_by = c(
    en = "Caused by: %1$s",
    fr = "Cause\u00a0: %1$s"
  ),
  details = c(
    en = "Details: %1$s",
    fr = "D\u00e9tails\u00a0: %1$s"
  ),

  # ---- qesR_error_input ----------------------------------------------------
  input_string = c(
    en = "`%1$s` must be a single non-empty character string.",
    fr = "`%1$s` doit \u00eatre une seule cha\u00eene de caract\u00e8res non vide."
  ),
  input_obs = c(
    en = "`obs` must be a single whole number greater than or equal to 1.",
    fr = "`obs` doit \u00eatre un seul nombre entier sup\u00e9rieur ou \u00e9gal \u00e0 1."
  ),
  input_codebook = c(
    en = "`codebook` must be a data.frame returned by qes_codebook().",
    fr = "`codebook` doit \u00eatre un data.frame renvoy\u00e9 par qes_codebook()."
  ),
  input_srvy_or_codebook = c(
    en = "Provide `srvy` or `codebook`.",
    fr = "Indiquez `srvy` ou `codebook`."
  ),
  input_codes = c(
    en = "`%1$s` must be a non-empty character vector of study codes.",
    fr = "`%1$s` doit \u00eatre un vecteur non vide de codes d'\u00e9tude."
  ),
  input_all_mixed = c(
    en = "`%1$s`: \"all\" must be used on its own, not together with study codes.",
    fr = "`%1$s`\u00a0: \u00ab\u00a0all\u00a0\u00bb s'utilise seul, sans autre code d'\u00e9tude."
  ),
  input_do = c(
    en = "`do` must be a data.frame or the name of one in the calling environment.",
    fr = "`do` doit \u00eatre un data.frame ou le nom d'un data.frame de l'environnement appelant."
  ),
  input_object_missing = c(
    en = "Object %1$s was not found in the calling environment.",
    fr = "L'objet %1$s est introuvable dans l'environnement appelant."
  ),
  input_save_dir = c(
    en = "Directory does not exist: %1$s.",
    fr = "Le dossier n'existe pas\u00a0: %1$s."
  ),

  # ---- qesR_error_unknown_study / _unknown_variable / _ambiguous_file ------
  unknown_study = c(
    en = "Unknown study code %1$s. See get_qescodes() for valid codes.",
    fr = "Code d'\u00e9tude inconnu %1$s. Voir get_qescodes() pour les codes valides."
  ),
  unknown_study_suggest = c(
    en = "Unknown study code %1$s. Did you mean %2$s?",
    fr = "Code d'\u00e9tude inconnu %1$s. Vouliez-vous dire %2$s\u00a0?"
  ),
  unknown_variable = c(
    en = "Column %1$s was not found in the data.",
    fr = "La colonne %1$s est introuvable dans les donn\u00e9es."
  ),
  unknown_variable_suggest = c(
    en = "Column %1$s was not found in the data. Close matches: %2$s.",
    fr = "La colonne %1$s est introuvable dans les donn\u00e9es. Correspondances proches\u00a0: %2$s."
  ),
  file_no_match = c(
    en = "No file of %1$s matches %2$s. Available files: %3$s.",
    fr = "Aucun fichier de %1$s ne correspond \u00e0 %2$s. Fichiers disponibles\u00a0: %3$s."
  ),

  # ---- qesR_error_network --------------------------------------------------
  network = c(
    en = "Could not download %1$s.",
    fr = "Impossible de t\u00e9l\u00e9charger %1$s."
  ),

  # ---- qesR_error_source ---------------------------------------------------
  source_metadata = c(
    en = "Dataverse did not return metadata for %1$s (doi:%2$s).",
    fr = "Dataverse n'a pas renvoy\u00e9 les m\u00e9tadonn\u00e9es de %1$s (doi:%2$s)."
  ),
  source_no_files = c(
    en = "No files were found for study %1$s.",
    fr = "Aucun fichier trouv\u00e9 pour l'\u00e9tude %1$s."
  ),
  source_zip_empty = c(
    en = "The zip archive contains no files.",
    fr = "L'archive zip ne contient aucun fichier."
  ),
  source_zip_no_data = c(
    en = "No supported data file was found in the zip archive.",
    fr = "L'archive zip ne contient aucun fichier de donn\u00e9es pris en charge."
  ),
  source_format = c(
    en = "qesR does not read files of type %1$s.",
    fr = "qesR ne lit pas les fichiers de type %1$s."
  ),
  master_none = c(
    en = "No study could be loaded. Check network access and study availability.",
    fr = "Aucune \u00e9tude n'a pu \u00eatre charg\u00e9e. V\u00e9rifiez l'acc\u00e8s au r\u00e9seau et la disponibilit\u00e9 des \u00e9tudes."
  ),
  master_strict = c(
    en = "The master build failed for %1$s study(ies): %2$s",
    fr = "La construction du fichier fusionn\u00e9 a \u00e9chou\u00e9 pour %1$s \u00e9tude(s)\u00a0: %2$s"
  ),

  # ---- warnings ------------------------------------------------------------
  question_missing = c(
    en = "No question label was found for %1$s.",
    fr = "Aucun libell\u00e9 de question trouv\u00e9 pour %1$s."
  ),

  # ---- once-per-session notices -------------------------------------------
  deprecated = c(
    en = "`%1$s()` is soft-deprecated; use `%2$s`. It keeps working and will not be removed.",
    fr = "`%1$s()` est obsol\u00e8te (d\u00e9pr\u00e9ciation douce)\u00a0; utilisez `%2$s`. Elle continue de fonctionner et ne sera pas retir\u00e9e."
  ),
  assign_default = c(
    en = paste0(
      "%1$s() returns its result and no longer assigns it into your workspace ",
      "by default. Write `%2$s <- %1$s(...)`, or pass `assign_global = TRUE`. ",
      "This note is shown once per session."
    ),
    fr = paste0(
      "%1$s() renvoie son r\u00e9sultat et ne l'assigne plus par d\u00e9faut dans ",
      "votre espace de travail. \u00c9crivez `%2$s <- %1$s(...)`, ou passez ",
      "`assign_global = TRUE`. Cette note s'affiche une fois par session."
    )
  ),

  # ---- progress (silenced by quiet = TRUE) ---------------------------------
  get_qes_banner = c(
    en = "%1$s: %2$s\nDOI: %3$s\nDocumentation: %4$s",
    fr = "%1$s\u00a0: %2$s\nDOI\u00a0: %3$s\nDocumentation\u00a0: %4$s"
  ),
  codebook_counts = c(
    en = "Codebook variables available: %1$s\nCodebook/support files available: %2$s",
    fr = "Variables du codebook\u00a0: %1$s\nFichiers de documentation\u00a0: %2$s"
  ),
  no_codebook_files = c(
    en = "No codebook/support files found for %1$s.",
    fr = "Aucun fichier de documentation trouv\u00e9 pour %1$s."
  ),
  master_skip = c(
    en = "Skipping %1$s due to a download or read error.",
    fr = "%1$s est ignor\u00e9e \u00e0 cause d'une erreur de t\u00e9l\u00e9chargement ou de lecture."
  ),
  master_rows_loaded = c(
    en = "[%1$s] rows loaded: %2$s",
    fr = "[%1$s] lignes charg\u00e9es\u00a0: %2$s"
  ),
  master_n_rows = c(
    en = "Master dataset rows: %1$s",
    fr = "Lignes du fichier fusionn\u00e9\u00a0: %1$s"
  ),
  master_n_loaded = c(
    en = "Master dataset studies loaded: %1$s",
    fr = "\u00c9tudes charg\u00e9es dans le fichier fusionn\u00e9\u00a0: %1$s"
  ),
  master_n_skipped = c(
    en = "Master dataset studies skipped: %1$s",
    fr = "\u00c9tudes ignor\u00e9es\u00a0: %1$s"
  ),
  master_n_dedup = c(
    en = "Master dataset within-study duplicate respondents removed: %1$s",
    fr = "R\u00e9pondants en double retir\u00e9s (au sein d'une m\u00eame \u00e9tude)\u00a0: %1$s"
  ),
  master_n_empty = c(
    en = "Master dataset all-empty rows removed: %1$s",
    fr = "Lignes enti\u00e8rement vides retir\u00e9es\u00a0: %1$s"
  ),
  master_n_extra = c(
    en = "Master dataset cross-study variables added: %1$s",
    fr = "Variables communes \u00e0 plusieurs \u00e9tudes ajout\u00e9es\u00a0: %1$s"
  ),
  master_n_renamed = c(
    en = "Master dataset opaque legacy variables renamed: %1$s",
    fr = "Variables h\u00e9rit\u00e9es opaques renomm\u00e9es\u00a0: %1$s"
  ),
  master_map_saved = c(
    en = "Master dataset variable name map saved to: %1$s",
    fr = "Table de correspondance des noms enregistr\u00e9e dans\u00a0: %1$s"
  ),
  master_saved = c(
    en = "Master dataset saved to: %1$s",
    fr = "Fichier fusionn\u00e9 enregistr\u00e9 dans\u00a0: %1$s"
  )
)
