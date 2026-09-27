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
  input_regex = c(
    en = "`%1$s` must be a valid regular expression; %2$s is not.",
    fr = "`%1$s` doit \u00eatre une expression r\u00e9guli\u00e8re valide\u00a0; %2$s ne l'est pas."
  ),
  input_obs = c(
    en = "`obs` must be a single whole number greater than or equal to 1.",
    fr = "`obs` doit \u00eatre un seul nombre entier sup\u00e9rieur ou \u00e9gal \u00e0 1."
  ),
  input_codebook = c(
    en = "`codebook` must be a data.frame returned by qes_codebook().",
    fr = "`codebook` doit \u00eatre un data.frame renvoy\u00e9 par qes_codebook()."
  ),
  input_codebook_plain = c(
    en = "`%1$s` is a plain data frame, not a qesR codebook or data read by get_qes() (a codebook read back from a file loses its class). Rebuild it with qes_codebook(\"<study code>\").",
    fr = "`%1$s` est un simple data.frame, et non un codebook de qesR ni des donn\u00e9es lues par get_qes() (un codebook relu depuis un fichier perd sa classe). Reconstruisez-le avec qes_codebook(\"<code d'\u00e9tude>\")."
  ),
  input_codebook_data = c(
    en = "`%1$s` is data read by get_qes(), not a codebook. Build its codebook with qes_codebook(<data>, layout = ...).",
    fr = "`%1$s` contient des donn\u00e9es lues par get_qes(), et non un codebook. Construisez son codebook avec qes_codebook(<donn\u00e9es>, layout = ...)."
  ),
  input_codebook_srvy = c(
    en = "`%1$s` must be a study code, a codebook returned by qes_codebook() or a data frame returned by get_qes().",
    fr = "`%1$s` doit \u00eatre un code d'\u00e9tude, un codebook renvoy\u00e9 par qes_codebook() ou un data.frame renvoy\u00e9 par get_qes()."
  ),
  input_missing_data = c(
    en = "`%1$s` must be a data frame returned by get_qes().",
    fr = "`%1$s` doit \u00eatre un data.frame renvoy\u00e9 par get_qes()."
  ),
  input_srvy_or_codebook = c(
    en = "Provide `srvy` or `codebook`.",
    fr = "Indiquez `srvy` ou `codebook`."
  ),
  input_codes = c(
    en = "`%1$s` must be a non-empty character vector of study codes.",
    fr = "`%1$s` doit \u00eatre un vecteur non vide de codes d'\u00e9tude."
  ),
  input_variables = c(
    en = "`%1$s` must be a non-empty character vector of variable names.",
    fr = "`%1$s` doit \u00eatre un vecteur non vide de noms de variables."
  ),
  input_all_mixed = c(
    en = "`%1$s`: \"all\" must be used on its own, not together with study codes.",
    fr = "`%1$s`\u00a0: \u00ab\u00a0all\u00a0\u00bb s'utilise seul, sans autre code d'\u00e9tude."
  ),
  input_choice = c(
    en = "`%1$s` must be one or more of %2$s.",
    fr = "`%1$s` doit prendre une ou plusieurs des valeurs %2$s."
  ),
  input_flag = c(
    en = "`%1$s` must be TRUE or FALSE.",
    fr = "`%1$s` doit valoir TRUE ou FALSE."
  ),
  input_do = c(
    en = "`do` must be a data.frame or the name of one in the calling environment.",
    fr = "`do` doit \u00eatre un data.frame ou le nom d'un data.frame de l'environnement appelant."
  ),
  input_object_missing = c(
    en = "Object %1$s was not found in the calling environment.",
    fr = "L'objet %1$s est introuvable dans l'environnement appelant."
  ),
  input_master_study = c(
    en = "get_qes_master() builds only the qesR 0.4.4 studies; %1$s will be added to the master in qesR 0.7.0. Read it on its own with get_qes().",
    fr = "get_qes_master() ne construit que les \u00e9tudes de qesR 0.4.4\u00a0; %1$s sera ajout\u00e9 au fichier fusionn\u00e9 dans qesR 0.7.0. Lisez-le seul avec get_qes()."
  ),
  input_decon_study = c(
    en = "get_decon() builds only the qesR 0.4.4 studies (and \"qes_demo\"), one at a time; %1$s is not one of them. Read other studies with get_qes().",
    fr = "get_decon() ne construit que les \u00e9tudes de qesR 0.4.4 (et \u00ab\u00a0qes_demo\u00a0\u00bb), une \u00e0 la fois\u00a0; %1$s n'en fait pas partie. Lisez les autres \u00e9tudes avec get_qes()."
  ),
  input_save_dir = c(
    en = "Directory does not exist: %1$s.",
    fr = "Le dossier n'existe pas\u00a0: %1$s."
  ),

  input_option_count = c(
    en = "Option %1$s must be a whole number of at least 1.",
    fr = "L'option %1$s doit \u00eatre un nombre entier sup\u00e9rieur ou \u00e9gal \u00e0 1."
  ),
  input_option_choice = c(
    en = "Option %1$s must be one of %2$s.",
    fr = "L'option %1$s doit prendre l'une des valeurs %2$s."
  ),
  input_option_dir = c(
    en = "Option %1$s must be the path of an existing directory.",
    fr = "L'option %1$s doit \u00eatre le chemin d'un dossier existant."
  ),
  input_choice_one = c(
    en = "`%1$s` must be one of %2$s.",
    fr = "`%1$s` doit prendre l'une des valeurs %2$s."
  ),
  input_path_dir = c(
    en = "`path` must be an existing directory. qesR writes only into a directory you have created.",
    fr = "`path` doit \u00eatre un dossier existant. qesR n'\u00e9crit que dans un dossier que vous avez cr\u00e9\u00e9."
  ),
  download_exists = c(
    en = "These files already exist and differ from the files to download: %1$s. Nothing was written. Use `overwrite = TRUE` to replace them, or choose another `path`.",
    fr = "Ces fichiers existent d\u00e9j\u00e0 et diff\u00e8rent des fichiers \u00e0 t\u00e9l\u00e9charger\u00a0: %1$s. Rien n'a \u00e9t\u00e9 \u00e9crit. Utilisez `overwrite = TRUE` pour les remplacer, ou choisissez un autre `path`."
  ),
  download_write = c(
    en = "qesR could not write into %1$s.",
    fr = "qesR n'a pas pu \u00e9crire dans %1$s."
  ),
  download_latest_demo = c(
    en = "%1$s ships with qesR and has no Dataverse deposit, so it has no latest version. Use `version = \"pinned\"`.",
    fr = "%1$s est fourni avec qesR et n'a pas de d\u00e9p\u00f4t Dataverse, donc pas de derni\u00e8re version. Utilisez `version = \"pinned\"`."
  ),
  input_older_than = c(
    en = "`older_than` must be a single non-negative number of days, or a difftime.",
    fr = "`older_than` doit \u00eatre un seul nombre de jours positif ou nul, ou un difftime."
  ),

  # ---- qesR_error_unknown_study / _unknown_variable / _ambiguous_file ------
  unknown_study = c(
    en = "Unknown study code %1$s. See qes_studies() for valid codes.",
    fr = "Code d'\u00e9tude inconnu %1$s. Voir qes_studies() pour les codes valides."
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
  unknown_variable_study = c(
    en = "Variable %1$s is not in study %2$s.",
    fr = "La variable %1$s ne fait pas partie de l'\u00e9tude %2$s."
  ),
  unknown_variable_study_suggest = c(
    en = "Variable %1$s is not in study %2$s. Close matches: %3$s.",
    fr = "La variable %1$s ne fait pas partie de l'\u00e9tude %2$s. Correspondances proches\u00a0: %3$s."
  ),
  file_no_match = c(
    en = "No data file of %1$s matches %2$s. Its data files: %3$s.",
    fr = "Aucun fichier de donn\u00e9es de %1$s ne correspond \u00e0 %2$s. Ses fichiers de donn\u00e9es\u00a0: %3$s."
  ),
  file_ambiguous = c(
    en = "%2$s matches several data files of %1$s: %3$s. Use a pattern that matches only one.",
    fr = "%2$s correspond \u00e0 plusieurs fichiers de donn\u00e9es de %1$s\u00a0: %3$s. Utilisez un motif qui n'en d\u00e9signe qu'un."
  ),

  # ---- qesR_error_network --------------------------------------------------
  network = c(
    en = "Could not download %1$s.",
    fr = "Impossible de t\u00e9l\u00e9charger %1$s."
  ),
  network_attempts = c(
    en = "Could not download %1$s (%2$s attempt(s)).",
    fr = "Impossible de t\u00e9l\u00e9charger %1$s (%2$s tentative(s))."
  ),
  http_status = c(
    en = "The server answered HTTP status %2$s for %1$s (%3$s attempt(s)).",
    fr = "Le serveur a r\u00e9pondu par le statut HTTP %2$s pour %1$s (%3$s tentative(s))."
  ),
  http_not_found = c(
    en = "%1$s was not found (HTTP 404). The file may have been removed from its Dataverse deposit; run qes_studies(check_updates = TRUE) to check the pinned versions.",
    fr = "%1$s est introuvable (HTTP 404). Le fichier a peut-\u00eatre \u00e9t\u00e9 retir\u00e9 de son d\u00e9p\u00f4t Dataverse\u00a0; lancez qes_studies(check_updates = TRUE) pour v\u00e9rifier les versions retenues."
  ),
  http_retry_after_long = c(
    en = "The server asked qesR to wait %2$s seconds before requesting %1$s again. qesR waits at most 120 seconds; try again later.",
    fr = "Le serveur demande d'attendre %2$s secondes avant de redemander %1$s. qesR attend au plus 120 secondes\u00a0; r\u00e9essayez plus tard."
  ),
  http_refused = c(
    en = "The server refused the automated request for %1$s (HTTP %2$s). qesR does not retry or work around this. Try again later, or download the file in a web browser.",
    fr = "Le serveur a refus\u00e9 la requ\u00eate automatis\u00e9e pour %1$s (HTTP %2$s). qesR ne r\u00e9essaie pas et ne contourne pas ce refus. R\u00e9essayez plus tard, ou t\u00e9l\u00e9chargez le fichier dans un navigateur."
  ),
  http_refused_manual = c(
    en = "The server refused the automated request for %1$s (HTTP %2$s). qesR does not retry or work around this. Download the file in a web browser (for a data file, choose the original file format) and save it as %3$s; qesR uses it once its md5 matches the catalog. To keep it between sessions, first set options(qesR.cache_dir = \"<folder>\") to an existing folder and save the file as <folder>/%4$s instead.",
    fr = "Le serveur a refus\u00e9 la requ\u00eate automatis\u00e9e pour %1$s (HTTP %2$s). qesR ne r\u00e9essaie pas et ne contourne pas ce refus. T\u00e9l\u00e9chargez le fichier dans un navigateur (pour un fichier de donn\u00e9es, choisissez le format original) et enregistrez-le sous %3$s\u00a0; qesR l'utilise d\u00e8s que sa somme md5 correspond au catalogue. Pour le garder d'une session \u00e0 l'autre, fixez d'abord options(qesR.cache_dir = \"<dossier>\") sur un dossier existant et enregistrez plut\u00f4t le fichier sous <dossier>/%4$s."
  ),
  http_refused_save = c(
    en = "The server refused the automated request for %1$s (HTTP %2$s). qesR does not retry or work around this. Download the file in a web browser and save it as %3$s.",
    fr = "Le serveur a refus\u00e9 la requ\u00eate automatis\u00e9e pour %1$s (HTTP %2$s). qesR ne r\u00e9essaie pas et ne contourne pas ce refus. T\u00e9l\u00e9chargez le fichier dans un navigateur et enregistrez-le sous %3$s."
  ),
  tls = c(
    en = "The secure (TLS) connection to %1$s failed. qesR never retries without certificate checks; check the system's certificates or proxy settings.",
    fr = "La connexion s\u00e9curis\u00e9e (TLS) \u00e0 %1$s a \u00e9chou\u00e9. qesR ne r\u00e9essaie jamais sans v\u00e9rifier les certificats\u00a0; v\u00e9rifiez les certificats du syst\u00e8me ou les r\u00e9glages du proxy."
  ),
  offline = c(
    en = "Could not reach %1$s: its name could not be resolved. Check your internet connection.",
    fr = "Impossible de joindre %1$s\u00a0: son nom n'a pas pu \u00eatre r\u00e9solu. V\u00e9rifiez votre connexion internet."
  ),

  # ---- qesR_error_source ---------------------------------------------------
  source_format = c(
    en = "qesR does not read files of type %1$s.",
    fr = "qesR ne lit pas les fichiers de type %1$s."
  ),
  checksum = c(
    en = "File %2$s of study %1$s failed its md5 check (expected %3$s, got %4$s). It was not kept; try again, and report the problem if it persists.",
    fr = "Le fichier %2$s de l'\u00e9tude %1$s n'a pas pass\u00e9 le contr\u00f4le md5 (attendu %3$s, obtenu %4$s). Il n'a pas \u00e9t\u00e9 conserv\u00e9\u00a0; r\u00e9essayez, et signalez le probl\u00e8me s'il persiste."
  ),
  rowcount = c(
    en = "File %2$s of study %1$s does not have the size the catalog records (expected %3$s rows x columns, found %4$s). qesR does not use it; report the problem.",
    fr = "Le fichier %2$s de l'\u00e9tude %1$s n'a pas la taille inscrite au catalogue (attendu %3$s lignes x colonnes, trouv\u00e9 %4$s). qesR ne l'utilise pas\u00a0; signalez le probl\u00e8me."
  ),
  checksum_cached = c(
    en = "The file found at %1$s is not file %3$s of study %2$s: its md5 is %5$s, not %4$s. qesR deleted it, and the server refused the automated download. If you saved it from a web browser, download it again choosing the original file format (not the tab-delimited version), and save it at the same place.",
    fr = "Le fichier trouv\u00e9 \u00e0 %1$s n'est pas le fichier %3$s de l'\u00e9tude %2$s\u00a0: sa somme md5 est %5$s, et non %4$s. qesR l'a supprim\u00e9, et le serveur a refus\u00e9 le t\u00e9l\u00e9chargement automatis\u00e9. Si vous l'avez enregistr\u00e9 depuis un navigateur, t\u00e9l\u00e9chargez-le \u00e0 nouveau en choisissant le format original (et non la version tabul\u00e9e), puis enregistrez-le au m\u00eame endroit."
  ),
  latest_unreadable = c(
    en = "Could not read the latest version of doi:%2$s (study %1$s). Nothing was written.",
    fr = "Impossible de lire la derni\u00e8re version de doi:%2$s (\u00e9tude %1$s). Rien n'a \u00e9t\u00e9 \u00e9crit."
  ),
  latest_deaccessioned = c(
    en = "The latest version (%3$s) of doi:%2$s (study %1$s) is deaccessioned. Nothing was written; `version = \"pinned\"` fetches the files qesR pins.",
    fr = "La derni\u00e8re version (%3$s) de doi:%2$s (\u00e9tude %1$s) est retir\u00e9e. Rien n'a \u00e9t\u00e9 \u00e9crit\u00a0; `version = \"pinned\"` t\u00e9l\u00e9charge les fichiers retenus par qesR."
  ),
  latest_missing = c(
    en = "File %2$s of study %1$s is not in version %3$s of its deposit (or has no md5 to check it against). Nothing was written; `version = \"pinned\"` fetches the files qesR pins.",
    fr = "Le fichier %2$s de l'\u00e9tude %1$s n'est pas dans la version %3$s de son d\u00e9p\u00f4t (ou n'a pas de somme md5 permettant de le v\u00e9rifier). Rien n'a \u00e9t\u00e9 \u00e9crit\u00a0; `version = \"pinned\"` t\u00e9l\u00e9charge les fichiers retenus par qesR."
  ),
  catalog_invalid = c(
    en = "The qesR catalog file %1$s is invalid: %2$s. Reinstall qesR.",
    fr = "Le fichier de catalogue de qesR %1$s est invalide\u00a0: %2$s. R\u00e9installez qesR."
  ),
  master_none = c(
    en = "No study could be loaded. Check network access and study availability.",
    fr = "Aucune \u00e9tude n'a pu \u00eatre charg\u00e9e. V\u00e9rifiez l'acc\u00e8s au r\u00e9seau et la disponibilit\u00e9 des \u00e9tudes."
  ),
  master_strict = c(
    en = "The master build failed for %1$s study(ies): %2$s",
    fr = "La construction du fichier fusionn\u00e9 a \u00e9chou\u00e9 pour %1$s \u00e9tude(s)\u00a0: %2$s"
  ),

  # ---- qesR_error_cache ----------------------------------------------------
  cache_dir_missing = c(
    en = "The cache directory %1$s (option qesR.cache_dir) does not exist. qesR only uses a directory you have created.",
    fr = "Le dossier de cache %1$s (option qesR.cache_dir) n'existe pas. qesR n'utilise qu'un dossier que vous avez cr\u00e9\u00e9."
  ),
  cache_not_ours = c(
    en = "%1$s already exists and is not a qesR cache (it has no .qesR-cache file); qesR will not write into it. Choose another cache directory.",
    fr = "%1$s existe d\u00e9j\u00e0 et n'est pas un cache qesR (il n'a pas de fichier .qesR-cache)\u00a0; qesR n'y \u00e9crira pas. Choisissez un autre dossier de cache."
  ),
  cache_unmarked = c(
    en = "%1$s is not a qesR cache (it has no .qesR-cache file); qes_cache_clear() will not delete anything in it.",
    fr = "%1$s n'est pas un cache qesR (il n'a pas de fichier .qesR-cache)\u00a0; qes_cache_clear() n'y supprimera rien."
  ),
  cache_write = c(
    en = "qesR could not write %1$s.",
    fr = "qesR n'a pas pu \u00e9crire %1$s."
  ),

  # ---- qesR_error_no_provenance --------------------------------------------
  no_provenance = c(
    en = "`%1$s` does not record which study it comes from (it may have lost its attributes, for example through merge()). Pass study codes instead.",
    fr = "`%1$s` n'indique pas de quelle \u00e9tude il provient (ses attributs ont pu \u00eatre perdus, par exemple avec merge()). Passez plut\u00f4t des codes d'\u00e9tude."
  ),

  no_provenance_level = c(
    en = "`%1$s` records study-level provenance only; %2$s-level provenance is recorded for harmonized data.",
    fr = "`%1$s` n'enregistre que la provenance au niveau de l'\u00e9tude\u00a0; la provenance au niveau %2$s est enregistr\u00e9e pour les donn\u00e9es harmonis\u00e9es."
  ),

  # ---- warnings ------------------------------------------------------------
  question_missing = c(
    en = "No question label was found for %1$s.",
    fr = "Aucun libell\u00e9 de question trouv\u00e9 pour %1$s."
  ),
  question_truncated = c(
    en = "The question text of %1$s was cut at 80 characters in its source file; the full wording is in document %2$s (see qes_docs()).",
    fr = "Le texte de la question %1$s a \u00e9t\u00e9 coup\u00e9 \u00e0 80 caract\u00e8res dans son fichier source\u00a0; le libell\u00e9 complet se trouve dans le document %2$s (voir qes_docs())."
  ),
  unpinned = c(
    en = "qes_download(version = \"latest\") saved files that the qesR catalog does not pin, for %1$s. They were checked against the md5 given by Dataverse, not by qesR, and get_qes() keeps reading the pinned files.",
    fr = "qes_download(version = \"latest\") a enregistr\u00e9 des fichiers que le catalogue de qesR ne retient pas, pour %1$s. Ils ont \u00e9t\u00e9 v\u00e9rifi\u00e9s par la somme md5 donn\u00e9e par Dataverse, et non par qesR, et get_qes() continue de lire les fichiers retenus."
  ),
  encoding = c(
    en = "File %2$s of study %1$s still has %3$s label(s) or text value(s) with a replacement or control character after decoding (in %4$s). Please report it.",
    fr = "Le fichier %2$s de l'\u00e9tude %1$s contient encore %3$s \u00e9tiquette(s) ou valeur(s) texte avec un caract\u00e8re de remplacement ou de contr\u00f4le apr\u00e8s d\u00e9codage (dans %4$s). Merci de le signaler."
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

  arg_ignored = c(
    en = "In %1$s(), `%2$s` no longer changes the result and is ignored. This note is shown once per session.",
    fr = "Dans %1$s(), `%2$s` ne change plus le r\u00e9sultat et est ignor\u00e9. Cette note s'affiche une fois par session."
  ),
  legacy_values_changed = c(
    en = paste0(
      "Harmonized values changed in qesR 0.5.0: get_qes_master() and get_decon() ",
      "keep every respondent and set values verified to be wrong to NA ",
      "(attr(, \"legacy_na_columns\") lists them); see NEWS. Results from ",
      "qesR <= 0.4.4 are reproducible by installing qesR 0.4.4 ",
      "(remotes::install_github(\"ThomasGareau/qesR\", ref = \"v0.4.4\")). ",
      "This note is shown once per session."
    ),
    fr = paste0(
      "Les valeurs harmonis\u00e9es ont chang\u00e9 dans qesR 0.5.0\u00a0: ",
      "get_qes_master() et get_decon() gardent tous les r\u00e9pondants et ",
      "mettent \u00e0 NA les valeurs v\u00e9rifi\u00e9es comme fausses ",
      "(attr(, \"legacy_na_columns\") les \u00e9num\u00e8re)\u00a0; voir NEWS. ",
      "Les r\u00e9sultats de qesR <= 0.4.4 se reproduisent en installant qesR ",
      "0.4.4 (remotes::install_github(\"ThomasGareau/qesR\", ref = \"v0.4.4\")). ",
      "Cette note s'affiche une fois par session."
    )
  ),
  legacy_master_columns = c(
    en = paste0(
      "get_qes_master() no longer appends the %1$s columns that qesR 0.4.4 built ",
      "by stacking variables that share a name across studies; ",
      "attr(, \"removed_columns\") lists them. Read those items from each study ",
      "with get_qes(). This note is shown once per session."
    ),
    fr = paste0(
      "get_qes_master() n'ajoute plus les %1$s colonnes que qesR 0.4.4 ",
      "construisait en empilant des variables de m\u00eame nom d'une \u00e9tude ",
      "\u00e0 l'autre\u00a0; attr(, \"removed_columns\") les \u00e9num\u00e8re. ",
      "Lisez ces questions dans chaque \u00e9tude avec get_qes(). Cette note ",
      "s'affiche une fois par session."
    )
  ),
  legacy_decon_columns = c(
    en = paste0(
      "In get_decon(), party_best and partylean are NA in every study, and turnout ",
      "and votechoice are NA except for qes2022: their qesR 0.4.4 sources were ",
      "other questions. This note is shown once per session."
    ),
    fr = paste0(
      "Dans get_decon(), party_best et partylean valent NA dans toutes les ",
      "\u00e9tudes, et turnout et votechoice valent NA sauf pour qes2022\u00a0: ",
      "leurs sources dans qesR 0.4.4 \u00e9taient d'autres questions. Cette ",
      "note s'affiche une fois par session."
    )
  ),

  missing_untyped = c(
    en = "%1$s of %2$s variable(s) have no missing codes in the codebook and were left unchanged.",
    fr = "%1$s variable(s) sur %2$s n'ont aucun code manquant dans le codebook et n'ont pas \u00e9t\u00e9 modifi\u00e9es."
  ),
  search_none = c(
    en = "No variable matches.",
    fr = "Aucune variable ne correspond."
  ),
  search_more = c(
    en = "... and %1$s more row(s).",
    fr = "... et %1$s ligne(s) de plus."
  ),
  search_not_searchable = c(
    en = "Not searchable yet: %1$s (its metadata is built from your copy of the data by qes_codebook() or get_qes()).",
    fr = "Pas encore consultable\u00a0: %1$s (ses m\u00e9tadonn\u00e9es sont construites \u00e0 partir de votre copie des donn\u00e9es par qes_codebook() ou get_qes())."
  ),

  # ---- progress (silenced by quiet = TRUE) ---------------------------------
  check_updates = c(
    en = "Checking doi:%1$s for a newer version.",
    fr = "Recherche d'une version plus r\u00e9cente de doi:%1$s."
  ),
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
  file_redirect = c(
    en = "No data file of %1$s matches %2$s; it matches the data file of study %3$s, which is read instead.",
    fr = "Aucun fichier de donn\u00e9es de %1$s ne correspond \u00e0 %2$s\u00a0; le motif d\u00e9signe le fichier de donn\u00e9es de l'\u00e9tude %3$s, qui est lu \u00e0 la place."
  ),
  download_file = c(
    en = "Downloading %1$s from %2$s.",
    fr = "T\u00e9l\u00e9chargement de %1$s depuis %2$s."
  ),
  cached_file = c(
    en = "Using the cached copy of %1$s.",
    fr = "Utilisation de la copie en cache de %1$s."
  ),
  cache_rejected = c(
    en = "The cached copy %1$s does not match the catalog (md5 %3$s, expected %2$s); qesR deletes it and downloads the file again.",
    fr = "La copie en cache %1$s ne correspond pas au catalogue (somme md5 %3$s, attendue %2$s)\u00a0; qesR la supprime et t\u00e9l\u00e9charge le fichier \u00e0 nouveau."
  ),
  cache_created = c(
    en = "qesR keeps downloaded files in %1$s. qes_cache_info() lists them and qes_cache_clear() deletes them.",
    fr = "qesR conserve les fichiers t\u00e9l\u00e9charg\u00e9s dans %1$s. qes_cache_info() les \u00e9num\u00e8re et qes_cache_clear() les supprime."
  ),
  disk_cache_tip = c(
    en = "Downloaded files are kept only until R closes. To keep them between sessions, set options(qesR.cache = \"disk\"). This tip is shown once per session.",
    fr = "Les fichiers t\u00e9l\u00e9charg\u00e9s ne sont conserv\u00e9s que jusqu'\u00e0 la fermeture de R. Pour les garder d'une session \u00e0 l'autre, fixez options(qesR.cache = \"disk\"). Ce conseil s'affiche une fois par session."
  ),
  download_kept = c(
    en = "%1$s is already there, with the expected md5; kept.",
    fr = "%1$s est d\u00e9j\u00e0 pr\u00e9sent, avec la somme md5 attendue\u00a0; conserv\u00e9."
  ),
  download_done = c(
    en = "%1$s file(s) saved in %3$s, %2$s already there.",
    fr = "%1$s fichier(s) enregistr\u00e9(s) dans %3$s, %2$s d\u00e9j\u00e0 pr\u00e9sent(s)."
  ),
  download_none = c(
    en = "No file of %1$s matches the request; nothing was written.",
    fr = "Aucun fichier de %1$s ne correspond \u00e0 la demande\u00a0; rien n'a \u00e9t\u00e9 \u00e9crit."
  ),

  # ---- print.qes_provenance() ------------------------------------------------
  prov_head = c(
    en = "%1$s: file %2$s (%3$s) of the Dataverse dataset https://doi.org/%4$s, version %5$s.",
    fr = "%1$s\u00a0: fichier %2$s (%3$s) du jeu de donn\u00e9es Dataverse https://doi.org/%4$s, version %5$s."
  ),
  prov_head_demo = c(
    en = "%1$s: file %2$s (%3$s), synthetic data shipped with qesR.",
    fr = "%1$s\u00a0: fichier %2$s (%3$s), donn\u00e9es synth\u00e9tiques fournies avec qesR."
  ),
  prov_unpinned = c(
    en = "This is not the version pinned by the qesR catalog.",
    fr = "Ce n'est pas la version retenue par le catalogue de qesR."
  ),
  prov_md5_verified = c(
    en = "md5 %1$s, verified.",
    fr = "Somme md5 %1$s, v\u00e9rifi\u00e9e."
  ),
  prov_md5_expected = c(
    en = "Expected md5 %1$s (not yet checked).",
    fr = "Somme md5 attendue %1$s (pas encore v\u00e9rifi\u00e9e)."
  ),
  prov_dims = c(
    en = "%1$s rows, %2$s columns.",
    fr = "%1$s lignes, %2$s colonnes."
  ),
  prov_retrieved = c(
    en = "Retrieved on %1$s UTC (%2$s).",
    fr = "Obtenu le %1$s UTC (%2$s)."
  ),
  prov_reader = c(
    en = "Read with %1$s, haven %2$s.",
    fr = "Lu avec %1$s, haven %2$s."
  ),
  prov_licence = c(
    en = "Licence: %1$s. qesR catalog %2$s.",
    fr = "Licence\u00a0: %1$s. Catalogue qesR %2$s."
  ),
  prov_footer = c(
    en = "as.data.frame() gives every column.",
    fr = "as.data.frame() donne toutes les colonnes."
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
  master_saved = c(
    en = "Master dataset saved to: %1$s",
    fr = "Fichier fusionn\u00e9 enregistr\u00e9 dans\u00a0: %1$s"
  )
)
