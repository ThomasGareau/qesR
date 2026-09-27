#' qesR: Access Quebec Election Study Datasets
#'
#' The `qesR` package provides survey-code based access to Quebec election
#' study datasets available via Dataverse repositories.
#'
#' @section Functions:
#' Grouped as in the reference index of the website.
#'
#' | Group | Function | What it does |
#' |---|---|---|
#' | Discover | [qes_studies()] | Lists the studies: code, title, authors, year, design, population, DOI, pinned version, licence. Offline; `check_updates = TRUE` asks Dataverse whether a newer version exists. |
#' | Discover | [qes_docs()] | Lists the codebooks, questionnaires and reports of each study, offline. |
#' | Get data | [get_qes()] | Loads a study: `qes2018 <- get_qes("qes2018")`. The data are returned, never written into your workspace by default. |
#' | Get data | [qes_download()] | Saves the original files (data and documents), md5-checked, in a folder you choose. |
#' | Get data | [get_qes_master()] | The legacy merged file of qesR 0.4.4: 30 harmonized columns, 11 studies, rendered from the harmonization engine since qesR 0.7.0 (see `NEWS`). |
#' | Metadata and search | [qes_codebook()] | A study's codebook: labels, question text (English and French), value labels, missing codes. |
#' | Metadata and search | [qes_question()] | The exact wording of one or more questions, in English or French. |
#' | Metadata and search | [qes_search()] | Searches variables across every study, ignoring case and accents: `qes_search("souverain")`. |
#' | Metadata and search | [qes_missing()] | Sets "don't know", "refused" and declared missing codes to `NA`. |
#' | Harmonization (experimental) | [qes_spec()] | The harmonization spec: which studies have which harmonized variable ("target"), how comparable each study's question is, and how its codes map. |
#' | Harmonization (experimental) | [qes_harmonize()] | One data frame across studies, one column per target, every missing value with a reason, from the reviewed spec only; waves, weights and eligibility of each respondent. |
#' | Harmonization (experimental) | [qes_design()] | Harmonized data as a survey design of the survey or srvyr package, with the weight that fits the targets. |
#' | Reproducibility | [qes_provenance()] | Which file the data came from: DOI, version, file, md5, date; for harmonized data, also the spec row and grade of each cell. |
#' | Reproducibility | [qes_cite()] | Citation of qesR and of each dataset, as text, BibTeX or `bibentry`. |
#' | Cache | [qes_cache_info()], [qes_cache_clear()] | Lists or deletes the files kept in the download cache. |
#'
#' Harmonization across studies is experimental: [qes_spec()] shows the
#' reviewed spec, [qes_harmonize()] applies it and [qes_design()] turns the
#' result into a survey design; the reference generated from the spec is
#' `vignette("harmonization-reference", package = "qesR")`.
#'
#' The functions of qesR 0.4.4 (`get_codebook()`, `get_question()`,
#' `get_preview()`, `get_qescodes()`, ...) keep working and will not be
#' removed; [qesR-deprecated] gives the replacement of each one.
#'
#' Guides: `vignette("get-started", package = "qesR")` (from a study code to a
#' weighted estimate), `vignette("citations", package = "qesR")`,
#' `vignette("migrating-0.5", package = "qesR")` and
#' `vignette("harmonization-reference", package = "qesR")` (the reference
#' generated from the harmonization spec). The website,
#' <https://thomasgareau.github.io/qesR/>, also has the study catalog and
#' analysis examples built from the full data files, in English and French.
#'
#' @section Returning data:
#' Every function returns its result visibly and, by default, writes nothing
#' into your workspace: write `qes2018 <- get_qes("qes2018")`. Functions with
#' an `assign_global` argument assign only when it is `TRUE`, and then into the
#' environment they were called from (the global environment only when called
#' at top level).
#'
#' @section Options:
#' \describe{
#'   \item{`qesR.lang` (environment variable `QESR_LANG`)}{Language of
#'     messages, warnings and errors: `"en"` or `"fr"`. Unset by default, in
#'     which case qesR follows `LANGUAGE`, then the messages locale. It never
#'     changes the data or text that functions return.}
#'   \item{`qesR.quiet_deprecated`}{`TRUE` hides the one-time notices of the
#'     soft-deprecated legacy functions (see [qesR-deprecated]). Default
#'     `FALSE`.}
#'   \item{`qesR.cache` (environment variable `QESR_CACHE`)}{Where downloaded
#'     files are kept: `"session"` (default; a folder in [tempdir()], deleted
#'     when R exits), `"disk"` (kept between sessions in
#'     `tools::R_user_dir("qesR", "cache")`, created only when you choose it)
#'     or `"none"`. See [qes_cache_info()].}
#'   \item{`qesR.cache_dir` (environment variable `QESR_CACHE_DIR`)}{An
#'     existing directory to keep downloads in, instead of the default disk
#'     location; implies `"disk"`. qesR works only in a `qesR` subfolder that
#'     it creates and marks.}
#'   \item{`qesR.max_tries`}{Attempts per request before giving up. Default
#'     4.}
#'   \item{`qesR.stall_timeout`}{Seconds a download may stay below 1 KB/s
#'     before it is abandoned (and retried). Default 60.}
#' }
#'
#' @section Conditions:
#' Errors, warnings and messages have classes, so code can react to them with
#' `tryCatch()` or `withCallingHandlers()` without matching message text.
#' Errors inherit from `qesR_error`:
#' \describe{
#'   \item{`qesR_error_input`}{an invalid argument (fields `arg`, `value`),
#'     including files that [qes_download()] would overwrite (field
#'     `paths`);}
#'   \item{`qesR_error_unknown_study`}{an unknown study code (fields `study`,
#'     `suggestions`);}
#'   \item{`qesR_error_unknown_variable`}{an unknown column (fields
#'     `variables`, `suggestions`);}
#'   \item{`qesR_error_ambiguous_file`}{a `file` pattern that matches no data
#'     file of the study, or several (fields `study`, `pattern`,
#'     `candidates`);}
#'   \item{`qesR_error_network`}{a failed download (fields `url`, `attempts`,
#'     and `parent`, the root cause). Its subclasses are
#'     `qesR_error_http` (an HTTP error status; fields `status`,
#'     `retry_after`, `server_message`), itself the parent of
#'     `qesR_error_http_refused` (the server refused the automated request;
#'     field `manual_path`); `qesR_error_tls` (the secure connection failed;
#'     qesR never retries without TLS certificate checks); and
#'     `qesR_error_offline` (the server's name could not be resolved);}
#'   \item{`qesR_error_source`}{a problem with the files served for a study
#'     (fields `study`, `file_id`), such as a file missing from the latest
#'     version of its deposit; subclasses `qesR_error_checksum` (a file
#'     that does not match its catalog md5; fields `expected`, `actual`) and
#'     `qesR_error_rowcount` (a data file whose rows and columns differ from
#'     the catalog; fields `expected`, `actual`);}
#'   \item{`qesR_error_cache`}{a cache directory that is missing or is not a
#'     qesR cache (fields `path`, `reason`);}
#'   \item{`qesR_error_no_provenance`}{an object that no longer records which
#'     study it comes from, passed to [qes_cite()] or [qes_provenance()], or
#'     a provenance level the object does not record (field `level`);}
#'   \item{`qesR_error_dependency`}{a suggested package that a function
#'     needs is not installed, such as \pkg{survey} for [qes_design()]
#'     (field `package`).}
#' }
#' Warnings inherit from `qesR_warning`, among them `qesR_warning_encoding`
#' (a label or text value that still holds a replacement or control
#' character after reading; fields `study`, `file_id`, `n`, `variables`) and
#' `qesR_warning_unpinned` (files saved by `qes_download(version =
#' "latest")`, which the catalog does not pin; fields `study`, `file_id`) and
#' `qesR_warning_truncated` (a question text that its source file cut at 80
#' characters, from [get_question()]; fields `variable`, `doc_ref`).
#' Messages inherit from `qesR_message`; [qes_missing()] counts the variables
#' it left unchanged in a `qesR_message_missing_untyped` (silenced by
#' `quiet = TRUE`).
#' Progress messages (classes `qesR_message_download` and
#' `qesR_message_cached`) are silenced by `quiet = TRUE`, as is the one-time
#' tip suggesting the disk cache (`qesR_message_disk_cache_tip`). These
#' notices are shown at most once per session and are
#' not silenced by `quiet`: `qesR_message_deprecated` (see
#' [qesR-deprecated]); `qesR_message_assign_default`, shown when `get_qes()`,
#' `get_qes_master()` or `get_decon()` is called without `assign_global`;
#' `qesR_message_arg_ignored`, shown when a legacy argument that no longer
#' changes the result is used; and `qesR_message_values_changed` and
#' `qesR_message_legacy_columns`, shown by [get_qes_master()] and
#' [get_decon()], whose values changed in qesR 0.5.0 and 0.7.0. Every condition carries
#' the fields `id` (its message key) and `lang` (the language of its message).
#'
#' @section Study catalog:
#' qesR ships a catalog of the studies it can load: [qes_studies()] lists
#' them, [qes_docs()] lists their documents and [qes_cite()] cites them, all
#' without a network request. Each study is pinned to one Dataverse dataset
#' version and one data file, identified by its md5 checksum.
#'
#' @section Data files:
#' [get_qes()] reads the original upload of each study's pinned data file
#' (SPSS or Stata), after checking its md5 and its rows and columns against
#' the catalog. Names, codes and missing values are those of the file;
#' labels come from the file (for `qes2012`, from the SPSS twin of the same
#' data, whose labels are complete). Codes an SPSS file declares as
#' user-missing are kept as values, with the declaration in the column
#' attributes `qes_na_values` and `qes_na_range`. The attribute
#' `qes_provenance` records which file was read; [qes_provenance()] returns
#' it and prints it as a paragraph for a replication log.
#' [qes_download()] saves the original files themselves (data file and
#' documents), md5-checked, in a directory you choose.
#'
#' @section Codebooks and search:
#' The description of every variable (label, question text in English and
#' French, value labels, missing codes) ships with qesR for the studies
#' released under CC0, so [qes_codebook()], [qes_question()] and
#' [qes_search()] work offline. Question text comes from the deposited
#' questionnaires. `qes2022` (CC BY-NC 4.0) ships no metadata: its codebook
#' is built from your own copy of the data file and kept in the download
#' cache. [qes_missing()] sets "don't know", "refused" and declared missing
#' codes to `NA`.
#'
#' @section Network use:
#' Requests go to the Dataverse servers listed in the catalog, one at a time
#' and at least one second apart per server. They carry the User-Agent
#' `qesR/<version> R/<version>` and no other identifying information.
#' A request that fails for a passing reason (a timeout, a dropped
#' connection, HTTP 408, 429, 500, 502, 503 or 504) is tried again, up to
#' `qesR.max_tries` attempts in all, after a growing pause, or after the delay
#' the server asks for in `Retry-After` (at most two minutes; a longer delay
#' is an error). A download that stalls is abandoned after
#' `qesR.stall_timeout` seconds. A file is saved under its final name only
#' once it is complete and, for catalog files, once its md5 matches the
#' catalog.
#'
#' Some servers (Harvard Dataverse) may refuse automated requests. qesR does
#' not work around this: the error (class `qesR_error_http_refused`) says
#' where to save the file after downloading it in a web browser.
#'
#' Downloaded catalog files are kept in a cache, by default only for the
#' session; see [qes_cache_info()] and [qes_cache_clear()].
#'
#' @section En français:
#' **Données retournées.** Chaque fonction retourne son résultat de façon
#' visible et, par défaut, n'écrit rien dans votre espace de travail :
#' écrivez `qes2018 <- get_qes("qes2018")`. Les fonctions qui ont un argument
#' `assign_global` n'assignent que s'il vaut `TRUE`, et alors dans
#' l'environnement d'où elles sont appelées.
#'
#' **Options.** `qesR.lang` (variable d'environnement `QESR_LANG`) fixe la
#' langue des messages, avertissements et erreurs : `"en"` ou `"fr"`. Sans
#' elle, qesR suit `LANGUAGE`, puis la locale des messages. La langue ne change
#' jamais les données ni le texte retournés. `qesR.quiet_deprecated = TRUE`
#' masque les notes uniques des fonctions héritées (voir [qesR-deprecated]).
#' `qesR.cache` (`QESR_CACHE`) fixe où sont gardés les fichiers téléchargés :
#' `"session"` (par défaut ; un dossier de [tempdir()], supprimé à la
#' fermeture de R), `"disk"` (gardés d'une session à l'autre dans
#' `tools::R_user_dir("qesR", "cache")`, créé seulement sur demande) ou
#' `"none"`. `qesR.cache_dir` (`QESR_CACHE_DIR`) désigne un dossier existant
#' de votre choix et implique `"disk"` ; qesR n'y utilise qu'un sous-dossier
#' `qesR` qu'il crée et marque. `qesR.max_tries` (4 par défaut) est le nombre
#' de tentatives par requête et `qesR.stall_timeout` (60 par défaut) le
#' nombre de secondes sous 1 Ko/s après lequel un téléchargement est
#' abandonné.
#'
#' **Conditions.** Les erreurs, avertissements et messages ont des classes,
#' ce qui permet d'y réagir avec `tryCatch()` ou `withCallingHandlers()` sans
#' lire le texte. Les erreurs héritent de `qesR_error` : `qesR_error_input`
#' (champs `arg`, `value` ; aussi pour des fichiers que [qes_download()]
#' écraserait, champ `paths`), `qesR_error_unknown_study` (`study`,
#' `suggestions`), `qesR_error_unknown_variable` (`variables`,
#' `suggestions`), `qesR_error_ambiguous_file` (`study`, `pattern`,
#' `candidates` ; motif `file` qui ne désigne aucun fichier de données de
#' l'étude, ou plusieurs), `qesR_error_network` (`url`, `attempts`, et `parent`, la
#' cause première), avec ses sous-classes `qesR_error_http` (statut HTTP
#' d'erreur ; `status`, `retry_after`, `server_message`), elle-même parente
#' de `qesR_error_http_refused` (requête automatisée refusée ;
#' `manual_path`), `qesR_error_tls` (échec de la connexion sécurisée ; qesR ne
#' désactive jamais la vérification des certificats TLS) et
#' `qesR_error_offline` (nom du serveur introuvable) ; `qesR_error_source`
#' (`study`, `file_id` ; par exemple un fichier absent de la dernière
#' version de son dépôt), avec `qesR_error_checksum` (fichier dont la somme
#' md5 ne correspond pas au catalogue ; `expected`, `actual`) et
#' `qesR_error_rowcount` (fichier de données dont les lignes et colonnes
#' diffèrent du catalogue ; `expected`, `actual`) ;
#' `qesR_error_cache` (dossier de cache absent ou qui n'est pas un cache
#' qesR ; `path`, `reason`) ; `qesR_error_no_provenance` (objet qui
#' n'indique plus son étude, passé à [qes_cite()] ou [qes_provenance()], ou
#' niveau de provenance que l'objet n'enregistre pas ; champ `level`) et
#' `qesR_error_dependency` (package suggéré non installé dont une fonction a
#' besoin, comme \pkg{survey} pour [qes_design()] ; champ `package`). Les
#' avertissements héritent de `qesR_warning`, dont `qesR_warning_encoding`
#' (étiquette ou valeur texte qui garde un caractère de remplacement ou de
#' contrôle après lecture ; `study`, `file_id`, `n`, `variables`) et
#' `qesR_warning_unpinned` (fichiers enregistrés par `qes_download(version =
#' "latest")`, que le catalogue ne retient pas ; `study`, `file_id`) et
#' `qesR_warning_truncated` (texte de question coupé à 80 caractères dans
#' son fichier source, signalé par [get_question()] ; `variable`,
#' `doc_ref`), et les messages de `qesR_message` ; [qes_missing()] compte
#' les variables laissées inchangées dans un
#' `qesR_message_missing_untyped` (masqué par `quiet = TRUE`). Les messages
#' de progression (`qesR_message_download`, `qesR_message_cached`) et le
#' conseil unique sur le cache disque (`qesR_message_disk_cache_tip`) sont
#' masqués par `quiet = TRUE`.
#' `qesR_message_deprecated`, `qesR_message_assign_default`,
#' `qesR_message_arg_ignored` (argument hérité qui ne change plus le résultat),
#' `qesR_message_values_changed` et `qesR_message_legacy_columns` (valeurs de
#' [get_qes_master()] et [get_decon()] modifiées dans qesR 0.5.0 et 0.7.0)
#' s'affichent au plus une fois par session et ne sont pas masqués par
#' `quiet`. Chaque
#' condition porte les champs `id` (sa clé de message) et `lang` (la langue de
#' son message).
#'
#' **Catalogue.** qesR fournit un catalogue des études qu'il peut charger :
#' [qes_studies()] les énumère, [qes_docs()] donne leurs documents et
#' [qes_cite()] les cite, sans aucune requête réseau. Chaque étude est fixée à
#' une version de jeu de données Dataverse et à un fichier de données,
#' identifié par sa somme de contrôle md5.
#'
#' **Fichiers de données.** [get_qes()] lit le fichier original déposé pour
#' chaque étude (SPSS ou Stata), après avoir vérifié sa somme md5, ses lignes
#' et ses colonnes par rapport au catalogue. Les noms, les codes et les
#' valeurs manquantes sont ceux du fichier ; les étiquettes viennent du
#' fichier (pour `qes2012`, du fichier SPSS jumeau des mêmes données, dont les
#' étiquettes sont complètes). Les codes qu'un fichier SPSS déclare comme
#' valeurs manquantes sont gardés comme valeurs, et la déclaration est
#' conservée dans les attributs de colonne `qes_na_values` et `qes_na_range`.
#' L'attribut `qes_provenance` indique quel fichier a été lu ;
#' [qes_provenance()] le renvoie et l'affiche comme un paragraphe pour un
#' journal de réplication. [qes_download()] enregistre les fichiers originaux
#' eux-mêmes (fichier de données et documents), vérifiés par somme md5, dans
#' un dossier de votre choix.
#'
#' **Codebooks et recherche.** La description de chaque variable
#' (étiquette, texte de la question en anglais et en français, étiquettes de
#' valeurs, codes manquants) est livrée avec qesR pour les études sous
#' licence CC0 : [qes_codebook()], [qes_question()] et [qes_search()]
#' fonctionnent sans réseau. Le texte des questions vient des questionnaires
#' déposés. Pour `qes2022` (CC BY-NC 4.0), aucune métadonnée n'est livrée :
#' le codebook est construit à partir de votre copie du fichier et conservé
#' dans le cache. [qes_missing()] remplace par `NA` les codes « ne sait
#' pas », « refus » et les codes manquants déclarés.
#'
#' **Réseau.** Les requêtes vont aux serveurs Dataverse du catalogue, une à la
#' fois et à au moins une seconde d'intervalle par serveur. Elles portent
#' l'en-tête User-Agent `qesR/<version> R/<version>` et aucune autre
#' information identifiante. Une requête qui échoue pour une raison passagère
#' (délai dépassé, connexion interrompue, HTTP 408, 429, 500, 502, 503 ou 504)
#' est reprise, jusqu'à `qesR.max_tries` tentatives en tout, après une pause
#' croissante ou le délai demandé par le serveur dans `Retry-After` (deux
#' minutes au plus ; au-delà, c'est une erreur). Un fichier ne reçoit son nom
#' définitif qu'une fois complet et, pour les fichiers du catalogue, une fois
#' sa somme md5 vérifiée. Certains serveurs (Harvard Dataverse) peuvent
#' refuser les requêtes automatisées : qesR ne contourne pas ce refus, et
#' l'erreur (`qesR_error_http_refused`) indique où enregistrer le fichier
#' téléchargé dans un navigateur. Les fichiers du catalogue sont gardés dans
#' un cache, par défaut pour la session seulement ; voir [qes_cache_info()] et
#' [qes_cache_clear()].
#'
#' @examples
#' qes_studies()[, c("study", "year", "title_en")]
#'
#' # messages in French; returned data is unchanged
#' old <- options(qesR.lang = "fr")
#' tryCatch(
#'   get_qes("QES 2022"),
#'   qesR_error_unknown_study = function(e) e$suggestions
#' )
#' options(old)
#'
#' # the synthetic demonstration study ships with qesR: no download
#' demo <- get_qes("qes_demo", quiet = TRUE)
#' qes_provenance(demo)
"_PACKAGE"
