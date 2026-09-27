#' qesR: Access Quebec Election Study Datasets
#'
#' The `qesR` package provides survey-code based access to Quebec election
#' study datasets available via Dataverse repositories.
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
#'   \item{`qesR_error_input`}{an invalid argument (fields `arg`, `value`);}
#'   \item{`qesR_error_unknown_study`}{an unknown study code (fields `study`,
#'     `suggestions`);}
#'   \item{`qesR_error_unknown_variable`}{an unknown column (fields
#'     `variables`, `suggestions`);}
#'   \item{`qesR_error_ambiguous_file`}{a `file` pattern that matches no file
#'     (fields `study`, `pattern`, `candidates`);}
#'   \item{`qesR_error_network`}{a failed download (fields `url`, `attempts`,
#'     and `parent`, the root cause). Its subclasses are
#'     `qesR_error_http` (an HTTP error status; fields `status`,
#'     `retry_after`, `server_message`), itself the parent of
#'     `qesR_error_http_refused` (the server refused the automated request;
#'     field `manual_path`); `qesR_error_tls` (the secure connection failed;
#'     qesR never retries without TLS certificate checks); and
#'     `qesR_error_offline` (the server's name could not be resolved);}
#'   \item{`qesR_error_source`}{a problem with the files served for a study
#'     (fields `study`, `file_id`); subclass `qesR_error_checksum` (a file
#'     that does not match its catalog md5; fields `expected`, `actual`);}
#'   \item{`qesR_error_cache`}{a cache directory that is missing or is not a
#'     qesR cache (fields `path`, `reason`);}
#'   \item{`qesR_error_no_provenance`}{an object that no longer records which
#'     study it comes from, passed to [qes_cite()].}
#' }
#' Warnings inherit from `qesR_warning` and messages from `qesR_message`.
#' Progress messages (classes `qesR_message_download` and
#' `qesR_message_cached`) are silenced by `quiet = TRUE`, as is the one-time
#' tip suggesting the disk cache (`qesR_message_disk_cache_tip`). Three notices are shown at most once per session and are
#' not silenced by `quiet`: `qesR_message_deprecated` (see
#' [qesR-deprecated]); `qesR_message_assign_default`, shown when `get_qes()`,
#' `get_qes_master()` or `get_decon()` is called without `assign_global`; and
#' `qesR_message_arg_ignored`, shown when a legacy argument that no longer
#' changes the result is used. Every condition carries
#' the fields `id` (its message key) and `lang` (the language of its message).
#'
#' @section Study catalog:
#' qesR ships a catalog of the studies it can load: [qes_studies()] lists
#' them, [qes_docs()] lists their documents and [qes_cite()] cites them, all
#' without a network request. Each study is pinned to one Dataverse dataset
#' version and one data file, identified by its md5 checksum.
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
#' (champs `arg`, `value`), `qesR_error_unknown_study` (`study`,
#' `suggestions`), `qesR_error_unknown_variable` (`variables`,
#' `suggestions`), `qesR_error_ambiguous_file` (`study`, `pattern`,
#' `candidates`), `qesR_error_network` (`url`, `attempts`, et `parent`, la
#' cause première), avec ses sous-classes `qesR_error_http` (statut HTTP
#' d'erreur ; `status`, `retry_after`, `server_message`), elle-même parente
#' de `qesR_error_http_refused` (requête automatisée refusée ;
#' `manual_path`), `qesR_error_tls` (échec de la connexion sécurisée ; qesR ne
#' désactive jamais la vérification des certificats TLS) et
#' `qesR_error_offline` (nom du serveur introuvable) ; `qesR_error_source`
#' (`study`, `file_id`), avec `qesR_error_checksum` (fichier dont la somme
#' md5 ne correspond pas au catalogue ; `expected`, `actual`) ;
#' `qesR_error_cache` (dossier de cache absent ou qui n'est pas un cache
#' qesR ; `path`, `reason`) et `qesR_error_no_provenance` (objet qui
#' n'indique plus son étude, passé à [qes_cite()]). Les avertissements
#' héritent de `qesR_warning` et les messages de `qesR_message`. Les messages
#' de progression (`qesR_message_download`, `qesR_message_cached`) et le
#' conseil unique sur le cache disque (`qesR_message_disk_cache_tip`) sont
#' masqués par `quiet = TRUE`.
#' `qesR_message_deprecated`, `qesR_message_assign_default` et
#' `qesR_message_arg_ignored` (argument hérité qui ne change plus le résultat)
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
#' \donttest{
#'   qes2022 <- get_qes("qes2022")
#' }
"_PACKAGE"
