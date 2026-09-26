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
#'     and `parent`, the root cause). qesR never retries a download without TLS
#'     certificate checks;}
#'   \item{`qesR_error_source`}{a problem with the files served for a study
#'     (fields `study`, `file_id`);}
#'   \item{`qesR_error_no_provenance`}{an object that no longer records which
#'     study it comes from, passed to [qes_cite()].}
#' }
#' Warnings inherit from `qesR_warning` and messages from `qesR_message`.
#' Progress messages (class `qesR_message_download`) are silenced by
#' `quiet = TRUE`. Three notices are shown at most once per session and are
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
#'
#' **Conditions.** Les erreurs, avertissements et messages ont des classes,
#' ce qui permet d'y réagir avec `tryCatch()` ou `withCallingHandlers()` sans
#' lire le texte. Les erreurs héritent de `qesR_error` : `qesR_error_input`
#' (champs `arg`, `value`), `qesR_error_unknown_study` (`study`,
#' `suggestions`), `qesR_error_unknown_variable` (`variables`,
#' `suggestions`), `qesR_error_ambiguous_file` (`study`, `pattern`,
#' `candidates`), `qesR_error_network` (`url`, `attempts`, et `parent`, la
#' cause première ; qesR ne désactive jamais la vérification des certificats
#' TLS), `qesR_error_source` (`study`, `file_id`) et
#' `qesR_error_no_provenance` (objet qui n'indique plus son étude, passé à
#' [qes_cite()]). Les avertissements
#' héritent de `qesR_warning` et les messages de `qesR_message`. Les messages
#' de progression (`qesR_message_download`) sont masqués par `quiet = TRUE`.
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
#' information identifiante.
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
