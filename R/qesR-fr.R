#' qesR en français
#'
#' @description
#' qesR charge dans R les Études électorales québécoises et d'autres
#' enquêtes électorales québécoises, à partir d'un code d'étude. Chaque code
#' désigne un dépôt Dataverse (Borealis ou Harvard Dataverse), fixé à une
#' version du jeu de données et à un fichier de données original (SPSS ou
#' Stata), vérifié par sa somme de contrôle md5 avant usage. Le catalogue des
#' études, leurs documents, leurs citations et, pour les études sous licence
#' CC0, la description de chaque variable sont livrés avec le package et
#' s'utilisent sans réseau.
#'
#' Cette page est le point d'entrée en français. Les autres pages d'aide sont
#' en anglais ; certaines ont une section « En français ».
#'
#' @section Fonctions:
#' | Fonction | Rôle |
#' |---|---|
#' | [qes_studies()] | Liste les études : code, titre, auteurs, année, devis, population, DOI, version fixée, licence. Sans réseau ; `check_updates = TRUE` demande à Dataverse si une version plus récente existe. |
#' | [qes_docs()] | Liste les livres de codes, questionnaires et rapports de chaque étude, sans réseau. |
#' | [get_qes()] | Charge une étude : `qes2018 <- get_qes("qes2018")`. Les données sont retournées, jamais écrites dans votre espace de travail par défaut. |
#' | [qes_codebook()] | Codebook d'une étude : étiquettes, texte des questions (anglais et français), étiquettes de valeurs, codes manquants. |
#' | [qes_question()] | Texte exact d'une ou de plusieurs questions, en français ou en anglais. |
#' | [qes_search()] | Cherche des variables dans toutes les études, sans tenir compte de la casse ni des accents : `qes_search("souverain|sovereign")`. |
#' | [qes_missing()] | Remplace par `NA` les codes « ne sait pas », « refus » et les codes manquants déclarés. |
#' | [qes_download()] | Enregistre les fichiers originaux (données et documents), vérifiés par md5, dans un dossier de votre choix. |
#' | [qes_provenance()] | Indique de quel fichier viennent les données : DOI, version, fichier, md5, date. |
#' | [qes_cite()] | Citation de qesR et de chaque jeu de données, en texte, BibTeX ou `bibentry`. |
#' | [get_qes_master()] | Fichier fusionné hérité de qesR 0.4.4 : 30 colonnes harmonisées, 11 études. Ses valeurs ont changé dans qesR 0.5.0 (voir `NEWS`). |
#' | [qes_cache_info()], [qes_cache_clear()] | Liste ou supprime les fichiers gardés dans le cache de téléchargement. |
#'
#' Les fonctions de qesR 0.4.4 (`get_codebook()`, `get_question()`,
#' `get_preview()`, `get_qescodes()`, ...) continuent de fonctionner et ne
#' seront pas retirées ; voir [qesR-deprecated] pour la fonction qui remplace
#' chacune.
#'
#' @section Langue:
#' `options(qesR.lang = "fr")` affiche les messages, avertissements et
#' erreurs en français. La langue ne change jamais les données ni le texte
#' retournés : l'argument `lang` de [qes_codebook()], [qes_question()] et
#' [qes_cite()] choisit la langue du texte retourné.
#'
#' @section Données de démonstration:
#' L'étude synthétique `qes_demo` est livrée avec qesR : `get_qes("qes_demo")`
#' la lit sans téléchargement. Ses 60 répondants sont tirés au hasard ; aucune
#' valeur ne vient d'un vrai répondant.
#'
#' @section Guides:
#' `vignette("fr-demarrage", package = "qesR")` (démarrage),
#' `vignette("fr-citations", package = "qesR")` (citations) et
#' `vignette("fr-migrer-0.5", package = "qesR")` (passer de qesR 0.4.4 à
#' 0.5.0).
#'
#' @section Licences:
#' Les données ne font pas partie du package : qesR les télécharge depuis
#' leurs dépôts. La plupart des études sont sous licence CC0 1.0. L'étude de
#' 2022 est sous licence CC BY-NC 4.0 (attribution, pas d'usage commercial) ;
#' qesR ne livre aucune de ses métadonnées. Le fichier `COPYRIGHTS` du package
#' (`system.file("COPYRIGHTS", package = "qesR")`) détaille le contenu
#' livré et sa source.
#'
#' @examples
#' old <- options(qesR.lang = "fr")
#' qes_studies()[, c("study", "year", "title_fr")]
#' qes_question("qes2014", "Q19", lang = "fr")
#' demo <- get_qes("qes_demo", quiet = TRUE)
#' qes_cite("qes2014", lang = "fr")
#' options(old)
#'
#' @name qesR-fr
#' @aliases qesR-fr
NULL
