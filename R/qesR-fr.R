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
#' Les fonctions sont regroupées comme dans la référence du site web.
#'
#' | Groupe | Fonction | Rôle |
#' |---|---|---|
#' | Découvrir | [qes_studies()] | Liste les études : code, titre, auteurs, année, devis, population, DOI, version fixée, licence. Sans réseau ; `check_updates = TRUE` demande à Dataverse si une version plus récente existe. |
#' | Découvrir | [qes_docs()] | Liste les livres de codes, questionnaires et rapports de chaque étude, sans réseau. |
#' | Obtenir les données | [get_qes()] | Charge une étude : `qes2018 <- get_qes("qes2018")`. Les données sont retournées, jamais écrites dans votre espace de travail par défaut. |
#' | Obtenir les données | [qes_download()] | Enregistre les fichiers originaux (données et documents), vérifiés par md5, dans un dossier de votre choix. |
#' | Obtenir les données | [get_qes_master()] | Fichier fusionné hérité de qesR 0.4.4 : 30 colonnes harmonisées, 11 études. Ses valeurs ont changé dans qesR 0.5.0 (voir `NEWS`). |
#' | Métadonnées et recherche | [qes_codebook()] | Codebook d'une étude : étiquettes, texte des questions (anglais et français), étiquettes de valeurs, codes manquants. |
#' | Métadonnées et recherche | [qes_question()] | Texte exact d'une ou de plusieurs questions, en français ou en anglais. |
#' | Métadonnées et recherche | [qes_search()] | Cherche des variables dans toutes les études, sans tenir compte de la casse ni des accents : `qes_search("souverain")`. |
#' | Métadonnées et recherche | [qes_missing()] | Remplace par `NA` les codes « ne sait pas », « refus » et les codes manquants déclarés. |
#' | Reproductibilité | [qes_provenance()] | Indique de quel fichier viennent les données : DOI, version, fichier, md5, date. |
#' | Reproductibilité | [qes_cite()] | Citation de qesR et de chaque jeu de données, en texte, BibTeX ou `bibentry`. |
#' | Cache | [qes_cache_info()], [qes_cache_clear()] | Liste ou supprime les fichiers gardés dans le cache de téléchargement. |
#'
#' L'harmonisation entre études (`qes_harmonize()`, `qes_spec()`,
#' `qes_design()`) est prévue pour qesR 0.6.0, à titre expérimental ; elle ne
#' fait pas partie de cette version.
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
#' `vignette("fr-demarrage", package = "qesR")` (démarrage : du code d'étude
#' à une estimation pondérée), `vignette("fr-citations", package = "qesR")`
#' (citations) et `vignette("fr-migrer-0.5", package = "qesR")` (passer de
#' qesR 0.4.4 à 0.5.0). Le site web, <https://thomasgareau.github.io/qesR/>,
#' offre aussi en français le catalogue des études et des exemples
#' d'analyse construits à partir des fichiers complets (menu « Guides
#' (FR) »).
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
