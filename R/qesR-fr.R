#' qesR en français
#'
#' @description
#' qesR charge dans R les Études électorales québécoises et d'autres
#' enquêtes électorales québécoises, à partir d'un code d'étude. Chaque code
#' désigne un dépôt Dataverse (Borealis ou Harvard Dataverse), fixé à une
#' version du jeu de données et à un fichier de données original (SPSS ou
#' Stata), vérifié par sa somme de contrôle md5 avant usage. Le catalogue des
#' études, leurs documents, leurs citations et la description de chaque
#' variable sont livrés avec le package et s'utilisent sans réseau.
#'
#' Cette page est le point d'entrée en français. Les autres pages d'aide sont
#' en anglais ; certaines ont une section « En français ».
#'
#' @section Fonctions:
#' Les fonctions sont regroupées comme dans la référence du site web.
#'
#' | Groupe | Fonction | Rôle |
#' |---|---|---|
#' | Données | [get_qes()] | Charge une étude : `qes2018 <- get_qes("qes2018")`. Les données sont retournées, jamais écrites dans votre espace de travail par défaut. |
#' | Données | [get_qes_master()] | Le fichier fusionné : 11 études empilées dans un seul tableau, avec 30 colonnes harmonisées dans un format fixe, construit à partir des variables harmonisées. |
#' | Études et documents | [qes_studies()] | Liste les études : code, titre, auteurs, année, devis, population, DOI, version fixée, licence. Sans réseau ; `check_updates = TRUE` demande à Dataverse si une version plus récente existe. |
#' | Études et documents | [qes_docs()] | Liste les livres de codes, questionnaires et rapports de chaque étude, sans réseau. |
#' | Études et documents | [qes_download()] | Enregistre les fichiers originaux (données et documents), vérifiés par md5, dans un dossier de votre choix. |
#' | Codebooks et recherche | [qes_codebook()] | Codebook d'une étude : étiquettes, texte des questions (anglais et français), étiquettes de valeurs, codes manquants. |
#' | Codebooks et recherche | [qes_question()] | Texte exact d'une ou de plusieurs questions, en français ou en anglais. |
#' | Codebooks et recherche | [qes_search()] | Cherche des variables dans toutes les études, sans tenir compte de la casse ni des accents : `qes_search("souverain")`. |
#' | Codebooks et recherche | [qes_missing()] | Remplace par `NA` les codes « ne sait pas », « refus » et les codes manquants déclarés. |
#' | Harmonisation | [qes_spec()] | La spécification d'harmonisation : quelles études ont quelle variable harmonisée (« cible »), la comparabilité de la question de chaque étude et l'appariement de ses codes. |
#' | Harmonisation | [qes_harmonize()] | Un seul tableau pour plusieurs études, une colonne par cible, chaque valeur manquante avec son motif, selon les règles d'harmonisation ; vagues, pondérations et admissibilité de chaque personne. |
#' | Harmonisation | [qes_decon()] | Un seul tableau de variables harmonisées de façon souple pour toutes les études, sous des noms simples à la manière de cesR (`education`, `income_cat`, `vote_choice`, `sovereignty`...) : un concept par colonne même quand les questions diffèrent, en catégories communes larges, avec la source et le recodage de chaque étude. |
#' | Harmonisation | [qes_design()] | Les données harmonisées en plan de sondage des packages survey ou srvyr, avec la pondération qui convient aux cibles. |
#' | Harmonisation | [qes_party_lineage()] | Réunit l'ADQ et la CAQ (et, au besoin, Option nationale et Québec solidaire) en une seule filiation, pour les séries chronologiques des partis québécois. |
#' | Reproductibilité | [qes_provenance()] | Indique de quel fichier viennent les données : DOI, version, fichier, md5, date ; pour les données harmonisées, aussi la ligne de la spécification et le niveau de chaque cellule. |
#' | Reproductibilité | [qes_cite()] | Citation de qesR et de chaque jeu de données, en texte, BibTeX ou `bibentry`. |
#' | Cache | [qes_cache_info()], [qes_cache_clear()] | Liste ou supprime les fichiers gardés dans le cache de téléchargement. |
#'
#' L'harmonisation entre études est expérimentale : [qes_spec()] montre la
#' spécification révisée, [qes_harmonize()] l'applique et [qes_design()] en
#' fait un plan de sondage ; la référence
#' générée à partir de la spécification est
#' `vignette("fr-reference-harmonisation", package = "qesR")`. [qes_decon()]
#' en est la version souple : une colonne par concept pour chaque étude, en
#' catégories communes larges et sans niveau de comparabilité.
#'
#' Les anciens noms de fonctions (`get_codebook()`, `get_question()`,
#' `get_preview()`, `get_qescodes()`, ...) continuent de fonctionner et ne
#' seront pas retirés ; voir [qesR-deprecated] pour la fonction qui remplace
#' chacun.
#'
#' @section Langue:
#' `options(qesR.lang = "fr")` affiche les messages, avertissements et
#' erreurs en français. Elle ne change jamais les données ni le texte
#' retournés, et ne fixe jamais l'argument `lang` des fonctions : il faut le
#' donner à chaque appel. Il n'a pas le même rôle partout :
#' * [qes_harmonize()] : `"en"` (par défaut) ou `"fr"`, la langue des
#'   niveaux des facteurs, des étiquettes de variables et des populations
#'   (`lang = "fr"` donne `Homme`, `Femme`, ...) ; les codes ne changent
#'   pas ;
#' * [qes_spec()] : `"en"` (par défaut) ou `"fr"`, la langue des
#'   étiquettes, définitions, justifications et notes ;
#' * [qes_codebook()] et [qes_question()] : `NULL` (par défaut), la langue
#'   de l'étude, ou `"en"`, `"fr"` ; la langue du texte des questions (les
#'   étiquettes sont celles du fichier) ;
#' * [qes_search()] : `"both"` (par défaut), `"en"` ou `"fr"`, les langues
#'   où chercher et celle de la colonne `question` ;
#' * [qes_cite()] : `"en"` (par défaut) ou `"fr"`, la langue des quelques
#'   mots que qesR ajoute aux citations ;
#' * [qes_docs()] et [qes_download()] : un filtre, les langues des documents
#'   à garder (`NULL`, par défaut, les garde toutes).
#'
#' @section Données de démonstration:
#' L'étude synthétique `qes_demo` est livrée avec qesR : `get_qes("qes_demo")`
#' la lit sans téléchargement. Ses 60 répondants sont tirés au hasard ; aucune
#' valeur ne vient d'un vrai répondant.
#'
#' @section Guides:
#' `vignette("fr-demarrage", package = "qesR")` (démarrage : du code d'étude
#' à une estimation pondérée), `vignette("fr-citations", package = "qesR")`
#' (citations), `vignette("fr-migrer-0.7", package = "qesR")` (mettre à jour du
#' code ancien) et
#' `vignette("fr-reference-harmonisation", package = "qesR")` (référence de
#' l'harmonisation, générée à partir de la spécification). Le site web, <https://thomasgareau.github.io/qesR/>,
#' offre aussi en français le catalogue des études et des exemples
#' d'analyse construits à partir des fichiers complets (bouton FR/EN de la barre de navigation).
#'
#' @section Licence:
#' La licence MIT de qesR (fichier `LICENSE`) couvre le code du package
#' seulement. Les données ne font pas partie du package : qesR les
#' télécharge depuis leurs dépôts. La plupart des études sont sous licence
#' CC0 1.0. L'étude de 2022 est sous licence CC BY-NC 4.0 (attribution, pas
#' d'usage commercial) : les métadonnées que qesR en livre (étiquettes,
#' texte des questions, effectifs) sont tirées de Mahéo, Bélanger,
#' Stephenson et Harell (2023), « 2022 Quebec Election Study », Harvard
#' Dataverse, V1.1, \doi{10.7910/DVN/PAQBDR}, restent sous cette licence
#' (<https://creativecommons.org/licenses/by-nc/4.0/deed.fr>) et ne sont pas
#' couvertes par la licence MIT du package ; cela n'implique aucune
#' approbation de qesR par les auteurs. Les effectifs du recensement
#' servant de référence sont adaptés de Statistique Canada (Licence ouverte
#' de Statistique Canada). Le fichier `COPYRIGHTS` du package
#' (`system.file("COPYRIGHTS", package = "qesR")`) détaille le contenu
#' livré, sa source, sa licence et les modifications faites par qesR.
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
