# Référence de l'harmonisation

*[English
version](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md)*

Les variables harmonisées (« cibles ») de
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
étude par étude. Toute la page ci-dessous est générée à partir de la
spécification fournie avec qesR : elle décrit donc toujours les règles
que la version installée applique.

Cette référence est générée à partir de la spécification d’harmonisation
fournie avec qesR : version 4.2.0 du 2026-09-28, empreinte du contenu
`02b3b7edc509deff0db16859bef7bfb6`. Elle est **expérimentale** : les
cibles, les niveaux de comparabilité et les appariements sont révisés
étude par étude et peuvent changer. Rien sur cette page n’est écrit à la
main ;
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
renvoie les mêmes informations sous forme de tableaux.

## Comment lire cette référence

Chaque cible correspond à un seul stimulus de question : une
formulation, une échelle, un moment ou un format différent donne une
autre cible, et les cibles ne sont jamais regroupées. Pour chaque étude,
le tableau de couverture donne la variable source et sa vague, le niveau
de comparabilité de la question de l’étude par rapport à la question
d’ancrage de la cible et sa raison, l’instrument (le format de la
question), les niveaux offerts, le libellé de la question (ou, quand il
ne peut pas être fourni, le document et la page qui le donnent), la
question filtre et le sens de chacun de ses codes, la pondération
recommandée de l’étude (signalée quand elle est à réviser :
qes_harmonize() ne l’applique pas, et ses colonnes de pondération valent
NA) et si « je ne sais pas » était offert.

Une ligne sans mention est approuvée par un réviseur (statut stable ;
dans la spécification 4.0.0, par une double révision automatisée sur les
fichiers et documents originaux, et non par une révision humaine), et
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
l’applique par défaut. Une ligne marquée « en révision » a été vérifiée
sur les fichiers et documents originaux mais n’est pas approuvée (la
colonne `review_note` de `qes_spec("crosswalk")` dit pourquoi elle est
retenue) ; une ligne marquée « provisoire » n’est pas encore vérifiée.
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
n’applique celles-ci qu’avec `include_draft = TRUE`.

Licence : la description de qes2022 (étiquettes, texte des questions,
effectifs) est tirée de « 2022 Quebec Election Study » (Mahéo, Bélanger,
Stephenson et Harell, 2023, <https://doi.org/10.7910/DVN/PAQBDR>) et est
sous licence CC BY-NC 4.0
(<https://creativecommons.org/licenses/by-nc/4.0/>) : attribution, pas
d’usage commercial. Adaptée par qesR (extraite, reformatée, typée et
totalisée). Elle n’est pas couverte par la licence MIT de qesR.
Modifications et liste des fichiers : system.file(“COPYRIGHTS”, package
= “qesR”). Citation complète : qes_cite(“qes2022”).

### Niveaux de comparabilité

- **`identical`** (Identique) : le même énoncé dans chaque langue, les
  mêmes options, la même option « je ne sais pas », le même univers et
  le même mode que la question d’ancrage ;
- **`comparable`** (Comparable) : le même concept et le même stimulus ;
  les différences (un adverbe de temps, l’ordre des options, l’offre de
  « je ne sais pas », les petits partis nommés) ne devraient pas
  modifier les proportions des niveaux communs ;
- **`approximate`** (Approximatif) : le même concept, mais un format, un
  filtre ou un mode qui devrait modifier les proportions ;
  `qes_harmonize(min_grade = "comparable")` met ces cellules à `NA`
  (motif `below_grade`) ;
- **`not_comparable`** (Non comparable) : un autre concept, ou une
  source à ne pas utiliser : consignée ici, jamais appariée.

### Zéros structurels

Un niveau que la question d’une étude n’offrait pas (un parti absent de
sa liste, par exemple) est un *zéro structurel* : sa part dans cette
étude est nulle parce que personne ne pouvait le choisir, et non parce
que personne ne l’appuyait. Dans les tableaux de couverture, ces niveaux
suivent la mention « non offerts ».
`qes_provenance(x, level = "cell")$levels_not_offered` les donne pour
les données harmonisées.

### Pourquoi une valeur manque

Chaque valeur manquante d’une cible porte l’un de ces motifs
(`qes_harmonize(missing = "reasons")` les ajoute dans des colonnes
`<cible>__na`) :

| Code | Étiquette |
|----|----|
| `dk` | Ne sait pas |
| `refused` | Refus |
| `dk_refused` | Ne sait pas ou refus (un seul code) |
| `no_answer` | Sans réponse (non-réponse partielle) |
| `not_selected` | Non sélectionné (choix multiple) |
| `inapplicable` | Sans objet (écarté par un filtre) |
| `not_voted` | N’a pas voté |
| `spoiled` | Bulletin annulé |
| `ineligible` | Pas admissible au vote |
| `not_registered` | Pas inscrit sur la liste électorale |
| `not_in_wave` | Absent de cette vague |
| `not_mappable` | Catégorie source à cheval sur plusieurs niveaux |
| `sysmis` | Valeur manquante système |
| `not_asked` | Question non posée dans cette étude |
| `not_reviewed` | Ligne de correspondance pas encore approuvée par un réviseur |
| `below_grade` | Sous le niveau de comparabilité demandé |
| `unmapped` | Code non apparié |

## Cibles

### Plan de sondage

#### `survey_mode` : Mode d’entrevue

Comment la personne a été interrogée : en ligne, par téléphone ou de
façon mixte. Colonne de tête de tout résultat de qes_harmonize(),
remplie à partir du mode de chaque vague (waves.csv) ; il n’y a de
lignes de correspondance que pour les vagues dont le mode varie selon la
personne.

Famille `interview_mode` · type Catégorielle · moment Tout moment ·
statut Expérimental · ajoutée dans la spécification 0.2.0

**Niveaux**

| Code | Nom     | Étiquette |
|------|---------|-----------|
| 1    | `web`   | Web       |
| 2    | `phone` | Téléphone |
| 3    | `mixed` | Mixte     |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `method` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interview_mode | web, phone; non offerts : mixed | document 341538, method |  | `weight` | Non offert |
| qes2007 | `type` (post) | `identical` | Le mode d’entrevue tel que le fichier l’enregistre. | interview_mode | web, phone; non offerts : mixed | document 425921, type |  | `pond` | Non offert |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.2.0 (2026-09-27) : Vagues, pondérations et admissibilité : les
  nouvelles cibles age (âge en années, tel que demandé), birth_month,
  age_group3 (trois tranches d’âge, à partir de questions offrant ces
  tranches ou des tranches qui s’y regroupent exactement) et citizen, et
  survey_mode, qui donne le mode d’entrevue de la vague préélectorale de
  qes2018_panel, où il varie selon la personne. 11 lignes de
  correspondance : birth_year de qes2012, qes2014 et qes2018 ;
  birth_month et age de qes2018 ; age et citizen de qes2022 ; age_group3
  de qes2007_panel, qes2012_panel et qes2018_panel ; survey_mode de
  qes2018_panel. Avec leurs tables de correspondance, leurs cellules de
  gates.csv, leurs marges attendues et leurs empreintes de colonnes.
  Aucune ligne, aucun code ni aucun niveau de comparabilité existant n’a
  changé.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.

### Vote et participation

#### `vote_prov_recall` : Vote provincial (rappel)

Parti pour lequel la personne dit avoir voté à l’élection générale
québécoise de l’étude, question posée après cette élection. Les
abstentionnistes, les bulletins annulés et les personnes non admissibles
ou non inscrites sont des valeurs manquantes avec un motif, jamais un
parti.

Famille `vote_prov` · type Catégorielle · moment Postélectoral · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Niveaux**

| Code | Nom     | Étiquette   |
|------|---------|-------------|
| 1    | `PLQ`   | PLQ         |
| 2    | `PQ`    | PQ          |
| 3    | `CAQ`   | CAQ         |
| 4    | `QS`    | QS          |
| 5    | `PVQ`   | PVQ         |
| 6    | `PCQ`   | PCQ         |
| 7    | `ON`    | ON          |
| 8    | `ADQ`   | ADQ         |
| 90   | `other` | Autre parti |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_votechoice` (pes) | `comparable` | Propose les quatre principaux partis et les conservateurs, mais ni le Parti vert ni Option nationale, sans « Je ne sais pas » ; l’annulation est une option et la question suit une question de participation qui ménage la face. | vote_recall_list | PLQ, PQ, CAQ, QS, PCQ, other; non offerts : PVQ, ON, ADQ | Pour quel parti avez-vous voté? | pes_turnout: 2 = not_voted, 3 = not_voted, 4 = not_voted, 5 = not_registered, 6 = dk | `pes_weight_general` | Non offert |
| qes2018 | `q6` (post) | `comparable` | Seuls les quatre principaux partis sont proposés (ni le Parti vert ni Option nationale), sans option « Je ne sais pas », l’annulation est une option et la question suit une question de participation qui ménage la face. | vote_recall_list | PLQ, PQ, CAQ, QS, other; non offerts : PVQ, PCQ, ON, ADQ | Pour quel parti avez-vous voté? | q5: 1 = not_voted, 2 = not_voted, 3 = not_voted, 5 = ineligible, 99 = refused, NA = inapplicable | `pond` | Non offert |
| qes2018_panel | `rts_q2` (post) | `comparable` | Liste des partis avec le nom des chefs, en mode mixte téléphone et Web ; l’ancrage est une liste Web sans les chefs. | vote_recall_list_leaders | PLQ, PQ, CAQ, QS, other; non offerts : PVQ, PCQ, ON, ADQ | Et pour qui avez-vous voté? | rts_q1: 1 = not_voted, 2 = not_voted, 4 = dk_refused | `weight_rts` | Non documenté |
| qes2014 | `Q3` (post) | `comparable` | Même liste de partis que l’ancrage, mais l’énoncé ne nomme pas la date de l’élection et aucune option « Je ne sais pas » n’est offerte. | vote_recall_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; non offerts : PCQ, ADQ | Pour quel parti avez-vous voté? | Q2: 2 = not_voted, 9 = refused | `POND` | Non offert |
| qes2012 | `q25` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | vote_recall_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; non offerts : PCQ, ADQ | Pour quel parti avez-vous voté lors de la dernière élection provinciale le 4 septembre 2012? | q21: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Offert explicitement |
| qes2012_panel | `voteprov` (post) | `approximate` | Rappel téléphonique spontané (options non lues) après une question de participation qui distingue le jour du scrutin et le vote par anticipation ; l’ancrage est une liste Web. | vote_recall_unprompted | CAQ, PLQ, PQ, QS, PVQ, ON, other; non offerts : PCQ, ADQ | Et pour quel parti avez-vous voté? (NE PAS LIRE) |  | `pond_post` (à réviser, non appliquée) | Non offert |
| qes2008 | `q12a` (post) | `comparable` | Même question (parti pour lequel la personne a voté) ; la liste nomme l’ADQ et non la CAQ ni ON, qui n’existaient pas ; la question de participation n’a pas de code « ne sais pas » ; téléphone selon les métadonnées du dépôt, l’ancrage est Web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; non offerts : CAQ, PCQ, ON | Pour quel parti avez-vous voté ? | q11: 2 = not_voted, 9 = refused |  | Non documenté |
| qes2007 | `q12` (post) | `comparable` | Même question (parti pour lequel la personne a voté, partis nommés dans l’énoncé) ; la liste nomme l’ADQ et non la CAQ ni ON, qui n’existaient pas ; l’étude combine des entrevues téléphoniques et Web (seul le questionnaire téléphonique est déposé), l’ancrage est Web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; non offerts : CAQ, PCQ, ON | Pour quel parti avez-vous voté ? Le Parti libéral, le Parti québécois, l’ADQ, Québec solidaire, le Parti vert ou un autre parti ? | q11: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Non documenté |
| qes2007_panel | `vote` (post) | `approximate` | Rappel téléphonique spontané (options non lues) après une question de participation en deux temps ; l’ancrage est une liste Web. | vote_recall_unprompted | ADQ, PLQ, PQ, QS, PVQ, other; non offerts : CAQ, PCQ, ON | Et pour quel parti avez-vous voté? |  | `pond_tot_am1` (à réviser, non appliquée) | Non offert |
| qes1998 | `q3post` (post) | `comparable` | Même question (parti pour lequel la personne a voté, liste lue) par téléphone, avec le même énoncé dans les questionnaires des deux firmes ; la Q3 de CREATEC utilise d’autres codes (1 PLQ, 2 PQ, 3 ADQ, 4 un autre parti) sans le Parti égalité, et le fichier regroupé les ramène aux codes de CROP, où le Parti égalité porte un astérisque (non lu) et n’est jamais choisi ; la liste nomme l’ADQ, pas la CAQ ; l’ancrage est Web. | vote_recall_list | ADQ, PLQ, PQ, other; non offerts : CAQ, QS, PVQ, PCQ, ON | 3\. Pour lequel des partis suivants avez-vous voté? |  | `ponder3` (à réviser, non appliquée) | Non offert |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `vote_prov_intent` : Intention de vote provinciale

Parti pour lequel la personne compte voter à la prochaine élection
générale québécoise, question posée avant cette élection, à la première
question, sans la relance des indécis. Ne voterait pas, aucun ou
annulerait est une réponse (niveau no_party), pas une valeur manquante.

Famille `vote_prov` · type Catégorielle · moment Préélectoral · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Niveaux**

| Code | Nom        | Étiquette                            |
|------|------------|--------------------------------------|
| 1    | `PLQ`      | PLQ                                  |
| 2    | `PQ`       | PQ                                   |
| 3    | `CAQ`      | CAQ                                  |
| 4    | `QS`       | QS                                   |
| 5    | `PVQ`      | PVQ                                  |
| 6    | `PCQ`      | PCQ                                  |
| 7    | `ON`       | ON                                   |
| 8    | `ADQ`      | ADQ                                  |
| 90   | `other`    | Autre parti                          |
| 95   | `no_party` | Ne voterait pas / aucun / annulerait |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_votechoice1` (cps) | `approximate` | Liste Web avec « je ne sais pas » et refus affichés, posée seulement aux personnes certaines ou susceptibles de voter (les autres ayant reçu une question distincte ou aucune), sans option « ne voterait pas » ; l’ancrage est une question téléphonique posée à tous. | vote_intent_list | PLQ, PQ, CAQ, QS, PCQ, other; non offerts : PVQ, ON, ADQ, no_party | Pour quel parti prévoyez-vous voter? | cps_turnout: 3 = inapplicable, 4 = inapplicable, 5 = inapplicable, 6 = ineligible | `cps_weight_general` | Offert explicitement |
| qes2018_panel | `rv1a` (pre) | `approximate` | Demande le candidat de quel parti la personne appuierait probablement si l’élection avait lieu demain, et demande à celles qui ont déjà voté par anticipation d’indiquer ce vote, de sorte qu’une partie des réponses rapportent un vote déjà exprimé ; mode mixte (850 en ligne, 400 par téléphone) ; l’ancrage demande pour quel parti elle voterait aujourd’hui, par téléphone. | vote_intent_list | PLQ, PQ, CAQ, QS, other, no_party; non offerts : PVQ, PCQ, ON, ADQ | En pensant à ce que vous ressentez maintenant, si une élection PROVINCIALE était tenue demain, le candidat de quel parti appuieriez-vous probablement? Si vous avez déjà voté par anticipation, veuillez indiquer pour quel parti. |  | `weight` | Non documenté |
| qes2012_panel | `intvoteprov1` (pre) | `approximate` | L’énoncé demande pour quel parti la personne voterait « ou serait tentée de voter », ce qui rapproche la première question d’une préférence ; le questionnaire préélectoral n’est pas déposé (libellé tiré de l’étiquette de la variable et du livre de codes, fichier 654292) ; par téléphone, comme l’ancrage. | vote_intent_or_lean_list | PLQ, PQ, CAQ, QS, PVQ, ON, other, no_party; non offerts : PCQ, ADQ | Si des élections provinciales devaient avoir lieu aujourd’hui, pour lequel des partis suivants voteriez-vous ou seriez-vous tenté de voter? |  | `pondam1` (à réviser, non appliquée) | Non documenté |
| qes_crop_2007_2010 | `intvoteprova` (chaque sondage) | `comparable` | Même première question que l’ancrage, mot pour mot, par la même firme (CROP) et le même mode téléphonique, avec les mêmes partis et chefs lus en rotation, et « annulerait/ne voterait pas » et « ne sait pas/refus » non lus ; les sondages sont des omnibus mensuels entre les élections, et non un panel de campagne, et le libellé n’est documenté que pour trois des 24 sondages (rapports CROP de mai 2008, janvier 2009 et mars 2009 ; aucun questionnaire n’est déposé). | vote_intent_list | ADQ, PLQ, PQ, QS, PVQ, other, no_party; non offerts : CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… |  | `XPOND` (à réviser, non appliquée) | Spontané seulement |
| qes2007_panel | `intvote1` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | vote_intent_list | ADQ, PLQ, PQ, QS, PVQ, other, no_party; non offerts : CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… |  | `pondam1` (à réviser, non appliquée) | Spontané seulement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `vote_prov_intent_push` : Intention de vote provinciale, indécis relancés

Intention de vote où les personnes indécises à la première question ont
été relancées sur le parti vers lequel elles penchent, les deux réponses
étant combinées. Stimulus différent de vote_prov_intent : jamais
regroupé avec elle.

Famille `vote_prov` · type Catégorielle · moment Préélectoral · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Niveaux**

| Code | Nom        | Étiquette                            |
|------|------------|--------------------------------------|
| 1    | `PLQ`      | PLQ                                  |
| 2    | `PQ`       | PQ                                   |
| 3    | `CAQ`      | CAQ                                  |
| 4    | `QS`       | QS                                   |
| 5    | `PVQ`      | PVQ                                  |
| 6    | `PCQ`      | PCQ                                  |
| 7    | `ON`       | ON                                   |
| 8    | `ADQ`      | ADQ                                  |
| 90   | `other`    | Autre parti                          |
| 95   | `no_party` | Ne voterait pas / aucun / annulerait |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `rv1ab` (pre) | `approximate` | Combinaison, faite par le producteur, de rv1a et de la relance rv1b, en mode mixte téléphone et Web ; l’ancrage est une relance téléphonique. Le filtre de la relance est plus étroit que celui de l’ancrage : les personnes qui disaient ne pas voter ou n’appuyer aucun parti (63) n’ont pas été relancées, alors que l’ancrage les relançait, et 31 des 233 indécis, tous interviewés par téléphone, n’ont pas reçu rv1b et restent indécis. | intent_lean_push | PLQ, PQ, CAQ, QS, other, no_party; non offerts : PVQ, PCQ, ON, ADQ | En pensant à ce que vous ressentez maintenant, si une élection PROVINCIALE était tenue demain, le candidat de quel parti appuieriez-vous probablement? (relance, rv1b : Et pour quel parti diriez-vous que vous auriez tendance à voter?) |  | `weight` | Non documenté |
| qes2012_panel | `intvoteprov` (pre) | `approximate` | Combinaison, faite par le producteur, de la première question (qui demande déjà un parti pour lequel la personne « serait tentée de voter ») et de la relance ; le questionnaire préélectoral n’est pas déposé (libellé tiré de l’étiquette de la variable et du livre de codes, fichier 654292) ; par téléphone, comme l’ancrage. | intent_lean_push | PLQ, PQ, CAQ, QS, PVQ, ON, other, no_party; non offerts : PCQ, ADQ | Q2+Q3 - Si des élections provinciales devaient avoir lieu aujourd’hui, pour lequel des partis suivants voteriez-vous ou seriez-vous tenté de voter? (relance : Peut-être que votre choix n’est pas définitif, mais y a-t-il tout de même un parti que vous seriez tenté d’appuyer?) |  | `pondam1` (à réviser, non appliquée) | Non documenté |
| qes_crop_2007_2010 | `intvoteprov` (chaque sondage) | `comparable` | Combinaison, faite par le producteur, de la première question et de la relance, comme dans l’ancrage, par la même firme et le même mode ; sondages téléphoniques CROP ; le livre de codes déposé (fichier 341537) ne donne que des étiquettes tronquées, mais les rapports de CROP pour La Presse (mai 2008, janvier 2009 ; voir dev/open-questions.md 1.1) donnent le même libellé et la même relance que l’ancrage, les partis et leurs chefs étant lus en rotation et le « ne sais pas » n’étant noté que s’il est spontané ; pas identique, car ce sont des sondages omnibus mensuels, pour la plupart hors campagne (l’élection de référence est celle de 2008 ou de 2012 selon le sondage), sur un échantillon stratifié par région (500 Montréal, 200 Québec, 300 ailleurs). | intent_lean_push | ADQ, PLQ, PQ, QS, PVQ, other, no_party; non offerts : CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… (relance : Peut-être n’êtes-vous pas complètement décidé(e), mais actuellement pour lequel de ces partis seriez-vous tenté(e) de voter? Est-ce…) |  | `XPOND` (à réviser, non appliquée) | Spontané seulement |
| qes2007_panel | `intvote` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | intent_lean_push | ADQ, PLQ, PQ, QS, PVQ, other, no_party; non offerts : CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? (relance, Q5 : pour lequel de ces partis seriez-vous tenté(e) de voter?) |  | `pondam1` (à réviser, non appliquée) | Spontané seulement |
| qes1998 | `intvote` (pre) | `approximate` | Combinaison, faite par le producteur, de la première question et de la relance, mais sondages téléphoniques de deux firmes regroupés dans une variable (CREATEC et CROP, chacune avec son questionnaire ; celui de CREATEC n’est pas déposé) ; CREATEC n’a pas posé la question aux 79 personnes qui, à sa question sur la participation, disaient ne probablement pas (22) ou certainement pas (35) aller voter, ou ne savaient pas ou refusaient de répondre (22), alors que CROP l’a posée à tous. | intent_lean_push | ADQ, PLQ, PQ, other, no_party; non offerts : CAQ, QS, PVQ, PCQ, ON | 7a. S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Est-ce… (relance, 7b : Peut-être n’êtes-vous pas complètement décidé(e), mais actuellement pour lequel de ces partis seriez-vous tenté(e) de voter?) |  | `ponder3` (à réviser, non appliquée) | Non documenté |

**Non utilisé**

- qes1998 `intvote2` (pre) : `not_comparable`. Les étiquettes de valeur
  sont décalées dans la source : les effectifs sont ceux de vpl, dont le
  code 1 est l’ADQ, 3 le PLQ et 4 le PQ, mais intvote2 les étiquette
  PLQ, ADQ et Parti égalité.

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.

#### `turnout_prov_recall` : A voté à l’élection provinciale (rappel)

Si la personne dit avoir voté à l’élection générale québécoise de
l’étude, question posée après cette élection. Les personnes non
admissibles ou non inscrites sont des valeurs manquantes avec un motif.

Famille `turnout_prov` · type Catégorielle · moment Postélectoral ·
statut Expérimental · ajoutée dans la spécification 0.1.0

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_turnout` (pes) | `approximate` | Format de participation qui ménage la face, avec trois façons de ne pas avoir voté, une option de non-inscription et une option « je ne m’en souviens pas » ; devrait modifier la part des votants. | turnout_excuse_format | yes, no | À chaque élection, certaines personnes sont dans l’incapacité d’aller voter parce qu’elles sont malades, occupées, ou pour toute autre raison. D’autres personnes ne veulent pas voter. Avez-vous voté lors de l’élection du 3 octobre 2022 au Québec? |  | `pes_weight_general` | Offert explicitement |
| qes2018 | `q5` (post) | `approximate` | Format de participation qui ménage la face, avec trois façons de ne pas avoir voté et une option de non-admissibilité, sans « Je ne sais pas » ; devrait modifier la part des votants. | turnout_excuse_format | yes, no | À chaque élection, plusieurs personnes sont incapables de voter parce qu’elles ne sont pas inscrites sur la liste électorale, elles sont malades ou elles n’ont pas le temps. Laquelle des situations suivantes correspond le mieux à votre cas? |  | `pond` | Non offert |
| qes2018_panel | `rts_q1` (post) | `approximate` | Format qui ménage la face (voulait voter mais n’a pas pu ; a décidé de ne pas voter ; ou est allé voter), en mode mixte téléphone et Web. | turnout_excuse_format | yes, no | À chaque élection, certaines personnes décident de ne pas voter, d’autres ne peuvent pas y aller pour différentes raisons. |  | `weight_rts` | Non documenté |
| qes2014 | `Q2` (post) | `comparable` | Même format oui/non, mais sans option « Je ne sais pas » (l’ancrage en offre une) et l’énoncé renvoie à « cette élection » au lieu d’en donner la date. | turnout_yesno | yes, no | Avez-vous voté à cette élection provinciale? |  | `POND` | Non offert |
| qes2012 | `q21` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | turnout_yesno | yes, no | Avez-vous voté lors de l’élection provinciale du 4 septembre 2012? |  | `pond` | Offert explicitement |
| qes2012_panel | `participation` (post) | `comparable` | Même question oui/non ; un oui est précisé (jour du scrutin ou vote par anticipation), les deux étant oui ; sans code « ne sais pas » (l’ancrage en offre un) ; par téléphone, l’ancrage est Web. | turnout_yesno_probe | yes, no | D’abord, pouvez-vous me dire si vous êtes allé voter lors de l’élection qui vient de se tenir au Québec? (SI OUI, SONDER) |  | `pond_post` (à réviser, non appliquée) | Non offert |
| qes2008 | `q11` (post) | `comparable` | Même question oui/non, mais sans code « ne sais pas » (l’ancrage en offre un) ; téléphone selon les métadonnées du dépôt, l’ancrage est Web. | turnout_yesno | yes, no | Avez-vous voté à cette élection provinciale ? |  |  | Non offert |
| qes2007 | `q11` (post) | `comparable` | Même question oui/non avec un code « ne se souvient pas » ; l’étude combine des entrevues téléphoniques et Web (seul le questionnaire téléphonique est déposé), l’ancrage est Web. | turnout_yesno | yes, no | Avez-vous voté à cette élection provinciale ? |  | `pond` | Non documenté |
| qes2007_panel | `voteoui` (post) | `comparable` | Même question oui/non ; un oui est précisé (jour du scrutin ou vote par anticipation), les deux étant oui ; sans code « ne sais pas » (l’ancrage en offre un) ; par téléphone, l’ancrage est Web. | turnout_yesno_probe | yes, no | D’abord, pouvez-vous me dire si vous êtes allé voter lors de l’élection qui vient de se tenir au Québec? (SI OUI, SONDER) |  | `pond_tot_am1` (à réviser, non appliquée) | Non offert |
| qes1998 | `q1post` (post) | `comparable` | Même question oui/non par téléphone, avec le même énoncé pour les deux firmes ; sans code « ne sais pas » (l’ancrage en offre un) ; l’ancrage est Web. | turnout_yesno | yes, no | 1\. Pouvez-vous me dire si vous avez voté à l’élection du 30 novembre dernier? |  | `ponder3` (à réviser, non appliquée) | Non offert |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 2.0.0 (2026-09-27) : Corrections de révision. MAJEURE (un filtre
  corrigé, des empreintes de colonnes modifiées) : les lignes religion
  de qes2012 (q103) et de qes2014 (Q63) sont filtrées sur leur question
  filtre (q102, Q62) : les personnes sans religion sont inapplicable
  (842 et 845) et celles qui ont préféré ne pas répondre à la question
  filtre refused (32 et 44), là où elles étaient sysmis ; valeurs
  inchangées. Un filtre s’applique maintenant aux règles weight, date et
  string comme aux règles map et numeric, un filtre sur une autre règle
  est une erreur V-S1, et gates.csv contient les cellules des lignes
  string filtrées pour V-D7. La participation déclarée de qes2007_panel
  lit la question de participation postélectorale voteoui (oui le jour
  du scrutin, oui par anticipation, non) au lieu de la recodification
  avote du producteur, avec les mêmes valeurs et la même empreinte de
  colonne ; niveau comparable (la même question oui/non précisée que
  qes2012_panel), et non approximate. Texte seulement : les
  justifications des lignes education4 de qes2018 et de qes2018_panel et
  la description de la cible disent où se place le diplôme d’études
  professionnelles (DEP : secondaire dans qes2018, collégial dans
  qes2018_panel et probablement là où aucune option DEP n’est offerte) ;
  la note de la colonne language de qes1998 dans legacy.csv dit que la
  constante n’est une langue maternelle que pour les lignes CREATEC (la
  langue parlée à la maison pour les 426 lignes CROP ; la définition
  regroupée n’est pas encore confirmée) ; la description de religion et
  la note héritée nomment le filtre ; les notes de preuve de qes2022 ne
  donnent que des codes et des pages du livre de codes (sa licence, CC
  BY-NC, tient son libellé et ses étiquettes hors du package).
- 2.0.1 (2026-09-27) : Texte seulement : les lignes de
  sovereignty_support et de sovereignty dans legacy.csv donnent à
  qes2007_panel le motif de qes2007, qes2008 et qes1998 (il a posé la
  question de 1995, voir sov_partnership_1995) et aux sondages CROP le
  leur (la première question référendaire, intvoterefa, n’a qu’une
  étiquette tronquée et aucun questionnaire déposé : son libellé est
  inconnu et elle n’est pas appariée) ; la colonne cause de legacy.csv
  (attr(, “legacy_na_columns”)\$cause de get_qes_master() et de
  get_decon()) a des valeurs descriptives (reported_vote_only,
  independence_question_only, no_valid_source, not_harmonized_yet) au
  lieu d’identifiants internes de décision, et le texte de legacy.csv,
  crosswalk.csv, valuemaps.csv et de ce journal dit chaque décision en
  mots. Aucune ligne, aucun code, niveau, marge ni empreinte n’a changé.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `turnout_prov_likely` : Probabilité de voter à l’élection provinciale

Probabilité que la personne dit avoir de voter à la prochaine élection
générale québécoise, question posée avant cette élection. Avoir déjà
voté (par anticipation) est une réponse. Stimulus différent de
turnout_prov_recall (participation déclarée) : jamais regroupé avec
elle.

Famille `turnout_prov` · type Ordinale · moment Préélectoral · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Niveaux**

| Code | Nom             | Étiquette                  |
|------|-----------------|----------------------------|
| 5    | `already_voted` | A déjà voté                |
| 1    | `certain`       | Certain(e) de voter        |
| 2    | `likely`        | Probablement voter         |
| 3    | `unlikely`      | Peu probable de voter      |
| 4    | `certain_not`   | Certain(e) de ne pas voter |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_turnout` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | turnout_likely | certain, likely, unlikely, certain_not, already_voted | L’élection au Québec est prévue pour le 3 octobre 2022. Dans le cadre de cette élection, êtes-vous… |  | `cps_weight_general` | Non offert |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `vote_prov_intent_other` : Intention de vote provinciale : autre parti (texte)

Le parti que la personne a inscrit après avoir choisi un autre parti à
la question d’intention de vote (vote_prov_intent, niveau other), tel
qu’inscrit. Texte libre, non harmonisé.

Famille `vote_prov` · type Texte · moment Préélectoral · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_votechoice1_8_TEXT` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | open_text |  | Pour quel parti prévoyez-vous voter? \[Autre parti (veuillez spécifier)\] | cps_turnout: 3 = inapplicable, 4 = inapplicable, 5 = inapplicable, 6 = ineligible | `cps_weight_general` | Non offert |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

### Identification partisane

#### `pid_prov` : Identification partisane provinciale

Parti provincial auquel la personne s’identifie habituellement ; aucun
est une réponse.

Famille `party_id` · type Catégorielle · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Niveaux**

| Code | Nom     | Étiquette        |
|------|---------|------------------|
| 1    | `PLQ`   | PLQ              |
| 2    | `PQ`    | PQ               |
| 3    | `CAQ`   | CAQ              |
| 4    | `QS`    | QS               |
| 5    | `PVQ`   | PVQ              |
| 6    | `PCQ`   | PCQ              |
| 7    | `ON`    | ON               |
| 8    | `ADQ`   | ADQ              |
| 90   | `other` | Autre parti      |
| 97   | `none`  | Aucun de ceux-là |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_provpid` (cps) | `comparable` | Même énoncé que l’ancrage, mais la liste retire les options oniste et vert, ajoute conservateur et un autre parti, et n’offre pas « je ne sais pas ». | pid_prov_list | PLQ, PQ, CAQ, QS, PCQ, other, none; non offerts : PVQ, ON, ADQ | En politique provinciale, vous considérez-vous habituellement comme étant: |  | `cps_weight_general` | Non offert |
| qes2018 | `q56` (post) | `comparable` | Même énoncé que l’ancrage, mais seuls les quatre principaux partis sont proposés (ni oniste ni vert). | pid_prov_list | PLQ, PQ, CAQ, QS, none; non offerts : PVQ, PCQ, ON, ADQ, other | En politique provinciale, vous considérez-vous habituellement comme un…? |  | `pond` | Offert explicitement |
| qes2014 | `Q55` (post) | `identical` | Même énoncé et mêmes six partis, options aucun, je ne sais pas et refus que l’ancrage, en anglais et en français, Web. | pid_prov_list | PLQ, PQ, CAQ, QS, ON, PVQ, none; non offerts : PCQ, ADQ, other | En politique provinciale, vous considérez-vous habituellement comme un…? |  | `POND` | Offert explicitement |
| qes2012 | `q92` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | pid_prov_list | PLQ, PQ, CAQ, QS, ON, PVQ, none; non offerts : PCQ, ADQ, other | En politique provinciale, vous considérez-vous habituellement comme un…? |  | `pond` | Offert explicitement |
| qes2008 | `q70` (post) | `comparable` | Même concept (identification partisane provinciale habituelle, aucun est une réponse) ; l’énoncé dit « habituellement est-ce que vous vous identifiez » et la liste nomme l’ADQ ; téléphone selon les métadonnées du dépôt, l’ancrage est Web. | pid_prov_list | PLQ, PQ, ADQ, QS, PVQ, other, none; non offerts : CAQ, PCQ, ON | En politique provinciale, habituellement est-ce que vous vous identifiez au / à…? |  |  | Non documenté |
| qes2007 | `q70` (post) | `comparable` | Même concept (identification partisane provinciale habituelle, aucun est une réponse) ; l’énoncé dit « habituellement est-ce que vous vous identifiez » et la liste nomme l’ADQ ; l’étude combine des entrevues téléphoniques et Web (seul le questionnaire téléphonique est déposé), l’ancrage est Web. | pid_prov_list | PLQ, PQ, ADQ, QS, PVQ, other, none; non offerts : CAQ, PCQ, ON | En politique provinciale, habituellement est-ce que vous vous identifiez …? |  | `pond` | Non documenté |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `pid_fed` : Identification partisane fédérale

Le parti fédéral dont la personne se sent habituellement proche ; aucun
est une réponse.

Famille `party_id` · type Catégorielle · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Niveaux**

| Code | Nom     | Étiquette        |
|------|---------|------------------|
| 1    | `LPC`   | Libéral          |
| 2    | `CPC`   | Conservateur     |
| 3    | `NDP`   | NPD              |
| 4    | `BQ`    | Bloc Québécois   |
| 5    | `GPC`   | Vert             |
| 6    | `PPC`   | PPC              |
| 90   | `other` | Un autre parti   |
| 97   | `none`  | Aucun de ceux-là |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_fedpid` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | pid_list | LPC, CPC, NDP, BQ, GPC, PPC, other, none | En politique fédérale, vous considérez-vous habituellement comme étant : |  | `cps_weight_general` | Non offert |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

### Attitudes

#### `sov_indep` : Vote référendaire : pays indépendant

Comment la personne voterait à un référendum demandant si le Québec doit
devenir un pays indépendant.

Famille `sovereignty` · type Catégorielle · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Niveaux**

| Code | Nom              | Étiquette                      |
|------|------------------|--------------------------------|
| 1    | `yes`            | Oui                            |
| 2    | `no`             | Non                            |
| 95   | `would_not_vote` | N’irait pas voter / annulerait |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_qc_referendum` (cps) | `comparable` | L’énoncé ajoute un adverbe de temps ; « je ne sais pas » est offert, mais pas « je préfère ne pas répondre ». | sov_indep_country | yes, no; non offerts : would_not_vote | Si un référendum sur l’indépendance avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? |  | `cps_weight_general` | Offert explicitement |
| qes2018 | `q26` (post) | `comparable` | L’énoncé ajoute un adverbe de temps (« today », « aujourd’hui ») ; sinon même libellé et mêmes options que l’ancrage. | sov_indep_country | yes, no; non offerts : would_not_vote | Si un référendum sur l’indépendance avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? |  | `pond` | Offert explicitement |
| qes2014 | `Q19` (post) | `identical` | Même énoncé que l’ancrage en anglais et en français, mêmes options (oui, non, je ne sais pas, je préfère ne pas répondre), posé à tous, Web. | sov_indep_country | yes, no; non offerts : would_not_vote | Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? |  | `POND` | Offert explicitement |
| qes2012 | `q52` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | sov_indep_country | yes, no; non offerts : would_not_vote | Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? |  | `pond` | Offert explicitement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 2.0.1 (2026-09-27) : Texte seulement : les lignes de
  sovereignty_support et de sovereignty dans legacy.csv donnent à
  qes2007_panel le motif de qes2007, qes2008 et qes1998 (il a posé la
  question de 1995, voir sov_partnership_1995) et aux sondages CROP le
  leur (la première question référendaire, intvoterefa, n’a qu’une
  étiquette tronquée et aucun questionnaire déposé : son libellé est
  inconnu et elle n’est pas appariée) ; la colonne cause de legacy.csv
  (attr(, “legacy_na_columns”)\$cause de get_qes_master() et de
  get_decon()) a des valeurs descriptives (reported_vote_only,
  independence_question_only, no_valid_source, not_harmonized_yet) au
  lieu d’identifiants internes de décision, et le texte de legacy.csv,
  crosswalk.csv, valuemaps.csv et de ce journal dit chaque décision en
  mots. Aucune ligne, aucun code, niveau, marge ni empreinte n’a changé.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `sov_sovereign_country` : Vote référendaire : pays souverain

Comment la personne voterait à un référendum demandant si le Québec doit
devenir un pays souverain. Stimulus différent de sov_indep (souverain et
non indépendant) : jamais regroupé avec elle. La question de relance
posée aux indécis ne fait pas partie de cette cible.

Famille `sovereignty` · type Catégorielle · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Niveaux**

| Code | Nom              | Étiquette                      |
|------|------------------|--------------------------------|
| 1    | `yes`            | Oui                            |
| 2    | `no`             | Non                            |
| 95   | `would_not_vote` | N’irait pas voter / annulerait |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2012_panel | `intvoteref` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible ; libellé tiré de l’étiquette de la variable et du livre de codes seulement (le questionnaire préélectoral n’est pas déposé), la lecture de « ne voterait pas » et de « ne sait pas » n’est donc pas documentée. | sov_sovereign_country | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays souverain, voteriez-vous oui ou voteriez-vous non? |  | `pondam1` (à réviser, non appliquée) | Non documenté |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 3.0.0 (2026-09-27) : Pondérations, vagues et terrain à partir des
  documents trouvés pour les études qui n’en avaient pas
  (dev/open-questions.md). MAJEURE (un rôle de pondération corrigé et
  des pondérations recommandées modifiées) : pond de qes2008 reproduit
  le vote officiel de 2008 parmi les votants et est enregistrée comme
  vote_calibrated, de sorte que qes2008 n’a aucune pondération
  recommandée (ses deux pondérations sont calées sur le vote ou la
  participation ; V-S13 n’admet maintenant aucune pondération
  recommandée que dans ce cas) et que ses pondérations harmonisées
  restent NA ; pond de qes2007 (le livre des auteurs : sexe, âge, langue
  maternelle et région, pondérée dans chaque mode) et weight et
  weight_rts du panel de 2018 (le rapport d’Ipsos et Durand et Blais
  2020 : sexe, âge, région, langue maternelle et scolarité
  universitaire) sont révisées, de sorte que qes_harmonize() donne leurs
  pondérations, NA auparavant. La vague postélectorale du panel de 2007
  reçoit une pondération recommandée, pond_tot_am1, à réviser. Les
  autres pondérations gardent leur statut, avec ce que les fichiers et
  les documents montrent maintenant : la décomposition de 1998 (ponder3
  = ponderc x poids / c ; poids est le facteur de plan du recontact
  stratifié ; le dépôt conseille ponderc, qui ne corrige pas la
  sur-sélection), XPOND de CROP (les rapports de CROP : le recensement
  de 2006, selon le sexe, l’âge, la région et la langue parlée à la
  maison), les cellules des pondérations du panel de 2012, les marges de
  qes2012. Vagues : les dates de terrain des 24 sondages CROP (les
  rapports de CROP et les archives de quebecpolitique.com), l’élection à
  laquelle renvoie leur question sur le vote précédent QP4 et ce que
  l’on sait du libellé de leur première question référendaire (un pays
  souverain, documenté pour 19 sondages ; toujours non appariée,
  puisqu’une ligne de correspondance s’applique à tous les sondages) ;
  les dates du panel de 2018 (du 2018-09-26 au 09-28, du 2018-10-12 au
  10-19) et ses modes ; la définition des francophones de 1998,
  confirmée par l’appariement des lignes du fichier regroupé aux
  fichiers des firmes ; les indices sur le mode de qes2008 (toujours le
  téléphone, comme le dit le dépôt). Texte seulement : la preuve de
  lang_mother de qes2018 (les codes 2 et 96 ne sont pas intervertis), la
  preuve du genre des sondages CROP (un sondage sans SEXE), la
  description de sov_sovereign_country (la question de relance ne fait
  pas partie de la cible) et les notes de legacy.csv des colonnes de
  souveraineté, de participation et de vote des sondages CROP et des
  colonnes language et survey_weight de qes1998. Aucune table de
  correspondance, aucun filtre, niveau, marge ni empreinte de colonne
  n’a changé.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.

#### `sov_favour` : Appui à l’indépendance du Québec (4 points)

Degré d’appui ou d’opposition de la personne à l’indépendance du Québec,
sur une échelle à quatre points. Ce n’est pas un vote référendaire.

Famille `sovereignty` · type Ordinale · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Niveaux**

| Code | Nom                   | Étiquette       |
|------|-----------------------|-----------------|
| 1    | `very_favourable`     | Très favorable  |
| 2    | `somewhat_favourable` | Assez favorable |
| 3    | `somewhat_opposed`    | Assez opposé    |
| 4    | `very_opposed`        | Très opposé     |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `rts_q7` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible ; libellé tiré du livre de codes seulement (aucun questionnaire déposé), l’offre de « ne sait pas » sur le Web n’est donc pas documentée. | sov_favour_4pt | very_favourable, somewhat_favourable, somewhat_opposed, very_opposed | En ce qui concerne l’indépendance du Québec, c’est-à-dire que le Québec ne fasse plus partie du Canada, êtes-vous personnellement |  | `weight_rts` | Non documenté |

**Non utilisé**

- qes2018_panel `independance` (post) : `not_comparable`. Un recodage de
  rts_q7 (1-2 favorable, 3-4 opposé, 5 et manquant mis à manquant), pas
  une question distincte : jamais apparié.

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.

#### `lr_self` : Autopositionnement gauche-droite (0-10)

Position que la personne se donne sur une échelle de 0 (gauche) à 10
(droite).

Famille `left_right` · type Numérique · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Plage valide** : 0-10

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_ideoself_1` (cps) | `approximate` | Question isolée de 0 à 10, énoncé différent, sans « je ne sais pas » et non précédé des positionnements des partis ; la non-réponse partielle est bien plus faible que la part de « je ne sais pas » de l’ancrage. | lr_0_10 |  | En politique, on parle parfois de gauche et de droite. Où vous placeriez-vous sur une échelle de 0 à 10, où 0 indique la gauche et 10 la droite? |  | `cps_weight_general` | Non offert |
| qes2018 | `q36_1` (post) | `comparable` | Même énoncé que l’ancrage, précédé des positionnements des partis, mais les extrémités se lisent « à gauche » et « à droite » au lieu de « le plus à gauche » et « le plus à droite ». | lr_0_10 |  | Et sur la même échelle, où vous placeriez-vous, de manière générale? |  | `pond` | Offert explicitement |
| qes2018_panel | `rts_q8` (post) | `approximate` | Énoncé différent, mode mixte téléphone et Web ; le libellé déposé est tronqué à 80 caractères, de sorte que les libellés des extrémités ne peuvent être comparés. | lr_0_10 |  | On utilise souvent un axe gauche-droite pour situer les opinions politiques des gens. Sur une échelle de 0 |  | `weight_rts` | Non documenté |
| qes2014 | `Q32` (post) | `identical` | Même énoncé et mêmes libellés des extrémités que l’ancrage en anglais et en français, précédé des mêmes positionnements des partis, Web. | lr_0_10 |  | Et sur la même échelle, où vous placeriez-vous, de manière générale? |  | `POND` | Offert explicitement |
| qes2012 | `q71` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | lr_0_10 |  | Et sur la même échelle, où vous placeriez-vous, de manière générale? |  | `pond` | Offert explicitement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `interest_4pt` : Intérêt pour la politique (4 points)

Intérêt de la personne pour la politique, sur une échelle verbale à
quatre points. Jamais converti en ni regroupé avec les échelles
d’intérêt de 0 à 10.

Famille `interest` · type Ordinale · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Niveaux**

| Code | Nom          | Étiquette                |
|------|--------------|--------------------------|
| 1    | `very`       | Très intéressé(e)        |
| 2    | `quite`      | Plutôt intéressé(e)      |
| 3    | `hardly`     | Pas très intéressé(e)    |
| 4    | `not_at_all` | Pas du tout intéressé(e) |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `q27` (post) | `comparable` | L’énoncé ajoute « et les enjeux publics » ; les options se lisent très, assez, peu et pas du tout intéressé(e). | interest_4pt | very, quite, hardly, not_at_all | Quel est votre intérêt pour la politique et les enjeux publics en général? Êtes-vous: |  | `pond` | Offert explicitement |
| qes2014 | `Q28` (post) | `comparable` | Même énoncé en anglais et en français et mêmes options en français, mais la troisième option anglaise est « Not very interested » là où l’ancrage a « Hardly interested ». | interest_4pt | very, quite, hardly, not_at_all | Quel est votre intérêt pour la politique en général? Êtes-vous: |  | `POND` | Offert explicitement |
| qes2012 | `q67` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interest_4pt | very, quite, hardly, not_at_all | Quel est votre intérêt pour la politique en général? Êtes-vous: |  | `pond` | Offert explicitement |

**Non utilisé**

- qes2012_panel `interetrec` (pre) : `not_comparable`. Pas une question
  : un score d’intérêt pour la campagne que le producteur a dérivé de la
  part de chacun des quatre débats des chefs que la personne a regardée.

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.

#### `sov_partnership_1995` : Vote référendaire : la question de souveraineté-partenariat de 1995

Comment la personne voterait si un référendum avait lieu aujourd’hui sur
la question du référendum de 1995, la souveraineté assortie d’une offre
de partenariat au reste du Canada. Stimulus différent de sov_indep et de
sov_sovereign_country : jamais regroupé avec elles. La question de
relance posée aux indécis ne fait pas partie de cette cible.

Famille `sovereignty` · type Catégorielle · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.3.0

**Niveaux**

| Code | Nom              | Étiquette                      |
|------|------------------|--------------------------------|
| 1    | `yes`            | Oui                            |
| 2    | `no`             | Non                            |
| 95   | `would_not_vote` | N’irait pas voter / annulerait |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q19` (post) | `comparable` | Même énoncé et mêmes options que l’ancrage en anglais et en français ; le mode diffère (téléphone selon les métadonnées du dépôt ; l’étude d’ancrage combine téléphone et Web). | sov_partnership_1995 | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON ? |  |  | Non documenté |
| qes2007 | `q19` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible : la question de 1995 lue au complet ; « ne voterait pas/annulerait », « ne sais pas » et le refus sont des codes ; « ne sais pas » est relancé à q20, qui ne fait pas partie de la cible. | sov_partnership_1995 | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON ? |  | `pond` | Non documenté |
| qes2007_panel | `intref1` (pre) | `comparable` | Même question et mêmes options, par téléphone ; l’énoncé dit « accompagnée d’une offre de partenariat » où l’ancrage dit « assortie d’une offre » ; « ne voterait pas », « ne sait pas » et le refus ne sont pas lus. | sov_partnership_1995 | yes, no, would_not_vote | document 352416, 13 |  | `pondam1` (à réviser, non appliquée) | Spontané seulement |
| qes1998 | `q16a_crop` (pre) | `comparable` | Même énoncé et mêmes options que l’ancrage, par téléphone ; posée par CROP seulement (426 des 1 483), pas par CREATEC. | sov_partnership_1995 | yes, no, would_not_vote | 16a. Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON | firme_post: 1 = inapplicable | `ponder3` (à réviser, non appliquée) | Non documenté |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.3.0 (2026-09-27) : Les études restantes : qes2007, qes2008,
  qes_crop_2007_2010 et qes1998 entrent dans la spécification, avec
  leurs vagues et pondérations, et qes2012_panel reçoit ses lignes de
  vote et de participation. Les sondages CROP regroupés ont une vague
  par sondage mensuel (24), chacune avec son élection et sa propre
  normalisation de la pondération, et des lignes de correspondance de
  vague \* qui s’appliquent à chaque sondage. qes1998 est le panel
  regroupé des sondages CREATEC et CROP, francophones seulement, avec la
  firme comme strate. La nouvelle cible sov_partnership_1995 (la
  question de souveraineté-partenariat de 1995) a des lignes pour
  qes2007, qes2008, qes1998 et qes2007_panel. 25 lignes de
  correspondance appariées et 2 lignes de documentation (intvote2 de
  qes1998, dont les étiquettes de valeur sont décalées dans la source,
  et interetrec de qes2012_panel), avec leurs tables de correspondance,
  leurs cellules de gates.csv, leurs marges attendues et leurs
  empreintes de colonnes. Toutes les pondérations de ces études sont à
  réviser (les dépôts ne disent pas quelle pondération appliquer ; les
  livres de codes de 1998 indiquent que les personnes indécises, qui
  annuleraient leur vote ou qui refusaient de le révéler ont été
  sur-sélectionnées, ce que notent les pondérations, les vagues et les
  lignes de correspondance de qes1998). La vague postélectorale de
  qes2012_panel reçoit sa date d’entrevue (ResLastCallDate_last). Aucune
  ligne, aucun code ni aucun niveau de comparabilité existant n’a
  changé.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.

#### `interest_0_10` : Intérêt pour la politique (0-10)

Intérêt de la personne pour la politique en général, de 0 (aucun
intérêt) à 10 (beaucoup d’intérêt). Jamais regroupé avec l’échelle en
quatre points (interest_4pt) ni converti vers elle.

Famille `interest` · type Numérique · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Plage valide** : 0-10

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_interest_1` (cps) | `approximate` | Un curseur de 0 à 10 sur le Web, sans option « ne sait pas » ; l’ancrage est un nombre sur une échelle lue, avec « ne sait pas » et refus spontanés. | interest_0_10_slider |  | Quel est votre niveau d’intérêt pour la politique en général? Veuillez glisser le curseur sur un chiffre de 0 à 10, où 0 indique aucun intérêt du tout et 10 indique beaucoup d’intérêt. |  | `cps_weight_general` | Non offert |
| qes2007 | `q15` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interest_0_10 |  | Et toujours avec la même échelle, quel est votre intérêt pour la politique en général ? (Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt) |  | `pond` | Spontané seulement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `interest_election_0_10` : Intérêt pour l’élection provinciale (0-10)

Intérêt de la personne pour l’élection générale québécoise qui vient
d’avoir lieu, de 0 (aucun intérêt) à 10 (beaucoup d’intérêt), question
posée après l’élection. Intérêt pour une élection, pas pour la politique
en général.

Famille `interest` · type Numérique · moment Postélectoral · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Plage valide** : 0-10

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q14` (post) | `comparable` | Même question et même échelle ; le mode reste à confirmer, l’ancrage combine téléphone et Web. | interest_0_10 |  | Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt, quel a été votre intérêt pour l’élection PROVINCIALE qui vient d’avoir lieu ? |  |  | Offert explicitement |
| qes2007 | `q14` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interest_0_10 |  | Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt, quel a été votre intérêt pour l’élection PROVINCIALE qui vient d’avoir lieu ? |  | `pond` | Spontané seulement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.

#### `interest_campaign_4pt` : Intérêt pour la campagne électorale (4 points)

Intérêt de la personne pour la campagne électorale québécoise en cours,
sur une échelle verbale en quatre points, question posée pendant la
campagne. Intérêt pour une campagne, pas pour la politique en général
(interest_4pt).

Famille `interest` · type Ordinale · moment Préélectoral · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Niveaux**

| Code | Nom          | Étiquette                |
|------|--------------|--------------------------|
| 1    | `very`       | Très intéressé(e)        |
| 2    | `quite`      | Plutôt intéressé(e)      |
| 3    | `hardly`     | Pas très intéressé(e)    |
| 4    | `not_at_all` | Pas du tout intéressé(e) |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2007_panel | `interet` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interest_4pt | very, quite, hardly, not_at_all | Personnellement, vous intéressez-vous beaucoup, assez, peu ou pas du tout à la présente campagne électorale au Québec? |  | `pondam1` (à réviser, non appliquée) | Spontané seulement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.

### Sociodémographie

#### `birth_year` : Année de naissance

Année de naissance de la personne.

Famille `birth` · type Numérique · moment Invariant · statut
Expérimental · ajoutée dans la spécification 0.1.0

**Plage valide** : 1900-2010

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_yob` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | yob_list |  | Enfin, en quelle année êtes-vous né? |  | `cps_weight_general` | Non offert |
| qes2018 | `ageyear_1` (post) | `comparable` | Même question (année de naissance) ; l’année et le mois sont saisis, avec une option pour ne pas répondre (l’âge est alors demandé) ; l’ancrage propose une liste d’années sans cette option. | yob_month_entry |  | En quelle année êtes-vous né(e)? | agensp: 1 = refused | `pond` | Offert explicitement |
| qes2014 | `QAGE` (post) | `comparable` | Même question (année de naissance) ; l’année est saisie dans une case, avec une option pour ne pas répondre ; l’ancrage propose une liste d’années sans cette option. | yob_entry |  | En quelle année êtes-vous né(e)? |  | `POND` | Offert explicitement |
| qes2012 | `agex` (post) | `comparable` | Même question (année de naissance) ; l’année est saisie dans une case, avec une option pour ne pas répondre ; l’ancrage propose une liste d’années sans cette option. | yob_entry |  | En quelle année êtes-vous né(e)? |  | `pond` | Offert explicitement |
| qes2008 | `q75` (post) | `comparable` | Même question (année de naissance) ; l’année est saisie, avec une option pour ne pas répondre ; l’ancrage propose une liste d’années sans cette option. | yob_entry |  | Pour terminer l’entrevue, nous aimerions avoir quelques informations qui nous aideront à vérifier si notre échantillon représente bien l’ensemble de la population québécoise. D’abord, en quelle année êtes-vous né(e) ? (exemple:1972) Je préfère ne pas répondre 9999 |  |  | Non documenté |
| qes2007 | `q75` (post) | `comparable` | Même question (année de naissance) ; l’année est saisie, avec un code de refus ; l’ancrage propose une liste d’années sans ce code. | yob_entry |  | Pour terminer l’entrevue, nous aimerions avoir quelques informations qui nous aideront à vérifier si notre échantillon représente bien l’ensemble de la population québécoise. D’abord, en quelle année êtes-vous né(e) ? Notez l’année de naissance refus 9999 |  | `pond` | Non documenté |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.2.0 (2026-09-27) : Vagues, pondérations et admissibilité : les
  nouvelles cibles age (âge en années, tel que demandé), birth_month,
  age_group3 (trois tranches d’âge, à partir de questions offrant ces
  tranches ou des tranches qui s’y regroupent exactement) et citizen, et
  survey_mode, qui donne le mode d’entrevue de la vague préélectorale de
  qes2018_panel, où il varie selon la personne. 11 lignes de
  correspondance : birth_year de qes2012, qes2014 et qes2018 ;
  birth_month et age de qes2018 ; age et citizen de qes2022 ; age_group3
  de qes2007_panel, qes2012_panel et qes2018_panel ; survey_mode de
  qes2018_panel. Avec leurs tables de correspondance, leurs cellules de
  gates.csv, leurs marges attendues et leurs empreintes de colonnes.
  Aucune ligne, aucun code ni aucun niveau de comparabilité existant n’a
  changé.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `birth_month` : Mois de naissance

Mois de naissance de la personne (1 = janvier). Demandé avec l’année de
naissance dans certaines études ; avec birth_year, il indique si une
personne née 18 ans avant l’année de l’élection avait 18 ans le jour du
scrutin.

Famille `birth` · type Numérique · moment Invariant · statut
Expérimental · ajoutée dans la spécification 0.2.0

**Plage valide** : 1-12

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `agemonth_1` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | yob_month_entry |  | En quelle année êtes-vous né(e)? | agensp: 1 = refused | `pond` | Offert explicitement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.2.0 (2026-09-27) : Vagues, pondérations et admissibilité : les
  nouvelles cibles age (âge en années, tel que demandé), birth_month,
  age_group3 (trois tranches d’âge, à partir de questions offrant ces
  tranches ou des tranches qui s’y regroupent exactement) et citizen, et
  survey_mode, qui donne le mode d’entrevue de la vague préélectorale de
  qes2018_panel, où il varie selon la personne. 11 lignes de
  correspondance : birth_year de qes2012, qes2014 et qes2018 ;
  birth_month et age de qes2018 ; age et citizen de qes2022 ; age_group3
  de qes2007_panel, qes2012_panel et qes2018_panel ; survey_mode de
  qes2018_panel. Avec leurs tables de correspondance, leurs cellules de
  gates.csv, leurs marges attendues et leurs empreintes de colonnes.
  Aucune ligne, aucun code ni aucun niveau de comparabilité existant n’a
  changé.

#### `age` : Âge en années

Âge de la personne en années au moment de l’entrevue, tel que demandé.
Jamais calculé à partir de l’année de naissance, qui ne donne l’âge qu’à
un an près.

Famille `age_years` · type Numérique · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.2.0

**Plage valide** : 15-115

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_age_in_years` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | age_list |  | Afin d’être certains que nous nous adressons à un échantillon représentatif des Canadiens, nous avons besoin d’informations de base sur vous. Tout d’abord, quel âge avez-vous? |  | `cps_weight_general` | Non offert |
| qes2018 | `agenum` (post) | `approximate` | La même question (Quel âge avez-vous?, une liste d’âges) posée seulement aux personnes qui n’ont pas donné leur année et leur mois de naissance ; l’ancrage la pose à tout le monde. | age_list |  | Quel âge avez-vous? | agensp: 0 = inapplicable | `pond` | Offert explicitement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.2.0 (2026-09-27) : Vagues, pondérations et admissibilité : les
  nouvelles cibles age (âge en années, tel que demandé), birth_month,
  age_group3 (trois tranches d’âge, à partir de questions offrant ces
  tranches ou des tranches qui s’y regroupent exactement) et citizen, et
  survey_mode, qui donne le mode d’entrevue de la vague préélectorale de
  qes2018_panel, où il varie selon la personne. 11 lignes de
  correspondance : birth_year de qes2012, qes2014 et qes2018 ;
  birth_month et age de qes2018 ; age et citizen de qes2022 ; age_group3
  de qes2007_panel, qes2012_panel et qes2018_panel ; survey_mode de
  qes2018_panel. Avec leurs tables de correspondance, leurs cellules de
  gates.csv, leurs marges attendues et leurs empreintes de colonnes.
  Aucune ligne, aucun code ni aucun niveau de comparabilité existant n’a
  changé.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `age_group3` : Groupe d’âge (3 tranches)

Groupe d’âge de la personne au moment de l’entrevue, en trois tranches :
18-34, 35-54, 55 et plus. Construit seulement à partir d’une question
offrant ces tranches ou des tranches qui s’y regroupent exactement,
jamais par estimation.

Famille `age_bands` · type Ordinale · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.2.0

**Niveaux**

| Code | Nom        | Étiquette  |
|------|------------|------------|
| 1    | `a18_34`   | 18-34      |
| 2    | `a35_54`   | 35-54      |
| 3    | `a55_plus` | 55 et plus |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `age` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | age_3bands | a18_34, a35_54, a55_plus | document 341538, age |  | `weight` | Non documenté |
| qes2012_panel | `age` (pre) | `comparable` | Six tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64 et 65 et plus), par téléphone ; l’ancrage offre les trois tranches, par téléphone et en ligne. | age_6bands | a18_34, a35_54, a55_plus | document 654292, age |  | `pondam1` (à réviser, non appliquée) | Non documenté |
| qes_crop_2007_2010 | `QAGE` (chaque sondage) | `comparable` | Six tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64 et 65 et plus), par téléphone ; l’ancrage offre les trois tranches, par téléphone et en ligne. | age_6bands | a18_34, a35_54, a55_plus | Auquel des groupes d’àges suivants appartenez-vous? |  | `XPOND` (à réviser, non appliquée) | Non documenté |
| qes2008 | `q0age` (post) | `comparable` | Sept tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64, 65-74 et 75 et plus) ; l’ancrage offre les trois tranches. | age_7bands | a18_34, a35_54, a55_plus | Quel âge avez-vous ? |  |  | Non documenté |
| qes2007_panel | `age` (toute vague) | `comparable` | Six tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64 et 65 et plus), par téléphone ; l’ancrage offre les trois tranches, par téléphone et en ligne. | age_6bands | a18_34, a35_54, a55_plus | Auquel des groupes d’âges suivants appartenez-vous? |  |  | Non documenté |
| qes1998 | `age` (pre) | `comparable` | Six tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64 et 65 et plus), par téléphone ; l’ancrage offre les trois tranches, par téléphone et en ligne. | age_6bands | a18_34, a35_54, a55_plus | A quel groupe d’age appartenez-vous? (LIRE SI NECESSAIRE) |  | `ponder3` (à réviser, non appliquée) | Non documenté |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.2.0 (2026-09-27) : Vagues, pondérations et admissibilité : les
  nouvelles cibles age (âge en années, tel que demandé), birth_month,
  age_group3 (trois tranches d’âge, à partir de questions offrant ces
  tranches ou des tranches qui s’y regroupent exactement) et citizen, et
  survey_mode, qui donne le mode d’entrevue de la vague préélectorale de
  qes2018_panel, où il varie selon la personne. 11 lignes de
  correspondance : birth_year de qes2012, qes2014 et qes2018 ;
  birth_month et age de qes2018 ; age et citizen de qes2022 ; age_group3
  de qes2007_panel, qes2012_panel et qes2018_panel ; survey_mode de
  qes2018_panel. Avec leurs tables de correspondance, leurs cellules de
  gates.csv, leurs marges attendues et leurs empreintes de colonnes.
  Aucune ligne, aucun code ni aucun niveau de comparabilité existant n’a
  changé.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.

#### `citizen` : Citoyenneté canadienne

Si la personne a la citoyenneté canadienne, là où l’étude l’a demandé.
Seuls les citoyens peuvent voter aux élections québécoises.

Famille `citizenship` · type Catégorielle · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 0.2.0

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_citizen` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | citizen_status | yes, no | Êtes-vous… |  | `cps_weight_general` | Non offert |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.2.0 (2026-09-27) : Vagues, pondérations et admissibilité : les
  nouvelles cibles age (âge en années, tel que demandé), birth_month,
  age_group3 (trois tranches d’âge, à partir de questions offrant ces
  tranches ou des tranches qui s’y regroupent exactement) et citizen, et
  survey_mode, qui donne le mode d’entrevue de la vague préélectorale de
  qes2018_panel, où il varie selon la personne. 11 lignes de
  correspondance : birth_year de qes2012, qes2014 et qes2018 ;
  birth_month et age de qes2018 ; age et citizen de qes2022 ; age_group3
  de qes2007_panel, qes2012_panel et qes2018_panel ; survey_mode de
  qes2018_panel. Avec leurs tables de correspondance, leurs cellules de
  gates.csv, leurs marges attendues et leurs empreintes de colonnes.
  Aucune ligne, aucun code ni aucun niveau de comparabilité existant n’a
  changé.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `age_group6` : Groupe d’âge (6 tranches)

Groupe d’âge de la personne à l’entrevue, en six tranches : 18-24,
25-34, 35-44, 45-54, 55-64, 65 et plus. Construit seulement à partir
d’une question offrant ces tranches ou des tranches qui s’y regroupent
exactement, jamais d’une supposition.

Famille `age_bands` · type Ordinale · moment Tout moment · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Niveaux**

| Code | Nom        | Étiquette  |
|------|------------|------------|
| 1    | `a18_24`   | 18-24      |
| 2    | `a25_34`   | 25-34      |
| 3    | `a35_44`   | 35-44      |
| 4    | `a45_54`   | 45-54      |
| 5    | `a55_64`   | 55-64      |
| 6    | `a65_plus` | 65 et plus |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2012_panel | `age` (pre) | `comparable` | Les six tranches de l’ancrage, par téléphone ; les livres de codes ne disent pas si « ne sait pas » était offert. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | document 654292, age |  | `pondam1` (à réviser, non appliquée) | Non documenté |
| qes_crop_2007_2010 | `QAGE` (chaque sondage) | `identical` (ancrage) | Ligne d’ancrage de la cible. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Auquel des groupes d’àges suivants appartenez-vous? |  | `XPOND` (à réviser, non appliquée) | Non documenté |
| qes2008 | `q0age` (post) | `comparable` | Sept tranches d’âge, regroupées exactement dans les six de la cible (65-74 et 75 et plus en 65 et plus) ; l’ancrage offre les six tranches, par téléphone. | age_7bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Quel âge avez-vous ? |  |  | Non documenté |
| qes2007_panel | `age` (toute vague) | `comparable` | Les six tranches de l’ancrage, par téléphone ; les livres de codes ne disent pas si « ne sait pas » était offert. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Auquel des groupes d’âges suivants appartenez-vous? |  |  | Non documenté |
| qes1998 | `age` (pre) | `comparable` | Les six tranches de l’ancrage, par téléphone, posées par les deux firmes ; ni le livre de codes de l’ancrage ni celui-ci ne disent si « ne sait pas » était offert. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | A quel groupe d’age appartenez-vous? (LIRE SI NECESSAIRE) |  | `ponder3` (à réviser, non appliquée) | Non documenté |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.

#### `gender` : Genre

Genre de la personne, tel que demandé ou, dans certains sondages
téléphoniques, tel que noté par l’intervieweur. La plupart des études
n’offraient que homme et femme (une question sur le sexe, dans
certaines) ; qes2022 offrait aussi non binaire et un autre genre.

Famille `sex_gender` · type Catégorielle · moment Invariant · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Niveaux**

| Code | Nom         | Étiquette      |
|------|-------------|----------------|
| 1    | `man`       | Homme          |
| 2    | `woman`     | Femme          |
| 3    | `nonbinary` | Non binaire    |
| 4    | `other`     | Un autre genre |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_genderid` (cps) | `comparable` | Question sur l’identité de genre sur le Web qui ajoute non binaire et un autre genre aux deux options de l’ancrage ; ne devrait guère changer les parts d’hommes et de femmes. | gender_4 | man, woman, nonbinary, other | Êtes-vous… |  | `cps_weight_general` | Non offert |
| qes2018 | `qsexe` (post) | `comparable` | Mêmes deux options sur le Web et mêmes énoncés que l’ancrage (genre en anglais, sexe en français) ; le questionnaire français avec valeurs programmées ajoute la note de Statistique Canada invitant les personnes transgenres, transsexuelles et intersexuées à choisir le sexe auquel elles s’identifient le plus ; le fichier n’a pas d’étiquettes de valeur. | gender_2 | man, woman; non offerts : nonbinary, other | Quel est votre sexe? |  | `pond` | Non offert |
| qes2018_panel | `sexfix` (pre) | `comparable` | Mêmes deux options, sur le Web (850) et par téléphone (400) ; le livre de codes ne donne que l’étiquette Sexe, de sorte qu’on ignore si elle a été demandée ou notée. | gender_2 | man, woman; non offerts : nonbinary, other | Sexe: |  | `weight` | Non documenté |
| qes2014 | `QSEXE` (post) | `comparable` | Mêmes deux options sur le Web et mêmes énoncés que l’ancrage en anglais (What is your gender?) et en français (Quel est votre sexe?), plus une option de non-réponse absente du fichier ; gardée au niveau comparable, celui qu’elle avait avant la révision, parce qu’un niveau identique pour une ligne qui réunit des langues de passation demande un second réviseur (humain). | gender_2 | man, woman; non offerts : nonbinary, other | Quel est votre sexe? |  | `POND` | Non offert |
| qes2012 | `sexe` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | gender_2 | man, woman; non offerts : nonbinary, other | Quel est votre sexe? |  | `pond` | Non offert |
| qes2012_panel | `sexe` (pre) | `comparable` | Mêmes deux options, par téléphone ; le livre de codes ne donne que le nom de la variable, de sorte qu’on ignore si elle a été demandée ou notée. | sex_recorded | man, woman; non offerts : nonbinary, other | document 654292, sexe |  | `pondam1` (à réviser, non appliquée) | Non documenté |
| qes_crop_2007_2010 | `SEXE` (chaque sondage) | `comparable` | Mêmes deux options, noté par l’intervieweur (non demandé), par téléphone. | sex_recorded | man, woman; non offerts : nonbinary, other | INSCRIRE LE SEXE DU REPONDANT |  | `XPOND` (à réviser, non appliquée) | Non offert |
| qes2008 | `q76` (post) | `comparable` | Demandé (Un homme / Une femme) avec les mêmes deux options ; le mode de l’étude reste à confirmer (téléphone selon les métadonnées du dépôt), l’ancrage est Web. | gender_2 | man, woman; non offerts : nonbinary, other | Êtes-vous… |  |  | Non offert |
| qes2007 | `q76` (post) | `comparable` | Noté par l’intervieweur au téléphone sans le demander (NE PAS LIRE), et sur le Web pour les personnes jointes en ligne (seul le questionnaire téléphonique est déposé) ; mêmes deux options. | sex_recorded | man, woman; non offerts : nonbinary, other | (NE PAS LIRE) Indiquez le sexe du répondant: |  | `pond` | Non offert |
| qes2007_panel | `sexe` (toute vague) | `comparable` | Mêmes deux options, noté par l’intervieweur (non demandé), par téléphone. | sex_recorded | man, woman; non offerts : nonbinary, other | INSCRIRE LE SEXE DU RÉPONDANT |  |  | Non offert |
| qes1998 | `sexe_post` (pre) | `comparable` | Mêmes deux options, par téléphone ; les livres de codes ne donnent que l’étiquette SEXE (Sexe du répondant dans celui de CROP), de sorte qu’on ignore si elle a été demandée ou notée par l’intervieweur. | sex_recorded | man, woman; non offerts : nonbinary, other | SEXE |  | `ponder3` (à réviser, non appliquée) | Non offert |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `education4` : Scolarité (4 groupes)

Plus haut niveau de scolarité de la personne, en quatre groupes :
primaire ou moins, secondaire, collégial (cégep ou technique),
universitaire (terminé ou non). À partir du plus haut niveau atteint là
où l’étude le demande, du nombre d’années d’études sinon (niveau
approximatif). Un diplôme professionnel ou de métier (le DEP) n’a pas de
groupe propre et n’est pas classé de la même façon dans toutes les
études : secondaire là où la question présente le DEP comme un diplôme
du secondaire (qes2018), collégial là où il s’agit d’un certificat
d’école de métiers ou d’un cours technique (qes2018_panel et,
probablement, les études sans option DEP) ; voir les justifications des
niveaux de comparabilité.

Famille `education` · type Ordinale · moment Invariant · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Niveaux**

| Code | Nom          | Étiquette                    |
|------|--------------|------------------------------|
| 1    | `primary`    | Primaire ou moins            |
| 2    | `secondary`  | Secondaire                   |
| 3    | `college`    | Collégial (cégep, technique) |
| 4    | `university` | Universitaire                |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_edu` (cps) | `comparable` | Plus haut niveau atteint sur le Web, avec des options qui se regroupent exactement dans les quatre groupes (des études universitaires non terminées comptent comme universitaires, comme dans l’ancrage) ; pas d’option « ne sait pas ». | edu_levels | primary, secondary, college, university | Quel est votre plus haut niveau de scolarité complété? |  | `cps_weight_general` | Non offert |
| qes2018 | `qscol` (post) | `comparable` | Même énoncé que l’ancrage sur le Web, avec des options plus fines (chaque année du secondaire, le DEP, les programmes du cégep) qui se regroupent exactement dans les quatre groupes ; le fichier n’a pas d’étiquettes de valeur. Le diplôme d’études professionnelles (DEP, code 9) est secondaire ici, alors que le certificat d’école de métiers de qes2018_panel (d3 code 4) est collégial, et les questionnaires sans option DEP (l’ancrage qes2014, qes2012, qes2007, qes2008) classent probablement les titulaires d’un DEP en cours technique (collégial) : un diplôme professionnel ou de métier ne tombe pas dans le même groupe dans toutes les études. | edu_levels | primary, secondary, college, university | À quel niveau se situe la dernière année de scolarité que vous avez complétée? |  | `pond` | Offert explicitement |
| qes2018_panel | `d3` (pre) | `approximate` | Plus haut niveau atteint selon les catégories de Statistique Canada, par téléphone et sur le Web : un certificat d’école de métiers ou un apprentissage enregistré est une option, comptée comme collégiale (technique), et les certificats universitaires inférieurs au baccalauréat comme universitaires ; les étiquettes sont coupées à 60 caractères dans le fichier. Au Québec, cette option regroupe surtout le diplôme d’études professionnelles (DEP), que qes2018 (qscol code 9) compte comme secondaire : un diplôme professionnel ou de métier ne tombe pas dans le même groupe dans toutes les études. | edu_statcan | primary, secondary, college, university | Quel est le plus haut niveau de scolarité que vous avez atteint ? |  | `weight` | Non documenté |
| qes2014 | `QSCOL` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | edu_levels | primary, secondary, college, university | À quel niveau se situe la dernière année de scolarité que vous avez complétée? |  | `POND` | Non offert |
| qes2012 | `scol` (post) | `comparable` | Même question française que l’ancrage, sur le Web ; les options anglaises suivent un schéma britannique (further ou higher education), et un code du fichier ne figure dans aucun des deux questionnaires. | edu_levels | primary, secondary, college, university | À quel niveau se situe la dernière année de scolarité que vous avez complétée? |  | `pond` | Offert explicitement |
| qes_crop_2007_2010 | `scol` (chaque sondage) | `approximate` | Années d’études en quatre intervalles nommés d’après les niveaux (7 ou moins, primaire ; 8 à 12, secondaire ; 13 à 15, cégep ; 16 ou plus, université), par téléphone, et non le plus haut niveau atteint. | edu_years | primary, secondary, college, university | Combien d’années d’études avez-vous complétées? |  | `XPOND` (à réviser, non appliquée) | Non documenté |
| qes2008 | `q77` (post) | `comparable` | Niveau de scolarité avec et sans diplôme, options qui se regroupent exactement dans les quatre groupes ; le mode reste à confirmer, l’ancrage est Web. | edu_levels | primary, secondary, college, university | Quel est votre niveau d’éducation ? |  |  | Offert explicitement |
| qes2007 | `q77` (post) | `comparable` | Niveau de scolarité avec et sans diplôme, options qui se regroupent exactement dans les quatre groupes ; entrevues téléphoniques et Web, l’ancrage est Web. | edu_levels | primary, secondary, college, university | Quel est votre niveau d’éducation ? |  | `pond` | Spontané seulement |
| qes2007_panel | `scol` (toute vague) | `approximate` | Années d’études en quatre intervalles nommés d’après les niveaux (7 ou moins, primaire ; 8 à 12, secondaire ; 13 à 15, cégep ; 16 ou plus, université), par téléphone, et non le plus haut niveau atteint. | edu_years | primary, secondary, college, university | Combien d’années d’études avez-vous complétées? |  |  | Spontané seulement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 2.0.0 (2026-09-27) : Corrections de révision. MAJEURE (un filtre
  corrigé, des empreintes de colonnes modifiées) : les lignes religion
  de qes2012 (q103) et de qes2014 (Q63) sont filtrées sur leur question
  filtre (q102, Q62) : les personnes sans religion sont inapplicable
  (842 et 845) et celles qui ont préféré ne pas répondre à la question
  filtre refused (32 et 44), là où elles étaient sysmis ; valeurs
  inchangées. Un filtre s’applique maintenant aux règles weight, date et
  string comme aux règles map et numeric, un filtre sur une autre règle
  est une erreur V-S1, et gates.csv contient les cellules des lignes
  string filtrées pour V-D7. La participation déclarée de qes2007_panel
  lit la question de participation postélectorale voteoui (oui le jour
  du scrutin, oui par anticipation, non) au lieu de la recodification
  avote du producteur, avec les mêmes valeurs et la même empreinte de
  colonne ; niveau comparable (la même question oui/non précisée que
  qes2012_panel), et non approximate. Texte seulement : les
  justifications des lignes education4 de qes2018 et de qes2018_panel et
  la description de la cible disent où se place le diplôme d’études
  professionnelles (DEP : secondaire dans qes2018, collégial dans
  qes2018_panel et probablement là où aucune option DEP n’est offerte) ;
  la note de la colonne language de qes1998 dans legacy.csv dit que la
  constante n’est une langue maternelle que pour les lignes CREATEC (la
  langue parlée à la maison pour les 426 lignes CROP ; la définition
  regroupée n’est pas encore confirmée) ; la description de religion et
  la note héritée nomment le filtre ; les notes de preuve de qes2022 ne
  donnent que des codes et des pages du livre de codes (sa licence, CC
  BY-NC, tient son libellé et ses étiquettes hors du package).
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `lang_mother` : Langue maternelle

Première langue que la personne a apprise à la maison dans son enfance
et qu’elle comprend toujours : français, anglais ou autre langue. Une
personne qui déclare deux premières langues est une valeur manquante
(motif not_mappable), jamais attribuée à l’une d’elles.

Famille `language` · type Catégorielle · moment Invariant · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Niveaux**

| Code | Nom       | Étiquette |
|------|-----------|-----------|
| 1    | `french`  | Français  |
| 2    | `english` | Anglais   |
| 3    | `other`   | Autre     |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `qlangue` (post) | `comparable` | Même énoncé sur le Web, qui demande la langue principale apprise en premier ; le fichier n’a pas d’étiquettes de valeur. | lang_first | french, english, other | Quelle est la langue principale que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `pond` | Offert explicitement |
| qes2018_panel | `s1` (pre) | `comparable` | Énoncé plus court (première langue apprise et encore comprise, sans « à la maison dans votre enfance »), par téléphone et sur le Web ; une seule langue. | lang_first | french, english, other | Quelle est la première langue que vous avez apprise et que vous comprenez toujours? |  | `weight` | Non documenté |
| qes2014 | `QLANG` (post) | `comparable` | Même énoncé sur le Web, avec trois options pour deux premières langues, qui sont ici des valeurs manquantes (not_mappable). | lang_first_multi | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `POND` | Offert explicitement |
| qes2012 | `langu` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | lang_first | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `pond` | Offert explicitement |
| qes2012_panel | `lmat` (pre) | `comparable` | Langue maternelle définie comme la première langue apprise et encore comprise, par téléphone ; une seule langue. | lang_first | french, english, other | Quelle est votre langue maternelle, c’est-à-dire celle que vous avez appris à parler en premier et que vous comprenez toujours? |  | `pondam1` (à réviser, non appliquée) | Non documenté |
| qes_crop_2007_2010 | `lmat` (chaque sondage) | `comparable` | Langue maternelle demandée sans définition, par téléphone ; une seule langue. | lang_mother | french, english, other | Quelle est votre langue maternelle? |  | `XPOND` (à réviser, non appliquée) | Non documenté |
| qes2008 | `langu` (post) | `comparable` | Même énoncé et mêmes options ; le mode reste à confirmer, l’ancrage est Web. | lang_first | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours ? |  |  | Offert explicitement |
| qes2007 | `langu` (post) | `comparable` | Même énoncé, avec des options pour deux premières langues, qui sont ici des valeurs manquantes (not_mappable) ; entrevues téléphoniques et Web, l’ancrage est Web. | lang_first_multi | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours ? |  | `pond` | Spontané seulement |
| qes2007_panel | `lmat` (toute vague) | `comparable` | Langue maternelle définie comme la première langue apprise et encore parlée, par téléphone ; une seule langue. | lang_first | french, english, other | Quelle est votre langue maternelle, c’est-à-dire la première langue que vous avez apprise et que vous pouvez encore parler? |  |  | Spontané seulement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 2.0.0 (2026-09-27) : Corrections de révision. MAJEURE (un filtre
  corrigé, des empreintes de colonnes modifiées) : les lignes religion
  de qes2012 (q103) et de qes2014 (Q63) sont filtrées sur leur question
  filtre (q102, Q62) : les personnes sans religion sont inapplicable
  (842 et 845) et celles qui ont préféré ne pas répondre à la question
  filtre refused (32 et 44), là où elles étaient sysmis ; valeurs
  inchangées. Un filtre s’applique maintenant aux règles weight, date et
  string comme aux règles map et numeric, un filtre sur une autre règle
  est une erreur V-S1, et gates.csv contient les cellules des lignes
  string filtrées pour V-D7. La participation déclarée de qes2007_panel
  lit la question de participation postélectorale voteoui (oui le jour
  du scrutin, oui par anticipation, non) au lieu de la recodification
  avote du producteur, avec les mêmes valeurs et la même empreinte de
  colonne ; niveau comparable (la même question oui/non précisée que
  qes2012_panel), et non approximate. Texte seulement : les
  justifications des lignes education4 de qes2018 et de qes2018_panel et
  la description de la cible disent où se place le diplôme d’études
  professionnelles (DEP : secondaire dans qes2018, collégial dans
  qes2018_panel et probablement là où aucune option DEP n’est offerte) ;
  la note de la colonne language de qes1998 dans legacy.csv dit que la
  constante n’est une langue maternelle que pour les lignes CREATEC (la
  langue parlée à la maison pour les 426 lignes CROP ; la définition
  regroupée n’est pas encore confirmée) ; la description de religion et
  la note héritée nomment le filtre ; les notes de preuve de qes2022 ne
  donnent que des codes et des pages du livre de codes (sa licence, CC
  BY-NC, tient son libellé et ses étiquettes hors du package).
- 2.0.1 (2026-09-27) : Texte seulement : les lignes de
  sovereignty_support et de sovereignty dans legacy.csv donnent à
  qes2007_panel le motif de qes2007, qes2008 et qes1998 (il a posé la
  question de 1995, voir sov_partnership_1995) et aux sondages CROP le
  leur (la première question référendaire, intvoterefa, n’a qu’une
  étiquette tronquée et aucun questionnaire déposé : son libellé est
  inconnu et elle n’est pas appariée) ; la colonne cause de legacy.csv
  (attr(, “legacy_na_columns”)\$cause de get_qes_master() et de
  get_decon()) a des valeurs descriptives (reported_vote_only,
  independence_question_only, no_valid_source, not_harmonized_yet) au
  lieu d’identifiants internes de décision, et le texte de legacy.csv,
  crosswalk.csv, valuemaps.csv et de ce journal dit chaque décision en
  mots. Aucune ligne, aucun code, niveau, marge ni empreinte n’a changé.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.

#### `born_canada` : Né(e) au Canada

Si la personne est née au Canada. À partir d’une question sur le lieu de
naissance (Québec, ailleurs au Canada, hors du Canada) là où l’étude
pose celle-là, regroupée exactement.

Famille `birthplace` · type Catégorielle · moment Invariant · statut
Expérimental · ajoutée dans la spécification 1.0.0

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_borncda` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | born_canada | yes, no | Êtes-vous né(e) au Canada? |  | `cps_weight_general` | Non offert |
| qes2018 | `q69` (post) | `comparable` | Question sur le lieu de naissance (au Québec, ailleurs au Canada, hors du Canada), regroupée exactement en né ou non au Canada, sur le Web ; l’ancrage le demande directement. Le fichier n’a pas d’étiquettes de valeur. | birthplace3 | yes, no | Où êtes-vous né(e)? |  | `pond` | Offert explicitement |
| qes2014 | `Q65` (post) | `comparable` | Question sur le lieu de naissance (au Québec, ailleurs au Canada, hors du Canada), regroupée exactement en né ou non au Canada, sur le Web ; l’ancrage le demande directement. | birthplace3 | yes, no | Où êtes-vous né(e)? |  | `POND` | Offert explicitement |
| qes2012 | `q105` (post) | `comparable` | Question sur le lieu de naissance (au Québec, ailleurs au Canada, hors du Canada), regroupée exactement en né ou non au Canada, sur le Web ; l’ancrage le demande directement. | birthplace3 | yes, no | Où êtes-vous né(e)? |  | `pond` | Offert explicitement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `income_native` : Revenu du ménage (tranches propres à chaque étude)

Revenu du ménage avant impôts de la personne, tel que chaque étude l’a
enregistré : le texte de la tranche propre à l’étude (ou le montant, là
où l’étude le demandait). Les tranches diffèrent d’une étude à l’autre,
de sorte que les valeurs ne sont pas comparables entre études ; « ne
sait pas » et les refus sont des valeurs manquantes.

Famille `income` · type Texte · moment Invariant · statut Expérimental ·
ajoutée dans la spécification 1.0.0

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_income` (cps) | `approximate` | Le montant en dollars, inscrit sur le Web, et non une tranche. | income_amount |  | Quel est le revenu total de votre ménage avant impôts en 2021? Cela doit inclure toutes les sources de revenus au millier de dollars près. |  | `cps_weight_general` | Non offert |
| qes2018_panel | `d5` (pre) | `approximate` | Sept tranches de moins de 20 000 \$ à 150 000 \$ et plus, par téléphone et sur le Web, et non les neuf de l’ancrage ; le libellé déposé de la question est coupé, de sorte qu’on ignore s’il s’agit du revenu avant impôts et pour quelle année. | income_7brackets |  | Laquelle des catégories suivantes décrit le mieux le revenu total de votre foyer, c’est-à-dire le total des revenus |  | `weight` | Non documenté |
| qes2014 | `Q57` (post) | `comparable` | Les mêmes neuf tranches que l’ancrage, pour l’année précédente, sur le Web ; pas d’option « ne sait pas ». | income_9brackets |  | Parmi les catégories suivantes, laquelle reflète le mieux le revenu total avant impôt de tous les membres de votre foyer pour l’année 2013? 3. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Était-ce: |  | `POND` | Non offert |
| qes2012 | `reven` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | income_9brackets |  | Et maintenant le revenu total de votre ménage avant impôts pour l’année 2011. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Était-ce: |  | `pond` | Offert explicitement |
| qes_crop_2007_2010 | `revenu` (chaque sondage) | `approximate` | Cinq tranches de 20 000 \$ jusqu’à 80 000 \$ et plus, par téléphone, et non les neuf de l’ancrage. | income_5brackets |  | Dans laquelle des catégories suivantes se situe le revenu |  | `XPOND` (à réviser, non appliquée) | Non documenté |
| qes2008 | `q78` (post) | `approximate` | Dix tranches, de moins de 20 000 \$ puis par pas de 10 000 \$ jusqu’à plus de 100 000 \$, et non les neuf de l’ancrage ; pour l’année précédant l’élection (2007). | income_10brackets |  | Quel était le revenu total de votre ménage avant impôts en 2007. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Est-ce…? |  |  | Offert explicitement |
| qes2007 | `q78` (post) | `approximate` | Dix tranches de 10 000 \$ jusqu’à plus de 100 000 \$, et non les neuf de l’ancrage ; pour l’année précédant l’élection. | income_10brackets |  | Et maintenant le revenu total de votre ménage avant impôts en 2006. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Est-ce…? |  | `pond` | Offert explicitement |
| qes2007_panel | `revenu` (toute vague) | `approximate` | Cinq tranches de 20 000 \$ jusqu’à 80 000 \$ et plus, par téléphone, et non les neuf de l’ancrage. | income_5brackets |  | Dans laquelle des catégories suivantes se situe le revenu annuel total, avant impôts et déductions, de tous les membres de votre foyer, en vous incluant? Est-ce… |  |  | Spontané seulement |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.1.0 (2026-09-28) : Approbation du contenu indépendante des
  pondérations. Les 38 lignes de correspondance de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010 que la double
  révision automatisée de la spécification 4.0.0 avait approuvées sur
  leur contenu sont stables, et appliquées par défaut par
  qes_harmonize(), get_qes_master() et get_decon() ; elles n’étaient
  retenues en révision que parce que les pondérations recommandées de
  leurs vagues sont à réviser. Leur review_note dit toujours que la
  révision était automatisée et non humaine, et que la pondération est
  suivie à part (dev/open-questions.md Q1). Le contrôle de publication
  V-S13 n’échoue plus pour une ligne stable d’une vague dont la
  pondération recommandée est à réviser ; il exige toujours qu’une
  pondération recommandée ne soit jamais calée sur le vote ou la
  participation, et une pondération recommandée par vague (aucune là où
  toutes sont calées). Les pondérations à réviser restent non appliquées
  : weight_pre et weight_post y valent NA, avec le message
  qesR_message_weight_review, et dans get_qes_master() le motif
  not_reviewed avec la cause weight_needs_review. QSEXE de qes2014
  (gender) est stable au niveau comparable, celui qu’elle avait avant la
  révision (l’énoncé anglais ajouté par la révision est conservé) ;
  identical attend un second réviseur humain. Texte seulement : les deux
  lignes de documentation (règle none), interetrec de qes2012_panel et
  intvote2 de qes1998, ont un wording_ref vers leur entrée du livre de
  codes, que V-S11 exige d’une ligne stable ; dans legacy.csv, la note
  de la ligne survey_weight de chaque étude nomme la pondération et son
  statut au registre (XPOND de CROP et pond de qes2012_panel sont à
  réviser, pond de qes2008 est calée sur le vote, pond de qes2007_panel
  n’est pas enregistrée), les définitions de weight_pre et de
  weight_post disent NA là où la pondération est à réviser, et les
  valeurs manquantes voulues de get_qes_master() ont une ligne propre
  avec une cause et une note (education de qes1998, income et religion
  de qes2018 : invalid_044_source ; political_interest de qes2012_panel
  : not_comparable_source ; language de qes2022 : not_harmonized_yet).
  Aucune correspondance de valeurs, aucun filtre, ensemble de niveaux,
  marge attendue ni empreinte de colonne n’a changé : MINEURE, des
  lignes sont seulement ajoutées au résultat par défaut.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.

#### `religion` : Religion (catégories propres à chaque étude)

Religion de la personne, telle que chaque étude l’a enregistrée : le
texte de la catégorie propre à l’étude. Les catégories diffèrent d’une
étude à l’autre. Là où la question n’était posée qu’aux personnes qui
appartiennent à une religion, les autres sont des valeurs manquantes :
motif inapplicable pour celles qui ont dit n’appartenir à aucune,
refused pour celles qui n’ont pas voulu répondre à la question filtre,
qui sert de filtre à la ligne de correspondance.

Famille `faith` · type Texte · moment Invariant · statut Expérimental ·
ajoutée dans la spécification 1.0.0

**Couverture**

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_religion` (cps) | `approximate` | Une seule question avec une longue liste de confessions et « aucune » (sans question filtre), sur le Web ; et non les six catégories de l’ancrage. | religion_list |  | Quelle est votre religion, si vous en avez une? |  | `cps_weight_general` | Non offert |
| qes2014 | `Q63` (post) | `comparable` | Même question filtre et mêmes catégories que l’ancrage, avec un énoncé légèrement plus long, sur le Web. | religion_list |  | À quelle religion appartenez-vous? | Q62: 2 = inapplicable, 9 = refused | `POND` | Non offert |
| qes2012 | `q103` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | religion_list |  | Quelle religion? | q102: 2 = inapplicable, 3 = refused | `pond` | Non offert |

**Historique**

- 0.1.0 (2026-09-27) : Première spécification : 11 cibles de base avec
  des lignes vérifiées sur les fichiers originaux, en révision, pour
  qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel et
  qes2018_panel, leurs ensembles de niveaux, vagues et pondérations.
- 0.1.1 (2026-09-27) : Contrôles hors ligne : gates.csv, les effectifs
  croisés du code de filtre et du code source parmi les membres de la
  vague pour les 28 lignes projetables des études dont les métadonnées
  sont fournies, et expected/marginals.csv, les marges non pondérées
  projetées des 28 lignes projetables des études dont les métadonnées
  sont fournies. Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 0.1.2 (2026-09-27) : Empreintes des colonnes du moteur :
  expected/hashes.csv, la somme md5 de chaque colonne harmonisée (étude,
  cible) que qes_harmonize() produit sur les fichiers retenus, pour les
  35 lignes appariées, vérifiée sur les fichiers originaux par les tests
  en direct (V-L1). Aucune ligne, aucun code ni aucun niveau de
  comparabilité n’a changé.
- 1.0.0 (2026-09-27) : Le passage des fonctions héritées au moteur (qesR
  0.7.0) : get_qes_master() et get_decon() sont produits par le moteur,
  selon la nouvelle table legacy.csv (le rendu de chaque colonne héritée
  à partir des cibles, vérifié par la nouvelle règle V-S18). 13
  nouvelles cibles avec 58 lignes de correspondance pour les 11 études :
  gender, education4 (quatre groupes), lang_mother (deux premières
  langues sont not_mappable : attribuées à aucune), born_canada,
  income_native et religion (les catégories propres à chaque étude, en
  texte : règle string avec le nouvel argument from_label = TRUE,
  l’étiquette de valeur du code), pid_fed, interest_0_10,
  interest_election_0_10 et interest_campaign_4pt (jamais regroupées
  avec interest_4pt), age_group6, turnout_prov_likely et
  vote_prov_intent_other (qes2022), et la participation déclarée du
  panel de 2007 (avote). Une ligne de correspondance peut nommer la
  vague \* pour une cible de temporalité static ou any : elle s’applique
  aux membres de toute vague de l’étude (les questions invariantes du
  panel de 2007, posées dans la vague à laquelle la personne a
  participé). MAJEURE : la ligne age_group3 du panel de 2007 passe de sa
  vague préélectorale à la vague \*, de sorte que ses 391 personnes
  jointes seulement après l’élection ont maintenant un groupe d’âge
  (l’empreinte de leur colonne change) ; aucune autre ligne, aucun code,
  niveau, marge ni empreinte existants ne changent. Toutes les nouvelles
  lignes sont en révision, vérifiées sur les fichiers originaux et les
  documents.
- 2.0.0 (2026-09-27) : Corrections de révision. MAJEURE (un filtre
  corrigé, des empreintes de colonnes modifiées) : les lignes religion
  de qes2012 (q103) et de qes2014 (Q63) sont filtrées sur leur question
  filtre (q102, Q62) : les personnes sans religion sont inapplicable
  (842 et 845) et celles qui ont préféré ne pas répondre à la question
  filtre refused (32 et 44), là où elles étaient sysmis ; valeurs
  inchangées. Un filtre s’applique maintenant aux règles weight, date et
  string comme aux règles map et numeric, un filtre sur une autre règle
  est une erreur V-S1, et gates.csv contient les cellules des lignes
  string filtrées pour V-D7. La participation déclarée de qes2007_panel
  lit la question de participation postélectorale voteoui (oui le jour
  du scrutin, oui par anticipation, non) au lieu de la recodification
  avote du producteur, avec les mêmes valeurs et la même empreinte de
  colonne ; niveau comparable (la même question oui/non précisée que
  qes2012_panel), et non approximate. Texte seulement : les
  justifications des lignes education4 de qes2018 et de qes2018_panel et
  la description de la cible disent où se place le diplôme d’études
  professionnelles (DEP : secondaire dans qes2018, collégial dans
  qes2018_panel et probablement là où aucune option DEP n’est offerte) ;
  la note de la colonne language de qes1998 dans legacy.csv dit que la
  constante n’est une langue maternelle que pour les lignes CREATEC (la
  langue parlée à la maison pour les 426 lignes CROP ; la définition
  regroupée n’est pas encore confirmée) ; la description de religion et
  la note héritée nomment le filtre ; les notes de preuve de qes2022 ne
  donnent que des codes et des pages du livre de codes (sa licence, CC
  BY-NC, tient son libellé et ses étiquettes hors du package).
- 4.0.0 (2026-09-28) : Approbation des lignes révisées. Une double
  révision automatisée a vérifié les 132 lignes de correspondance sur
  les fichiers et documents originaux (une passe sur les codes et les
  données, une sur le libellé et la comparabilité, avec arbitrage en cas
  de désaccord) ; ce n’est pas une révision humaine, et reviewed_by le
  dit. reviewed_on est le 2026-09-27, et la nouvelle colonne review_note
  dit ce que la révision a corrigé et pourquoi une ligne reste en
  révision (V-S11 l’exige pour une ligne révisée laissée en révision).
  93 lignes sont approuvées (statut stable) et appliquées par défaut par
  qes_harmonize(). 39 restent en révision : les 38 lignes de qes1998,
  qes2007_panel, qes2012_panel et qes_crop_2007_2010, dont les
  pondérations recommandées sont à réviser (une ligne stable y échoue au
  contrôle de publication V-S13), et la ligne gender de qes2014, dont le
  niveau est passé à identical et qui demande un second réviseur.
  get_qes_master() et get_decon() n’appliquent plus que les lignes
  approuvées (include_draft = FALSE) : une colonne dont la question est
  dans une ligne encore en révision est NA, motif not_reviewed dans
  attr(, “legacy_na_columns”), et attr(, “source_map”) reçoit le statut
  de chaque ligne. MAJEURE (des empreintes de colonnes modifiées) : le
  montant de revenu 0 de qes2022, un champ laissé vide que le
  questionnaire renvoyait à la question de relance par tranches
  cps_income2, est une valeur manquante (no_answer) ; le texte saisi
  d’un autre parti de qes2022 est filtré sur cps_turnout comme sa ligne
  mère (3 à 5 inapplicable, 6 ineligible). Niveaux et métadonnées : rv1a
  et rv1ab de qes2018_panel passent de comparable à approximate
  (l’énoncé demande aussi leur vote à celles et ceux qui ont voté par
  anticipation ; un filtre de relance plus étroit que celui de
  l’ancrage) ; QSEXE de qes2014 passe de comparable à identical (les
  énoncés de l’ancrage dans les deux langues) ; QSCOL de qes2014 a
  dk_offered none ; cps_ideoself_1 de qes2022 a l’instrument lr_0_10
  (aucun curseur n’est documenté) ; les intentions de vote de CROP ont
  dk_offered volunteered et leur libellé français tiré des rapports de
  CROP. Texte seulement : le libellé, les justifications de niveau, les
  preuves et les notes de 36 lignes sont corrigés (chacune le dit dans
  sa review_note), dont les effectifs des membres des vagues de
  qes2007_panel et l’endroit où ses questions invariantes ont été
  posées. Dans gates.csv, un texte saisi compte comme un seul jeton et
  un texte vide comme valeur manquante système.
- 4.2.0 (2026-09-28) : Les métadonnées de qes2022 sont livrées (décision
  OD3 levée par le propriétaire le 2026-09-28 ; elles restent sous la
  licence de l’étude, CC BY-NC 4.0, inst/COPYRIGHTS section 2). Les 18
  lignes de correspondance de qes2022 reçoivent leur wording_en et leur
  wording_fr, cités du livre de codes bilingue de l’étude (fichier
  7449514 ; pour le texte saisi d’un autre parti, l’énoncé de
  cps_votechoice1 avec son option), et ses 65 lignes de tables de
  valeurs l’étiquette de valeur du fichier retenu (source_label) au lieu
  du md5 de cette étiquette (source_label_hash). gates.csv reçoit les
  252 cellules des 16 lignes projetables ou filtrées de qes2022 et
  expected/marginals.csv les 225 cellules de marge de ses 15 lignes
  projetables, les effectifs que le dossier data-raw/nc/, exclu du
  build, gardait jusqu’ici pour l’intégration continue (identiques,
  recalculés sur le fichier retenu par data-raw/build_sources.R et
  data-raw/project_marginals.R). Le contrôle V-S11 n’interdit plus le
  libellé et les étiquettes d’une étude dont les métadonnées ne sont pas
  livrées (celles de toutes les études le sont). Aucune ligne, table de
  valeurs, aucun filtre, niveau, ensemble de niveaux, aucune marge
  enregistrée ni empreinte de colonne n’a changé : MINEURE, des clés
  sont seulement ajoutées à expected/.
