# Référence des variables

*[English
version](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md)*

Les variables harmonisées (« cibles ») de
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
étude par étude. La page est générée à partir des règles livrées avec
qesR : elle décrit donc toujours ce que la version installée applique.

## Aller à une variable

[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
renvoie les mêmes informations sous forme de tableaux, et
`qes_provenance(x, level = "spec")` la version des règles qui a produit
des données harmonisées.

## Comment lire cette référence

Chaque cible correspond à un seul stimulus de question : une
formulation, une échelle, un moment ou un format différent donne une
autre cible ; une variable regroupée (dernier chapitre) réunit des
cibles en une seule colonne et indique de laquelle vient chaque valeur.
Pour chaque étude, le tableau de couverture d’une cible donne :

- **Source** : la variable et la vague qui l’a posée ;
- **Niveau** et **Raison** : à quel point la question de l’étude est
  comparable à la question d’ancrage de la cible, et pourquoi ;
- **Instrument** et **Niveaux offerts** : le format de la question et
  les réponses offertes (les autres sont des zéros structurels) ;
- **Libellé** : le texte de la question, ou le document et la page qui
  le donnent ;
- **Filtre** : la question filtre, et le sens de chacun de ses codes ;
- **Pondération** : la pondération recommandée de l’étude pour cette
  vague (signalée quand elle n’est pas encore utilisable : ses colonnes
  de pondération valent alors `NA`) ;
- **Ne sait pas** : si « je ne sais pas » était offert.

Une ligne marquée *en attente d’approbation* est vérifiée mais pas
encore approuvée, et n’est pas utilisée par défaut : ses valeurs sont
manquantes. Pour l’utiliser quand même, passez `include_draft = TRUE` à
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md).

Les libellés et les étiquettes de `qes2022` cités sur cette page
viennent de *2022 Quebec Election Study* (Mahéo, Bélanger, Stephenson et
Harell, 2023, <https://doi.org/10.7910/DVN/PAQBDR>) et gardent sa
licence, [CC BY-NC
4.0](https://creativecommons.org/licenses/by-nc/4.0/). Voir [Citer qesR
et les
études](https://thomasgareau.github.io/qesR/articles/fr-citations.html#licences-et-attribution)
pour l’attribution.

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
remplie à partir du mode de chaque vague ; il n’y a de lignes de
correspondance que pour les vagues dont le mode varie selon la personne.

Famille `interview_mode` · type Catégorielle · moment Tout moment

**Niveaux**

| Code | Nom     | Étiquette |
|------|---------|-----------|
| 1    | `web`   | Web       |
| 2    | `phone` | Téléphone |
| 3    | `mixed` | Mixte     |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `method` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interview_mode | web, phone; non offerts : mixed | document 341538, method |  | `weight` | Non offert |
| qes2007 | `type` (post) | `identical` | Le mode d’entrevue tel que le fichier l’enregistre. | interview_mode | web, phone; non offerts : mixed | document 425921, type |  | `pond` | Non offert |

### Vote et participation

#### `vote_prov_recall` : Vote provincial (rappel)

Parti pour lequel la personne dit avoir voté à l’élection générale
québécoise de l’étude, question posée après cette élection. Les
abstentionnistes, les bulletins annulés et les personnes non admissibles
ou non inscrites sont des valeurs manquantes avec un motif, jamais un
parti.

Famille `vote_prov` · type Catégorielle · moment Postélectoral

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

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_votechoice` (pes) | `comparable` | Propose les quatre principaux partis et les conservateurs, mais ni le Parti vert ni Option nationale, sans « Je ne sais pas » ; l’annulation est une option et la question suit une question de participation qui ménage la face. | vote_recall_list | PLQ, PQ, CAQ, QS, PCQ, other; non offerts : PVQ, ON, ADQ | Pour quel parti avez-vous voté? | pes_turnout: 2 = not_voted, 3 = not_voted, 4 = not_voted, 5 = not_registered, 6 = dk | `pes_weight_general` | Non offert |
| qes2018 | `q6` (post) | `comparable` | Seuls les quatre principaux partis sont proposés (ni le Parti vert ni Option nationale), sans option « Je ne sais pas », l’annulation est une option et la question suit une question de participation qui ménage la face. | vote_recall_list | PLQ, PQ, CAQ, QS, other; non offerts : PVQ, PCQ, ON, ADQ | Pour quel parti avez-vous voté? | q5: 1 = not_voted, 2 = not_voted, 3 = not_voted, 5 = ineligible, 99 = refused, NA = inapplicable | `pond` | Non offert |
| qes2018_panel | `rts_q2` (post) | `comparable` | Liste des partis avec le nom des chefs, en mode mixte téléphone et Web ; l’ancrage est une liste Web sans les chefs. | vote_recall_list_leaders | PLQ, PQ, CAQ, QS, other; non offerts : PVQ, PCQ, ON, ADQ | Et pour qui avez-vous voté? | rts_q1: 1 = not_voted, 2 = not_voted, 4 = dk_refused | `weight_rts` | Non documenté |
| qes2014 | `Q3` (post) | `comparable` | Même liste de partis que l’ancrage, mais l’énoncé ne nomme pas la date de l’élection et aucune option « Je ne sais pas » n’est offerte. | vote_recall_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; non offerts : PCQ, ADQ | Pour quel parti avez-vous voté? | Q2: 2 = not_voted, 9 = refused | `POND` | Non offert |
| qes2012 | `q25` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | vote_recall_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; non offerts : PCQ, ADQ | Pour quel parti avez-vous voté lors de la dernière élection provinciale le 4 septembre 2012? | q21: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Offert explicitement |
| qes2012_panel | `voteprov` (post) | `approximate` | Rappel téléphonique spontané (options non lues) après une question de participation qui distingue le jour du scrutin et le vote par anticipation ; l’ancrage est une liste Web. | vote_recall_unprompted | CAQ, PLQ, PQ, QS, PVQ, ON, other; non offerts : PCQ, ADQ | Et pour quel parti avez-vous voté? (NE PAS LIRE) |  | `pond_post` (pas encore utilisable) | Non offert |
| qes2008 | `q12a` (post) | `comparable` | Même question (parti pour lequel la personne a voté) ; la liste nomme l’ADQ et non la CAQ ni ON, qui n’existaient pas ; la question de participation n’a pas de code « ne sais pas » ; téléphone selon les métadonnées du dépôt, l’ancrage est Web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; non offerts : CAQ, PCQ, ON | Pour quel parti avez-vous voté ? | q11: 2 = not_voted, 9 = refused |  | Non documenté |
| qes2007 | `q12` (post) | `comparable` | Même question (parti pour lequel la personne a voté, partis nommés dans l’énoncé) ; la liste nomme l’ADQ et non la CAQ ni ON, qui n’existaient pas ; l’étude combine des entrevues téléphoniques et Web (seul le questionnaire téléphonique est déposé), l’ancrage est Web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; non offerts : CAQ, PCQ, ON | Pour quel parti avez-vous voté ? Le Parti libéral, le Parti québécois, l’ADQ, Québec solidaire, le Parti vert ou un autre parti ? | q11: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Non documenté |
| qes2007_panel | `vote` (post) | `approximate` | Rappel téléphonique spontané (options non lues) après une question de participation en deux temps ; l’ancrage est une liste Web. | vote_recall_unprompted | ADQ, PLQ, PQ, QS, PVQ, other; non offerts : CAQ, PCQ, ON | Et pour quel parti avez-vous voté? |  | `pond_tot_am1` (pas encore utilisable) | Non offert |
| qes1998 | `q3post` (post) | `comparable` | Même question (parti pour lequel la personne a voté, liste lue) par téléphone, avec le même énoncé dans les questionnaires des deux firmes ; la Q3 de CREATEC utilise d’autres codes (1 PLQ, 2 PQ, 3 ADQ, 4 un autre parti) sans le Parti égalité, et le fichier regroupé les ramène aux codes de CROP, où le Parti égalité porte un astérisque (non lu) et n’est jamais choisi ; la liste nomme l’ADQ, pas la CAQ ; l’ancrage est Web. | vote_recall_list | ADQ, PLQ, PQ, other; non offerts : CAQ, QS, PVQ, PCQ, ON | 3\. Pour lequel des partis suivants avez-vous voté? |  | `ponder3` (pas encore utilisable) | Non offert |

#### `vote_prov_intent` : Intention de vote provinciale

Parti pour lequel la personne compte voter à la prochaine élection
générale québécoise, question posée avant cette élection, à la première
question, sans la relance des indécis. Ne voterait pas, aucun ou
annulerait est une réponse (niveau no_party), pas une valeur manquante.

Famille `vote_prov` · type Catégorielle · moment Préélectoral

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

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_votechoice1` (cps) | `approximate` | Liste Web avec « je ne sais pas » et refus affichés, posée seulement aux personnes certaines ou susceptibles de voter (les autres ayant reçu une question distincte ou aucune), sans option « ne voterait pas » ; l’ancrage est une question téléphonique posée à tous. | vote_intent_list | PLQ, PQ, CAQ, QS, PCQ, other; non offerts : PVQ, ON, ADQ, no_party | Pour quel parti prévoyez-vous voter? | cps_turnout: 3 = inapplicable, 4 = inapplicable, 5 = inapplicable, 6 = ineligible | `cps_weight_general` | Offert explicitement |
| qes2018_panel | `rv1a` (pre) | `approximate` | Demande le candidat de quel parti la personne appuierait probablement si l’élection avait lieu demain, et demande à celles qui ont déjà voté par anticipation d’indiquer ce vote, de sorte qu’une partie des réponses rapportent un vote déjà exprimé ; mode mixte (850 en ligne, 400 par téléphone) ; l’ancrage demande pour quel parti elle voterait aujourd’hui, par téléphone. | vote_intent_list | PLQ, PQ, CAQ, QS, other, no_party; non offerts : PVQ, PCQ, ON, ADQ | En pensant à ce que vous ressentez maintenant, si une élection PROVINCIALE était tenue demain, le candidat de quel parti appuieriez-vous probablement? Si vous avez déjà voté par anticipation, veuillez indiquer pour quel parti. |  | `weight` | Non documenté |
| qes2012_panel | `intvoteprov1` (pre) | `approximate` | L’énoncé demande pour quel parti la personne voterait « ou serait tentée de voter », ce qui rapproche la première question d’une préférence ; le questionnaire préélectoral n’est pas déposé (libellé tiré de l’étiquette de la variable et du livre de codes, fichier 654292) ; par téléphone, comme l’ancrage. | vote_intent_or_lean_list | PLQ, PQ, CAQ, QS, PVQ, ON, other, no_party; non offerts : PCQ, ADQ | Si des élections provinciales devaient avoir lieu aujourd’hui, pour lequel des partis suivants voteriez-vous ou seriez-vous tenté de voter? |  | `pondam1` (pas encore utilisable) | Non documenté |
| qes_crop_2007_2010 | `intvoteprova` (chaque sondage) | `comparable` | Même première question que l’ancrage, mot pour mot, par la même firme (CROP) et le même mode téléphonique, avec les mêmes partis et chefs lus en rotation, et « annulerait/ne voterait pas » et « ne sait pas/refus » non lus ; les sondages sont des omnibus mensuels entre les élections, et non un panel de campagne, et le libellé n’est documenté que pour trois des 24 sondages (rapports CROP de mai 2008, janvier 2009 et mars 2009 ; aucun questionnaire n’est déposé). | vote_intent_list | ADQ, PLQ, PQ, QS, PVQ, other, no_party; non offerts : CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… |  | `XPOND` (pas encore utilisable) | Spontané seulement |
| qes2007_panel | `intvote1` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | vote_intent_list | ADQ, PLQ, PQ, QS, PVQ, other, no_party; non offerts : CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… |  | `pondam1` (pas encore utilisable) | Spontané seulement |

#### `vote_prov_intent_push` : Intention de vote provinciale, indécis relancés

Intention de vote où les personnes qui n’ont nommé aucun parti à la
première question (les indécises et, dans certaines études, aussi celles
qui ne voteraient pas, pour aucun parti ou refusaient) ont été relancées
sur le parti vers lequel elles penchent, les deux réponses étant
combinées. Stimulus différent de vote_prov_intent, et cible distincte ;
la variable regroupée vote_choice l’utilise avant vote_prov_intent, et
vote_choice\_\_type indique de laquelle vient une valeur.

Famille `vote_prov` · type Catégorielle · moment Préélectoral

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

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_votechoice1` + `cps_votechoice2` + `cps_votelean` (cps) | `approximate` | Liste Web avec « je ne sais pas » et refus affichés : la première question (cps_votechoice1) posée aux personnes certaines ou susceptibles de voter, une question conditionnelle (« si vous décidez de voter », cps_votechoice2) posée à celles peu susceptibles de voter, et la question d’inclination (cps_votelean) posée à celles qui ne savaient pas à l’une ou l’autre ; pas d’option « ne voterait pas » ; l’ancrage est une question téléphonique posée à tous. | intent_lean_push | PLQ, PQ, CAQ, QS, PCQ, other; non offerts : PVQ, ON, ADQ, no_party | Pour quel parti prévoyez-vous voter? \[Si peu susceptible de voter :\] Si vous décidez de voter, pour quel parti prévoyez-vous voter? \[Si ne sait pas :\] Êtes-vous tenté(e) d’appuyer un parti en particulier? | cps_turnout: 3 = inapplicable, 4 = inapplicable, 5 = inapplicable, 6 = ineligible | `cps_weight_general` | Offert explicitement |
| qes2018_panel | `rv1ab` (pre) | `approximate` | Combinaison, faite par le producteur, de rv1a et de la relance rv1b, en mode mixte téléphone et Web ; l’ancrage est une relance téléphonique. Le filtre de la relance est plus étroit que celui de l’ancrage : les personnes qui disaient ne pas voter ou n’appuyer aucun parti (63) n’ont pas été relancées, alors que l’ancrage les relançait, et 31 des 233 indécis, tous interviewés par téléphone, n’ont pas reçu rv1b et restent indécis. rv1a, dont rv1ab reprend la réponse pour les codes 1 à 6 (1 017 personnes), demande à celles qui ont déjà voté par anticipation d’indiquer ce vote, de sorte qu’une partie des réponses rapportent un vote déjà exprimé. | intent_lean_push | PLQ, PQ, CAQ, QS, other, no_party; non offerts : PVQ, PCQ, ON, ADQ | En pensant à ce que vous ressentez maintenant, si une élection PROVINCIALE était tenue demain, le candidat de quel parti appuieriez-vous probablement? Si vous avez déjà voté par anticipation, veuillez indiquer pour quel parti. (relance, rv1b : Et pour quel parti diriez-vous que vous auriez tendance à voter?) |  | `weight` | Non documenté |
| qes2012_panel | `intvoteprov` (pre) | `approximate` | Combinaison, faite par le producteur, de la première question (qui demande déjà un parti pour lequel la personne « serait tentée de voter ») et de la relance ; le questionnaire préélectoral n’est pas déposé (libellé tiré de l’étiquette de la variable et du livre de codes, fichier 654292) ; par téléphone, comme l’ancrage. | intent_lean_push | PLQ, PQ, CAQ, QS, PVQ, ON, other, no_party; non offerts : PCQ, ADQ | Q2+Q3 - Si des élections provinciales devaient avoir lieu aujourd’hui, pour lequel des partis suivants voteriez-vous ou seriez-vous tenté de voter? (relance : Peut-être que votre choix n’est pas définitif, mais y a-t-il tout de même un parti que vous seriez tenté d’appuyer?) |  | `pondam1` (pas encore utilisable) | Non documenté |
| qes_crop_2007_2010 | `intvoteprov` (chaque sondage) | `comparable` | Combinaison, faite par le producteur, de la première question et de la relance, comme dans l’ancrage, par la même firme et le même mode ; sondages téléphoniques CROP ; le livre de codes déposé (fichier 341537) ne donne que des étiquettes tronquées, mais les rapports de CROP pour La Presse (mai 2008, janvier 2009) donnent le même libellé et la même relance que l’ancrage, les partis et leurs chefs étant lus en rotation et le « ne sais pas » n’étant noté que s’il est spontané ; pas identique, car ce sont des sondages omnibus mensuels, pour la plupart hors campagne (l’élection de référence est celle de 2008 ou de 2012 selon le sondage), sur un échantillon stratifié par région (500 Montréal, 200 Québec, 300 ailleurs). | intent_lean_push | ADQ, PLQ, PQ, QS, PVQ, other, no_party; non offerts : CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… (relance : Peut-être n’êtes-vous pas complètement décidé(e), mais actuellement pour lequel de ces partis seriez-vous tenté(e) de voter? Est-ce…) |  | `XPOND` (pas encore utilisable) | Spontané seulement |
| qes2007_panel | `intvote` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | intent_lean_push | ADQ, PLQ, PQ, QS, PVQ, other, no_party; non offerts : CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? (relance, Q5 : pour lequel de ces partis seriez-vous tenté(e) de voter?) |  | `pondam1` (pas encore utilisable) | Spontané seulement |
| qes1998 | `intvote` (pre) | `approximate` | Combinaison, faite par le producteur, de la première question et de la relance, mais sondages téléphoniques de deux firmes regroupés dans une variable (CREATEC et CROP, chacune avec son questionnaire ; celui de CREATEC n’est pas déposé) ; CREATEC n’a pas posé la question aux 79 personnes qui, à sa question sur la participation, disaient ne probablement pas (22) ou certainement pas (35) aller voter, ou ne savaient pas ou refusaient de répondre (22), alors que CROP l’a posée à tous. | intent_lean_push | ADQ, PLQ, PQ, other, no_party; non offerts : CAQ, QS, PVQ, PCQ, ON | 7a. S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Est-ce… (relance, 7b : Peut-être n’êtes-vous pas complètement décidé(e), mais actuellement pour lequel de ces partis seriez-vous tenté(e) de voter?) |  | `ponder3` (pas encore utilisable) | Non documenté |

**Non utilisé**

- qes1998 `intvote2` (pre) : `not_comparable`. Les étiquettes de valeur
  sont décalées dans la source : les effectifs sont ceux de vpl, dont le
  code 1 est l’ADQ, 3 le PLQ et 4 le PQ, mais intvote2 les étiquette
  PLQ, ADQ et Parti égalité.

#### `turnout_prov_recall` : A voté à l’élection provinciale (rappel)

Si la personne dit avoir voté à l’élection générale québécoise de
l’étude, question posée après cette élection. Les personnes non
admissibles ou non inscrites sont des valeurs manquantes avec un motif.

Famille `turnout_prov` · type Catégorielle · moment Postélectoral

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_turnout` (pes) | `approximate` | Format de participation qui ménage la face, avec trois façons de ne pas avoir voté, une option de non-inscription et une option « je ne m’en souviens pas » ; devrait modifier la part des votants. | turnout_excuse_format | yes, no | À chaque élection, certaines personnes sont dans l’incapacité d’aller voter parce qu’elles sont malades, occupées, ou pour toute autre raison. D’autres personnes ne veulent pas voter. Avez-vous voté lors de l’élection du 3 octobre 2022 au Québec? |  | `pes_weight_general` | Offert explicitement |
| qes2018 | `q5` (post) | `approximate` | Format de participation qui ménage la face, avec trois façons de ne pas avoir voté et une option de non-admissibilité, sans « Je ne sais pas » ; devrait modifier la part des votants. | turnout_excuse_format | yes, no | À chaque élection, plusieurs personnes sont incapables de voter parce qu’elles ne sont pas inscrites sur la liste électorale, elles sont malades ou elles n’ont pas le temps. Laquelle des situations suivantes correspond le mieux à votre cas? |  | `pond` | Non offert |
| qes2018_panel | `rts_q1` (post) | `approximate` | Format qui ménage la face (voulait voter mais n’a pas pu ; a décidé de ne pas voter ; ou est allé voter), en mode mixte téléphone et Web. | turnout_excuse_format | yes, no | À chaque élection, certaines personnes décident de ne pas voter, d’autres ne peuvent pas y aller pour différentes raisons. |  | `weight_rts` | Non documenté |
| qes2014 | `Q2` (post) | `comparable` | Même format oui/non, mais sans option « Je ne sais pas » (l’ancrage en offre une) et l’énoncé renvoie à « cette élection » au lieu d’en donner la date. | turnout_yesno | yes, no | Avez-vous voté à cette élection provinciale? |  | `POND` | Non offert |
| qes2012 | `q21` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | turnout_yesno | yes, no | Avez-vous voté lors de l’élection provinciale du 4 septembre 2012? |  | `pond` | Offert explicitement |
| qes2012_panel | `participation` (post) | `comparable` | Même question oui/non ; un oui est précisé (jour du scrutin ou vote par anticipation), les deux étant oui ; sans code « ne sais pas » (l’ancrage en offre un) ; par téléphone, l’ancrage est Web. | turnout_yesno_probe | yes, no | D’abord, pouvez-vous me dire si vous êtes allé voter lors de l’élection qui vient de se tenir au Québec? (SI OUI, SONDER) |  | `pond_post` (pas encore utilisable) | Non offert |
| qes2008 | `q11` (post) | `comparable` | Même question oui/non, mais sans code « ne sais pas » (l’ancrage en offre un) ; téléphone selon les métadonnées du dépôt, l’ancrage est Web. | turnout_yesno | yes, no | Avez-vous voté à cette élection provinciale ? |  |  | Non offert |
| qes2007 | `q11` (post) | `comparable` | Même question oui/non avec un code « ne se souvient pas » ; l’étude combine des entrevues téléphoniques et Web (seul le questionnaire téléphonique est déposé), l’ancrage est Web. | turnout_yesno | yes, no | Avez-vous voté à cette élection provinciale ? |  | `pond` | Non documenté |
| qes2007_panel | `voteoui` (post) | `comparable` | Même question oui/non ; un oui est précisé (jour du scrutin ou vote par anticipation), les deux étant oui ; sans code « ne sais pas » (l’ancrage en offre un) ; par téléphone, l’ancrage est Web. | turnout_yesno_probe | yes, no | D’abord, pouvez-vous me dire si vous êtes allé voter lors de l’élection qui vient de se tenir au Québec? (SI OUI, SONDER) |  | `pond_tot_am1` (pas encore utilisable) | Non offert |
| qes1998 | `q1post` (post) | `comparable` | Même question oui/non par téléphone, avec le même énoncé pour les deux firmes ; sans code « ne sais pas » (l’ancrage en offre un) ; l’ancrage est Web. | turnout_yesno | yes, no | 1\. Pouvez-vous me dire si vous avez voté à l’élection du 30 novembre dernier? |  | `ponder3` (pas encore utilisable) | Non offert |

#### `turnout_prov_likely` : Probabilité de voter à l’élection provinciale

Probabilité que la personne dit avoir de voter à la prochaine élection
générale québécoise, question posée avant cette élection. Avoir déjà
voté (par anticipation) est une réponse. Stimulus différent de
turnout_prov_recall (participation déclarée), et cible distincte ; la
variable regroupée turnout ne l’utilise que sur demande (types =
list(turnout = c(“recall”, “intention”))).

Famille `turnout_prov` · type Ordinale · moment Préélectoral

**Niveaux**

| Code | Nom             | Étiquette                  |
|------|-----------------|----------------------------|
| 5    | `already_voted` | A déjà voté                |
| 1    | `certain`       | Certain(e) de voter        |
| 2    | `likely`        | Probablement voter         |
| 3    | `unlikely`      | Peu probable de voter      |
| 4    | `certain_not`   | Certain(e) de ne pas voter |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_turnout` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | turnout_likely | certain, likely, unlikely, certain_not, already_voted | L’élection au Québec est prévue pour le 3 octobre 2022. Dans le cadre de cette élection, êtes-vous… |  | `cps_weight_general` | Non offert |

#### `vote_prov_intent_other` : Intention de vote provinciale : autre parti (texte)

Le parti que la personne a inscrit après avoir choisi un autre parti à
la question d’intention de vote (vote_prov_intent, niveau other), tel
qu’inscrit. Texte libre, non harmonisé.

Famille `vote_prov` · type Texte · moment Préélectoral

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_votechoice1_8_TEXT` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | open_text |  | Pour quel parti prévoyez-vous voter? \[Autre parti (veuillez spécifier)\] | cps_turnout: 3 = inapplicable, 4 = inapplicable, 5 = inapplicable, 6 = ineligible | `cps_weight_general` | Non offert |

#### `vote_prov_prev` : Vote provincial à l’élection précédente (rappel)

Parti pour lequel la personne dit avoir voté à l’élection générale
québécoise qui précède celle de l’étude (election_ref indique laquelle).
Rappelé des années plus tard : penche vers le gagnant de cette élection.
Les abstentionnistes, les bulletins annulés et les personnes qui
n’étaient pas admissibles alors sont des valeurs manquantes avec un
motif, jamais un parti.

Famille `vote_prov_past` · type Catégorielle · moment Tout moment

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

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_qc_vote_2018` (cps) | `comparable` | Le vote de 2018, demandé pendant la campagne de 2022 après une question distincte sur la participation (cps_qc_turnout_2018 : non donne not_voted, pas admissible donne ineligible) ; Web, quatre partis nommés ; aucune option « je ne sais pas », refus ou bulletin annulé, de sorte que ces réponses ont été saisies sous « un autre parti » et restent dans other. | vote_recall_prev_list | PLQ, PQ, CAQ, QS, other; non offerts : PVQ, PCQ, ON, ADQ | Pour quel parti avez-vous voté lors de l’élection de 2018 au Québec? | cps_qc_turnout_2018: 2 = not_voted, 3 = ineligible | `cps_weight_general` | Non offert |
| qes2018 | `q9` (post) | `comparable` (en attente d’approbation) | Le vote de 2014 (« il y a 4 ans »), demandé après l’élection de 2018 ; la liste nomme quatre partis (les autres sont « un autre parti ») ; non posée aux 270 personnes à qui le vote de 2018 n’a pas été demandé non plus (moins de 18 ans en 2018 ou âge inconnu). | vote_recall_prev_list | PLQ, PQ, CAQ, QS, other; non offerts : PVQ, PCQ, ON, ADQ | Pour quel parti aviez-vous voté il y a 4 ans lors de l’élection provinciale précédente, tenue le 7 avril 2014? |  | `pond` | Offert explicitement |
| qes2014 | `Q6` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible : le vote de 2012, demandé après l’élection de 2014. | vote_recall_prev_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; non offerts : PCQ, ADQ | Pour quel parti aviez-vous voté lors de l’élection provinciale du 4 septembre 2012? |  | `POND` | Non offert |
| qes2008 | `q13` (post) | `comparable` (en attente d’approbation) | Le vote de mars 2007, demandé vingt mois plus tard, après l’élection de 2008, par téléphone ; la liste nomme les partis de 2007 ; « aucun » (8 lignes) peut être un bulletin annulé ou une abstention, il n’est donc pas apparié; le code 95 est « I did not vote / I spoiled my ballot » dans le questionnaire anglais (197296) et « n’a pas voté » dans le fichier : ses lignes not_voted peuvent inclure des bulletins annulés ; les 5 personnes nées en 1990 (moins de 18 ans en mars 2007) ont été interrogées et ont toutes répondu 95 : elles sont not_voted, et non ineligible, et celles nées en 1989 ne peuvent être départagées sans date de naissance. | vote_recall_prev_list | PLQ, PQ, QS, PVQ, ADQ, other; non offerts : CAQ, PCQ, ON | Pour quel parti aviez-vous voté lors de l’élection provinciale du 26 mars 2007? |  |  | Non documenté |

#### `vote_fed_recall` : Vote fédéral à la dernière élection fédérale (rappel)

Parti pour lequel la personne dit avoir voté à la dernière élection
fédérale canadienne avant l’étude (le libellé de chaque ligne la nomme :
janvier 2006, octobre 2008, mai 2011, 2021). Les abstentionnistes, les
bulletins annulés et les personnes qui n’étaient pas admissibles alors
sont des valeurs manquantes avec un motif, jamais un parti.

Famille `vote_fed` · type Catégorielle · moment Tout moment

**Niveaux**

| Code | Nom     | Étiquette      |
|------|---------|----------------|
| 1    | `LPC`   | Libéral        |
| 2    | `CPC`   | Conservateur   |
| 3    | `NDP`   | NPD            |
| 4    | `BQ`    | Bloc Québécois |
| 5    | `GPC`   | Vert           |
| 6    | `PPC`   | PPC            |
| 90   | `other` | Un autre parti |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_pastpartyvote` (cps) | `comparable` | Le vote fédéral de 2021, demandé pendant la campagne provinciale de 2022 après une question distincte sur la participation (cps_pastvote) ; Web, le PPC nommé. | vote_fed_recall_list | LPC, CPC, NDP, BQ, GPC, PPC, other | Pour quel parti avez-vous voté lors de la dernière élection fédérale de 2021? | cps_pastvote: 2 = not_voted, 3 = ineligible | `cps_weight_general` | Non offert |
| qes2012 | `q27` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible : le vote fédéral de mai 2011, après une question distincte sur la participation (q22). | vote_fed_recall_list | LPC, CPC, NDP, BQ, GPC, other; non offerts : PPC | Et lors de la dernière élection fédérale en mai 2011? Avez-vous voté pour: | q22: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Offert explicitement |
| qes2008 | `q74` (post) | `comparable` | Le vote fédéral d’octobre 2008, demandé deux mois plus tard, avec l’abstention et le bulletin annulé comme réponses de la même question ; téléphone selon les métadonnées du dépôt (mode à confirmer). | vote_fed_recall_list | LPC, CPC, NDP, BQ, GPC, other; non offerts : PPC | Lors de la dernière élection FÉDÉRALE en octobre 2008, pour quel parti avez-vous voté ? |  |  | Non documenté |
| qes2007 | `q74` (post) | `approximate` | Le vote fédéral de janvier 2006, demandé environ 15 mois plus tard, avec l’abstention et le bulletin annulé comme réponses de la même question ; le questionnaire téléphonique déposé indique de ne pas lire la liste (spontané), mais l’étude combine des entrevues téléphoniques (1 003) et Web (1 172) et seul le questionnaire téléphonique est déposé ; on ne sait donc pas comment la version Web présentait les choix. | vote_fed_recall_unprompted | LPC, CPC, NDP, BQ, GPC, other; non offerts : PPC | Lors de la dernière élection FÉDÉRALE en Janvier 2006, pour quel parti avez-vous voté ? |  | `pond` | Non documenté |

### Identification partisane

#### `pid_prov` : Identification partisane provinciale

Parti provincial auquel la personne s’identifie habituellement ; aucun
est une réponse.

Famille `party_id` · type Catégorielle · moment Tout moment

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

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_provpid` (cps) | `comparable` | Même énoncé que l’ancrage, mais la liste retire les options oniste et vert, ajoute conservateur et un autre parti, et n’offre pas « je ne sais pas ». | pid_prov_list | PLQ, PQ, CAQ, QS, PCQ, other, none; non offerts : PVQ, ON, ADQ | En politique provinciale, vous considérez-vous habituellement comme étant: |  | `cps_weight_general` | Non offert |
| qes2018 | `q56` (post) | `comparable` | Même énoncé que l’ancrage, mais seuls les quatre principaux partis sont proposés (ni oniste ni vert). | pid_prov_list | PLQ, PQ, CAQ, QS, none; non offerts : PVQ, PCQ, ON, ADQ, other | En politique provinciale, vous considérez-vous habituellement comme un…? |  | `pond` | Offert explicitement |
| qes2014 | `Q55` (post) | `identical` | Même énoncé et mêmes six partis, options aucun, je ne sais pas et refus que l’ancrage, en anglais et en français, Web. | pid_prov_list | PLQ, PQ, CAQ, QS, ON, PVQ, none; non offerts : PCQ, ADQ, other | En politique provinciale, vous considérez-vous habituellement comme un…? |  | `POND` | Offert explicitement |
| qes2012 | `q92` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | pid_prov_list | PLQ, PQ, CAQ, QS, ON, PVQ, none; non offerts : PCQ, ADQ, other | En politique provinciale, vous considérez-vous habituellement comme un…? |  | `pond` | Offert explicitement |
| qes2008 | `q70` (post) | `comparable` | Même concept (identification partisane provinciale habituelle, aucun est une réponse) ; l’énoncé dit « habituellement est-ce que vous vous identifiez » et la liste nomme l’ADQ ; téléphone selon les métadonnées du dépôt, l’ancrage est Web. | pid_prov_list | PLQ, PQ, ADQ, QS, PVQ, other, none; non offerts : CAQ, PCQ, ON | En politique provinciale, habituellement est-ce que vous vous identifiez au / à…? |  |  | Non documenté |
| qes2007 | `q70` (post) | `comparable` | Même concept (identification partisane provinciale habituelle, aucun est une réponse) ; l’énoncé dit « habituellement est-ce que vous vous identifiez » et la liste nomme l’ADQ ; l’étude combine des entrevues téléphoniques et Web (seul le questionnaire téléphonique est déposé), l’ancrage est Web. | pid_prov_list | PLQ, PQ, ADQ, QS, PVQ, other, none; non offerts : CAQ, PCQ, ON | En politique provinciale, habituellement est-ce que vous vous identifiez …? |  | `pond` | Non documenté |

#### `pid_fed` : Identification partisane fédérale

Le parti fédéral dont la personne se sent habituellement proche ; aucun
est une réponse.

Famille `party_id` · type Catégorielle · moment Tout moment

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

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_fedpid` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | pid_list | LPC, CPC, NDP, BQ, GPC, PPC, other, none | En politique fédérale, vous considérez-vous habituellement comme étant : |  | `cps_weight_general` | Non offert |
| qes2012 | `q94` (post) | `comparable` | Même question que l’ancrage ; la liste n’a pas le Parti populaire (fondé en 2018) ni d’option « un autre parti » ; Web. | pid_list | LPC, CPC, NDP, BQ, GPC, none; non offerts : PPC, other | En politique fédérale, vous considérez-vous habituellement comme un…? |  | `pond` | Offert explicitement |

#### `pid_prov_strength` : Force de l’identification partisane provinciale

La force avec laquelle la personne s’identifie au parti provincial nommé
à la question d’identification partisane (très, assez, pas très
fortement), posée à celles qui ont nommé un parti. Celles qui n’en ont
nommé aucun, ne savaient pas ou refusaient n’ont pas été interrogées :
la valeur manque, motif inapplicable.

Famille `party_id` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom        | Étiquette          |
|------|------------|--------------------|
| 1    | `very`     | Très fortement     |
| 2    | `fairly`   | Assez fortement    |
| 3    | `not_very` | Pas très fortement |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_provpidstr` (cps) | `approximate` (en attente d’approbation) | La version anglaise demande la force du sentiment (very, fairly, not very strongly), mais la version française, remplie par 1 291 des 1 521 personnes (cps_UserLanguage FR-CA), qui donnent 1 164 des 1 352 réponses, demande à quel point la personne se sent proche (très proche, proche, pas très proche), formulation de proximité cotée approximative en 2007 et 2008 ; posée à qui a nommé un parti à cps_provpid (aucun de ceux-là, code 6, est écarté), Web, sans option « je ne sais pas », pendant la campagne. | pid_strength_3 | very, fairly, not_very | À quel point vous sentez-vous \${e://Field/fr_pid_pr}? | cps_provpid: 6 = inapplicable | `cps_weight_general` | Non offert |
| qes2018 | `q57` (post) | `comparable` | Même énoncé et mêmes options que l’ancrage, posée à qui a nommé un parti à q56, dont la liste n’offre que quatre partis. | pid_strength_3 | very, fairly, not_very | Vous sentez-vous très fortement \[insérer réponse de Q56\], assez fortement, ou pas très fortement? | q56: 97 = inapplicable, 98 = inapplicable, 99 = inapplicable | `pond` | Offert explicitement |
| qes2014 | `Q56` (post) | `identical` | Même énoncé, mêmes options et même filtre que l’ancrage (posée à qui a nommé un parti à Q55), Web. | pid_strength_3 | very, fairly, not_very | Vous sentez-vous très fortement \[insérer réponse de Q55\], assez fortement, ou pas très fortement? | Q55: 97 = inapplicable, 98 = inapplicable, 99 = inapplicable | `POND` | Offert explicitement |
| qes2012 | `q93` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | pid_strength_3 | very, fairly, not_very | Vous sentez-vous très fortement \[insérer réponse de Q92\], assez fortement, ou pas très fortement? | q92: 97 = inapplicable, 98 = inapplicable, 99 = inapplicable | `pond` | Offert explicitement |
| qes2008 | `q71` (post) | `approximate` | Demande à quel point la personne se sent proche du parti (très proche, assez proche, pas très proche), et non la force de son identification ; posée à qui a nommé un des partis de la liste à q70 (un autre parti, aucun, ne sait pas et refus sont écartés) ; par téléphone. | pid_closeness_3 | very, fairly, not_very | Vous sentez-vous très proche du / de , assez proche, ou pas très proche ? | q70: 96 = inapplicable, 97 = inapplicable, 98 = inapplicable, 99 = inapplicable |  | Non documenté |
| qes2007 | `q71` (post) | `approximate` | Demande à quel point la personne se sent proche du parti (très proche, assez proche, pas très proche), et non la force de son identification ; posée à qui a nommé un des partis de la liste à q70 (un autre parti, aucun, ne sait pas et refus sont écartés) ; par téléphone. | pid_closeness_3 | very, fairly, not_very | Vous sentez-vous très proche du , assez proche, ou pas très proche ? | q70: 96 = inapplicable, 97 = inapplicable, 98 = inapplicable, 99 = inapplicable | `pond` | Non documenté |

### Attitudes

#### `sov_indep` : Vote référendaire : pays indépendant

Comment la personne voterait à un référendum demandant si le Québec doit
devenir un pays indépendant.

Famille `sovereignty` · type Catégorielle · moment Tout moment

**Niveaux**

| Code | Nom              | Étiquette                      |
|------|------------------|--------------------------------|
| 1    | `yes`            | Oui                            |
| 2    | `no`             | Non                            |
| 95   | `would_not_vote` | N’irait pas voter / annulerait |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_qc_referendum` (cps) | `comparable` | L’énoncé ajoute un adverbe de temps ; « je ne sais pas » est offert, mais pas « je préfère ne pas répondre ». | sov_indep_country | yes, no; non offerts : would_not_vote | Si un référendum sur l’indépendance avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? |  | `cps_weight_general` | Offert explicitement |
| qes2018 | `q26` (post) | `comparable` | L’énoncé ajoute un adverbe de temps (« today », « aujourd’hui ») ; sinon même libellé et mêmes options que l’ancrage. | sov_indep_country | yes, no; non offerts : would_not_vote | Si un référendum sur l’indépendance avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? |  | `pond` | Offert explicitement |
| qes2014 | `Q19` (post) | `identical` | Même énoncé que l’ancrage en anglais et en français, mêmes options (oui, non, je ne sais pas, je préfère ne pas répondre), posé à tous, Web. | sov_indep_country | yes, no; non offerts : would_not_vote | Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? |  | `POND` | Offert explicitement |
| qes2012 | `q52` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | sov_indep_country | yes, no; non offerts : would_not_vote | Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? |  | `pond` | Offert explicitement |

#### `sov_sovereign_country` : Vote référendaire : pays souverain

Comment la personne voterait à un référendum demandant si le Québec doit
devenir un pays souverain. Stimulus différent de sov_indep (souverain et
non indépendant), et cible distincte ; la variable regroupée sov_support
les combine et indique le libellé dans sov_support\_\_type. La question
de relance posée aux indécis ne fait pas partie de cette cible.

Famille `sovereignty` · type Catégorielle · moment Tout moment

**Niveaux**

| Code | Nom              | Étiquette                      |
|------|------------------|--------------------------------|
| 1    | `yes`            | Oui                            |
| 2    | `no`             | Non                            |
| 95   | `would_not_vote` | N’irait pas voter / annulerait |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2012_panel | `intvoteref` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible ; libellé tiré de l’étiquette de la variable et du livre de codes seulement (le questionnaire préélectoral n’est pas déposé), la lecture de « ne voterait pas » et de « ne sait pas » n’est donc pas documentée. | sov_sovereign_country | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays souverain, voteriez-vous oui ou voteriez-vous non? |  | `pondam1` (pas encore utilisable) | Non documenté |

#### `sov_favour` : Appui à l’indépendance du Québec (4 points)

Degré d’appui ou d’opposition de la personne à l’indépendance du Québec,
sur une échelle à quatre points. Ce n’est pas un vote référendaire.

Famille `sovereignty` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom                   | Étiquette       |
|------|-----------------------|-----------------|
| 1    | `very_favourable`     | Très favorable  |
| 2    | `somewhat_favourable` | Assez favorable |
| 3    | `somewhat_opposed`    | Assez opposé    |
| 4    | `very_opposed`        | Très opposé     |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `rts_q7` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible ; libellé tiré du livre de codes seulement (aucun questionnaire déposé), l’offre de « ne sait pas » sur le Web n’est donc pas documentée. | sov_favour_4pt | very_favourable, somewhat_favourable, somewhat_opposed, very_opposed | En ce qui concerne l’indépendance du Québec, c’est-à-dire que le Québec ne fasse plus partie du Canada, êtes-vous personnellement |  | `weight_rts` | Non documenté |

**Non utilisé**

- qes2018_panel `independance` (post) : `not_comparable`. Un recodage de
  rts_q7 (1-2 favorable, 3-4 opposé, 5 et manquant mis à manquant), pas
  une question distincte : jamais apparié.

#### `lr_self` : Autopositionnement gauche-droite (0-10)

Position que la personne se donne sur une échelle de 0 (gauche) à 10
(droite).

Famille `left_right` · type Numérique · moment Tout moment

**Plage valide** : 0-10

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_ideoself_1` (cps) | `approximate` | Question isolée de 0 à 10, énoncé différent, sans « je ne sais pas » et non précédé des positionnements des partis ; la non-réponse partielle est bien plus faible que la part de « je ne sais pas » de l’ancrage. | lr_0_10 |  | En politique, on parle parfois de gauche et de droite. Où vous placeriez-vous sur une échelle de 0 à 10, où 0 indique la gauche et 10 la droite? |  | `cps_weight_general` | Non offert |
| qes2018 | `q36_1` (post) | `comparable` | Même énoncé que l’ancrage, précédé des positionnements des partis, mais les extrémités se lisent « à gauche » et « à droite » au lieu de « le plus à gauche » et « le plus à droite ». | lr_0_10 |  | Et sur la même échelle, où vous placeriez-vous, de manière générale? |  | `pond` | Offert explicitement |
| qes2018_panel | `rts_q8` (post) | `approximate` | Énoncé différent, mode mixte téléphone et Web ; le libellé déposé est tronqué à 80 caractères, de sorte que les libellés des extrémités ne peuvent être comparés. | lr_0_10 |  | On utilise souvent un axe gauche-droite pour situer les opinions politiques des gens. Sur une échelle de 0 |  | `weight_rts` | Non documenté |
| qes2014 | `Q32` (post) | `identical` | Même énoncé et mêmes libellés des extrémités que l’ancrage en anglais et en français, précédé des mêmes positionnements des partis, Web. | lr_0_10 |  | Et sur la même échelle, où vous placeriez-vous, de manière générale? |  | `POND` | Offert explicitement |
| qes2012 | `q71` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | lr_0_10 |  | Et sur la même échelle, où vous placeriez-vous, de manière générale? |  | `pond` | Offert explicitement |

#### `interest_4pt` : Intérêt pour la politique (4 points)

Intérêt de la personne pour la politique, sur une échelle verbale à
quatre points. Cible distincte des échelles d’intérêt de 0 à 10, jamais
convertie ; la variable regroupée pol_interest la note sur 0-1, au mieux
au niveau approximate.

Famille `interest` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom          | Étiquette                |
|------|--------------|--------------------------|
| 1    | `very`       | Très intéressé(e)        |
| 2    | `quite`      | Plutôt intéressé(e)      |
| 3    | `hardly`     | Pas très intéressé(e)    |
| 4    | `not_at_all` | Pas du tout intéressé(e) |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `q27` (post) | `comparable` | L’énoncé ajoute « et les enjeux publics » ; les options se lisent très, assez, peu et pas du tout intéressé(e). | interest_4pt | very, quite, hardly, not_at_all | Quel est votre intérêt pour la politique et les enjeux publics en général? Êtes-vous: |  | `pond` | Offert explicitement |
| qes2014 | `Q28` (post) | `comparable` | Même énoncé en anglais et en français et mêmes options en français, mais la troisième option anglaise est « Not very interested » là où l’ancrage a « Hardly interested ». | interest_4pt | very, quite, hardly, not_at_all | Quel est votre intérêt pour la politique en général? Êtes-vous: |  | `POND` | Offert explicitement |
| qes2012 | `q67` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interest_4pt | very, quite, hardly, not_at_all | Quel est votre intérêt pour la politique en général? Êtes-vous: |  | `pond` | Offert explicitement |

**Non utilisé**

- qes2012_panel `interetrec` (pre) : `not_comparable`. Pas une question
  : un score d’intérêt pour la campagne que le producteur a dérivé de la
  part de chacun des quatre débats des chefs que la personne a regardée.

#### `sov_partnership_1995` : Vote référendaire : la question de souveraineté-partenariat de 1995

Comment la personne voterait si un référendum avait lieu aujourd’hui sur
la question du référendum de 1995, la souveraineté assortie d’une offre
de partenariat au reste du Canada. Stimulus différent de sov_indep et de
sov_sovereign_country, et cible distincte ; la variable regroupée
sov_support les combine et indique le libellé dans sov_support\_\_type.
La question de relance posée aux indécis ne fait pas partie de cette
cible (sov_partnership_1995_push l’inclut).

Famille `sovereignty` · type Catégorielle · moment Tout moment

**Niveaux**

| Code | Nom              | Étiquette                      |
|------|------------------|--------------------------------|
| 1    | `yes`            | Oui                            |
| 2    | `no`             | Non                            |
| 95   | `would_not_vote` | N’irait pas voter / annulerait |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q19` (post) | `comparable` | Même énoncé et mêmes options que l’ancrage en anglais et en français ; le mode diffère (téléphone selon les métadonnées du dépôt ; l’étude d’ancrage combine téléphone et Web). | sov_partnership_1995 | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON ? |  |  | Non documenté |
| qes2007 | `q19` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible : la question de 1995 lue au complet ; « ne voterait pas/annulerait », « ne sais pas » et le refus sont des codes ; « ne sais pas » est relancé à q20, qui ne fait pas partie de la cible. | sov_partnership_1995 | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON ? |  | `pond` | Non documenté |
| qes2007_panel | `intref1` (pre) | `comparable` | Même question et mêmes options, par téléphone ; l’énoncé dit « accompagnée d’une offre de partenariat » où l’ancrage dit « assortie d’une offre » ; « ne voterait pas », « ne sait pas » et le refus ne sont pas lus. | sov_partnership_1995 | yes, no, would_not_vote | document 352416, 13 |  | `pondam1` (pas encore utilisable) | Spontané seulement |
| qes1998 | `q16a_crop` (pre) | `comparable` | Même énoncé et mêmes options que l’ancrage, par téléphone ; posée par CROP seulement (426 des 1 483), pas par CREATEC. | sov_partnership_1995 | yes, no, would_not_vote | 16a. Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON | firme_post: 1 = inapplicable | `ponder3` (pas encore utilisable) | Non documenté |

#### `interest_0_10` : Intérêt pour la politique (0-10)

Intérêt de la personne pour la politique en général, de 0 (aucun
intérêt) à 10 (beaucoup d’intérêt). Cible distincte de l’échelle en
quatre points (interest_4pt), jamais convertie ; la variable regroupée
pol_interest la divise par 10.

Famille `interest` · type Numérique · moment Tout moment

**Plage valide** : 0-10

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_interest_1` (cps) | `approximate` | Un curseur de 0 à 10 sur le Web, sans option « ne sait pas » ; l’ancrage est un nombre sur une échelle lue, avec « ne sait pas » et refus spontanés. | interest_0_10_slider |  | Quel est votre niveau d’intérêt pour la politique en général? Veuillez glisser le curseur sur un chiffre de 0 à 10, où 0 indique aucun intérêt du tout et 10 indique beaucoup d’intérêt. |  | `cps_weight_general` | Non offert |
| qes2007 | `q15` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interest_0_10 |  | Et toujours avec la même échelle, quel est votre intérêt pour la politique en général ? (Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt) |  | `pond` | Spontané seulement |

#### `interest_election_0_10` : Intérêt pour l’élection provinciale (0-10)

Intérêt de la personne pour l’élection générale québécoise qui vient
d’avoir lieu, de 0 (aucun intérêt) à 10 (beaucoup d’intérêt), question
posée après l’élection. Intérêt pour une élection, pas pour la politique
en général.

Famille `interest` · type Numérique · moment Postélectoral

**Plage valide** : 0-10

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q14` (post) | `comparable` | Même question et même échelle ; le mode reste à confirmer, l’ancrage combine téléphone et Web. | interest_0_10 |  | Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt, quel a été votre intérêt pour l’élection PROVINCIALE qui vient d’avoir lieu ? |  |  | Offert explicitement |
| qes2007 | `q14` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interest_0_10 |  | Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt, quel a été votre intérêt pour l’élection PROVINCIALE qui vient d’avoir lieu ? |  | `pond` | Spontané seulement |

#### `interest_campaign_4pt` : Intérêt pour la campagne électorale (4 points)

Intérêt de la personne pour la campagne électorale québécoise en cours,
sur une échelle verbale en quatre points, question posée pendant la
campagne. Intérêt pour une campagne, pas pour la politique en général
(interest_4pt).

Famille `interest` · type Ordinale · moment Préélectoral

**Niveaux**

| Code | Nom          | Étiquette                |
|------|--------------|--------------------------|
| 1    | `very`       | Très intéressé(e)        |
| 2    | `quite`      | Plutôt intéressé(e)      |
| 3    | `hardly`     | Pas très intéressé(e)    |
| 4    | `not_at_all` | Pas du tout intéressé(e) |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2007_panel | `interet` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | interest_4pt | very, quite, hardly, not_at_all | Personnellement, vous intéressez-vous beaucoup, assez, peu ou pas du tout à la présente campagne électorale au Québec? |  | `pondam1` (pas encore utilisable) | Spontané seulement |

#### `sov_partnership_1995_push` : Vote référendaire : la question de 1995, indécis relancés

Comment la personne voterait sur la question du référendum de 1995 (la
souveraineté assortie d’une offre de partenariat au reste du Canada),
les personnes qui ne savaient pas à la première question ayant été
relancées sur le côté vers lequel elles pencheraient, les deux réponses
étant combinées : la première réponse s’il y en a une, sinon la réponse
à la relance. Cible distincte de sov_partnership_1995, qui ne contient
que la première question ; la variable regroupée sov_support utilise
celle-ci en premier.

Famille `sovereignty` · type Catégorielle · moment Tout moment

**Niveaux**

| Code | Nom              | Étiquette                      |
|------|------------------|--------------------------------|
| 1    | `yes`            | Oui                            |
| 2    | `no`             | Non                            |
| 95   | `would_not_vote` | N’irait pas voter / annulerait |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q19` + `q20` (post) | `comparable` | Même première question et même relance que l’ancrage (q19, puis q20 pour les personnes qui ne savaient pas) ; le mode diffère (téléphone selon les métadonnées du dépôt ; l’étude d’ancrage combine téléphone et Web). | sov_partnership_1995_push | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON ? \[Si ne sait pas :\] Même si vous n’avez peut-être pas encore fait votre choix, s’il y avait un référendum aujourd’hui sur cette question, seriez-vous tenté(e) de voter pour le OUI ou pour le NON ? |  |  | Non documenté |
| qes2007 | `q19` + `q20` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible : la question de 1995 lue au complet (q19) et, pour les personnes qui ne savaient pas, la relance q20. | sov_partnership_1995_push | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON ? \[Si ne sait pas :\] Même si vous n’avez peut-être pas encore fait votre choix, s’il y avait un référendum aujourd’hui sur cette question, seriez-vous tenté(e) de voter pour le OUI ou pour le NON ? |  | `pond` | Non documenté |
| qes2007_panel | `intref1` + `intref2` (pre) | `comparable` | Même question et même relance que l’ancrage, par téléphone ; l’énoncé dit « accompagnée d’une offre de partenariat » où l’ancrage dit « assortie d’une offre » ; « ne voterait pas », « ne sait pas » et le refus ne sont pas lus. La relance intref2 a été posée aux personnes qui ne voteraient pas, ne savaient pas ou refusaient (seules celles qui ne savaient pas sont relancées ici, comme dans l’ancrage). | sov_partnership_1995_push | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté accompagnée d’une offre de partenariat au reste du Canada, voteriez-vous Oui ou voteriez-vous Non? \[Si ne sait pas :\] Même si vous n’avez peut-être pas encore fait votre choix, s’il y avait un référendum aujourd’hui sur cette question, seriez-vous tenté(e) de voter Oui ou de voter Non? |  | `pondam1` (pas encore utilisable) | Spontané seulement |
| qes1998 | `q16a_crop` + `q16b_crop` (pre) | `comparable` | Même question et même relance que l’ancrage, par téléphone ; posée par CROP seulement (426 des 1 483), pas par CREATEC. La relance q16b_crop a été posée aux personnes qui ne voteraient pas, ne savaient pas ou refusaient (seules celles qui ne savaient pas sont relancées ici, comme dans l’ancrage). | sov_partnership_1995_push | yes, no, would_not_vote | 16a. Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON \[Si ne sait pas :\] 16b. Même si vous n’avez peut-être pas encore fait votre choix, s’il y avait un référendum aujourd’hui sur cette question, seriez-vous tenté(e) de voter pour le OUI ou pour le NON? | firme_post: 1 = inapplicable | `ponder3` (pas encore utilisable) | Non documenté |

#### `satis_demo_qc` : Satisfaction à l’égard de la démocratie au Québec

À quel point la personne est satisfaite, dans l’ensemble, de la façon
dont la démocratie fonctionne au Québec, sur une échelle verbale à
quatre points (très, assez, pas très, pas du tout satisfaite).

Famille `democracy_satisfaction` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom          | Étiquette                |
|------|--------------|--------------------------|
| 1    | `very`       | Très satisfait(e)        |
| 2    | `fairly`     | Assez satisfait(e)       |
| 3    | `not_very`   | Pas très satisfait(e)    |
| 4    | `not_at_all` | Pas du tout satisfait(e) |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_satis_prov` (cps) | `comparable` | Même concept et mêmes options ; l’énoncé demande « quel est votre niveau de satisfaction » ; Web, sans option « je ne sais pas » (une question sautée est no_answer) ; posée pendant la campagne. | satis_4pt | very, fairly, not_very, not_at_all | Dans l’ensemble, quel est votre niveau de satisfaction quant au fonctionnement de la démocratie au Québec? |  | `cps_weight_general` | Non offert |
| qes2018 | `q1` (post) | `comparable` | L’énoncé demande « à quel point êtes-vous satisfait(e) » (ancrage : « êtes-vous satisfait(e) ») ; la deuxième option se lit « Somewhat satisfied » en anglais (ancrage : « Fairly satisfied ») et la troisième « Peu satisfait(e) » en français (ancrage : « Pas très satisfait(e) ») ; Web, « je ne sais pas » et refus affichés. | satis_4pt | very, fairly, not_very, not_at_all | Dans l’ensemble, à quel point êtes-vous satisfait(e) de la façon dont la démocratie fonctionne au Québec? Êtes-vous: |  | `pond` | Offert explicitement |
| qes2014 | `Q35` (post) | `identical` | Même énoncé et mêmes options que l’ancrage en anglais et en français, Web, avec « je ne sais pas » et refus affichés. | satis_4pt | very, fairly, not_very, not_at_all | Dans l’ensemble, êtes-vous satisfait(e) de la façon dont la démocratie fonctionne au Québec? Êtes-vous: |  | `POND` | Offert explicitement |
| qes2012 | `q74` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | satis_4pt | very, fairly, not_very, not_at_all | Dans l’ensemble, êtes-vous satisfait(e) de la façon dont la démocratie fonctionne au Québec? Êtes-vous: |  | `pond` | Offert explicitement |
| qes2008 | `q27` (post) | `comparable` | Même énoncé et mêmes options que l’ancrage en anglais et en français ; téléphone selon les métadonnées du dépôt (les questionnaires déposés ressemblent à un questionnaire Web) ; « je ne sais pas » et refus codés 8 et 9. | satis_4pt | very, fairly, not_very, not_at_all | Dans l’ensemble, êtes-vous SATISFAIT de la façon dont la démocratie fonctionne au Québec ? Êtes-vous… |  |  | Non documenté |
| qes2007 | `q27` (post) | `comparable` | Même énoncé et mêmes options que l’ancrage ; l’étude combine des entrevues téléphoniques et Web (seul le questionnaire téléphonique est déposé), « ne sais pas » non lu. | satis_4pt | very, fairly, not_very, not_at_all | Dans l’ensemble, êtes-vous SATISFAIT de la façon dont la démocratie fonctionne au Québec ? Êtes-vous… |  | `pond` | Non documenté |

#### `gov_satisfaction` : Satisfaction à l’égard du gouvernement du Québec

À quel point la personne est satisfaite de la performance du
gouvernement du Québec en place, sur une échelle à quatre points. Le
gouvernement change par construction : le libellé de chaque ligne le
nomme (le gouvernement libéral en 2012, le gouvernement péquiste en
2014, celui de Philippe Couillard en 2018, le gouvernement sous François
Legault en 2022).

Famille `government_satisfaction` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom          | Étiquette                |
|------|--------------|--------------------------|
| 1    | `very`       | Très satisfait(e)        |
| 2    | `fairly`     | Assez satisfait(e)       |
| 3    | `not_very`   | Pas très satisfait(e)    |
| 4    | `not_at_all` | Pas du tout satisfait(e) |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_province_gov_sat` (cps) | `comparable` | Le gouvernement caquiste sortant, nommé par son chef (sous François Legault) ; Web, sans option « je ne sais pas » ; posée pendant la campagne. | gov_satis_4pt | very, fairly, not_very, not_at_all | Quel est votre niveau de satisfaction de la performance du gouvernement du Québec sous François Legault? |  | `cps_weight_general` | Non offert |
| qes2018 | `q10` (post) | `comparable` | Le gouvernement libéral sortant, nommé par son chef (celui de Philippe Couillard) ; l’énoncé demande le niveau global de satisfaction et la troisième option se lit « Peu satisfait(e) ». | gov_satis_4pt | very, fairly, not_very, not_at_all | Quel est votre niveau global de satisfaction envers la performance du gouvernement libéral de Philippe Couillard? |  | `pond` | Offert explicitement |
| qes2014 | `Q10` (post) | `comparable` | Même énoncé et mêmes options que l’ancrage sur le gouvernement en place, le gouvernement péquiste sortant de Pauline Marois (nommé par son parti). | gov_satis_4pt | very, fairly, not_very, not_at_all | À quel point êtes-vous satisfait(e) de la performance du gouvernement péquiste en général? |  | `POND` | Offert explicitement |
| qes2012 | `q35` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible : le gouvernement libéral sortant de Jean Charest. | gov_satis_4pt | very, fairly, not_very, not_at_all | À quel point êtes-vous satisfait(e) de la performance du gouvernement libéral provincial en général? |  | `pond` | Offert explicitement |
| qes2007_panel | `satisf` (pre) | `approximate` | Une échelle bipolaire (très ou plutôt satisfait, plutôt ou très insatisfait) lue au téléphone pendant la campagne, sur le présent gouvernement du Québec (le gouvernement libéral de Jean Charest) ; plutôt insatisfait et très insatisfait sont pris pour pas très et pas du tout satisfait. | gov_satis_bipolar | very, fairly, not_very, not_at_all | Diriez-vous que vous êtes très satisfait(e), plutôt satisfait(e), plutôt insatisfait(e) ou très insatisfait(e) du présent gouvernement du Québec? |  | `pondam1` (pas encore utilisable) | Spontané seulement |

#### `econ_retro_qc` : L’économie du Québec depuis un an

Si la personne pense que l’économie du Québec s’est améliorée, est
restée à peu près la même ou s’est détériorée depuis un an (une
évaluation rétrospective et sociotropique).

Famille `economy_retrospective` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom      | Étiquette          |
|------|----------|--------------------|
| 1    | `better` | Améliorée          |
| 2    | `same`   | À peu près la même |
| 3    | `worse`  | Détériorée         |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_provecon` (cps) | `comparable` | Même question et mêmes options, Web, sans option « je ne sais pas » ; posée pendant la campagne, marquée par l’inflation de 2022. | econ_retro_3 | better, same, worse | Depuis un an, l’économie du Québec s’est-elle… |  | `cps_weight_general` | Non offert |
| qes2018 | `q53` (post) | `identical` | Même énoncé et mêmes options que l’ancrage en anglais et en français, Web, avec « je ne sais pas » et refus affichés (codes 98 et 99). | econ_retro_3 | better, same, worse | Selon vous, l’économie du Québec s’est-elle améliorée, détériorée ou est-elle restée à peu près la même depuis un an? |  | `pond` | Offert explicitement |
| qes2014 | `Q52` (post) | `identical` | Même énoncé et mêmes options que l’ancrage en anglais et en français, Web, avec « je ne sais pas » et refus affichés. | econ_retro_3 | better, same, worse | Selon vous, l’économie du Québec s’est-elle améliorée, détériorée ou est-elle restée à peu près la même depuis un an? |  | `POND` | Offert explicitement |
| qes2012 | `q91` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | econ_retro_3 | better, same, worse | Selon vous, l’économie du Québec s’est-elle améliorée, détériorée ou est-elle restée à peu près la même depuis un an? |  | `pond` | Offert explicitement |
| qes2008 | `q47` (post) | `comparable` | Même question et mêmes options, par téléphone, en décembre 2008 au début de la crise financière. | econ_retro_3 | better, same, worse | Selon vous, l’économie québécoise s’est-elle AMÉLIORÉE, DÉTÉRIORÉE, ou est-elle restée à PEU PRÈS LA MÊME depuis un an ? |  |  | Non documenté |
| qes2007 | `q47` (post) | `comparable` | Même question et mêmes options ; l’étude combine des entrevues téléphoniques et Web (seul le questionnaire téléphonique est déposé). | econ_retro_3 | better, same, worse | Selon vous, l’économie québécoise s’est-elle AMÉLIORÉE, DÉTÉRIORÉE, ou est-elle restée à PEU PRÈS LA MÊME depuis un an ? |  | `pond` | Non documenté |

#### `attach_qc` : Attachement au Québec

À quel point la personne se sent attachée au Québec, sur une échelle à
quatre points (très, assez, pas très, pas du tout).

Famille `attachment` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom          | Étiquette              |
|------|--------------|------------------------|
| 1    | `very`       | Très attaché(e)        |
| 2    | `fairly`     | Assez attaché(e)       |
| 3    | `not_very`   | Pas très attaché(e)    |
| 4    | `not_at_all` | Pas du tout attaché(e) |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_qc_attach` (cps) | `comparable` | Même énoncé que l’ancrage ; les options françaises se lisent « Assez attaché(e) » et « Peu attaché(e) » ; Web ; « je ne sais pas » est une option, mais il n’y a pas d’option de refus ; posée pendant la campagne (préélectorale), alors que l’ancrage est postélectoral. | attach_4pt | very, fairly, not_very, not_at_all | Quel est votre degré d’attachement au Québec? |  | `cps_weight_general` | Offert explicitement |
| qes2018 | `q18` (post) | `comparable` | Même énoncé que l’ancrage ; la deuxième option anglaise se lit « Somewhat attached » (ancrage « Fairly attached ») et les options françaises se lisent « Assez attaché(e) » et « Peu attaché(e) » (ancrage « Plutôt attaché(e) », « Pas très attaché(e) »). | attach_4pt | very, fairly, not_very, not_at_all | Quel est votre degré d’attachement au Québec? |  | `pond` | Offert explicitement |
| qes2014 | `Q12` (post) | `comparable` | Même énoncé que l’ancrage ; les options françaises se lisent « Plutôt attaché(e) » et « Pas très attaché(e) ». | attach_4pt | very, fairly, not_very, not_at_all | Quel est votre degré d’attachement au Québec? |  | `POND` | Offert explicitement |
| qes2012 | `q1` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | attach_4pt | very, fairly, not_very, not_at_all | Quel est votre degré d’attachement au Québec? |  | `pond` | Offert explicitement |

#### `attach_ca` : Attachement au Canada

À quel point la personne se sent attachée au Canada, sur une échelle à
quatre points (très, assez, pas très, pas du tout).

Famille `attachment` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom          | Étiquette              |
|------|--------------|------------------------|
| 1    | `very`       | Très attaché(e)        |
| 2    | `fairly`     | Assez attaché(e)       |
| 3    | `not_very`   | Pas très attaché(e)    |
| 4    | `not_at_all` | Pas du tout attaché(e) |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_can_attach` (cps) | `comparable` | Même énoncé et mêmes options que l’ancrage en anglais (sans le « And » initial) ; les options françaises se lisent « Assez attaché(e) » et « Peu attaché(e) » (ancrage : « Plutôt attaché(e) », « Pas très attaché(e) ») ; Web ; « je ne sais pas » est une option (code 5) ; posée pendant la campagne, l’ancrage après l’élection. | attach_4pt | very, fairly, not_very, not_at_all | Quel est votre degré d’attachement au Canada? |  | `cps_weight_general` | Offert explicitement |
| qes2018 | `q19` (post) | `comparable` | Même énoncé que l’ancrage ; l’option 2 se lit « Somewhat attached » en anglais (ancrage « Fairly attached ») et les options françaises se lisent « Assez attaché(e) » et « Peu attaché(e) » (ancrage « Plutôt attaché(e) », « Pas très attaché(e) »). Le fichier n’a pas d’étiquettes de valeur ; les codes proviennent du questionnaire avec valeurs programmées (367181). | attach_4pt | very, fairly, not_very, not_at_all | Et quel est votre degré d’attachement au Canada? |  | `pond` | Offert explicitement |
| qes2014 | `Q13` (post) | `comparable` | Même énoncé que l’ancrage ; les options françaises se lisent « Plutôt attaché(e) » et « Pas très attaché(e) ». | attach_4pt | very, fairly, not_very, not_at_all | Et quel est votre degré d’attachement au Canada? |  | `POND` | Offert explicitement |
| qes2012 | `q2` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | attach_4pt | very, fairly, not_very, not_at_all | Et quel est votre degré d’attachement au Canada? |  | `pond` | Offert explicitement |

#### `identity_qc_ca` : Identité québécoise ou canadienne

Comment la personne se définit, d’uniquement québécoise à uniquement
canadienne (la question en cinq points de Moreno) ; une autre définition
de soi est une réponse (niveau other).

Famille `national_identity` · type Catégorielle · moment Tout moment

**Niveaux**

| Code | Nom        | Étiquette                               |
|------|------------|-----------------------------------------|
| 1    | `qc_only`  | Uniquement québécois(e)                 |
| 2    | `qc_first` | D’abord québécois(e), puis canadien(ne) |
| 3    | `equal`    | Également québécois(e) et canadien(ne)  |
| 4    | `ca_first` | D’abord canadien(ne), puis québécois(e) |
| 5    | `ca_only`  | Uniquement canadien(ne)                 |
| 90   | `other`    | Autre                                   |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_identity_qc_ca` (cps) | `comparable` | L’énoncé français de l’ancrage, Web, avec « Je ne sais pas » comme option toujours présentée en dernier ; les cinq options ont été présentées d’uniquement canadien à uniquement québécois à 741 personnes et dans l’ordre inverse aux 780 autres (variables d’ordre d’affichage pes_identity_qc_ca_DO_1 à *DO_6), en une seule variable, si bien que l’effet d’ordre s’annule en moyenne comme en 2014, 2007 et 2008. Malgré son préfixe pes*, la question fait partie du sondage de campagne : toutes les personnes y ont répondu, celles de la vague postélectorale comme les 301 qui n’y ont pas participé (livre de codes p. 52, dans la section de la campagne). | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only; non offerts : other | Les gens ont différentes façons de se définir. Diriez-vous que vous vous considérez \_\_\_\_\_\_\_\_\_\_ |  | `cps_weight_general` | Offert explicitement |
| qes2014 | `Q14A` + `Q14B` (post) | `comparable` | La question de l’ancrage en anglais et en français, posée en échantillon divisé : la moitié de l’échantillon (Q14A, SEL1 = 1) a vu les options d’uniquement québécois à uniquement canadien, l’autre moitié (Q14B) dans l’ordre inverse ; les deux moitiés sont combinées, ce qui fait la moyenne de l’effet d’ordre. | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only; non offerts : other | Les gens ont différentes façons de se définir. Diriez-vous que vous vous considérez…? (options dans un ordre pour la moitié de l’échantillon, Q14A, dans l’ordre inverse pour l’autre moitié, Q14B) |  | `POND` | Offert explicitement |
| qes2012 | `q3` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only; non offerts : other | Les gens ont différentes façons de se définir. Diriez-vous que vous vous considérez…? |  | `pond` | Offert explicitement |
| qes2008 | `q18a` + `q18b` (post) | `comparable` | L’énoncé français de l’ancrage, les cinq options lues dans un ordre pour la moitié de l’échantillon (q18a) et dans l’ordre inverse pour l’autre moitié (q18b), combinées ; une autre définition de soi (96, « autres (précisez) ») figure comme option dans les deux questionnaires, sans consigne de lecture ; par téléphone (selon les métadonnées du dépôt). | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only, other | Les gens ont différentes façons de se définir. Diriez-vous que vous vous considérez…? |  |  | Non documenté |
| qes2007 | `q18a` + `q18b` (post) | `comparable` | L’énoncé français de l’ancrage, les cinq options lues dans un ordre pour la moitié de l’échantillon (q18a) et dans l’ordre inverse pour l’autre moitié (q18b), combinées ; une autre définition de soi (96) est spontanée ; téléphone et Web. | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only, other | Les gens ont différentes façons de se définir. Diriez-vous que vous vous considérez…? |  | `pond` | Non documenté |

#### `therm_leader_plq` : Évaluation du chef du PLQ (0-100)

À quel point la personne aime la personne à la tête du PLQ, de 0 (n’aime
vraiment pas du tout) à 100 (aime vraiment beaucoup) ; la personne
évaluée change par construction et chaque ligne la nomme. Ne pas
connaître la personne est « ne sait pas » (dk).

Famille `leader_ratings` · type Numérique · moment Tout moment

**Plage valide** : 0-100

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_leadertherm_1` (cps) | `approximate` | Un curseur de 0 à 100 pour chaque chef provincial, Web, posée pendant la campagne ; -99 est l’option explicite « Je ne connais pas ce chef(fe) » (livre de codes 7449514, p. 26) et, selon la note sous cps_intelligent, peut aussi inclure un curseur laissé vide (le fichier n’a aucune valeur manquante système) ; il est lu comme « ne sait pas ». | therm_0_100 |  | Sur la même échelle, que pensez-vous des chef(fe)s de partis politiques provinciaux énumérés ci-dessous? \[Dominique Anglade\] |  | `cps_weight_general` | Offert explicitement |
| qes2018 | `q33_a` (post) | `approximate` | Une échelle de 0 à 10 multipliée par 10 (affine 10\*x) : le même concept sur une échelle plus grossière ; ne pas connaître la personne (97) est « ne sait pas » ; Web. | therm_0_10 |  | Sur une échelle de 0 à 10, où 0 veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un politicien, et 10 veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP, que pensez-vous de… Philippe Couillard? |  | `pond` | Offert explicitement |
| qes2014 | `Q29A` (post) | `comparable` | Même énoncé et même échelle de 0 à 100 que l’ancrage, Web ; ne pas connaître la personne (997) est « ne sait pas ». | therm_0_100 |  | Sur une échelle de ZERO à CENT, où zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un politicien, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP, que pensez-vous de… |  | `POND` | Offert explicitement |
| qes2012 | `q68` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | therm_0_100 |  | Sur une échelle de ZERO à CENT, où zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un politicien, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP, que pensez-vous de JEAN CHAREST? |  | `pond` | Offert explicitement |
| qes2008 | `q39` (post) | `comparable` | Même échelle de 0 à 100 ; téléphone selon les métadonnées du dépôt (à confirmer) ; ne connaître aucun chef (995) ou celui-ci (997) est « ne sait pas ». Le libellé français est celui de l’ancrage ; le questionnaire anglais déposé demande seulement « How do you feel about JEAN CHAREST? » sans définir 0 et 100 (l’anglais de l’ancrage les définit). | therm_0_100 |  | Maintenant nous parlons des chefs de partis. En utilisant une échelle de zéro à cent. zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un chef, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP. Aimez-vous JEAN CHAREST ? |  |  | Non documenté |
| qes2007 | `q39` (post) | `comparable` | Même échelle de 0 à 100 ; l’étude combine des entrevues téléphoniques et Web ; ne connaître ni ce chef ni aucun chef (995, 997) est « ne sait pas ». | therm_0_100 |  | Maintenant nous parlons des chefs de partis. En utilisant une échelle de zéro à cent. zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un chef, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP. Aimez-vous JEAN CHAREST ? |  | `pond` | Non documenté |

#### `therm_leader_pq` : Évaluation du chef du PQ (0-100)

À quel point la personne aime la personne à la tête du PQ, de 0 (n’aime
vraiment pas du tout) à 100 (aime vraiment beaucoup) ; la personne
évaluée change par construction et chaque ligne la nomme. Ne pas
connaître la personne est « ne sait pas » (dk).

Famille `leader_ratings` · type Numérique · moment Tout moment

**Plage valide** : 0-100

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_leadertherm_2` (cps) | `approximate` | Un curseur de 0 à 100 pour chaque chef provincial, Web, posée pendant la campagne ; -99 est l’option explicite « Je ne connais pas ce chef(fe) » (livre de codes 7449514, p. 26) et, selon la note sous cps_intelligent, peut aussi inclure un curseur laissé vide (le fichier n’a aucune valeur manquante système) ; il est lu comme « ne sait pas ». | therm_0_100 |  | Sur la même échelle, que pensez-vous des chef(fe)s de partis politiques provinciaux énumérés ci-dessous? \[Paul St-Pierre Plamondon\] |  | `cps_weight_general` | Offert explicitement |
| qes2018 | `q33_b` (post) | `approximate` | Une échelle de 0 à 10 multipliée par 10 (affine 10\*x) : le même concept sur une échelle plus grossière ; ne pas connaître la personne (97) est « ne sait pas » ; Web. | therm_0_10 |  | Sur une échelle de 0 à 10, où 0 veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un politicien, et 10 veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP, que pensez-vous de… Jean-François Lisée? |  | `pond` | Offert explicitement |
| qes2014 | `Q29B` (post) | `comparable` | Même énoncé et même échelle de 0 à 100 que l’ancrage, Web ; ne pas connaître la personne (997) est « ne sait pas ». | therm_0_100 |  | Sur une échelle de ZERO à CENT, où zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un politicien, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP, que pensez-vous de… PAULINE MAROIS? |  | `POND` | Offert explicitement |
| qes2012 | `q68b` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | therm_0_100 |  | Sur la même échelle, que pensez-vous de PAULINE MAROIS? |  | `pond` | Offert explicitement |
| qes2008 | `q40` (post) | `comparable` | Même échelle de 0 à 100 ; téléphone selon les métadonnées du dépôt (à confirmer) ; ne connaître aucun chef (995) ou celui-ci (997) est « ne sait pas ». Le libellé français est celui de l’ancrage ; le questionnaire anglais déposé demande seulement « How do you feel about PAULINE MAROIS? » sans définir 0 et 100 (l’anglais de l’ancrage les définit). | therm_0_100 |  | Maintenant nous parlons des chefs de partis. En utilisant une échelle de zéro à cent. zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un chef, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP. Aimez-vous PAULINE MAROIS ? |  |  | Non documenté |
| qes2007 | `q40` (post) | `comparable` | Même échelle de 0 à 100 ; l’étude combine des entrevues téléphoniques et Web ; ne connaître ni ce chef ni aucun chef (995, 997) est « ne sait pas ». | therm_0_100 |  | Maintenant nous parlons des chefs de partis. En utilisant une échelle de zéro à cent. zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un chef, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP. Aimez-vous ANDRÉ BOISCLAIR ? |  | `pond` | Non documenté |

#### `therm_leader_caq` : Évaluation du chef du CAQ (0-100)

À quel point la personne aime la personne à la tête du CAQ, de 0 (n’aime
vraiment pas du tout) à 100 (aime vraiment beaucoup) ; la personne
évaluée change par construction et chaque ligne la nomme. Ne pas
connaître la personne est « ne sait pas » (dk).

Famille `leader_ratings` · type Numérique · moment Tout moment

**Plage valide** : 0-100

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_leadertherm_3` (cps) | `approximate` | Un curseur de 0 à 100 pour chaque chef provincial, Web, posée pendant la campagne ; -99 est l’option explicite « Je ne connais pas ce chef(fe) » (livre de codes 7449514, p. 26) et, selon la note sous cps_intelligent, peut aussi inclure un curseur laissé vide (le fichier n’a aucune valeur manquante système) ; il est lu comme « ne sait pas ». | therm_0_100 |  | Sur la même échelle, que pensez-vous des chef(fe)s de partis politiques provinciaux énumérés ci-dessous? \[François Legault\] |  | `cps_weight_general` | Offert explicitement |
| qes2018 | `q33_c` (post) | `approximate` | Une échelle de 0 à 10 multipliée par 10 (affine 10\*x) : le même concept sur une échelle plus grossière ; ne pas connaître la personne (97) est « ne sait pas » ; Web. | therm_0_10 |  | Sur une échelle de 0 à 10, où 0 veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un politicien, et 10 veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP, que pensez-vous de… François Legault? |  | `pond` | Offert explicitement |
| qes2014 | `Q29C` (post) | `comparable` | Même énoncé et même échelle de 0 à 100 que l’ancrage, Web ; ne pas connaître la personne (997) est « ne sait pas ». | therm_0_100 |  | Sur une échelle de ZERO à CENT, où zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un politicien, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP, que pensez-vous de… FRANÇOIS LEGAULT? |  | `POND` | Offert explicitement |
| qes2012 | `q68c` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | therm_0_100 |  | Sur la même échelle, que pensez-vous de FRANÇOIS LEGAULT? |  | `pond` | Offert explicitement |

#### `therm_leader_qs` : Évaluation du chef du QS (0-100)

À quel point la personne aime la personne à la tête du QS, de 0 (n’aime
vraiment pas du tout) à 100 (aime vraiment beaucoup) ; la personne
évaluée change par construction et chaque ligne la nomme (QS a deux
porte-parole : celle ou celui que l’étude a fait évaluer, ou sa
candidate ou son candidat au poste de première ministre quand l’étude a
fait évaluer les deux). Ne pas connaître la personne est « ne sait pas »
(dk).

Famille `leader_ratings` · type Numérique · moment Tout moment

**Plage valide** : 0-100

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_leadertherm_7` (cps) | `approximate` | Un curseur de 0 à 100 pour chaque chef provincial, Web, posée pendant la campagne ; -99 est l’option explicite « Je ne connais pas ce chef(fe) » (livre de codes 7449514, p. 26) et, selon la note sous cps_intelligent, peut aussi inclure un curseur laissé vide (le fichier n’a aucune valeur manquante système) ; il est lu comme « ne sait pas ». | therm_0_100 |  | Sur la même échelle, que pensez-vous des chef(fe)s de partis politiques provinciaux énumérés ci-dessous? \[Gabriel Nadeau-Dubois\] |  | `cps_weight_general` | Offert explicitement |
| qes2018 | `q33_d` (post) | `approximate` | Une échelle de 0 à 10 multipliée par 10 (affine 10\*x) : le même concept sur une échelle plus grossière ; ne pas connaître la personne (97) est « ne sait pas » ; Web. | therm_0_10 |  | Sur une échelle de 0 à 10, où 0 veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un politicien, et 10 veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP, que pensez-vous de… Manon Massé? |  | `pond` | Offert explicitement |
| qes2014 | `Q29D` (post) | `comparable` | Même énoncé et même échelle de 0 à 100 que l’ancrage, Web ; ne pas connaître la personne (997) est « ne sait pas ». | therm_0_100 |  | Sur une échelle de ZERO à CENT, où zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un politicien, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP, que pensez-vous de… FRANÇOISE DAVID? |  | `POND` | Offert explicitement |
| qes2012 | `q68d` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | therm_0_100 |  | Sur la même échelle, que pensez-vous de AMIR KHADIR? |  | `pond` | Offert explicitement |
| qes2008 | `q42` (post) | `comparable` | Même échelle de 0 à 100 ; téléphone selon les métadonnées du dépôt (à confirmer) ; ne connaître aucun chef (995) ou celui-ci (997) est « ne sait pas ». Le libellé français est celui de l’ancrage ; le questionnaire anglais déposé demande seulement « How do you feel about FRANÇOISE DAVID? » sans définir 0 et 100 (l’anglais de l’ancrage les définit). | therm_0_100 |  | Maintenant nous parlons des chefs de partis. En utilisant une échelle de zéro à cent. zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un chef, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP. Aimez-vous FRANÇOISE DAVID ? |  |  | Non documenté |
| qes2007 | `q42` (post) | `comparable` | Même échelle de 0 à 100 ; l’étude combine des entrevues téléphoniques et Web ; ne connaître ni ce chef ni aucun chef (995, 997) est « ne sait pas ». | therm_0_100 |  | Maintenant nous parlons des chefs de partis. En utilisant une échelle de zéro à cent. zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un chef, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP. Aimez-vous FRANÇOISE DAVID ? |  | `pond` | Non documenté |

#### `therm_leader_adq` : Évaluation du chef du ADQ (0-100)

À quel point la personne aime la personne à la tête du ADQ, de 0 (n’aime
vraiment pas du tout) à 100 (aime vraiment beaucoup) ; la personne
évaluée change par construction et chaque ligne la nomme. Ne pas
connaître la personne est « ne sait pas » (dk).

Famille `leader_ratings` · type Numérique · moment Tout moment

**Plage valide** : 0-100

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q41` (post) | `comparable` | Même échelle de 0 à 100 ; téléphone selon les métadonnées du dépôt (à confirmer) ; ne connaître aucun chef (995) ou celui-ci (997) est « ne sait pas ». Le libellé français est celui de l’ancrage ; le questionnaire anglais déposé demande seulement « How do you feel about MARIO DUMONT? » sans définir 0 et 100 (l’anglais de l’ancrage les définit). | therm_0_100 |  | Maintenant nous parlons des chefs de partis. En utilisant une échelle de zéro à cent. zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un chef, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP. Aimez-vous MARIO DUMONT ? |  |  | Non documenté |
| qes2007 | `q41` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | therm_0_100 |  | Maintenant nous parlons des chefs de partis. En utilisant une échelle de zéro à cent. zéro veut dire que vous N’AIMEZ VRAIMENT PAS DU TOUT un chef, et cent veut dire que vous L’AIMEZ VRAIMENT BEAUCOUP. Aimez-vous MARIO DUMONT ? |  | `pond` | Non documenté |

### Enjeux

#### `mip_issue` : Enjeu le plus important de l’élection

Quel enjeu, dans la liste fermée de l’étude, était le plus important
pour la personne à l’élection générale québécoise de l’étude. Les listes
changent d’une élection à l’autre : un enjeu qu’une étude n’a pas
proposé y est un zéro structurel (levels_not_offered), pas une absence
de préoccupation.

Famille `issues` · type Catégorielle · moment Tout moment

**Niveaux**

| Code | Nom               | Étiquette                           |
|------|-------------------|-------------------------------------|
| 1    | `economy`         | L’économie                          |
| 2    | `health`          | La santé                            |
| 3    | `environment`     | L’environnement                     |
| 4    | `education`       | L’éducation                         |
| 5    | `families`        | L’aide aux familles                 |
| 6    | `poverty`         | La pauvreté                         |
| 7    | `integrity`       | L’intégrité et la corruption        |
| 8    | `taxes_finances`  | Les taxes et les finances publiques |
| 9    | `sovereignty`     | La souveraineté du Québec           |
| 10   | `secularism`      | La laïcité de l’État (la charte)    |
| 11   | `immigration`     | L’immigration                       |
| 12   | `cost_of_living`  | Le coût de la vie                   |
| 13   | `housing`         | Le logement                         |
| 14   | `french_language` | La langue française                 |
| 90   | `other`           | Un autre enjeu                      |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_impissue_matrix` (cps) | `comparable` | La même question (« pour vous personnellement, le plus important ») sur l’élection de l’étude, avec la liste fermée propre à l’étude : les enjeux qu’elle ne proposait pas sont des zéros structurels. Quatorze enjeux, posée pendant la campagne, Web, sans option « je ne sais pas » ; le troisième lien de Québec et la violence par armes à feu sont d’autres enjeux. | mip_closed_list | economy, health, environment, education, poverty, integrity, taxes_finances, sovereignty, immigration, cost_of_living, housing, french_language, other; non offerts : families, secularism | Quel est l’enjeu le plus important, pour vous personnellement, dans cette élection provinciale? |  | `cps_weight_general` | Non offert |
| qes2018 | `q2` (post) | `comparable` | La même question (« pour vous personnellement, le plus important ») sur l’élection de l’étude, avec la liste fermée propre à l’étude : les enjeux qu’elle ne proposait pas sont des zéros structurels. Dix enjeux et un autre enjeu ; l’intégrité se lit « des politiciens et la corruption ». | mip_closed_list | economy, health, environment, education, families, poverty, integrity, taxes_finances, sovereignty, immigration, other; non offerts : secularism, cost_of_living, housing, french_language | Parmi les enjeux suivants, lequel était, pour vous personnellement, le plus important lors de l’élection provinciale du 1er octobre dernier? |  | `pond` | Offert explicitement |
| qes2014 | `Q1` (post) | `comparable` | La même question (« pour vous personnellement, le plus important ») sur l’élection de l’étude, avec la liste fermée propre à l’étude : les enjeux qu’elle ne proposait pas sont des zéros structurels. Dix enjeux, dont la charte de la laïcité (la Charte des valeurs du PQ). | mip_closed_list | economy, health, environment, education, families, poverty, integrity, taxes_finances, sovereignty, secularism; non offerts : immigration, cost_of_living, housing, french_language, other | Parmi les enjeux suivants, lequel était, pour vous personnellement, le plus important lors de l’élection provinciale du 7 avril dernier? |  | `POND` | Offert explicitement |
| qes2012 | `q34bb` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible : huit enjeux, sans option « autre enjeu ». | mip_closed_list | economy, health, environment, education, families, poverty, integrity, sovereignty; non offerts : taxes_finances, secularism, immigration, cost_of_living, housing, french_language, other | Parmi les enjeux suivants, lequel était, pour vous personnellement, le plus important lors de l’élection provinciale du 4 septembre dernier? |  | `pond` | Offert explicitement |
| qes2008 | `q1` (post) | `comparable` | La même question (« pour vous personnellement, le plus important ») sur l’élection de l’étude, avec la liste fermée propre à l’étude : les enjeux qu’elle ne proposait pas sont des zéros structurels. Six enjeux et un autre enjeu, par téléphone. | mip_closed_list | economy, health, environment, education, families, poverty, other; non offerts : integrity, taxes_finances, sovereignty, secularism, immigration, cost_of_living, housing, french_language | Parmi les enjeux suivants, lequel était, pour vous personnellement, le plus important lors de l’élection provinciale du 8 décembre dernier ? |  |  | Non offert |

### Sociodémographie

#### `birth_year` : Année de naissance

Année de naissance de la personne.

Famille `birth` · type Numérique · moment Invariant

**Plage valide** : 1900-2010

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_yob` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | yob_list |  | Enfin, en quelle année êtes-vous né? |  | `cps_weight_general` | Non offert |
| qes2018 | `ageyear_1` (post) | `comparable` | Même question (année de naissance) ; l’année et le mois sont saisis, avec une option pour ne pas répondre (l’âge est alors demandé) ; l’ancrage propose une liste d’années sans cette option. | yob_month_entry |  | En quelle année êtes-vous né(e)? | agensp: 1 = refused | `pond` | Offert explicitement |
| qes2014 | `QAGE` (post) | `comparable` | Même question (année de naissance) ; l’année est saisie dans une case, avec une option pour ne pas répondre ; l’ancrage propose une liste d’années sans cette option. | yob_entry |  | En quelle année êtes-vous né(e)? |  | `POND` | Offert explicitement |
| qes2012 | `agex` (post) | `comparable` | Même question (année de naissance) ; l’année est saisie dans une case, avec une option pour ne pas répondre ; l’ancrage propose une liste d’années sans cette option. | yob_entry |  | En quelle année êtes-vous né(e)? |  | `pond` | Offert explicitement |
| qes2008 | `q75` (post) | `comparable` | Même question (année de naissance) ; l’année est saisie, avec une option pour ne pas répondre ; l’ancrage propose une liste d’années sans cette option. | yob_entry |  | Pour terminer l’entrevue, nous aimerions avoir quelques informations qui nous aideront à vérifier si notre échantillon représente bien l’ensemble de la population québécoise. D’abord, en quelle année êtes-vous né(e) ? (exemple:1972) Je préfère ne pas répondre 9999 |  |  | Non documenté |
| qes2007 | `q75` (post) | `comparable` | Même question (année de naissance) ; l’année est saisie, avec un code de refus ; l’ancrage propose une liste d’années sans ce code. | yob_entry |  | Pour terminer l’entrevue, nous aimerions avoir quelques informations qui nous aideront à vérifier si notre échantillon représente bien l’ensemble de la population québécoise. D’abord, en quelle année êtes-vous né(e) ? Notez l’année de naissance refus 9999 |  | `pond` | Non documenté |

#### `birth_month` : Mois de naissance

Mois de naissance de la personne (1 = janvier). Demandé avec l’année de
naissance dans certaines études ; avec birth_year, il indique si une
personne née 18 ans avant l’année de l’élection avait 18 ans le jour du
scrutin.

Famille `birth` · type Numérique · moment Invariant

**Plage valide** : 1-12

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `agemonth_1` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | yob_month_entry |  | En quelle année êtes-vous né(e)? | agensp: 1 = refused | `pond` | Offert explicitement |

#### `age` : Âge en années

Âge de la personne en années au moment de l’entrevue, tel que demandé.
Jamais calculé à partir de l’année de naissance, qui ne donne l’âge qu’à
un an près.

Famille `age_years` · type Numérique · moment Tout moment

**Plage valide** : 15-115

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_age_in_years` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | age_list |  | Afin d’être certains que nous nous adressons à un échantillon représentatif des Canadiens, nous avons besoin d’informations de base sur vous. Tout d’abord, quel âge avez-vous? |  | `cps_weight_general` | Non offert |
| qes2018 | `agenum` (post) | `approximate` | La même question (Quel âge avez-vous?, une liste d’âges) posée seulement aux personnes qui n’ont pas donné leur année et leur mois de naissance ; l’ancrage la pose à tout le monde. | age_list |  | Quel âge avez-vous? | agensp: 0 = inapplicable | `pond` | Offert explicitement |

#### `age_group3` : Groupe d’âge (3 tranches)

Groupe d’âge de la personne au moment de l’entrevue, en trois tranches :
18-34, 35-54, 55 et plus. Construit à partir d’une question offrant ces
tranches ou des tranches qui s’y regroupent exactement ; là où l’étude
n’a pas une telle question, dérivé de l’âge (exact) ou de l’année de
naissance (niveau approximate : un âge à la limite d’une tranche peut
être décalé d’un an).

Famille `age_bands` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom        | Étiquette  |
|------|------------|------------|
| 1    | `a18_34`   | 18-34      |
| 2    | `a35_54`   | 35-54      |
| 3    | `a55_plus` | 55 et plus |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `age` (pre) | `identical` (ancrage) | Ligne d’ancrage de la cible. | age_3bands | a18_34, a35_54, a55_plus | document 341538, age |  | `weight` | Non documenté |
| qes2012_panel | `age` (pre) | `comparable` | Six tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64 et 65 et plus), par téléphone ; l’ancrage offre les trois tranches, par téléphone et en ligne. | age_6bands | a18_34, a35_54, a55_plus | document 654292, age |  | `pondam1` (pas encore utilisable) | Non documenté |
| qes_crop_2007_2010 | `QAGE` (chaque sondage) | `comparable` | Six tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64 et 65 et plus), par téléphone ; l’ancrage offre les trois tranches, par téléphone et en ligne. | age_6bands | a18_34, a35_54, a55_plus | Auquel des groupes d’àges suivants appartenez-vous? |  | `XPOND` (pas encore utilisable) | Non documenté |
| qes2008 | `q0age` (post) | `comparable` | Sept tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64, 65-74 et 75 et plus) ; l’ancrage offre les trois tranches. | age_7bands | a18_34, a35_54, a55_plus | Quel âge avez-vous ? |  |  | Non documenté |
| qes2007_panel | `age` (toute vague) | `comparable` | Six tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64 et 65 et plus), par téléphone ; l’ancrage offre les trois tranches, par téléphone et en ligne. | age_6bands | a18_34, a35_54, a55_plus | Auquel des groupes d’âges suivants appartenez-vous? |  |  | Non documenté |
| qes1998 | `age` (pre) | `comparable` | Six tranches d’âge, regroupées exactement dans les trois de la cible (18-24 et 25-34, 35-44 et 45-54, 55-64 et 65 et plus), par téléphone ; l’ancrage offre les trois tranches, par téléphone et en ligne. | age_6bands | a18_34, a35_54, a55_plus | A quel groupe d’age appartenez-vous? (LIRE SI NECESSAIRE) |  | `ponder3` (pas encore utilisable) | Non documenté |

#### `citizen` : Citoyenneté canadienne

Si la personne a la citoyenneté canadienne, là où l’étude l’a demandé.
Seuls les citoyens peuvent voter aux élections québécoises.

Famille `citizenship` · type Catégorielle · moment Tout moment

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_citizen` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | citizen_status | yes, no | Êtes-vous… |  | `cps_weight_general` | Non offert |

#### `age_group6` : Groupe d’âge (6 tranches)

Groupe d’âge de la personne à l’entrevue, en six tranches : 18-24,
25-34, 35-44, 45-54, 55-64, 65 et plus. Construit à partir d’une
question offrant ces tranches ou des tranches qui s’y regroupent
exactement ; là où l’étude n’a pas une telle question, dérivé de l’âge
(exact) ou de l’année de naissance (niveau approximate : un âge à la
limite d’une tranche peut être décalé d’un an).

Famille `age_bands` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom        | Étiquette  |
|------|------------|------------|
| 1    | `a18_24`   | 18-24      |
| 2    | `a25_34`   | 25-34      |
| 3    | `a35_44`   | 35-44      |
| 4    | `a45_54`   | 45-54      |
| 5    | `a55_64`   | 55-64      |
| 6    | `a65_plus` | 65 et plus |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2012_panel | `age` (pre) | `comparable` | Les six tranches de l’ancrage, par téléphone ; les livres de codes ne disent pas si « ne sait pas » était offert. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | document 654292, age |  | `pondam1` (pas encore utilisable) | Non documenté |
| qes_crop_2007_2010 | `QAGE` (chaque sondage) | `identical` (ancrage) | Ligne d’ancrage de la cible. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Auquel des groupes d’àges suivants appartenez-vous? |  | `XPOND` (pas encore utilisable) | Non documenté |
| qes2008 | `q0age` (post) | `comparable` | Sept tranches d’âge, regroupées exactement dans les six de la cible (65-74 et 75 et plus en 65 et plus) ; l’ancrage offre les six tranches, par téléphone. | age_7bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Quel âge avez-vous ? |  |  | Non documenté |
| qes2007_panel | `age` (toute vague) | `comparable` | Les six tranches de l’ancrage, par téléphone ; les livres de codes ne disent pas si « ne sait pas » était offert. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Auquel des groupes d’âges suivants appartenez-vous? |  |  | Non documenté |
| qes1998 | `age` (pre) | `comparable` | Les six tranches de l’ancrage, par téléphone, posées par les deux firmes ; ni le livre de codes de l’ancrage ni celui-ci ne disent si « ne sait pas » était offert. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | A quel groupe d’age appartenez-vous? (LIRE SI NECESSAIRE) |  | `ponder3` (pas encore utilisable) | Non documenté |

#### `gender` : Genre

Genre de la personne, tel que demandé ou, dans certains sondages
téléphoniques, tel que noté par l’intervieweur. La plupart des études
n’offraient que homme et femme (une question sur le sexe, dans
certaines) ; qes2022 offrait aussi non binaire et un autre genre.

Famille `sex_gender` · type Catégorielle · moment Invariant

**Niveaux**

| Code | Nom         | Étiquette      |
|------|-------------|----------------|
| 1    | `man`       | Homme          |
| 2    | `woman`     | Femme          |
| 3    | `nonbinary` | Non binaire    |
| 4    | `other`     | Un autre genre |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_genderid` (cps) | `comparable` | Question sur l’identité de genre sur le Web qui ajoute non binaire et un autre genre aux deux options de l’ancrage ; ne devrait guère changer les parts d’hommes et de femmes. | gender_4 | man, woman, nonbinary, other | Êtes-vous… |  | `cps_weight_general` | Non offert |
| qes2018 | `qsexe` (post) | `comparable` | Mêmes deux options sur le Web et mêmes énoncés que l’ancrage (genre en anglais, sexe en français) ; le questionnaire français avec valeurs programmées ajoute la note de Statistique Canada invitant les personnes transgenres, transsexuelles et intersexuées à choisir le sexe auquel elles s’identifient le plus ; le fichier n’a pas d’étiquettes de valeur. | gender_2 | man, woman; non offerts : nonbinary, other | Quel est votre sexe? |  | `pond` | Non offert |
| qes2018_panel | `sexfix` (pre) | `comparable` | Mêmes deux options, sur le Web (850) et par téléphone (400) ; le livre de codes ne donne que l’étiquette Sexe, de sorte qu’on ignore si elle a été demandée ou notée. | gender_2 | man, woman; non offerts : nonbinary, other | Sexe: |  | `weight` | Non documenté |
| qes2014 | `QSEXE` (post) | `comparable` | Mêmes deux options sur le Web et mêmes énoncés que l’ancrage en anglais (What is your gender?) et en français (Quel est votre sexe?), plus une option de non-réponse absente du fichier ; gardée au niveau comparable, celui qu’elle avait avant la révision : un niveau identique pour une ligne qui réunit les langues de passation n’est accordé qu’après une révision par une personne, que cette ligne n’a pas encore eue. | gender_2 | man, woman; non offerts : nonbinary, other | Quel est votre sexe? |  | `POND` | Non offert |
| qes2012 | `sexe` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | gender_2 | man, woman; non offerts : nonbinary, other | Quel est votre sexe? |  | `pond` | Non offert |
| qes2012_panel | `sexe` (pre) | `comparable` | Mêmes deux options, par téléphone ; le livre de codes ne donne que le nom de la variable, de sorte qu’on ignore si elle a été demandée ou notée. | sex_recorded | man, woman; non offerts : nonbinary, other | document 654292, sexe |  | `pondam1` (pas encore utilisable) | Non documenté |
| qes_crop_2007_2010 | `SEXE` (chaque sondage) | `comparable` | Mêmes deux options, noté par l’intervieweur (non demandé), par téléphone. | sex_recorded | man, woman; non offerts : nonbinary, other | INSCRIRE LE SEXE DU REPONDANT |  | `XPOND` (pas encore utilisable) | Non offert |
| qes2008 | `q76` (post) | `comparable` | Demandé (Un homme / Une femme) avec les mêmes deux options ; le mode de l’étude reste à confirmer (téléphone selon les métadonnées du dépôt), l’ancrage est Web. | gender_2 | man, woman; non offerts : nonbinary, other | Êtes-vous… |  |  | Non offert |
| qes2007 | `q76` (post) | `comparable` | Noté par l’intervieweur au téléphone sans le demander (NE PAS LIRE), et sur le Web pour les personnes jointes en ligne (seul le questionnaire téléphonique est déposé) ; mêmes deux options. | sex_recorded | man, woman; non offerts : nonbinary, other | (NE PAS LIRE) Indiquez le sexe du répondant: |  | `pond` | Non offert |
| qes2007_panel | `sexe` (toute vague) | `comparable` | Mêmes deux options, noté par l’intervieweur (non demandé), par téléphone. | sex_recorded | man, woman; non offerts : nonbinary, other | INSCRIRE LE SEXE DU RÉPONDANT |  |  | Non offert |
| qes1998 | `sexe_post` (pre) | `comparable` | Mêmes deux options, par téléphone ; les livres de codes ne donnent que l’étiquette SEXE (Sexe du répondant dans celui de CROP), de sorte qu’on ignore si elle a été demandée ou notée par l’intervieweur. | sex_recorded | man, woman; non offerts : nonbinary, other | SEXE |  | `ponder3` (pas encore utilisable) | Non offert |

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

Famille `education` · type Ordinale · moment Invariant

**Niveaux**

| Code | Nom          | Étiquette                    |
|------|--------------|------------------------------|
| 1    | `primary`    | Primaire ou moins            |
| 2    | `secondary`  | Secondaire                   |
| 3    | `college`    | Collégial (cégep, technique) |
| 4    | `university` | Universitaire                |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_edu` (cps) | `comparable` | Plus haut niveau atteint sur le Web, avec des options qui se regroupent exactement dans les quatre groupes (des études universitaires non terminées comptent comme universitaires, comme dans l’ancrage) ; pas d’option « ne sait pas ». | edu_levels | primary, secondary, college, university | Quel est votre plus haut niveau de scolarité complété? |  | `cps_weight_general` | Non offert |
| qes2018 | `qscol` (post) | `comparable` | Même énoncé que l’ancrage sur le Web, avec des options plus fines (chaque année du secondaire, le DEP, les programmes du cégep) qui se regroupent exactement dans les quatre groupes ; le fichier n’a pas d’étiquettes de valeur. Le diplôme d’études professionnelles (DEP, code 9) est secondaire ici, alors que le certificat d’école de métiers de qes2018_panel (d3 code 4) est collégial, et les questionnaires sans option DEP (l’ancrage qes2014, qes2012, qes2007, qes2008) classent probablement les titulaires d’un DEP en cours technique (collégial) : un diplôme professionnel ou de métier ne tombe pas dans le même groupe dans toutes les études. | edu_levels | primary, secondary, college, university | À quel niveau se situe la dernière année de scolarité que vous avez complétée? |  | `pond` | Offert explicitement |
| qes2018_panel | `d3` (pre) | `approximate` | Plus haut niveau atteint selon les catégories de Statistique Canada, par téléphone et sur le Web : un certificat d’école de métiers ou un apprentissage enregistré est une option, comptée comme collégiale (technique), et les certificats universitaires inférieurs au baccalauréat comme universitaires ; les étiquettes sont coupées à 60 caractères dans le fichier. Au Québec, cette option regroupe surtout le diplôme d’études professionnelles (DEP), que qes2018 (qscol code 9) compte comme secondaire : un diplôme professionnel ou de métier ne tombe pas dans le même groupe dans toutes les études. | edu_statcan | primary, secondary, college, university | Quel est le plus haut niveau de scolarité que vous avez atteint ? |  | `weight` | Non documenté |
| qes2014 | `QSCOL` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | edu_levels | primary, secondary, college, university | À quel niveau se situe la dernière année de scolarité que vous avez complétée? |  | `POND` | Non offert |
| qes2012 | `scol` (post) | `comparable` | Même question française que l’ancrage, sur le Web ; les options anglaises suivent un schéma britannique (further ou higher education), et un code du fichier ne figure dans aucun des deux questionnaires. | edu_levels | primary, secondary, college, university | À quel niveau se situe la dernière année de scolarité que vous avez complétée? |  | `pond` | Offert explicitement |
| qes_crop_2007_2010 | `scol` (chaque sondage) | `approximate` | Années d’études en quatre intervalles nommés d’après les niveaux (7 ou moins, primaire ; 8 à 12, secondaire ; 13 à 15, cégep ; 16 ou plus, université), par téléphone, et non le plus haut niveau atteint. | edu_years | primary, secondary, college, university | Combien d’années d’études avez-vous complétées? |  | `XPOND` (pas encore utilisable) | Non documenté |
| qes2008 | `q77` (post) | `comparable` | Niveau de scolarité avec et sans diplôme, options qui se regroupent exactement dans les quatre groupes ; le mode reste à confirmer, l’ancrage est Web. | edu_levels | primary, secondary, college, university | Quel est votre niveau d’éducation ? |  |  | Offert explicitement |
| qes2007 | `q77` (post) | `comparable` | Niveau de scolarité avec et sans diplôme, options qui se regroupent exactement dans les quatre groupes ; entrevues téléphoniques et Web, l’ancrage est Web. | edu_levels | primary, secondary, college, university | Quel est votre niveau d’éducation ? |  | `pond` | Spontané seulement |
| qes2007_panel | `scol` (toute vague) | `approximate` | Années d’études en quatre intervalles nommés d’après les niveaux (7 ou moins, primaire ; 8 à 12, secondaire ; 13 à 15, cégep ; 16 ou plus, université), par téléphone, et non le plus haut niveau atteint. | edu_years | primary, secondary, college, university | Combien d’années d’études avez-vous complétées? |  |  | Spontané seulement |

#### `lang_mother` : Langue maternelle

Première langue que la personne a apprise à la maison dans son enfance
et qu’elle comprend toujours : français, anglais ou autre langue. Une
personne qui déclare deux premières langues est une valeur manquante
(motif not_mappable), jamais attribuée à l’une d’elles.

Famille `language` · type Catégorielle · moment Invariant

**Niveaux**

| Code | Nom       | Étiquette |
|------|-----------|-----------|
| 1    | `french`  | Français  |
| 2    | `english` | Anglais   |
| 3    | `other`   | Autre     |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_lang_1` + `cps_lang_2` + `cps_lang_3` (cps) | `approximate` | Une question à choix multiples (les langues apprises en premier et encore comprises) : une personne qui a coché une seule option la reçoit ; deux ou plus (146) sont not_mappable, comme une seconde langue maternelle ailleurs ; Web, posée pendant la campagne. | lang_first_multiselect | french, english, other | Quelle est la/les première(s) langue(s) que vous avez apprise(s) et que vous comprenez encore? (Sélectionnez toutes celles qui s’ appliquent) |  | `cps_weight_general` | Non offert |
| qes2018 | `qlangue` (post) | `comparable` | Même énoncé sur le Web, qui demande la langue principale apprise en premier ; le fichier n’a pas d’étiquettes de valeur. | lang_first | french, english, other | Quelle est la langue principale que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `pond` | Offert explicitement |
| qes2018_panel | `s1` (pre) | `comparable` | Énoncé plus court (première langue apprise et encore comprise, sans « à la maison dans votre enfance »), par téléphone et sur le Web ; une seule langue. | lang_first | french, english, other | Quelle est la première langue que vous avez apprise et que vous comprenez toujours? |  | `weight` | Non documenté |
| qes2014 | `QLANG` (post) | `comparable` | Même énoncé sur le Web, avec trois options pour deux premières langues, qui sont ici des valeurs manquantes (not_mappable). | lang_first_multi | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `POND` | Offert explicitement |
| qes2012 | `langu` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | lang_first | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `pond` | Offert explicitement |
| qes2012_panel | `lmat` (pre) | `comparable` | Langue maternelle définie comme la première langue apprise et encore comprise, par téléphone ; une seule langue. | lang_first | french, english, other | Quelle est votre langue maternelle, c’est-à-dire celle que vous avez appris à parler en premier et que vous comprenez toujours? |  | `pondam1` (pas encore utilisable) | Non documenté |
| qes_crop_2007_2010 | `lmat` (chaque sondage) | `comparable` | Langue maternelle demandée sans définition, par téléphone ; une seule langue. | lang_mother | french, english, other | Quelle est votre langue maternelle? |  | `XPOND` (pas encore utilisable) | Non documenté |
| qes2008 | `langu` (post) | `comparable` | Même énoncé et mêmes options ; le mode reste à confirmer, l’ancrage est Web. | lang_first | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours ? |  |  | Offert explicitement |
| qes2007 | `langu` (post) | `comparable` | Même énoncé, avec des options pour deux premières langues, qui sont ici des valeurs manquantes (not_mappable) ; entrevues téléphoniques et Web, l’ancrage est Web. | lang_first_multi | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours ? |  | `pond` | Spontané seulement |
| qes2007_panel | `lmat` (toute vague) | `comparable` | Langue maternelle définie comme la première langue apprise et encore parlée, par téléphone ; une seule langue. | lang_first | french, english, other | Quelle est votre langue maternelle, c’est-à-dire la première langue que vous avez apprise et que vous pouvez encore parler? |  |  | Spontané seulement |

#### `born_canada` : Né(e) au Canada

Si la personne est née au Canada. À partir d’une question sur le lieu de
naissance (Québec, ailleurs au Canada, hors du Canada) là où l’étude
pose celle-là, regroupée exactement.

Famille `birthplace` · type Catégorielle · moment Invariant

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_borncda` (cps) | `identical` (ancrage) | Ligne d’ancrage de la cible. | born_canada | yes, no | Êtes-vous né(e) au Canada? |  | `cps_weight_general` | Non offert |
| qes2018 | `q69` (post) | `comparable` | Question sur le lieu de naissance (au Québec, ailleurs au Canada, hors du Canada), regroupée exactement en né ou non au Canada, sur le Web ; l’ancrage le demande directement. Le fichier n’a pas d’étiquettes de valeur. | birthplace3 | yes, no | Où êtes-vous né(e)? |  | `pond` | Offert explicitement |
| qes2014 | `Q65` (post) | `comparable` | Question sur le lieu de naissance (au Québec, ailleurs au Canada, hors du Canada), regroupée exactement en né ou non au Canada, sur le Web ; l’ancrage le demande directement. | birthplace3 | yes, no | Où êtes-vous né(e)? |  | `POND` | Offert explicitement |
| qes2012 | `q105` (post) | `comparable` | Question sur le lieu de naissance (au Québec, ailleurs au Canada, hors du Canada), regroupée exactement en né ou non au Canada, sur le Web ; l’ancrage le demande directement. | birthplace3 | yes, no | Où êtes-vous né(e)? |  | `pond` | Offert explicitement |

#### `income_native` : Revenu du ménage (tranches propres à chaque étude)

Revenu du ménage avant impôts de la personne, tel que chaque étude l’a
enregistré : le texte de la tranche propre à l’étude (ou le montant, là
où l’étude le demandait). Les tranches diffèrent d’une étude à l’autre,
de sorte que les valeurs ne sont pas comparables entre études ; « ne
sait pas » et les refus sont des valeurs manquantes.

Famille `income` · type Texte · moment Invariant

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_income` (cps) | `approximate` | Le montant en dollars, inscrit sur le Web, et non une tranche. | income_amount |  | Quel est le revenu total de votre ménage avant impôts en 2021? Cela doit inclure toutes les sources de revenus au millier de dollars près. |  | `cps_weight_general` | Non offert |
| qes2018_panel | `d5` (pre) | `approximate` | Sept tranches de moins de 20 000 \$ à 150 000 \$ et plus, par téléphone et sur le Web, et non les neuf de l’ancrage ; le libellé déposé de la question est coupé, de sorte qu’on ignore s’il s’agit du revenu avant impôts et pour quelle année. | income_7brackets |  | Laquelle des catégories suivantes décrit le mieux le revenu total de votre foyer, c’est-à-dire le total des revenus |  | `weight` | Non documenté |
| qes2014 | `Q57` (post) | `comparable` | Les mêmes neuf tranches que l’ancrage, pour l’année précédente, sur le Web ; pas d’option « ne sait pas ». | income_9brackets |  | Parmi les catégories suivantes, laquelle reflète le mieux le revenu total avant impôt de tous les membres de votre foyer pour l’année 2013? 3. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Était-ce: |  | `POND` | Non offert |
| qes2012 | `reven` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | income_9brackets |  | Et maintenant le revenu total de votre ménage avant impôts pour l’année 2011. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Était-ce: |  | `pond` | Offert explicitement |
| qes_crop_2007_2010 | `revenu` (chaque sondage) | `approximate` | Cinq tranches de 20 000 \$ jusqu’à 80 000 \$ et plus, par téléphone, et non les neuf de l’ancrage. | income_5brackets |  | Dans laquelle des catégories suivantes se situe le revenu |  | `XPOND` (pas encore utilisable) | Non documenté |
| qes2008 | `q78` (post) | `approximate` | Dix tranches, de moins de 20 000 \$ puis par pas de 10 000 \$ jusqu’à plus de 100 000 \$, et non les neuf de l’ancrage ; pour l’année précédant l’élection (2007). | income_10brackets |  | Quel était le revenu total de votre ménage avant impôts en 2007. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Est-ce…? |  |  | Offert explicitement |
| qes2007 | `q78` (post) | `approximate` | Dix tranches de 10 000 \$ jusqu’à plus de 100 000 \$, et non les neuf de l’ancrage ; pour l’année précédant l’élection. | income_10brackets |  | Et maintenant le revenu total de votre ménage avant impôts en 2006. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Est-ce…? |  | `pond` | Offert explicitement |
| qes2007_panel | `revenu` (toute vague) | `approximate` | Cinq tranches de 20 000 \$ jusqu’à 80 000 \$ et plus, par téléphone, et non les neuf de l’ancrage. | income_5brackets |  | Dans laquelle des catégories suivantes se situe le revenu annuel total, avant impôts et déductions, de tous les membres de votre foyer, en vous incluant? Est-ce… |  |  | Spontané seulement |

#### `religion` : Religion (catégories propres à chaque étude)

Religion de la personne, telle que chaque étude l’a enregistrée : le
texte de la catégorie propre à l’étude. Les catégories diffèrent d’une
étude à l’autre. Là où la question n’était posée qu’aux personnes qui
appartiennent à une religion, les autres sont des valeurs manquantes :
motif inapplicable pour celles qui ont dit n’appartenir à aucune,
refused pour celles qui n’ont pas voulu répondre à la question filtre,
qui sert de filtre à la ligne de correspondance.

Famille `faith` · type Texte · moment Invariant

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_religion` (cps) | `approximate` | Une seule question avec une longue liste de confessions et « aucune » (sans question filtre), sur le Web ; et non les six catégories de l’ancrage. | religion_list |  | Quelle est votre religion, si vous en avez une? |  | `cps_weight_general` | Non offert |
| qes2014 | `Q63` (post) | `comparable` | Même question filtre et mêmes catégories que l’ancrage, avec un énoncé légèrement plus long, sur le Web. | religion_list |  | À quelle religion appartenez-vous? | Q62: 2 = inapplicable, 9 = refused | `POND` | Non offert |
| qes2012 | `q103` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | religion_list |  | Quelle religion? | q102: 2 = inapplicable, 3 = refused | `pond` | Non offert |

#### `region_cma3` : Région (RMR de Montréal et de Québec)

Où vit la personne : la région métropolitaine de recensement (RMR) de
Montréal, celle de Québec ou le reste du Québec, d’après la région que
l’étude a enregistrée pour l’échantillonnage, les quotas ou la
pondération (pas une question en soi).

Famille `region` · type Catégorielle · moment Invariant

**Niveaux**

| Code | Nom          | Étiquette       |
|------|--------------|-----------------|
| 1    | `mtl_cma`    | RMR de Montréal |
| 2    | `quebec_cma` | RMR de Québec   |
| 3    | `rest`       | Reste du Québec |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `regio` (post) | `identical` | La région d’échantillonnage de l’étude : les trois mêmes territoires que l’ancrage. | region_cma_sample | mtl_cma, quebec_cma, rest | document 425914, regio |  | `pond` | Non offert |
| qes2018_panel | `region` (pre) | `approximate` | La région recodée par le producteur (île de Montréal, sa « Couronne », la « Région de Québec », reste du Québec) : la couronne et la région de Québec ne sont pas documentées comme les régions métropolitaines de recensement. | region_recoded | mtl_cma, quebec_cma, rest | document 333052, region |  | `weight` | Non offert |
| qes2014 | `REGIO` (post) | `identical` | La région d’échantillonnage de l’étude : les trois mêmes territoires que l’ancrage. | region_cma_sample | mtl_cma, quebec_cma, rest | document 425916, REGIO |  | `POND` | Non offert |
| qes2012 | `regio` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible : la variable de région dérivée par le producteur, REGIO (RMR de Montréal, RMR de Québec, autres régions ; étiquette de variable vide), qui correspond à la RÉGION parmi les variables de pondération de POND (rapport technique, fichier 196368 : SEXE, ÂGE, RÉGION, LANGUE). Pas une question : Q110 (17 régions administratives) est dans le fichier sous q0qc, mais REGIO découpe les régions à cheval sur les RMR ; ce n’est donc pas un simple recodage de q0qc. Q111 (code postal) n’est pas dans le fichier. | region_cma_sample | mtl_cma, quebec_cma, rest | document 425917, REGIO; document 196368, RÉGION |  | `pond` | Non offert |
| qes2012_panel | `reg` (pre) | `comparable` | La région d’échantillonnage du panel en quatre territoires (île de Montréal, reste de la RMR de Montréal, RMR de Québec, reste du Québec), les deux premiers réunis exactement. | region_cma_sample | mtl_cma, quebec_cma, rest | document 361043, reg |  | `pondam1` (pas encore utilisable) | Non offert |
| qes_crop_2007_2010 | `REG` (chaque sondage) | `comparable` | La région d’échantillonnage de CROP en quatre territoires (île de Montréal, reste de la RMR de Montréal, RMR de Québec, reste du Québec), les deux premiers réunis exactement. | region_cma_sample | mtl_cma, quebec_cma, rest | document 329990, REG |  | `XPOND` (pas encore utilisable) | Non offert |
| qes2008 | `regio` (post) | `comparable` | Calculée dans le questionnaire (CALCM/CALCQ/CALCA -\> REGIO) à partir des questions de filtrage sur la région administrative (Q0QC) et la ville (Q0QCA-Q0QCE), et utilisée pour les quotas : les trois mêmes territoires que l’ancrage, les parties RMR étant définies par les listes de villes du questionnaire. Niveau comparable plutôt qu’identical : le dépôt indique un mode téléphonique (non confirmé), contrairement à l’ancrage Web. | region_cma_sample | mtl_cma, quebec_cma, rest | Dans quelle région du Québec demeurez-vous? \[+ Dans quelle ville demeurez-vous?\] |  |  | Non offert |
| qes2007 | `nomx` (post) | `comparable` | Les 21 sous-groupes d’échantillonnage (régions administratives, celles qui bordent les deux régions métropolitaines étant divisées en leur partie RMR et le reste), réunis en trois territoires : RMR de Montréal = Montréal, Laval et les parties RMR de Lanaudière, des Laurentides et de la Montérégie ; RMR de Québec = Québec RMR et la partie RMR de Chaudière-Appalaches. | region_admin_cma | mtl_cma, quebec_cma, rest | document 425921, nomx |  | `pond` | Non offert |

#### `lang_home` : Langue parlée le plus souvent à la maison

La langue que la personne parle le plus souvent à la maison : français,
anglais ou une autre langue.

Famille `language` · type Catégorielle · moment Invariant

**Niveaux**

| Code | Nom       | Étiquette |
|------|-----------|-----------|
| 1    | `french`  | Français  |
| 2    | `english` | Anglais   |
| 3    | `other`   | Autre     |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_langhome_1` + `pes_langhome_2` + `pes_langhome_3` + `pes_langhome_4` + `pes_langhome_5` + `pes_langhome_6` + `pes_langhome_7` + `pes_langhome_8` + `pes_langhome_9` + `pes_langhome_10` + `pes_langhome_11` + `pes_langhome_12` + `pes_langhome_13` + `pes_langhome_14` + `pes_langhome_15` + `pes_langhome_16` + `pes_langhome_17` (pes) | `approximate` | Une question à choix multiples (les langues parlées normalement à la maison, 17 options), et non celle parlée le plus souvent : une personne qui a coché des options d’un seul niveau le reçoit ; l’anglais et le français, ou l’un d’eux avec une autre langue (171 en tout), sont not_mappable ; Web, posée après l’élection. | lang_home_multiselect | french, english, other | Quelle est la langue (ou les langues) que vous parlez normalement à la maison? |  | `pes_weight_general` | Non offert |
| qes2018 | `q70` (post) | `comparable` | Même question et même liste que l’ancrage, Web, avec « je ne sais pas » et refus affichés. | lang_home_list | french, english, other | Quelle langue parlez-vous le plus souvent à la maison? |  | `pond` | Offert explicitement |
| qes2014 | `Q66` (post) | `comparable` | Même question et même liste que l’ancrage, Web, avec « je ne sais pas » et refus affichés. | lang_home_list | french, english, other | Quelle langue parlez-vous le plus souvent à la maison? |  | `POND` | Offert explicitement |
| qes2012 | `q107` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | lang_home_list | french, english, other | Quelle langue parlez-vous le plus souvent à la maison? |  | `pond` | Non documenté |
| qes_crop_2007_2010 | `lusag` (chaque sondage) | `comparable` | La langue parlée le plus souvent dans le foyer, trois options, par téléphone, dans chaque sondage. | lang_home_3 | french, english, other | Quelle langue parle-t-on le plus souvent dans votre foyer? |  | `XPOND` (pas encore utilisable) | Non documenté |
| qes2008 | `q80` (post) | `comparable` | Même question que l’ancrage ; la liste est presque la même (sans l’inuktitut, donc le cri au code 13, et sans code « je ne sais pas ») ; téléphone selon les métadonnées du dépôt, l’ancrage est Web. | lang_home_list | french, english, other | Quelle langue parlez-vous LE PLUS SOUVENT à la maison ? |  |  | Non offert |
| qes2007 | `q80` (post) | `comparable` | Même question et même liste que l’ancrage ; l’étude combine des entrevues téléphoniques et Web. | lang_home_list | french, english, other | Quelle langue parlez-vous LE PLUS SOUVENT à la maison ? |  | `pond` | Non documenté |
| qes2007_panel | `lusage` (toute vague) | `comparable` | La langue parlée le plus souvent dans le foyer, et non celle de la personne elle-même, trois options, par téléphone ; une seule langue. | lang_home_3 | french, english, other | Quelle langue parle-t-on le plus souvent dans votre foyer? |  |  | Spontané seulement |

#### `relig_attend` : Assistance aux services religieux

À quelle fréquence la personne assiste aux services de son lieu de
culte, sans compter les mariages et les funérailles, de chaque semaine à
presque jamais ou jamais. Là où l’étude n’a interrogé que les personnes
qui appartiennent à une religion, les autres sont presque jamais ou
jamais (niveau approximate).

Famille `faith` · type Ordinale · moment Tout moment

**Niveaux**

| Code | Nom           | Étiquette                  |
|------|---------------|----------------------------|
| 1    | `weekly`      | Chaque semaine             |
| 2    | `twice_month` | Deux fois par mois         |
| 3    | `monthly`     | Une fois par mois          |
| 4    | `yearly`      | Une ou deux fois par année |
| 5    | `never`       | Presque jamais ou jamais   |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `q68` (post) | `approximate` | Posée seulement aux personnes qui appartiennent à une religion (la question filtre) : les autres reçoivent presque jamais ou jamais, et celles qui ont refusé le filtre sont inapplicable. L’ancrage interroge tout le monde : les proportions ne sont pas directement comparables. | attend_5_filtered | weekly, twice_month, monthly, yearly, never | Sans compter les mariages et les funérailles, combien de fois assistez-vous aux messes à votre lieu de culte? | q66: 2 = never, 9 = inapplicable | `pond` | Offert explicitement |
| qes2014 | `Q64` (post) | `approximate` | Posée seulement aux personnes qui appartiennent à une religion (la question filtre) : les autres reçoivent presque jamais ou jamais, et celles qui ont refusé le filtre sont inapplicable. L’ancrage interroge tout le monde : les proportions ne sont pas directement comparables. | attend_5_filtered | weekly, twice_month, monthly, yearly, never | Sans compter les mariages et les funérailles, combien de fois assistez-vous aux messes à votre lieu de culte? | Q62: 2 = never, 9 = inapplicable | `POND` | Offert explicitement |
| qes2012 | `q104` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible : posée à toutes les personnes. | attend_5 | weekly, twice_month, monthly, yearly, never | Sans compter les mariages et les funérailles, combien de fois assistez-vous aux messes à votre lieu de culte? |  | `pond` | Offert explicitement |
| qes2008 | `q81` (post) | `comparable` | Même question et mêmes options, posée à toutes les personnes, par téléphone. | attend_5 | weekly, twice_month, monthly, yearly, never | Sans compter les mariages et les funérailles, combien de fois assistez-vous aux messes à votre lieu de culte ? Est-ce… |  |  | Non documenté |
| qes2007 | `q81` (post) | `comparable` | Même question et mêmes options, posée à toutes les personnes ; l’étude combine des entrevues téléphoniques et Web. | attend_5 | weekly, twice_month, monthly, yearly, never | Sans compter les mariages et les funérailles, combien de fois assistez-vous aux messes à votre lieu de culte ? Est-ce à chaque semaine, deux fois par mois, une fois par mois, une ou deux fois par année ou presque jamais ? |  | `pond` | Non documenté |

#### `birthplace3` : Lieu de naissance (Québec, reste du Canada, étranger)

Où la personne est née : au Québec, ailleurs au Canada ou hors du
Canada. Là où les deux sont harmonisées, born_canada vaut oui exactement
là où celle-ci vaut quebec ou other_canada.

Famille `birthplace` · type Catégorielle · moment Invariant

**Niveaux**

| Code | Nom            | Étiquette          |
|------|----------------|--------------------|
| 1    | `quebec`       | Québec             |
| 2    | `other_canada` | Ailleurs au Canada |
| 3    | `abroad`       | Hors du Canada     |

Couverture par étude

| Étude | Source | Niveau | Raison | Instrument | Niveaux offerts | Libellé | Filtre | Pondération | Ne sait pas |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `q69` (post) | `comparable` | Même question et mêmes options, Web, avec « je ne sais pas » affiché. | birthplace_3 | quebec, other_canada, abroad | Où êtes-vous né(e)? |  | `pond` | Offert explicitement |
| qes2014 | `Q65` (post) | `comparable` | Même question et mêmes options, Web, avec « je ne sais pas » affiché. | birthplace_3 | quebec, other_canada, abroad | Où êtes-vous né(e)? |  | `POND` | Offert explicitement |
| qes2012 | `q105` (post) | `identical` (ancrage) | Ligne d’ancrage de la cible. | birthplace_3 | quebec, other_canada, abroad | Où êtes-vous né(e)? |  | `pond` | Offert explicitement |

## Variables regroupées

Une variable regroupée est une seule colonne pour toutes les études qui
regroupe plusieurs cibles, ses membres : `vote_choice` regroupe le vote
déclaré et les intentions de vote, `sov_support` les libellés
référendaires, `pol_interest` les échelles d’intérêt. Les membres
restent des cibles, chacune un seul stimulus ; la colonne regroupée
indique, ligne par ligne, de quel membre vient sa valeur
(`<variable>__type`), le niveau de comparabilité de ce membre
(`<variable>__grade`, jamais relevé ; une transformation avec perte le
plafonne à approximate) et sa question (`<variable>__item`,
`étude:vague:variables sources`).
`qes_harmonize(targets = "vote_choice")` renvoie la colonne regroupée et
ses compagnes ; `types = list(vote_choice = "recall")` ne garde que
certains membres.

Comment une ligne reçoit sa valeur : les membres des types demandés sont
essayés par ordre de priorité. Le premier membre dont la cellule a une
valeur, ou une valeur manquante qui est une réponse (ne sait pas, refus,
n’a pas voté, …), détermine la ligne. Un membre qui n’a pas interrogé la
personne (absente de la vague, question non posée, ligne non approuvée,
sous le niveau demandé, écartée par un filtre, valeur manquante système,
code à cheval sur plusieurs niveaux) passe au suivant. Quand tous les
membres passent, la ligne est `NA` avec le motif du premier membre
utilisable qui a une ligne dans l’étude et la vague, sinon du premier
membre qui a une ligne. En disposition par répondant (une ligne par
personne), les valeurs d’une étude viennent d’une seule vague : celle du
premier membre qu’elle applique, pour qu’une seule colonne de
pondération leur convienne ; la disposition longue (`layout = "long"`,
une ligne par personne et par vague) garde toutes les vagues.

### `vote_choice` : Choix de vote provincial (regroupé)

Le parti du vote de la personne à l’élection générale québécoise de
l’étude, une seule variable pour toutes les études : le vote déclaré
(rappel), demandé après l’élection, là où l’étude l’a demandé ; sinon
l’intention de vote avec relance, vers le parti vers lequel elles
penchent, des personnes qui n’ont nommé aucun parti (les indécis et,
dans certaines études, celles qui ne voteraient pas, pour aucun parti ou
refusaient), si bien que no_party est plus bas sous intention_push que
sous intention ; sinon l’intention de vote à la première question. Les
libellés diffèrent d’une étude à l’autre et vote_choice\_\_type dit de
quelle question vient chaque valeur. Ne voterait pas, aucun ou
annulerait (niveau no_party) est une réponse seulement dans les
intentions ; dans un vote déclaré, les abstentionnistes et les bulletins
annulés sont des valeurs manquantes avec un motif.

type Catégorielle

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

**Membres**

| Priorité | Type | Membre (cible) | Par défaut | Transformation | Plafond |
|----|----|----|----|----|----|
| 1 | `recall`: Vote déclaré (après l’élection). Le geste sur lequel porte l’étude, celui que mesurent les résultats officiels : utilisé en premier. | [`vote_prov_recall`](#target-vote_prov_recall) | oui | `identity` |  |
| 2 | `intention_push`: Intention de vote, indécis relancés. La première question plus une relance posée à qui n’y a nommé aucun parti : un parti nommé à la première question n’est jamais changé. Les personnes relancées diffèrent selon l’étude : les indécis partout, et aussi celles qui ne voteraient pas, pour aucun parti ou annuleraient (qes1998, qes2007_panel, qes2012_panel, les sondages CROP) ou qui refusaient (qes1998, qes2012_panel). La relance peut changer une telle réponse en un parti (moins ferme) ou en une autre réponse sans parti : dans qes2012_panel, sur 172 « je ne sais pas », 77 nomment un parti, 1 ne voterait pas et 1 refuse, sur 30 refus, 6 nomment un parti, 2 ne voteraient pas et 4 ne savent pas, et sur 14 « ne voterait pas », 3 nomment un parti et 2 ne savent pas ; dans qes2007_panel, sur 81 « ne voterait pas, aucun ou annulerait », 25 nomment un parti et 14 ne savent pas ou refusent ; dans qes1998, 5 réponses « ne voterait pas » et 20 refus nomment un parti, et 4 et 6 deviennent « je ne sais pas ». no_party est donc plus bas sous intention_push que sous intention. Dans qes2022, elle ajoute aussi l’intention conditionnelle des personnes peu susceptibles de voter, à qui la première question n’a pas été posée (cps_votechoice2, « Si vous décidez de voter, pour quel parti prévoyez-vous voter? », puis cps_votelean si elles ne savaient pas : 62 personnes, dont 43 nomment un parti). | [`vote_prov_intent_push`](#target-vote_prov_intent_push) | oui | `identity` |  |
| 3 | `intention`: Intention de vote (première question) | [`vote_prov_intent`](#target-vote_prov_intent) | oui | `identity` |  |

Couverture par étude

| Étude | `recall` | `intention_push` | `intention` | Disposition par répondant |
|----|----|----|----|----|
| `qes2022` | Comparable (pes) | Approximatif (cps) | Approximatif (cps) | `recall` |
| `qes2018` | Comparable (post) | — | — | `recall` |
| `qes2018_panel` | Comparable (post) | Approximatif (pre) | Approximatif (pre) | `recall` |
| `qes2014` | Comparable (post) | — | — | `recall` |
| `qes2012` | Identique (post) | — | — | `recall` |
| `qes2012_panel` | Approximatif (post) | Approximatif (pre) | Approximatif (pre) | `recall` |
| `qes_crop_2007_2010` | — | Comparable (chaque sondage) | Comparable (chaque sondage) | `intention_push` |
| `qes2008` | Comparable (post) | — | — | `recall` |
| `qes2007` | Comparable (post) | — | — | `recall` |
| `qes2007_panel` | Approximatif (post) | Identique (pre) | Identique (pre) | `recall` |
| `qes1998` | Comparable (post) | Approximatif (pre) | — | `recall` |

Chaque cellule donne le niveau du membre dans l’étude (plafonné) et la
vague qui a posé la question ; un tiret signifie que l’étude n’a pas de
question pour le membre. La dernière colonne donne le membre dont la
disposition par répondant tire les valeurs de l’étude, parmi les membres
par défaut (la disposition longue utilise toutes les vagues).

### `sov_support` : Appui à la souveraineté (regroupé)

Comment la personne voterait (oui, non, n’irait pas voter) sur la
souveraineté du Québec, une seule variable pour toutes les études qui
l’ont demandé, à travers les libellés référendaires : un pays
indépendant ; un pays souverain ; la question de 1995 (la souveraineté
assortie d’une offre de partenariat), avec relance des indécis là où
l’étude les a relancés ; et, regroupé en oui ou non, être favorable ou
opposé à l’indépendance du Québec. L’appui dépend du libellé :
sov_support\_\_type dit de quel libellé vient chaque valeur.

type Catégorielle

**Niveaux**

| Code | Nom              | Étiquette                      |
|------|------------------|--------------------------------|
| 1    | `yes`            | Oui                            |
| 2    | `no`             | Non                            |
| 95   | `would_not_vote` | N’irait pas voter / annulerait |

**Membres**

| Priorité | Type | Membre (cible) | Par défaut | Transformation | Plafond |
|----|----|----|----|----|----|
| 1 | `independence`: Référendum sur un pays indépendant | [`sov_indep`](#target-sov_indep) | oui | `identity` |  |
| 2 | `sovereign_country`: Référendum sur un pays souverain | [`sov_sovereign_country`](#target-sov_sovereign_country) | oui | `identity` |  |
| 3 | `partnership_1995_push`: Question de 1995, indécis relancés | [`sov_partnership_1995_push`](#target-sov_partnership_1995_push) | oui | `identity` |  |
| 4 | `partnership_1995`: Question de 1995 (souveraineté-partenariat) | [`sov_partnership_1995`](#target-sov_partnership_1995) | oui | `identity` |  |
| 5 | `favour`: Favorable ou opposé à l’indépendance (regroupé en oui ou non). Quatre points regroupés en deux : au mieux approximate. | [`sov_favour`](#target-sov_favour) | oui | `recode:very_favourable=yes,somewhat_favourable=yes,somewhat_opposed=no,very_opposed=no` | `approximate` |

Couverture par étude

| Étude | `independence` | `sovereign_country` | `partnership_1995_push` | `partnership_1995` | `favour` | Disposition par répondant |
|----|----|----|----|----|----|----|
| `qes2022` | Comparable (cps) | — | — | — | — | `independence` |
| `qes2018` | Comparable (post) | — | — | — | — | `independence` |
| `qes2018_panel` | — | — | — | — | Approximatif (post) | `favour` |
| `qes2014` | Identique (post) | — | — | — | — | `independence` |
| `qes2012` | Identique (post) | — | — | — | — | `independence` |
| `qes2012_panel` | — | Identique (pre) | — | — | — | `sovereign_country` |
| `qes_crop_2007_2010` | — | — | — | — | — | — |
| `qes2008` | — | — | Comparable (post) | Comparable (post) | — | `partnership_1995_push` |
| `qes2007` | — | — | Identique (post) | Identique (post) | — | `partnership_1995_push` |
| `qes2007_panel` | — | — | Comparable (pre) | Comparable (pre) | — | `partnership_1995_push` |
| `qes1998` | — | — | Comparable (pre) | Comparable (pre) | — | `partnership_1995_push` |

Chaque cellule donne le niveau du membre dans l’étude (plafonné) et la
vague qui a posé la question ; un tiret signifie que l’étude n’a pas de
question pour le membre. La dernière colonne donne le membre dont la
disposition par répondant tire les valeurs de l’étude, parmi les membres
par défaut (la disposition longue utilise toutes les vagues).

### `pol_interest` : Intérêt pour la politique (regroupé, 0-1)

Intérêt de la personne pour la politique, sur une échelle de 0 (pas du
tout) à 1 (très), une seule variable pour toutes les études qui l’ont
demandé : les questions à quatre points notées 1, 0,7, 0,3 et 0 (très,
plutôt, pas très et pas du tout intéressé), les questions de 0 à 10
divisées par 10. L’intérêt pour la politique en général passe avant
l’intérêt pour la campagne ou l’élection. pol_interest\_\_type dit de
quelle question vient chaque valeur ; une question à quatre points notée
a au mieux le niveau approximate, ses notes étant une hypothèse.

type Numérique

**Plage valide** : 0-1

**Membres**

| Priorité | Type | Membre (cible) | Par défaut | Transformation | Plafond |
|----|----|----|----|----|----|
| 1 | `general_4pt`: Intérêt pour la politique, quatre points (notés). Notée 1, 0,7, 0,3 et 0 (très, plutôt, pas très et pas du tout intéressé). | [`interest_4pt`](#target-interest_4pt) | oui | `score:very=1,quite=0.7,hardly=0.3,not_at_all=0` | `approximate` |
| 2 | `general_0_10`: Intérêt pour la politique, 0-10 | [`interest_0_10`](#target-interest_0_10) | oui | `affine:0.1*x` |  |
| 3 | `campaign_4pt`: Intérêt pour la campagne, quatre points (notés) | [`interest_campaign_4pt`](#target-interest_campaign_4pt) | oui | `score:very=1,quite=0.7,hardly=0.3,not_at_all=0` | `approximate` |
| 4 | `election_0_10`: Intérêt pour l’élection, 0-10. Intérêt pour une élection, pas pour la politique : au mieux approximate. | [`interest_election_0_10`](#target-interest_election_0_10) | oui | `affine:0.1*x` | `approximate` |

Couverture par étude

| Étude | `general_4pt` | `general_0_10` | `campaign_4pt` | `election_0_10` | Disposition par répondant |
|----|----|----|----|----|----|
| `qes2022` | — | Approximatif (cps) | — | — | `general_0_10` |
| `qes2018` | Approximatif (post) | — | — | — | `general_4pt` |
| `qes2018_panel` | — | — | — | — | — |
| `qes2014` | Approximatif (post) | — | — | — | `general_4pt` |
| `qes2012` | Approximatif (post) | — | — | — | `general_4pt` |
| `qes2012_panel` | — | — | — | — | — |
| `qes_crop_2007_2010` | — | — | — | — | — |
| `qes2008` | — | — | — | Approximatif (post) | `election_0_10` |
| `qes2007` | — | Identique (post) | — | Approximatif (post) | `general_0_10` |
| `qes2007_panel` | — | — | Approximatif (pre) | — | `campaign_4pt` |
| `qes1998` | — | — | — | — | — |

Chaque cellule donne le niveau du membre dans l’étude (plafonné) et la
vague qui a posé la question ; un tiret signifie que l’étude n’a pas de
question pour le membre. La dernière colonne donne le membre dont la
disposition par répondant tire les valeurs de l’étude, parmi les membres
par défaut (la disposition longue utilise toutes les vagues).

### `turnout` : Participation à l’élection provinciale (regroupée)

Si la personne a voté à l’élection générale québécoise de l’étude (oui,
non) : la participation déclarée, demandée après l’élection. Avec types
= list(turnout = c(“recall”, “intention”)), une étude qui n’a pas
demandé la participation déclarée donne à la place la probabilité de
voter demandée avant l’élection, regroupée en oui (certain ou probable
de voter, a déjà voté) ou non (peu probable, certain de ne pas voter),
au mieux au niveau approximate : une intention n’est pas une
participation, elle n’est donc pas utilisée par défaut.

type Catégorielle

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

**Membres**

| Priorité | Type | Membre (cible) | Par défaut | Transformation | Plafond |
|----|----|----|----|----|----|
| 1 | `recall`: Participation déclarée (après l’élection) | [`turnout_prov_recall`](#target-turnout_prov_recall) | oui | `identity` |  |
| 2 | `intention`: Probabilité de voter (avant l’élection, regroupée). Non utilisée par défaut : une intention de voter n’est pas une participation. | [`turnout_prov_likely`](#target-turnout_prov_likely) | non | `recode:certain=yes,likely=yes,already_voted=yes,unlikely=no,certain_not=no` | `approximate` |

Couverture par étude

| Étude | `recall` | `intention` | Disposition par répondant |
|----|----|----|----|
| `qes2022` | Approximatif (pes) | Approximatif (cps) | `recall` |
| `qes2018` | Approximatif (post) | — | `recall` |
| `qes2018_panel` | Approximatif (post) | — | `recall` |
| `qes2014` | Comparable (post) | — | `recall` |
| `qes2012` | Identique (post) | — | `recall` |
| `qes2012_panel` | Comparable (post) | — | `recall` |
| `qes_crop_2007_2010` | — | — | — |
| `qes2008` | Comparable (post) | — | `recall` |
| `qes2007` | Comparable (post) | — | `recall` |
| `qes2007_panel` | Comparable (post) | — | `recall` |
| `qes1998` | Comparable (post) | — | `recall` |

Chaque cellule donne le niveau du membre dans l’étude (plafonné) et la
vague qui a posé la question ; un tiret signifie que l’étude n’a pas de
question pour le membre. La dernière colonne donne le membre dont la
disposition par répondant tire les valeurs de l’étude, parmi les membres
par défaut (la disposition longue utilise toutes les vagues).

## Harmonisation souple : qes_decon()

[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md)
met un concept dans une seule colonne pour chaque étude, même quand le
libellé ou les choix de réponse diffèrent, avec des catégories communes
larges, sous des noms de colonnes simples. Elle échange l’exactitude
contre la couverture : une colonne souple n’a pas de niveau de
comparabilité et ne prétend pas que deux études ont posé la même
question ; les cibles ci-dessus gardent les versions strictes, avec
leurs niveaux. Une colonne est construite à partir d’une cible stricte
ou d’une variable regroupée quand il en existe une, recodée au besoin
dans les catégories de la colonne, et d’appariements souples des
questions propres aux études là où la couche stricte n’en a pas. Un
appariement souple est appliqué une fois approuvé par un réviseur.

### `citizenship` : Citoyenneté

Si la personne est citoyenne canadienne.

Base : `target:citizen` · `recode:yes=citizen,no=not_citizen` · une
valeur par personne

**Comment elle est assouplie** : La citoyenneté canadienne là où une
étude l’a enregistrée : posée directement en 2022, et tirée dans le
panel de 2018 de la question de sélection sur le droit de voter à
l’élection québécoise à venir, si bien que tous les répondants de ce
panel sont citoyens.

**Niveaux**

| Code | Nom           | Étiquette                    |
|------|---------------|------------------------------|
| 1    | `citizen`     | Citoyen(ne) canadien(ne)     |
| 2    | `not_citizen` | Pas citoyen(ne) canadien(ne) |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2018_panel | `qa` (pre) | 1 = Citoyen(ne) canadien(ne); 2 = Catégorie source à cheval sur plusieurs niveaux (NA); 3 = Ne sait pas ou refus (un seul code) (NA) | complète l’étude | La question de sélection demande si la personne peut voter à la prochaine élection québécoise, ce qui exige la citoyenneté canadienne ; tous les répondants ont répondu oui, si bien que la colonne est constante dans cette étude. | approuvée |

### `yob` : Année de naissance

L’année de naissance de la personne.

Base : `target:birth_year` · une valeur par personne

**Comment elle est assouplie** : L’année de naissance déclarée ; les
études qui n’ont demandé qu’un groupe d’âge n’en ont pas (voir
age_group).

**Plage valide** : 1900-2010

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `age_group` : Groupe d’âge

Le groupe d’âge de la personne.

Base : `target:age_group3` · une valeur par personne

**Comment elle est assouplie** : Trois groupes d’âge (18-34, 35-54, 55
et plus), tirés des tranches d’âge de l’étude ou de l’âge au début du
terrain ; les moins de 18 ans sont manquants.

**Niveaux**

| Code | Nom        | Étiquette  |
|------|------------|------------|
| 1    | `a18_34`   | 18-34      |
| 2    | `a35_54`   | 35-54      |
| 3    | `a55_plus` | 55 et plus |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `gender` : Genre

Le genre de la personne.

Base : `target:gender` · une valeur par personne

**Comment elle est assouplie** : Le genre ou le sexe tel que chaque
étude l’a demandé : homme ou femme partout, et en 2022 aussi non binaire
ou un autre genre ; les questions diffèrent, pas les catégories.

**Niveaux**

| Code | Nom         | Étiquette      |
|------|-------------|----------------|
| 1    | `man`       | Homme          |
| 2    | `woman`     | Femme          |
| 3    | `nonbinary` | Non binaire    |
| 4    | `other`     | Un autre genre |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `education4` : Scolarité (quatre groupes)

Le plus haut niveau de scolarité atteint par la personne, en quatre
groupes.

Base : appariements souples seulement · une valeur par personne

**Comment elle est assouplie** : Les niveaux de chaque étude sont
regroupés en quatre : sans diplôme d’études secondaires, secondaire,
collégial (cégep, formation technique ou professionnelle) et
universitaire, y compris des études universitaires non terminées ; le
panel de 2007 et les sondages CROP ont demandé les années d’études, si
bien que leur groupe secondaire compte aussi ceux qui ont quitté le
secondaire sans diplôme, et leur groupe collégial peut compter des
personnes ayant fait des études universitaires sans diplôme. qes1998 est
manquante (non appariable) : le groupe du milieu de son fichier
regroupé, 10 à 15 années d’études, chevauche le secondaire et le
collégial (education, en trois groupes, l’inclut). La cible stricte du
même nom dans qes_harmonize() regroupe autrement : son groupe le plus
bas est primaire ou moins.

**Niveaux**

| Code | Nom           | Étiquette                                     |
|------|---------------|-----------------------------------------------|
| 1    | `no_diploma`  | Sans diplôme d’études secondaires             |
| 2    | `high_school` | Diplôme d’études secondaires                  |
| 3    | `college`     | Collégial, cégep ou formation professionnelle |
| 4    | `university`  | Universitaire                                 |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `cps_edu` (cps) | 1-4 = Sans diplôme d’études secondaires; 5 = Diplôme d’études secondaires; 6, 7 = Collégial, cégep ou formation professionnelle; 8-11 = Universitaire; -99 = Sans réponse (non-réponse partielle) (NA) | complète l’étude |  | approuvée |
| qes2018 | `qscol` (post) | 1-7 = Sans diplôme d’études secondaires; 8 = Diplôme d’études secondaires; 9-12 = Collégial, cégep ou formation professionnelle; 13-15 = Universitaire; 99 = Refus (NA) | complète l’étude | Le code 9 (secondaire 5 avec un diplôme d’études professionnelles, DEP) est collégial ; la cible stricte education4 le classe au secondaire. | approuvée |
| qes2014 | `QSCOL` (post) | 1-4 = Sans diplôme d’études secondaires; 5 = Diplôme d’études secondaires; 6-8 = Collégial, cégep ou formation professionnelle; 9-11 = Universitaire; 99 = Refus (NA) | complète l’étude |  | approuvée |
| qes2012 | `scol` (post) | 1-4 = Sans diplôme d’études secondaires; 5 = Diplôme d’études secondaires; 6-8 = Collégial, cégep ou formation professionnelle; 9-12 = Universitaire; 98 = Ne sait pas (NA); 99 = Refus (NA) | complète l’étude | Le code 8 (postsecondaire non universitaire ; le cours technique du questionnaire français) est collégial ; le code 10 (certificat et diplôme), absent des deux questionnaires, est universitaire, comme le code 9 (études supérieures non terminées). | approuvée |
| qes2018_panel | `d3` (pre) | 1, 2 = Sans diplôme d’études secondaires; 3 = Diplôme d’études secondaires; 4, 5 = Collégial, cégep ou formation professionnelle; 6-8 = Universitaire; 9 = Ne sait pas ou refus (un seul code) (NA) | complète l’étude | Le code 4 (apprentissage enregistré ou autre certificat d’une école de métiers) est collégial ; le code 6 (certificat universitaire inférieur au baccalauréat) est universitaire. | approuvée |
| qes2007_panel | `scol` (toute vague) | 1 = Sans diplôme d’études secondaires; 2 = Diplôme d’études secondaires; 3 = Collégial, cégep ou formation professionnelle; 4 = Universitaire; 9 = Refus (NA) | complète l’étude | Années d’études, en tranches nommées d’après un ordre d’enseignement : 7 ans ou moins (primaire) est sans diplôme, 8 à 12 ans (secondaire) est secondaire, 13 à 15 ans (cégep, école technique) est collégial et 16 ans ou plus est universitaire. Le groupe secondaire compte donc aussi ceux qui ont quitté le secondaire sans diplôme, et le groupe collégial peut compter des personnes ayant fait des études universitaires sans diplôme, que les autres études classent à l’universitaire. | approuvée |
| qes2007 | `q77` (post) | 1-4 = Sans diplôme d’études secondaires; 5 = Diplôme d’études secondaires; 6, 7 = Collégial, cégep ou formation professionnelle; 8-11 = Universitaire; 98 = Ne sait pas (NA); 99 = Refus (NA) | complète l’étude |  | approuvée |
| qes2008 | `q77` (post) | 1-4 = Sans diplôme d’études secondaires; 5 = Diplôme d’études secondaires; 6, 7 = Collégial, cégep ou formation professionnelle; 8-11 = Universitaire; 98 = Ne sait pas (NA); 99 = Refus (NA) | complète l’étude |  | approuvée |
| qes1998 | `scol` (pre) | 1-3 = Catégorie source à cheval sur plusieurs niveaux (NA); 9 = Ne sait pas ou refus (un seul code) (NA) | complète l’étude | Le fichier regroupé classe les années d’études en 1-9, 10-15 et université ; 10 à 15 ans chevauche le secondaire et le collégial, et n’apparier que les deux autres groupes fausserait toutes les proportions, si bien que l’étude est laissée de côté des quatre groupes. Les trois groupes de education l’incluent. | approuvée |
| qes_crop_2007_2010 | `scol` (chaque sondage) | 1 = Sans diplôme d’études secondaires; 2 = Diplôme d’études secondaires; 3 = Collégial, cégep ou formation professionnelle; 4 = Universitaire; 9 = Refus (NA) | complète l’étude | Années d’études selon les quatre intervalles de CROP (7 ou moins, primaire ; 8 à 12, secondaire ; 13 à 15, cégep ou école technique ; 16 ou plus, université) : 7 ou moins est sans diplôme et 8 à 12 est secondaire, si bien que ce groupe compte aussi ceux qui ont quitté le secondaire sans diplôme ; le classement par années, et non par plus haut niveau atteint, peut aussi placer des études universitaires non terminées (14 ou 15 ans) au collégial. | approuvée |

### `education` : Scolarité

Le plus haut niveau de scolarité atteint par la personne, en trois
groupes.

Base : `column:education4` ·
`recode:no_diploma=no_diploma,high_school=high_school_college,college=high_school_college,university=university`
· une valeur par personne

**Comment elle est assouplie** : Les niveaux de chaque étude sont
regroupés en trois : sans diplôme d’études secondaires, secondaire au
collégial (diplôme d’études secondaires, cégep, formation technique ou
professionnelle) et université, y compris des études universitaires non
terminées. Ce sont les groupes de education4, secondaire et collégial
réunis, ce qui permet d’inclure qes1998. Là où une étude a demandé les
années d’études (le panel de 2007, les sondages CROP et les répondants
de CROP dans qes1998), les tranches ne suivent pas les diplômes : des
personnes sans diplôme (10 ans dans qes1998 ; 8 à 10 ans dans les
autres, puisque le secondaire se termine après 11 ans au Québec) sont
classées au secondaire au collégial, et des étudiants universitaires (14
ou 15 ans) ne sont pas classés à l’université.

**Niveaux**

| Code | Nom                   | Étiquette                         |
|------|-----------------------|-----------------------------------|
| 1    | `no_diploma`          | Sans diplôme d’études secondaires |
| 2    | `high_school_college` | Secondaire au collégial (cégep)   |
| 3    | `university`          | Université                        |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes1998 | `scol` (pre) | 1 = Sans diplôme d’études secondaires; 2 = Secondaire au collégial (cégep); 3 = Université; 9 = Ne sait pas ou refus (un seul code) (NA) | remplace la base | Les trois groupes du fichier regroupé portent les étiquettes 1-9 ans, 10-15 ans et université et plus, mais les deux firmes les ont construits autrement. Pour les répondants de CREATEC (firme_post = 1), scol recode le plus haut niveau atteint (r15 : primaire ou études secondaires partielles ; secondaire terminé ou formation technique, collégiale ou cégep ; université, terminée ou non), si bien que les trois groupes tiennent. Pour les répondants de CROP (firme_post = 2), il regroupe les années d’études (question 19 de CROP : 7 ou moins, 8-9, 10-11, 12-15, 16 ou plus), et l’appariement est approximatif : le secondaire se terminant après 11 ans au Québec, une personne qui compte 10 ans n’a pas de diplôme mais est classée au secondaire au collégial, et des personnes qui comptent 14 ou 15 ans sont des étudiants universitaires classés au secondaire au collégial. Le code 9, sans étiquette, est lu comme ne sait pas ou refus. | approuvée |

### `income_cat` : Revenu du ménage (tiers)

Le revenu total du ménage avant impôts, en tiers des répondants de
l’étude.

Base : appariements souples seulement · une valeur par personne

**Comment elle est assouplie** : Le revenu du ménage en tiers des
répondants de chaque étude : les tranches de revenu ne sont jamais
coupées, si bien qu’un tiers réunit les tranches entières dont le point
milieu y tombe et compte rarement exactement un tiers ; les seuils en
dollars diffèrent d’une étude à l’autre.

**Niveaux**

| Code | Nom      | Étiquette                |
|------|----------|--------------------------|
| 1    | `low`    | Faible (tiers inférieur) |
| 2    | `middle` | Moyen                    |
| 3    | `high`   | Élevé (tiers supérieur)  |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `cps_income` (cps) | montant moins de 52200 = Faible (tiers inférieur); 52200 à moins de 95600 = Moyen; 95600 ou plus = Élevé (tiers supérieur); -99, 0 passent à cps_income2: 1-3 = Faible (tiers inférieur); 4 = Moyen; 5-8 = Élevé (tiers supérieur); -99 = Sans réponse (non-réponse partielle) (NA) | complète l’étude | Le montant (revenu de 2021) en tiers des 1 444 montants donnés, non pondérés : faible sous 52 200 \$, élevé 95 600 \$ ou plus. Un montant de 0 ou de -99 (aucun montant) passe à la question par tranches cps_income2, dont les tranches sont classées selon leur point milieu, avec les mêmes seuils. | approuvée |
| qes2018 | `q61` (post) | 1-4 = Faible (tiers inférieur); 5, 6 = Moyen; 7-9 = Élevé (tiers supérieur); 99 = Refus (NA) | complète l’étude | Tiers selon la règle du point milieu sur les effectifs non pondérés des tranches. Faible est sous 40 000 \$, élevé 72 000 \$ ou plus (revenu de 2017). | approuvée |
| qes2014 | `Q57` (post) | 1-4 = Faible (tiers inférieur); 5, 6 = Moyen; 7-9 = Élevé (tiers supérieur); 99 = Refus (NA) | complète l’étude | Tiers selon la règle du point milieu sur les effectifs non pondérés des tranches. Faible est sous 40 000 \$, élevé 72 000 \$ ou plus (revenu de 2013). | approuvée |
| qes2012 | `reven` (post) | 1-4 = Faible (tiers inférieur); 5-7 = Moyen; 8, 9 = Élevé (tiers supérieur); 98 = Ne sait pas (NA); 99 = Refus (NA) | complète l’étude | Tiers selon la règle du point milieu sur les effectifs non pondérés des tranches. Faible est sous 40 000 \$, élevé 88 000 \$ ou plus (revenu de 2011). | approuvée |
| qes2018_panel | `d5` (pre) | 1, 2 = Faible (tiers inférieur); 3, 4 = Moyen; 5-7 = Élevé (tiers supérieur); 8 = Ne sait pas ou refus (un seul code) (NA) | complète l’étude | Tiers selon la règle du point milieu sur les effectifs non pondérés des tranches. Faible est sous 40 000 \$, élevé 80 000 \$ ou plus. | approuvée |
| qes2007_panel | `revenu` (toute vague) | 1, 2 = Faible (tiers inférieur); 3 = Moyen; 4, 5 = Élevé (tiers supérieur); 9 = Ne sait pas ou refus (un seul code) (NA) | complète l’étude | Tiers selon la règle du point milieu sur les effectifs non pondérés des tranches. Faible est sous 40 000 \$, élevé 60 000 \$ ou plus. | approuvée |
| qes2007 | `q78` (post) | 1-3 = Faible (tiers inférieur); 4-6 = Moyen; 7-10 = Élevé (tiers supérieur); 98 = Ne sait pas (NA); 99 = Refus (NA) | complète l’étude | Tiers selon la règle du point milieu sur les effectifs non pondérés des tranches. Faible est sous 40 000 \$, élevé 70 000 \$ ou plus (revenu de 2006). | approuvée |
| qes2008 | `q78` (post) | 1-3 = Faible (tiers inférieur); 4-6 = Moyen; 7-10 = Élevé (tiers supérieur); 98 = Ne sait pas (NA); 99 = Refus (NA) | complète l’étude | Tiers selon la règle du point milieu sur les effectifs non pondérés des tranches. Faible est sous 40 000 \$, élevé 70 000 \$ ou plus (revenu de 2007). | approuvée |
| qes_crop_2007_2010 | `revenu` (chaque sondage) | 1, 2 = Faible (tiers inférieur); 3, 4 = Moyen; 5 = Élevé (tiers supérieur); 9 = Ne sait pas ou refus (un seul code) (NA) | complète l’étude | Tiers selon la règle du point milieu sur les effectifs non pondérés des tranches. Faible est sous 40 000 \$, élevé 80 000 \$ ou plus ; les tiers regroupent les 24 sondages. | approuvée |

### `language` : Langue maternelle

La langue maternelle de la personne : français, anglais ou une autre
langue.

Base : `target:lang_mother` · une valeur par personne

**Comment elle est assouplie** : La langue apprise en premier dans
l’enfance, en trois groupes ; une personne qui déclare le français et
une autre langue est classée au français, et l’anglais et une langue
autre que le français, à l’anglais, et les répondants de CREATEC de 1998
sont francophones par la conception de leur échantillon.

**Niveaux**

| Code | Nom       | Étiquette |
|------|-----------|-----------|
| 1    | `french`  | Français  |
| 2    | `english` | Anglais   |
| 3    | `other`   | Autre     |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `cps_lang_2` (cps) | la première option cochée, dans cet ordre : cps_lang_2 (Français), cps_lang_1 (Anglais), cps_lang_3 (Autre) | remplace la base | Une personne qui a déclaré deux langues maternelles est classée au français si l’une est le français, sinon à l’anglais si l’une est l’anglais. | approuvée |
| qes2014 | `QLANG` (post) | 1, 4, 5 = Français; 2, 6 = Anglais; 3 = Autre; 8 = Ne sait pas (NA); 9 = Refus (NA) | remplace la base | Une personne qui a déclaré deux langues maternelles est classée au français si l’une est le français, sinon à l’anglais si l’une est l’anglais. | approuvée |
| qes2007 | `langu` (post) | 1, 4, 7 = Français; 2, 5 = Anglais; 3, 6 = Autre; 9 = Ne sait pas ou refus (un seul code) (NA) | remplace la base | Une personne qui a déclaré deux langues maternelles est classée au français si l’une est le français, sinon à l’anglais si l’une est l’anglais. | approuvée |
| qes1998 | `firme_post` (pre) | 1 = Français; 2 = Catégorie source à cheval sur plusieurs niveaux (NA) | complète l’étude | L’échantillon de CREATEC (firme_post = 1) ne compte que des personnes dont la langue maternelle est le français (livre de codes du fichier CREATEC) ; les répondants de CROP ont été choisis selon un autre critère et n’ont pas de langue maternelle. | approuvée |

### `language_fr` : Français, langue maternelle

Si le français est l’une des langues maternelles de la personne.

Base : `column:language` · `recode:french=yes,english=no,other=no` · une
valeur par personne

**Comment elle est assouplie** : Oui quand le français est parmi les
langues maternelles déclarées, si bien qu’une personne qui a deux
langues maternelles peut avoir oui à la fois dans language_fr et dans
language_eng.

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `cps_lang_2` (cps) | la première option cochée, dans cet ordre : cps_lang_2 (Oui), cps_lang_1 (Non), cps_lang_3 (Non) | remplace la base | Oui quand cette langue est parmi les langues maternelles déclarées. | approuvée |
| qes2014 | `QLANG` (post) | 1, 4, 5 = Oui; 2, 3, 6 = Non; 8 = Ne sait pas (NA); 9 = Refus (NA) | remplace la base | Oui quand cette langue est parmi les langues maternelles déclarées. | approuvée |
| qes2007 | `langu` (post) | 1, 4, 7 = Oui; 2, 3, 5, 6 = Non; 9 = Ne sait pas ou refus (un seul code) (NA) | remplace la base | Oui quand cette langue est parmi les langues maternelles déclarées. | approuvée |

### `language_eng` : Anglais, langue maternelle

Si l’anglais est l’une des langues maternelles de la personne.

Base : `column:language` · `recode:french=no,english=yes,other=no` · une
valeur par personne

**Comment elle est assouplie** : Oui quand l’anglais est parmi les
langues maternelles déclarées, si bien qu’une personne qui a deux
langues maternelles peut avoir oui à la fois dans language_eng et dans
language_fr.

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `cps_lang_1` (cps) | la première option cochée, dans cet ordre : cps_lang_1 (Oui), cps_lang_2 (Non), cps_lang_3 (Non) | remplace la base | Oui quand cette langue est parmi les langues maternelles déclarées. | approuvée |
| qes2014 | `QLANG` (post) | 1, 3, 5 = Non; 2, 4, 6 = Oui; 8 = Ne sait pas (NA); 9 = Refus (NA) | remplace la base | Oui quand cette langue est parmi les langues maternelles déclarées. | approuvée |
| qes2007 | `langu` (post) | 1, 3, 4, 6 = Non; 2, 5, 7 = Oui; 9 = Ne sait pas ou refus (un seul code) (NA) | remplace la base | Oui quand cette langue est parmi les langues maternelles déclarées. | approuvée |

### `religion` : Religion

La religion de la personne : catholique, protestante, autre chrétienne,
autre religion ou aucune.

Base : appariements souples seulement · une valeur par personne

**Comment elle est assouplie** : La religion d’appartenance, en cinq
groupes ; la question de 2022 offre une longue liste, où l’agnosticisme
compte comme aucune religion, et les autres études demandent d’abord si
la personne appartient à une religion.

**Niveaux**

| Code | Nom               | Étiquette        |
|------|-------------------|------------------|
| 1    | `catholic`        | Catholique       |
| 2    | `protestant`      | Protestante      |
| 3    | `other_christian` | Autre chrétienne |
| 4    | `other`           | Autre religion   |
| 5    | `none`            | Aucune religion  |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `cps_religion` (cps) | 1, 2 = Aucune religion; 3-7, 22 = Autre religion; 8, 9, 13, 15-21 = Protestante; 10 = Catholique; 11, 12, 14 = Autre chrétienne; -99 = Sans réponse (non-réponse partielle) (NA) | complète l’étude | Les personnes sans appartenance religieuse sont aucune ; les religions juive, musulmane et les autres religions non chrétiennes sont autre. L’agnosticisme compte comme aucune ; les Églises orthodoxes, les Témoins de Jéhovah et les mormons sont autre chrétienne ; les autres dénominations chrétiennes de la liste sont protestantes. | approuvée |
| qes2018 | `q67` (post) | si q66: 2 = Aucune religion; 9 = Refus (NA); sinon q67: 1 = Catholique; 2 = Protestante; 3 = Autre chrétienne; 4, 5, 96 = Autre religion; 99 = Refus (NA) | complète l’étude | Les personnes sans appartenance religieuse sont aucune ; les religions juive, musulmane et les autres religions non chrétiennes sont autre. | approuvée |
| qes2014 | `Q63` (post) | si Q62: 2 = Aucune religion; 9 = Refus (NA); sinon Q63: 1 = Catholique; 2 = Protestante; 3 = Autre chrétienne; 4-6 = Autre religion; 9 = Refus (NA) | complète l’étude | Les personnes sans appartenance religieuse sont aucune ; les religions juive, musulmane et les autres religions non chrétiennes sont autre. | approuvée |
| qes2012 | `q103` (post) | si q102: 2 = Aucune religion; 3 = Refus (NA); sinon q103: 1 = Catholique; 2 = Protestante; 3 = Autre chrétienne; 4-6 = Autre religion; 9 = Refus (NA) | complète l’étude | Les personnes sans appartenance religieuse sont aucune ; les religions juive, musulmane et les autres religions non chrétiennes sont autre. | approuvée |

### `marital` : État matrimonial

L’état matrimonial de la personne, en quatre groupes.

Base : appariements souples seulement · une valeur par personne

**Comment elle est assouplie** : Marié(e) ou en couple, séparé(e) ou
divorcé(e), veuf ou veuve, ou jamais marié(e) ; 2012 et 2014 ont demandé
l’état civil officiel, sans union de fait, si bien que des conjoints de
fait y sont jamais marié(e)s.

**Niveaux**

| Code | Nom                  | Étiquette                    |
|------|----------------------|------------------------------|
| 1    | `married`            | Marié(e) ou en couple        |
| 2    | `separated_divorced` | Séparé(e) ou divorcé(e)      |
| 3    | `widowed`            | Veuf ou veuve                |
| 4    | `never_married`      | Célibataire, jamais marié(e) |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `pes_married` (pes) | 1, 2 = Marié(e) ou en couple; 3, 4 = Séparé(e) ou divorcé(e); 5 = Veuf ou veuve; 6 = Célibataire, jamais marié(e); -99 = Sans réponse (non-réponse partielle) (NA) | complète l’étude | Posée après l’élection : les répondants de la seule vague de campagne sont manquants (absents de cette vague). | approuvée |
| qes2018 | `qstat` (post) | 1, 6 = Marié(e) ou en couple; 2, 4 = Séparé(e) ou divorcé(e); 3 = Célibataire, jamais marié(e); 5 = Veuf ou veuve; 98 = Refus (NA) | complète l’étude | Marié(e) ou en union civile (1) et conjoint(e) de fait (6) sont marié(e). | approuvée |
| qes2014 | `Q68` (post) | 1, 6 = Marié(e) ou en couple; 2, 4 = Séparé(e) ou divorcé(e); 3 = Célibataire, jamais marié(e); 5 = Veuf ou veuve; 9 = Refus (NA) | complète l’étude | La question demande l’état civil officiel et n’offre pas l’union de fait. L’union civile (6) est marié(e). Le code 6 regroupe 309 des 1 501 réponses valides, bien plus que la part des unions civiles au Québec : la plupart des conjoints de fait l’ont probablement choisie ; ceux qui ont répondu célibataire sont jamais marié(e)s. | approuvée |
| qes2012 | `q109` (post) | 1, 6 = Marié(e) ou en couple; 2, 4 = Séparé(e) ou divorcé(e); 3 = Célibataire, jamais marié(e); 5 = Veuf ou veuve; 9 = Refus (NA) | complète l’étude | La question demande l’état civil officiel, sans union de fait ; l’union civile (6) est marié(e). Les conjoints de fait pouvaient répondre célibataire ou union civile : 360 répondants (24 %) ont choisi l’union civile, bien plus que les unions civiles légales au Québec, si bien que les conjoints de fait se répartissent entre marié(e) et jamais marié(e). | approuvée |

### `employment` : Occupation

La situation d’emploi de la personne : en emploi, au chômage, retraitée,
étudiante ou autre.

Base : appariements souples seulement · une valeur par personne

**Comment elle est assouplie** : La situation d’emploi principale en
cinq groupes ; une personne qui donne deux situations, comme retraitée
et salariée, prend celle qui n’est pas l’emploi, et la personne au
foyer, inapte au travail ou dans une autre situation est classée autre.
Dans le panel de 2018, seuls les répondants web ont été interrogés sur
leur emploi, si bien que ses répondants téléphoniques apparaissent comme
« ne sait pas ou refus ».

**Niveaux**

| Code | Nom          | Étiquette                               |
|------|--------------|-----------------------------------------|
| 1    | `working`    | En emploi (salarié(e) ou à son compte)  |
| 2    | `unemployed` | Au chômage                              |
| 3    | `retired`    | Retraité(e)                             |
| 4    | `student`    | Étudiant(e)                             |
| 5    | `other`      | À la maison, inapte au travail ou autre |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `pes_employed` (pes) | 1-3 = En emploi (salarié(e) ou à son compte); 4, 11 = Retraité(e); 5 = Au chômage; 6, 9 = Étudiant(e); 7, 8, 10, 12 = À la maison, inapte au travail ou autre; -99 = Sans réponse (non-réponse partielle) (NA) | complète l’étude | Une personne qui a donné deux situations (étudiante et salariée, retraitée et salariée, au foyer et salariée) prend celle qui n’est pas l’emploi ; deux emplois est en emploi ; au foyer, handicapé et autres situations sont autre. Posée après l’élection : les répondants de la seule vague de campagne sont manquants (absents de cette vague). | approuvée |
| qes2018 | `qoccup` (post) | 1, 2, 8 = En emploi (salarié(e) ou à son compte); 3, 11 = Retraité(e); 4 = Au chômage; 5, 9 = Étudiant(e); 6, 7, 10, 96 = À la maison, inapte au travail ou autre; 99 = Refus (NA) | complète l’étude | Une personne qui a donné deux situations (étudiante et salariée, retraitée et salariée, au foyer et salariée) prend celle qui n’est pas l’emploi ; deux emplois est en emploi ; au foyer, handicapé et autres situations sont autre. | approuvée |
| qes2014 | `Q58` (post) | 1, 2, 8 = En emploi (salarié(e) ou à son compte); 3, 11 = Retraité(e); 4 = Au chômage; 5, 9 = Étudiant(e); 6, 7, 10, 96 = À la maison, inapte au travail ou autre; 99 = Refus (NA) | complète l’étude | Une personne qui a donné deux situations (étudiante et salariée, retraitée et salariée, au foyer et salariée) prend celle qui n’est pas l’emploi ; deux emplois est en emploi ; au foyer, handicapé et autres situations sont autre. | approuvée |
| qes2012 | `occup` (post) | 1, 2, 8 = En emploi (salarié(e) ou à son compte); 3, 11 = Retraité(e); 4 = Au chômage; 5, 9 = Étudiant(e); 6, 7, 10, 96 = À la maison, inapte au travail ou autre; 99 = Refus (NA) | complète l’étude | Une personne qui a donné deux situations (étudiante et salariée, retraitée et salariée, au foyer et salariée) prend celle qui n’est pas l’emploi ; deux emplois est en emploi ; au foyer, handicapé et autres situations sont autre. | approuvée |
| qes2018_panel | `d4` (pre) | 1-3 = En emploi (salarié(e) ou à son compte); 4 = Au chômage; 5 = Étudiant(e); 6 = Retraité(e); 7, 8 = À la maison, inapte au travail ou autre; 9 = Ne sait pas ou refus (un seul code) (NA) | complète l’étude | Temps plein, temps partiel et autonome sont en emploi ; à l’extérieur du marché du travail (au foyer) et autre sont autre. La question n’a été posée qu’aux 850 répondants web : le fichier code 9 (ne sait pas) pour les 400 répondants téléphoniques (method 1-2), si bien que 400 des 406 valeurs manquantes sont des questions non posées et non de vrais « ne sait pas » (6 répondants web ont choisi 9). | approuvée |
| qes2007_panel | `occup` (toute vague) | 1, 2 = En emploi (salarié(e) ou à son compte); 3 = Au chômage; 4 = À la maison, inapte au travail ou autre; 5 = Retraité(e); 6 = Étudiant(e); 9 = Refus (NA) | complète l’étude | Temps plein et temps partiel sont en emploi ; à la maison à temps plein est autre. | approuvée |
| qes2007 | `q79` (post) | 1, 2, 8 = En emploi (salarié(e) ou à son compte); 3, 11 = Retraité(e); 4 = Au chômage; 5, 9 = Étudiant(e); 6, 7, 10, 96 = À la maison, inapte au travail ou autre; 99 = Refus (NA) | complète l’étude | Une personne qui a donné deux situations (étudiante et salariée, retraitée et salariée, au foyer et salariée) prend celle qui n’est pas l’emploi ; deux emplois est en emploi ; au foyer, handicapé et autres situations sont autre. | approuvée |
| qes2008 | `q79` (post) | 1, 2 = En emploi (salarié(e) ou à son compte); 3 = Retraité(e); 4 = Au chômage; 5 = Étudiant(e); 6, 7, 96 = À la maison, inapte au travail ou autre; 99 = Refus (NA) | complète l’étude | Une seule situation par personne (le questionnaire de 2008 n’a pas de codes de double situation) : à son compte et salarié sont en emploi ; au foyer, handicapé et autre (préciser) sont autre. | approuvée |
| qes1998 | `occup` (pre) | 1-3 = Catégorie source à cheval sur plusieurs niveaux (NA) | complète l’étude | Le fichier regroupé a temps plein, temps partiel et ne travaille pas ; ne travaille pas réunit chômeurs, retraités, étudiants et personnes au foyer, si bien que l’étude est laissée de côté. | approuvée |
| qes_crop_2007_2010 | `Occup` (chaque sondage) | 1, 2 = En emploi (salarié(e) ou à son compte); 3 = Au chômage; 4 = À la maison, inapte au travail ou autre; 5 = Retraité(e); 6 = Étudiant(e); 9 = Refus (NA) | complète l’étude | Temps plein et temps partiel sont en emploi ; à la maison à temps plein est autre. | approuvée |

### `union` : Appartenance à un syndicat

Si la personne, ou dans certaines études quelqu’un de son ménage, est
syndiquée.

Base : appariements souples seulement · une valeur par personne

**Comment elle est assouplie** : Si la personne est syndiquée en 2022,
mais si elle-même ou quelqu’un de son ménage l’est en 2014 et en 2018
(en 2018, les personnes qui vivent chez leurs parents ont été
interrogées sur leur famille : parents, frères ou sœurs). Seules ces
trois études posent la question, et celle de 2022 la pose après
l’élection, si bien que ses répondants de la seule vague de campagne
sont manquants.

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `pes_union` (pes) | 1 = Oui; 2 = Non | complète l’étude | La personne elle-même est syndiquée (et non son ménage). Posée après l’élection : les répondants de la seule vague de campagne sont manquants (absents de cette vague). | approuvée |
| qes2018 | `q65a` (post) | 1 = Oui; 2 = Non; 98 = Ne sait pas (NA); 99 = Refus (NA); Sans objet (écarté par un filtre) (NA), Valeur manquante système (NA) passent à q65b: 1 = Oui; 2 = Non; 98 = Ne sait pas (NA); 99 = Refus (NA) | complète l’étude | Q65 a deux versions, selon QPARENTS : les personnes qui ne vivent pas chez leurs parents (q65a, 2 558 répondants) ont été interrogées sur leur ménage, celles qui y vivent (q65b, 514) sur leur famille (parents, frères ou sœurs). La version du ménage est lue d’abord, puis celle de la famille pour les personnes à qui la première n’a pas été posée. La personne ou quelqu’un de son ménage (ou de sa famille) est syndiqué(e). | approuvée |
| qes2014 | `Q61` (post) | 1 = Oui; 2 = Non; 9 = Refus (NA) | complète l’étude | La personne ou quelqu’un de son ménage est syndiqué(e). | approuvée |

### `region` : Région

Où vit la personne : RMR de Montréal, RMR de Québec ou reste du Québec.

Base : `target:region_cma3` · une valeur par personne

**Comment elle est assouplie** : La région métropolitaine de recensement
de Montréal, celle de Québec ou le reste du Québec, d’après la variable
de région ou de sous-région de chaque étude, avec les limites qu’elle a
utilisées.

**Niveaux**

| Code | Nom          | Étiquette       |
|------|--------------|-----------------|
| 1    | `mtl_cma`    | RMR de Montréal |
| 2    | `quebec_cma` | RMR de Québec   |
| 3    | `rest`       | Reste du Québec |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2007_panel | `reg2` (toute vague) | 1-3, 5-11, 17-19, 21, 22 = Reste du Québec; 4, 20 = RMR de Québec; 12-16 = RMR de Montréal | complète l’étude | Sous-régions : les cinq parties de la RMR de Montréal (dans Lanaudière, les Laurentides, Laval, la Montérégie et Montréal) sont la RMR de Montréal, et les parties de la RMR de Québec dans la Capitale-Nationale et Chaudière-Appalaches sont la RMR de Québec. | approuvée |

### `region_admin` : Région administrative

La région administrative où vit la personne.

Base : appariements souples seulement · une valeur par personne

**Comment elle est assouplie** : Les 17 régions administratives du
Québec, là où une étude les a enregistrées ou des sous-régions qui s’y
emboîtent ; les sous-régions de l’étude de 2007 et du panel de 2007 qui
divisent une région sont réunies.

**Niveaux**

| Code | Nom                       | Étiquette                     |
|------|---------------------------|-------------------------------|
| 1    | `bas_saint_laurent`       | Bas-Saint-Laurent             |
| 2    | `saguenay_lac_saint_jean` | Saguenay–Lac-Saint-Jean       |
| 3    | `capitale_nationale`      | Capitale-Nationale            |
| 4    | `mauricie`                | Mauricie                      |
| 5    | `estrie`                  | Estrie                        |
| 6    | `montreal`                | Montréal                      |
| 7    | `outaouais`               | Outaouais                     |
| 8    | `abitibi_temiscamingue`   | Abitibi-Témiscamingue         |
| 9    | `cote_nord`               | Côte-Nord                     |
| 10   | `nord_du_quebec`          | Nord-du-Québec                |
| 11   | `gaspesie_iles`           | Gaspésie–Îles-de-la-Madeleine |
| 12   | `chaudiere_appalaches`    | Chaudière-Appalaches          |
| 13   | `laval`                   | Laval                         |
| 14   | `lanaudiere`              | Lanaudière                    |
| 15   | `laurentides`             | Laurentides                   |
| 16   | `monteregie`              | Montérégie                    |
| 17   | `centre_du_quebec`        | Centre-du-Québec              |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2018 | `q0qc` (post) | 1 = Bas-Saint-Laurent; 2 = Saguenay–Lac-Saint-Jean; 3 = Capitale-Nationale; 4 = Mauricie; 5 = Estrie; 6 = Montréal; 7 = Outaouais; 8 = Abitibi-Témiscamingue; 9 = Côte-Nord; 10 = Nord-du-Québec; 11 = Gaspésie–Îles-de-la-Madeleine; 12 = Chaudière-Appalaches; 13 = Laval; 14 = Lanaudière; 15 = Laurentides; 16 = Montérégie; 17 = Centre-du-Québec | complète l’étude |  | approuvée |
| qes2014 | `QREGION` (post) | 1 = Bas-Saint-Laurent; 2 = Saguenay–Lac-Saint-Jean; 3 = Capitale-Nationale; 4 = Mauricie; 5 = Estrie; 6 = Montréal; 7 = Outaouais; 8 = Abitibi-Témiscamingue; 9 = Côte-Nord; 10 = Nord-du-Québec; 11 = Gaspésie–Îles-de-la-Madeleine; 12 = Chaudière-Appalaches; 13 = Laval; 14 = Lanaudière; 15 = Laurentides; 16 = Montérégie; 17 = Centre-du-Québec | complète l’étude |  | approuvée |
| qes2012 | `q0qc` (post) | 1 = Bas-Saint-Laurent; 2 = Saguenay–Lac-Saint-Jean; 3 = Capitale-Nationale; 4 = Mauricie; 5 = Estrie; 6 = Montréal; 7 = Outaouais; 8 = Abitibi-Témiscamingue; 9 = Côte-Nord; 10 = Nord-du-Québec; 11 = Gaspésie–Îles-de-la-Madeleine; 12 = Chaudière-Appalaches; 13 = Laval; 14 = Lanaudière; 15 = Laurentides; 16 = Montérégie; 17 = Centre-du-Québec | complète l’étude |  | approuvée |
| qes2007_panel | `reg2` (toute vague) | 1 = Abitibi-Témiscamingue; 2 = Bas-Saint-Laurent; 3, 4 = Chaudière-Appalaches; 5 = Côte-Nord; 6 = Estrie; 7 = Gaspésie–Îles-de-la-Madeleine; 8, 12 = Lanaudière; 9, 13 = Laurentides; 10 = Mauricie; 11, 15 = Montérégie; 14 = Laval; 16 = Montréal; 17 = Nord-du-Québec; 18 = Outaouais; 19, 20 = Capitale-Nationale; 21 = Saguenay–Lac-Saint-Jean; 22 = Centre-du-Québec | complète l’étude | Les sous-régions qui divisent une région (sa partie dans une RMR et le reste) sont réunies. | approuvée |
| qes2007 | `nomx` (post) | 1 = Bas-Saint-Laurent; 2 = Saguenay–Lac-Saint-Jean; 3, 33 = Capitale-Nationale; 4 = Mauricie; 5 = Estrie; 6 = Montréal; 7 = Outaouais; 8 = Abitibi-Témiscamingue; 9 = Côte-Nord; 11 = Gaspésie–Îles-de-la-Madeleine; 12, 32 = Chaudière-Appalaches; 13 = Laval; 14, 24 = Lanaudière; 15, 25 = Laurentides; 16, 26 = Montérégie; 17 = Centre-du-Québec | complète l’étude | Les sous-régions qui divisent une région (sa partie dans une RMR et le reste) sont réunies ; le Nord-du-Québec n’a pas de code. | approuvée |

### `born_canada` : Né(e) au Canada

Si la personne est née au Canada.

Base : `target:born_canada` · une valeur par personne

**Comment elle est assouplie** : Né(e) au Canada ou non, tel que chaque
étude l’a demandé : d’après le lieu de naissance (Québec, ailleurs au
Canada ou à l’étranger), ou demandé directement en 2022.

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `born_quebec` : Né(e) au Québec

Si la personne est née au Québec.

Base : `target:birthplace3` ·
`recode:quebec=yes,other_canada=no,abroad=no` · une valeur par personne

**Comment elle est assouplie** : Né(e) au Québec ou non, d’après la
question sur le lieu de naissance (Québec, ailleurs au Canada ou à
l’étranger) ; 2022 n’a demandé que si la personne est née au Canada, si
bien qu’elle y est manquante.

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `vote_choice` : Choix de vote provincial

Le parti du vote de la personne à l’élection générale québécoise de
l’étude.

Base : `pooled:vote_choice` · sur la vague qui l’a posée

**Comment elle est assouplie** : Le vote déclaré là où une étude l’a
demandé, sinon l’intention de vote avec les indécis relancés vers le
parti dont ils se sentent proches, sinon la première question
d’intention (la variable regroupée vote_choice) ; vote_type dit
laquelle, et les partis sont ceux que chaque étude offrait.

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

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `vote_type` : Question du choix de vote

La question de vote_choice sur la ligne.

Base : `pooled:vote_choice__type` · sur la vague qui l’a posée

**Comment elle est assouplie** : La question d’où vient vote_choice sur
chaque ligne : le vote déclaré, l’intention relancée ou la première
question d’intention.

**Niveaux**

| Code | Nom              | Étiquette                             |
|------|------------------|---------------------------------------|
| 1    | `recall`         | Vote déclaré (après l’élection)       |
| 2    | `intention_push` | Intention de vote (indécis relancés)  |
| 3    | `intention`      | Intention de vote (première question) |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `turnout` : Participation (déclarée)

Si la personne a voté à l’élection générale québécoise de l’étude.

Base : `pooled:turnout` · sur la vague qui l’a posée

**Comment elle est assouplie** : Si la personne dit avoir voté à
l’élection générale québécoise de l’étude, question posée après
l’élection ; les libellés et les choix de réponse diffèrent, certains
offrant plusieurs façons de ne pas avoir voté.

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `vote_prev` : Vote à l’élection provinciale précédente

Le parti pour lequel la personne a voté à l’élection générale québécoise
précédente.

Base : `target:vote_prov_prev` · sur la vague qui l’a posée

**Comment elle est assouplie** : Le parti du vote déclaré à l’élection
générale québécoise qui précède l’étude ; les abstentionnistes sont
manquants, les sources nomment l’élection rappelée, et la question de
1998 ne nomme que le PLQ et le PQ.

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

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2007_panel | `voteprec` (pre) | 1 = ADQ; 2 = PLQ; 3 = PQ; 4 = Autre parti; 6 = N’a pas voté (NA); 8 = Ne sait pas (NA); 9 = Refus (NA) | complète l’étude | L’élection d’avril 2003 (QC2003) ; posée au deuxième sondage préélectoral seulement, si bien que les autres répondants sont des valeurs manquantes système. | approuvée |
| qes1998 | `vote94` (pre) | 1 = PLQ; 2 = PQ; 3 = N’a pas voté (NA); 4 = Ne sait pas ou refus (un seul code) (NA); 9 = Catégorie source à cheval sur plusieurs niveaux (NA) | complète l’étude | L’élection de 1994 (QC1994). Le fichier regroupé ne nomme que le PLQ et le PQ ; le code 9, sans étiquette, réunit les autres partis (dont l’ADQ) et des réponses inconnues, si bien que les électeurs de l’ADQ en 1994 sont manquants. | approuvée |
| qes_crop_2007_2010 | `QP4` (chaque sondage) | 1 = ADQ; 2 = PLQ; 3 = PQ; 4 = QS; 5 = PVQ; 6 = Autre parti; 7 = N’a pas voté (NA); 9 = Ne sait pas ou refus (un seul code) (NA) | complète l’étude | La dernière élection québécoise avant chaque sondage : QC2007 pour les sondages de juin 2007 à novembre 2008, QC2008 à partir de janvier 2009. La question originale (QP4) ; le code 7 réunit n’a pas voté et a annulé. | approuvée |

### `pid` : Identification partisane provinciale

Le parti québécois auquel la personne s’identifie.

Base : `target:pid_prov` · sur la vague qui l’a posée

**Comment elle est assouplie** : Le parti québécois auquel la personne
s’identifie, ou aucun, tel que chaque étude l’a demandé ; les partis
offerts diffèrent d’une étude à l’autre.

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

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `lr` : Autopositionnement gauche-droite (0-10)

La position de la personne sur une échelle gauche-droite.

Base : `target:lr_self` · sur la vague qui l’a posée

**Comment elle est assouplie** : L’autopositionnement sur une échelle
gauche-droite de 0 (gauche) à 10 (droite) ; une échelle d’une autre
longueur serait ramenée à 0-10, et toutes les études qui en ont posé une
allaient de 0 à 10.

**Plage valide** : 0-10

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `interest` : Intérêt pour la politique

Le degré d’intérêt de la personne pour la politique.

Base : `pooled:pol_interest` · `bands:0.55,0.85:low,medium,high` · sur
la vague qui l’a posée

**Comment elle est assouplie** : L’intérêt de 0 à 1 (interest_01) en
trois tranches, faible sous 0,55 et élevé à partir de 0,85 : à une
question de 0 à 10, 0 à 5 est faible, 6 à 8 moyen et 9 ou 10 élevé ; à
une question à quatre points, pas du tout ou peu intéressé est faible,
assez intéressé moyen et très intéressé élevé. Les deux formats ne
concordent toujours pas exactement, et l’étude de 2008 et le panel de
2007 ont demandé l’intérêt pour l’élection ou la campagne, pas pour la
politique.

**Niveaux**

| Code | Nom      | Étiquette |
|------|----------|-----------|
| 1    | `low`    | Faible    |
| 2    | `medium` | Moyen     |
| 3    | `high`   | Élevé     |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `interest_01` : Intérêt pour la politique (0-1)

Le degré d’intérêt de la personne pour la politique, de 0 à 1.

Base : `pooled:pol_interest` · sur la vague qui l’a posée

**Comment elle est assouplie** : L’intérêt de 0 à 1 (la variable
regroupée pol_interest) : réponses à quatre points notées 1, 0,7, 0,3 et
0, et réponses de 0 à 10 divisées par 10.

**Plage valide** : 0-1

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `sovereignty` : Vote référendaire sur la souveraineté

Comment la personne voterait à un référendum sur la souveraineté du
Québec.

Base : `pooled:sov_support` ·
`recode:yes=yes,no=no,would_not_vote=NA:not_voted` · sur la vague qui
l’a posée

**Comment elle est assouplie** : Oui ou non à un référendum sur la
souveraineté du Québec, quelle que soit la question (un pays
indépendant, un pays souverain, la question de 1995 ou être favorable à
l’indépendance) ; sovereignty_type dit laquelle, et ne voterait pas est
manquant.

**Niveaux**

| Code | Nom   | Étiquette |
|------|-------|-----------|
| 1    | `yes` | Oui       |
| 2    | `no`  | Non       |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes_crop_2007_2010 | `intvoterefa` (chaque sondage) | 1 = Oui; 2 = Non; 3 = Catégorie source à cheval sur plusieurs niveaux (NA); 8 = Ne sait pas (NA); 9 = Refus (NA); Ne sait pas (NA) passent à intvoterefb: 1 = Oui; 2 = Non; 3 = Catégorie source à cheval sur plusieurs niveaux (NA); 8 = Ne sait pas (NA); 9 = Refus (NA) | complète l’étude | La question référendaire des sondages CROP. Le fichier tronque son étiquette et le livre de codes déposé n’en dit pas plus. Les rapports de CROP donnent le libellé pour 19 des 24 sondages : un Québec qui « devienne un pays souverain ». Il n’est pas documenté pour les cinq autres ; en avril 2009, CROP a posé une question sur le pays souverain et une autre sur la souveraineté-partenariat, et pour mai 2009 la presse ne dit pas laquelle a donné le résultat publié. Pour ceux qui ne savaient pas, la relance (intvoterefb) est utilisée. Elle a aussi été posée à ceux qui ne voteraient pas ou refusaient, dont la première réponse est gardée. Ne voterait pas est manquant. | approuvée |

### `sovereignty_type` : Question du vote sur la souveraineté

La question de sovereignty sur la ligne.

Base : `pooled:sov_support__type` ·
`recode:independence=independence,sovereign_country=sovereign_country,partnership_1995_push=partnership_1995,partnership_1995=partnership_1995,favour=favour`
· sur la vague qui l’a posée

**Comment elle est assouplie** : La question d’où vient sovereignty sur
chaque ligne : un pays indépendant, un pays souverain, la question de
1995, être favorable à l’indépendance, ou la question des sondages CROP,
dont le libellé complet n’a pas été déposé.

**Niveaux**

| Code | Nom                 | Étiquette                                       |
|------|---------------------|-------------------------------------------------|
| 1    | `independence`      | Pays indépendant                                |
| 2    | `sovereign_country` | Pays souverain                                  |
| 3    | `partnership_1995`  | Question de 1995 (souveraineté-partenariat)     |
| 4    | `favour`            | Favorable à l’indépendance                      |
| 5    | `crop_undocumented` | Question référendaire CROP (libellé non déposé) |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes_crop_2007_2010 | `intvoterefa` (chaque sondage) | chaque personne = Question référendaire CROP (libellé non déposé) | complète l’étude | Chaque répondant des sondages CROP ; sovereignty dit s’il a répondu. | approuvée |

### `satis_democracy` : Satisfaction envers la démocratie au Québec

Le degré de satisfaction de la personne envers le fonctionnement de la
démocratie au Québec.

Base : `target:satis_demo_qc` · sur la vague qui l’a posée

**Comment elle est assouplie** : La satisfaction envers le
fonctionnement de la démocratie au Québec, en quatre points, telle que
chaque étude l’a demandée.

**Niveaux**

| Code | Nom          | Étiquette                |
|------|--------------|--------------------------|
| 1    | `very`       | Très satisfait(e)        |
| 2    | `fairly`     | Assez satisfait(e)       |
| 3    | `not_very`   | Pas très satisfait(e)    |
| 4    | `not_at_all` | Pas du tout satisfait(e) |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `gov_satisfaction` : Satisfaction envers le gouvernement du Québec

Le degré de satisfaction de la personne envers le gouvernement du
Québec.

Base : `target:gov_satisfaction` · sur la vague qui l’a posée

**Comment elle est assouplie** : La satisfaction envers le gouvernement
du Québec en place, en quatre points ; la question de 1998 porte sur le
gouvernement Bouchard, dans ses propres mots.

**Niveaux**

| Code | Nom          | Étiquette                |
|------|--------------|--------------------------|
| 1    | `very`       | Très satisfait(e)        |
| 2    | `fairly`     | Assez satisfait(e)       |
| 3    | `not_very`   | Pas très satisfait(e)    |
| 4    | `not_at_all` | Pas du tout satisfait(e) |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes1998 | `satisf` (pre) | 1 = Très satisfait(e); 2 = Assez satisfait(e); 3 = Pas très satisfait(e); 4 = Pas du tout satisfait(e); 9 = Ne sait pas ou refus (un seul code) (NA) | complète l’étude | La satisfaction envers le gouvernement Bouchard (PQ), dans les mots du fichier regroupé : très satisfait, plutôt satisfait, plutôt insatisfait, très insatisfait ; le code 9, sans étiquette, est lu comme ne sait pas ou refus. | approuvée |

### `econ_retro` : L’économie du Québec depuis un an

L’avis de la personne sur l’économie du Québec depuis un an.

Base : `target:econ_retro_qc` · sur la vague qui l’a posée

**Comment elle est assouplie** : Si l’économie du Québec s’est
améliorée, est restée à peu près la même ou s’est détériorée depuis un
an, tel que chaque étude l’a demandé.

**Niveaux**

| Code | Nom      | Étiquette          |
|------|----------|--------------------|
| 1    | `better` | Améliorée          |
| 2    | `same`   | À peu près la même |
| 3    | `worse`  | Détériorée         |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `econ_self` : Situation financière personnelle depuis un an

L’avis de la personne sur sa propre situation financière depuis un an.

Base : appariements souples seulement · sur la vague qui l’a posée

**Comment elle est assouplie** : Si la situation financière de la
personne s’est améliorée, est restée à peu près la même ou s’est
détériorée depuis un an. Seule qes2022 pose la question, pendant la
campagne ; la colonne est conservée pour les analyses à l’intérieur de
cette étude (essentielle), comme econ_self dans cesR, même si elle
compte moins de trois études. La question de 2012 et de 2014 sur sa
propre situation financière si le Québec devenait indépendant est une
autre question et n’est pas utilisée.

**Niveaux**

| Code | Nom      | Étiquette          |
|------|----------|--------------------|
| 1    | `better` | Améliorée          |
| 2    | `same`   | À peu près la même |
| 3    | `worse`  | Détériorée         |

Appariements par étude

| Étude | Source | Recodage | Usage | Notes | statut |
|----|----|----|----|----|----|
| qes2022 | `cps_ownfin` (cps) | 1 = Améliorée; 2 = Détériorée; 3 = À peu près la même | complète l’étude | Posée pendant la campagne à chaque personne ; le fichier n’a aucune valeur manquante. Ce n’est pas la question de 2012 et de 2014 sur sa propre situation financière si le Québec devenait indépendant (q85, Q47), qui porte sur une autre situation. | approuvée |

### `identity` : Identité québécoise et canadienne

Comment la personne se définit, comme Québécoise, Canadienne ou les
deux.

Base : `target:identity_qc_ca` · sur la vague qui l’a posée

**Comment elle est assouplie** : Uniquement québécois(e), d’abord
québécois(e), les deux également, d’abord canadien(ne) ou uniquement
canadien(ne), d’après une question ou les deux ordres d’un questionnaire
partagé.

**Niveaux**

| Code | Nom        | Étiquette                               |
|------|------------|-----------------------------------------|
| 1    | `qc_only`  | Uniquement québécois(e)                 |
| 2    | `qc_first` | D’abord québécois(e), puis canadien(ne) |
| 3    | `equal`    | Également québécois(e) et canadien(ne)  |
| 4    | `ca_first` | D’abord canadien(ne), puis québécois(e) |
| 5    | `ca_only`  | Uniquement canadien(ne)                 |
| 90   | `other`    | Autre                                   |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `attach_quebec` : Attachement au Québec

Le degré d’attachement de la personne au Québec.

Base : `target:attach_qc` · sur la vague qui l’a posée

**Comment elle est assouplie** : L’attachement au Québec en quatre
points, tel que chaque étude l’a demandé.

**Niveaux**

| Code | Nom          | Étiquette              |
|------|--------------|------------------------|
| 1    | `very`       | Très attaché(e)        |
| 2    | `fairly`     | Assez attaché(e)       |
| 3    | `not_very`   | Pas très attaché(e)    |
| 4    | `not_at_all` | Pas du tout attaché(e) |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.

### `attach_canada` : Attachement au Canada

Le degré d’attachement de la personne au Canada.

Base : `target:attach_ca` · sur la vague qui l’a posée

**Comment elle est assouplie** : L’attachement au Canada en quatre
points, tel que chaque étude l’a demandé.

**Niveaux**

| Code | Nom          | Étiquette              |
|------|--------------|------------------------|
| 1    | `very`       | Très attaché(e)        |
| 2    | `fairly`     | Assez attaché(e)       |
| 3    | `not_very`   | Pas très attaché(e)    |
| 4    | `not_at_all` | Pas du tout attaché(e) |

**Appariements souples**

Aucun : les valeurs de chaque étude viennent de la base.
