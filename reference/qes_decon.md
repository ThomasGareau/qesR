# One flat data frame of relaxed harmonized variables (experimental)

`qes_decon()` returns one data frame for every harmonized Quebec
Election Study, with one row per respondent and wave and one column per
concept, under plain names in the style of the cesR package
(`education`, `income_cat`, `vote_choice`, `sovereignty`, `lr`...). It
is a relaxed harmonization: one concept goes in one column for every
study, even when the wording or the answer options differ, with coarse
common categories (education in four groups, income in thirds of each
study's respondents, interest low, medium or high, a referendum vote of
yes or no whatever the question). It trades exactness for coverage: use
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
and its grades when the difference between two questions matters.

## Usage

``` r
qes_decon(studies = NULL, lang = c("en", "fr"), weights = TRUE, quiet = FALSE)
```

## Arguments

- studies:

  `NULL` (default) or `"all"`: every harmonized study (the 11 Quebec
  studies; the 1998 firms' own files are in `qes1998`). Otherwise study
  codes, as
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
  lists them; `"qes_demo"`, the synthetic demonstration study, is
  accepted and needs no download.

- lang:

  Language of the factor levels, of the `label`, `relaxed` and `sources`
  attributes and of `decon_sources`: `"en"` (default) or `"fr"`. Column
  names do not change.

- weights:

  If `TRUE` (default), adds `weight` and `weight_var`.

- quiet:

  If `TRUE`, no download, cache or summary messages.

## Value

A data frame (no class of its own), returned visibly, with the columns
`study`, `year` (the study's year; for the CROP polls, the year of the
respondent's poll), `wave`, `qes_id` (`<study>:<identifier>`), then
`weight` and `weight_var` (with `weights = TRUE`), then the relaxed
columns listed under *Columns*: factors (ordered for ordinal ones) with
every category as a level, or numbers. Each relaxed column carries
`attr(, "label")`, `attr(, "relaxed")` and `attr(, "sources")` (a data
frame: `study`, `wave`, `source_var`, `wording_ref`, `recode`). The data
frame carries `decon_sources` (`column`, `study`, `wave`, `base`,
`source_var`, `wording_ref`, `recode`, `relaxed` (`TRUE` where a relaxed
mapping or a collapse was used), `status` and `applied` of the source
row, `n_value` (respondents of a static column, rows of a wave column)
and `na_reasons` (`"dk=12; refused=3"`)), `weights` (`study`, `wave`,
`weight_var`, `status`, `reason`: `NA`, `"needs_review"`,
`"no_recommended_weight"` or `"not_in_data"`, `n_rows`, `n_weight`,
`mean`), `qes_provenance` (the files read; see
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)),
`spec_version`, `qesR_version` and `lang`.

## Details

Each column says how it was relaxed (`attr(, "relaxed")`, one sentence)
and where each study's values come from (`attr(, "sources")`: the source
variable, the document that gives its wording and the rule that turns
its codes into the column's categories). `attr(x, "decon_sources")`
gathers them for every column, with the count of values and of each
reason for a missing value. Relaxed columns carry no comparability
grade, and none of them claims that two studies asked the same question.

A column is built from a strict target or a pooled variable of the
harmonization spec where one exists (`gender`, `vote_choice`,
`satis_democracy`...), recoded into the column's categories where needed
(`born_quebec` from the birthplace, `interest` from the 0-1 interest
scale), and from relaxed mappings of the studies' own questions where
the strict layer has none or keeps a study out (`education`,
`income_cat`, `religion`, `employment`...). The relaxed mappings are
reviewed like the strict ones: a mapping not yet signed off (status
`review`) is not applied, and its cells are `NA` with reason
`not_reviewed` (see `qes_spec("relaxed_maps")`); a message says how
many. The 60 mappings of spec 4.5.0 are signed off (status `stable`) by
an automated double review against the original files and documents, not
a human review, as their `reviewed_by` and `review_note` say.

## Rows, waves and weights

The rows are those of `qes_harmonize(layout = "long")`: one per
respondent and wave they took part in, so a panel respondent has one row
per wave, and no row is dropped. Socio-demographic columns (education,
income, language, religion, region...) are the same on every row of a
respondent; the vote, turnout and attitudes sit on the row of the wave
that asked them (the vote intention on the campaign wave, the reported
vote on the post-election wave). With `weights = TRUE`, `weight` is the
wave's recommended weight rescaled to mean 1 in each study and wave, and
`weight_var` its source variable. Both are `NA` where the study has no
recommended weight (`qes2008`: its weights are calibrated on the
reported vote) or where the weight still needs review;
`attr(x, "weights")` says which, study by study and wave by wave.
Estimate within one study and wave, or with
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
on
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
output.

## Correspondence with cesR

cesR's
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md)
returns 21 columns of the 2019 Canadian Election Study. Their
counterparts here: `citizenship`, `yob`, `gender`, `education`, `lr`
(cesR's `lr`, `lr_bef` and `lr_aft`: no study asks the scale both before
and after the election), `religion`, `language`, `language_eng` and
`language_fr` (no study asks about Indigenous languages), `employment`,
`income_cat` (the income amount is not comparable across studies),
`marital` and `econ_retro` (Quebec's economy). `province_territory` is
dropped (every respondent lives in Quebec), and so are sexual
orientation, the federal economy and personal finances (one study at
most).

## Columns

The relaxed columns of spec 4.5.0, in order (`qes_spec("relaxed")` lists
them with their French labels):

- `citizenship`:

  Citizenship (one value per respondent, target:citizen). Canadian
  citizenship where a study recorded it: asked directly in 2022, and
  taken in the 2018 panel from the screening question on the right to
  vote in the coming Quebec election, so that every respondent of that
  panel is a citizen. Levels: Canadian citizen; Not a Canadian citizen.

- `yob`:

  Year of birth (one value per respondent, target:birth_year). The year
  of birth as reported; the studies that asked only an age group have
  none (see age_group). A number from 1900 to 2010.

- `age_group`:

  Age group (one value per respondent, target:age_group3). Three age
  groups (18-34, 35-54, 55 and over), from the study's own age bands or
  from the age at the start of fieldwork; respondents under 18 are
  missing. Levels: 18-34; 35-54; 55 and over.

- `gender`:

  Gender (one value per respondent, target:gender). Gender or sex as
  each study asked it: man or woman everywhere, and in 2022 also
  non-binary or another gender; the questions differ, the categories do
  not. Levels: Man; Woman; Non-binary; Another gender.

- `education`:

  Education (one value per respondent, relaxed mappings only). Each
  study's levels are grouped into four: no high school diploma, high
  school, college (CEGEP, technical or trade school) and university,
  including some university; the 2007 panel and the CROP polls asked
  years of schooling, so their high school group also holds those who
  left secondary school without a diploma, and their college group may
  hold some respondents with some university but no degree. Levels: No
  high school diploma; High school diploma; College, CEGEP or trade
  school; University.

- `income_cat`:

  Household income (thirds) (one value per respondent, relaxed mappings
  only). Household income in thirds of each study's own respondents:
  income brackets are never split, so a third holds the whole brackets
  whose midpoint falls in it and is rarely exactly a third; the dollar
  limits differ from study to study. Levels: Low (bottom third); Middle;
  High (top third).

- `language`:

  Mother tongue (one value per respondent, target:lang_mother). The
  language first learned in childhood, in three groups; a respondent who
  reported French and another language is French, and English and a
  language other than French is English, and the 1998 CREATEC
  respondents are French by the design of their sample. Levels: French;
  English; Other.

- `language_fr`:

  French as a mother tongue (one value per respondent, column:language).
  Yes when French is among the mother tongues reported, so a respondent
  with two mother tongues can be yes in both language_fr and
  language_eng. Levels: Yes; No.

- `language_eng`:

  English as a mother tongue (one value per respondent,
  column:language). Yes when English is among the mother tongues
  reported, so a respondent with two mother tongues can be yes in both
  language_eng and language_fr. Levels: Yes; No.

- `religion`:

  Religion (one value per respondent, relaxed mappings only). The
  religion the respondent belongs to, in five groups; the 2022 question
  offers a long list, where agnostic counts as no religion, and the
  other studies first ask whether the respondent belongs to a religion
  at all. Levels: Catholic; Protestant; Other Christian; Other religion;
  No religion.

- `marital`:

  Marital status (one value per respondent, relaxed mappings only).
  Married or living with a partner, separated or divorced, widowed, or
  never married; 2012 and 2014 asked the official civil status, with no
  common-law option, so some partners who live together are never
  married there. Levels: Married or living with a partner; Separated or
  divorced; Widowed; Single, never married.

- `employment`:

  Employment (one value per respondent, relaxed mappings only). The main
  employment status in five groups; a respondent who gave two statuses,
  such as retired and working, takes the one that is not work, and at
  home, unable to work and other statuses are other. Levels: Working
  (employee or self-employed); Unemployed; Retired; Student; At home,
  unable to work or other.

- `region`:

  Region (one value per respondent, target:region_cma3). The Montreal
  census metropolitan area, the Quebec City census metropolitan area or
  the rest of Quebec, from each study's region or sub-region variable,
  with the boundaries each study used. Levels: Montreal CMA; Quebec CMA;
  Rest of Quebec.

- `region_admin`:

  Administrative region (one value per respondent, relaxed mappings
  only). The 17 administrative regions of Quebec, where a study recorded
  them or sub-regions that fit within them; the sub-regions of the 2007
  study and the 2007 panel that split a region are joined. Levels:
  Bas-Saint-Laurent; Saguenay–Lac-Saint-Jean; Capitale-Nationale;
  Mauricie; Estrie; Montréal; Outaouais; Abitibi-Témiscamingue;
  Côte-Nord; Nord-du-Québec; Gaspésie–Îles-de-la-Madeleine;
  Chaudière-Appalaches; Laval; Lanaudière; Laurentides; Montérégie;
  Centre-du-Québec.

- `born_canada`:

  Born in Canada (one value per respondent, target:born_canada). Born in
  Canada or not, as each study asked it: from the birthplace (Quebec,
  elsewhere in Canada or abroad), or asked directly in 2022. Levels:
  Yes; No.

- `born_quebec`:

  Born in Quebec (one value per respondent, target:birthplace3). Born in
  Quebec or not, from the birthplace question (Quebec, elsewhere in
  Canada or abroad); 2022 asked only whether the respondent was born in
  Canada, so it is missing there. Levels: Yes; No.

- `vote_choice`:

  Provincial vote choice (on the wave that asked it,
  pooled:vote_choice). The reported vote where a study asked it, else
  the vote intention with the undecided pushed toward the party they
  lean to, else the first intention question (the pooled vote_choice);
  vote_type says which, and the parties are those each study offered.
  Levels: PLQ; PQ; CAQ; QS; PVQ; PCQ; ON; ADQ; Other party; Would not
  vote / none / would spoil.

- `vote_type`:

  Question of the vote choice (on the wave that asked it,
  pooled:vote_choice\_\_type). Which question vote_choice comes from on
  each row: the reported vote, the pushed intention or the first
  intention question. Levels: Reported vote (after the election); Vote
  intention (undecided pushed); Vote intention (first question).

- `turnout`:

  Turnout (reported) (on the wave that asked it, pooled:turnout).
  Whether the respondent says they voted in the Quebec general election
  of the study, asked after it; the wordings and answer options differ,
  some offering several ways of not voting. Levels: Yes; No.

- `vote_prev`:

  Vote at the previous provincial election (on the wave that asked it,
  target:vote_prov_prev). The party of the reported vote at the Quebec
  general election before the study; those who did not vote are missing,
  the sources name the election recalled, and the 1998 question names
  only the PLQ and the PQ. Levels: PLQ; PQ; CAQ; QS; PVQ; PCQ; ON; ADQ;
  Other party.

- `pid`:

  Provincial party identification (on the wave that asked it,
  target:pid_prov). The Quebec party the respondent identifies with, or
  none, as each study asked it; the parties offered differ from study to
  study. Levels: PLQ; PQ; CAQ; QS; PVQ; PCQ; ON; ADQ; Other party; None
  of these.

- `lr`:

  Left-right self-placement (0-10) (on the wave that asked it,
  target:lr_self). Self-placement on a left-right scale from 0 (left) to
  10 (right); a scale of another length would be rescaled to 0-10, and
  every study that asked one used 0 to 10. A number from 0 to 10.

- `interest`:

  Interest in politics (on the wave that asked it, pooled:pol_interest).
  Interest on 0 to 1 (interest_01) in three bands, low below 0.35 and
  high from 0.75; the 4-point and 0-10 questions do not line up, and the
  2008 study and the 2007 panel asked interest in the election or the
  campaign, not in politics. Levels: Low; Medium; High.

- `interest_01`:

  Interest in politics (0-1) (on the wave that asked it,
  pooled:pol_interest). Interest on 0 to 1 (the pooled pol_interest):
  four-point answers scored 1, 0.7, 0.3 and 0, and 0-10 answers divided
  by 10. A number from 0 to 1.

- `sovereignty`:

  Sovereignty referendum vote (on the wave that asked it,
  pooled:sov_support). Yes or no in a referendum on Quebec sovereignty,
  whatever the question (an independent country, a sovereign country,
  the 1995 question or being favourable to independence);
  sovereignty_type says which, and would not vote is missing. Levels:
  Yes; No.

- `sovereignty_type`:

  Question of the sovereignty vote (on the wave that asked it,
  pooled:sov_support\_\_type). Which question sovereignty comes from on
  each row: an independent country, a sovereign country, the 1995
  question, being favourable to independence, or the CROP polls'
  question, whose full wording was not deposited. Levels: Independent
  country; Sovereign country; 1995 question (sovereignty-partnership);
  Favourable to independence; CROP referendum question (wording not
  deposited).

- `satis_democracy`:

  Satisfaction with democracy in Quebec (on the wave that asked it,
  target:satis_demo_qc). Satisfaction with the way democracy works in
  Quebec, on four points, as each study asked it. Levels: Very
  satisfied; Fairly satisfied; Not very satisfied; Not at all satisfied.

- `gov_satisfaction`:

  Satisfaction with the Quebec government (on the wave that asked it,
  target:gov_satisfaction). Satisfaction with the Quebec government of
  the day, on four points; the 1998 question asks about the Bouchard
  government, in its own words. Levels: Very satisfied; Fairly
  satisfied; Not very satisfied; Not at all satisfied.

- `econ_retro`:

  Quebec's economy over the past year (on the wave that asked it,
  target:econ_retro_qc). Whether Quebec's economy got better, stayed
  about the same or got worse over the past year, as each study asked
  it. Levels: Better; About the same; Worse.

- `identity`:

  Québécois and Canadian identity (on the wave that asked it,
  target:identity_qc_ca). Québécois only, Québécois first, both equally,
  Canadian first or Canadian only, from one question or from the two
  orders of a split ballot. Levels: Québécois only; Québécois first,
  then Canadian; Equally Québécois and Canadian; Canadian first, then
  Québécois; Canadian only; Other.

- `attach_quebec`:

  Attachment to Quebec (on the wave that asked it, target:attach_qc).
  Attachment to Quebec on four points, as each study asked it. Levels:
  Very attached; Fairly attached; Not very attached; Not at all
  attached.

- `attach_canada`:

  Attachment to Canada (on the wave that asked it, target:attach_ca).
  Attachment to Canada on four points, as each study asked it. Levels:
  Very attached; Fairly attached; Not very attached; Not at all
  attached.

## Experimental

The relaxed layer is new in qesR 0.9.0 (spec 4.4.0; its mappings signed
off in spec 4.5.0). Its columns, groups and mappings may change, and a
new relaxed mapping starts in review.

## En français

`qes_decon()` renvoie un seul tableau pour toutes les études électorales
québécoises harmonisées, une ligne par personne et par vague, une
colonne par concept, sous des noms simples à la manière du package cesR.
C'est une harmonisation souple : un concept va dans une seule colonne
pour chaque étude, même quand le libellé ou les choix de réponse
diffèrent, avec des catégories communes larges (la scolarité en quatre
groupes, le revenu en tiers des répondants de chaque étude, l'intérêt
faible, moyen ou élevé, un vote référendaire oui ou non quelle que soit
la question). Elle échange l'exactitude contre la couverture : utilisez
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
et ses niveaux de comparabilité quand la différence entre deux questions
compte. Chaque colonne dit comment elle a été assouplie
(`attr(, "relaxed")`) et d'où viennent les valeurs de chaque étude
(`attr(, "sources")`) ; `attr(x, "decon_sources")` les réunit toutes.
Les colonnes souples n'ont aucun niveau de comparabilité. Les
appariements souples sont révisés comme les autres : un appariement pas
encore approuvé (statut `review`) n'est pas appliqué, et ses cellules
valent `NA` (motif `not_reviewed`). Les 60 appariements de la
spécification 4.5.0 sont approuvés (statut `stable`) par une double
révision automatisée sur les fichiers et les documents originaux, et non
par une révision humaine, comme le disent leurs champs `reviewed_by` et
`review_note`. Les noms de colonnes restent en anglais ; `lang = "fr"`
donne les étiquettes, les règles et les sources en français.

## See also

[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
for the strict, graded harmonization;
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
with `view = "relaxed"` for the columns and `view = "relaxed_maps"` for
each study's relaxed mapping;
[`qes_party_lineage()`](https://thomasgareau.github.io/qesR/reference/qes_party_lineage.md),
which also joins the ADQ and the CAQ in `vote_choice`, `vote_prev` and
`pid`.

Other harmonization:
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md),
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
[`qes_party_lineage()`](https://thomasgareau.github.io/qesR/reference/qes_party_lineage.md),
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)

## Examples

``` r
# the synthetic demonstration study, offline
d <- qes_decon("qes_demo", quiet = TRUE)
head(d[, c("study", "wave", "gender", "age_group", "vote_choice", "sovereignty")])
#>      study wave gender   age_group vote_choice sovereignty
#> 1 qes_demo post  Woman 55 and over         CAQ          No
#> 2 qes_demo post  Woman 55 and over          QS          No
#> 3 qes_demo post    Man 55 and over         PLQ         Yes
#> 4 qes_demo post    Man 55 and over          PQ         Yes
#> 5 qes_demo post    Man       35-54          QS          No
#> 6 qes_demo post  Woman       35-54         CAQ         Yes
attr(d$sovereignty, "relaxed")
#> [1] "Yes or no in a referendum on Quebec sovereignty, whatever the question (an independent country, a sovereign country, the 1995 question or being favourable to independence); sovereignty_type says which, and would not vote is missing."
attr(d$vote_choice, "sources")
#>      study wave source_var         wording_ref
#> 1 qes_demo post    Q2 / Q3 352010:Q3;352009:Q3
#>                                                                                                                                               recode
#> 1 where Q2: 2 = Did not vote (NA); 9 = Refused (NA); else Q3: 1 = PLQ; 2 = PQ; 3 = CAQ; 4 = QS; 5 = PVQ; 6 = ON; 96 = Other party; 99 = Refused (NA)

# where every column comes from, study by study
src <- attr(d, "decon_sources")
src[, c("column", "study", "source_var", "relaxed", "n_value")]
#>              column    study source_var relaxed n_value
#> 1               yob qes_demo       QAGE   FALSE      60
#> 2         age_group qes_demo       QAGE   FALSE      60
#> 3            gender qes_demo      QSEXE   FALSE      60
#> 4      region_admin qes_demo    QREGION    TRUE      60
#> 5       vote_choice qes_demo    Q2 / Q3   FALSE      44
#> 6         vote_type qes_demo    Q2 / Q3   FALSE      60
#> 7           turnout qes_demo         Q2   FALSE      57
#> 8                lr qes_demo        Q32   FALSE      55
#> 9          interest qes_demo        Q28    TRUE      59
#> 10      interest_01 qes_demo        Q28    TRUE      59
#> 11      sovereignty qes_demo        Q19    TRUE      55
#> 12 sovereignty_type qes_demo        Q19    TRUE      60

# French labels, same column names
d_fr <- qes_decon("qes_demo", lang = "fr", quiet = TRUE)
levels(d_fr$interest)
#> [1] "Faible" "Moyen"  "Élevé" 

# every study: qes_decon() reads the 11 files (downloaded once, then from
# the cache), e.g. d <- qes_decon(); table(d$study, d$sovereignty)
```
