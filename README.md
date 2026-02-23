# qesR

<p align="center">
  <img src="man/figures/logo.png" alt="qesR logo" width="220" />
</p>

Access Quebec Election Study datasets in R using simple survey-code calls.

This package mirrors the core ergonomics of `cesR` while targeting studies
listed in the Quebec opinion portal:
<https://csdc-cecd.ca/portail-quebecois-sur-lopinion-publique/#section1>

## Installation

Install from GitHub:

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
```

Install from a local source tarball:

```r
install.packages("/path/to/qesR_0.4.4.tar.gz", repos = NULL, type = "source")
```

Install from a local package folder:

```r
install.packages("/path/to/qesR", repos = NULL, type = "source")
```

## Merged Dataset

`qesR` includes a harmonized merged file across studies through
`get_qes_master()`.

```r
library(qesR)

master <- get_qes_master(strict = FALSE)
head(master)
```

The merged data can be saved directly:

```r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```

What it harmonizes:

Current harmonized columns in `qes_master`:

`qes_code`, `qes_year`, `qes_name_en`, `respondent_id`, `interview_start`, `interview_end`, `interview_recorded`, `language`, `citizenship`, `year_of_birth`, `age`, `age_group`, `gender`, `province_territory`, `education`, `income`, `religion`, `born_canada`, `political_interest`, `ideology`, `turnout`, `vote_choice`, `vote_choice_text`, `party_best`, `party_lean`, `sovereignty_support`, `sovereignty`, `federal_pid`, `provincial_pid`, `survey_weight`, `definibin`, `ethn1`, `influperso`, `luentend`, `occup`, `patron`, `pondam1`, `laide_familles_etait_enjeu_important_election_q10`, `principale_source_dinformation_politique_ne_pas_lire_q16`, `deuxieme_source_dinformation_politique_television_radio_journaux_q17`, `which_party_best_managing_health_care_system_q18`, `gens_ont_differentes_facons_se_definir_diriez_considerez_q18a`, `gens_ont_differentes_facons_se_definir_diriez_considerez_q18b`, `eme_si_navez_peut_pas_encore_fait_choix_q20`, `which_party_best_fighting_corruption_q20b`, `veuillez_indiquer_si_etes_fortement_daccord_plut_ot_q22`, `mesure_faites_confiance_gouvernements_faire_doit_fait_leur_q23`, `pensez_gens_gouvernement_gaspillent_beaucoup_quelque_peu_ou_q24`, `leducation_etait_enjeu_important_election_q3`, `echelle_zero_cent_zero_veut_dire_naimez_vraiment_q31`, `personnellement_trouvez_lidee_tenir_election_quebec_automne_etai_q33`, `recemment_gouvernement_quebec_commence_envisager_possibilite_mod_q35`, `veuillez_indiquer_si_etes_fortement_daccord_plut_ot_q37`, `veuillez_indiquer_si_etes_fortement_daccord_plut_ot_q38`, `maintenant_nous_parlons_chefs_partis_utilisant_echelle_zero_q39`, `according_what_main_difference_between_quebecois_people_peopl_q4`, `maintenant_nous_parlons_chefs_partis_utilisant_echelle_zero_q40`, `maintenant_nous_parlons_chefs_partis_utilisant_echelle_zero_q41`, `maintenant_nous_parlons_chefs_partis_utilisant_echelle_zero_q42`, `maintenant_nous_parlons_chefs_partis_utilisant_echelle_zero_q43`, `selon_lequel_chefs_parti_plus_competent_ne_pas_q44`, `would_say_canadian_government_intervenes_too_much_affairs_q45`, `selon_lequel_chefs_parti_plus_proche_gens_ne_q46`, `selon_leconomie_quebecoise_sest_elle_amelioree_deterioree_ou_q47`, `considerez_surtout_comme_federaliste_surtout_comme_souverainiste_q48`, `dapres_lavenir_q49`, `veuillez_indiquer_si_etes_fortement_daccord_plut_ot_q50`, `veuillez_indiquer_si_etes_fortement_daccord_plut_ot_q51`, `veuillez_indiquer_si_etes_fortement_daccord_plut_ot_q53`, `veuillez_indiquer_si_etes_fortement_daccord_plut_ot_q54`, `selon_parti_politique_meilleur_ameliorer_soins_sante_ne_q55`, `if_had_choose_between_no_change_more_powers_q56`, `parti_meilleur_proteger_lenvironnement_ne_pas_lire_liste_q58`, `parti_meilleur_lutter_contre_pauvrete_ne_pas_lire_q59`, `it_important_quebec_has_sufficient_voice_central_decision_q60`, `parti_meilleur_defendre_lidentite_culture_quebecoise_ne_pas_q61b`, `parti_meilleur_soccuper_caisse_dep_ot_placement_quebec_q61d`, `each_following_areas_think_decisions_should_made_by_q62a`, `each_following_areas_think_decisions_should_made_by_q62b`, `echelle_va_100_ou_veut_dire_naimez_vraiment_q64`, `echelle_va_100_ou_veut_dire_naimez_vraiment_q65`, `etes_ou_contre_mariage_entre_personnes_eme_sexe_q66`, `veuillez_indiquer_si_etes_fortement_daccord_plut_ot_q68`, `baisses_dimp_ots_etaient_enjeu_important_election_q7`, `politique_provinciale_habituellement_identifiez_q70`, `sentez_peu_plus_proche_lun_ou_lautre_partis_q72`, `parti_q73`, `lors_derniere_election_federale_janvier_2006_parti_avez_q74`, `travaillez_actuellement_compte_etes_salarie_avez_pris_retraite_q79`, `statut_politique_quebec_etait_enjeu_important_election_q8`, `langue_parlez_plus_souvent_maison_q80`, `some_people_say_governments_should_see_it_every_q81`, `pauvrete_etait_enjeu_important_election_q9`, `raison_vote_recodee_raison1`, `raison_vote_recodee_raison2`, `jour_semaine_s_jse`, `diriez_etes_tres_satisfait_plut_ot_satisfait_plut_satisf`, `var_27_personnellement_jusqua_point_fiez_sondages_savoir_gagnera_sefie`, `var_28a_diriez_sondages_pendant_campagnes_electorales_tres_bonne_sondbons`, `var_15a_parti_avez_vote_dernieres_elections_provinciales_davril_voteprec`

Harmonization rules include:

- text normalization for common response fields (so values are comparable across studies)
- derived `age_group` for respondents with valid age/year-of-birth information
- source-variable tracking via `attr(master, "source_map")`
- list of automatically added cross-study fields via `attr(master, "crossstudy_variables_added")`
- explicit renaming of opaque legacy columns (for example `q10`, `q16`, `voteprec`) to readable names in the merged file
- transparent old-to-new name mapping via `attr(master, "variable_name_map")` and `qes_master_variable_name_map.csv`

Deduplication in `get_qes_master()` is applied **within the same survey code**
only (e.g., duplicate IDs inside one file). Respondents are not removed across
panel vs. non-panel studies.

## Website

Project website (GitHub Pages): <https://thomasgareau.github.io/qesR/>

The site includes researcher-focused tabs for:

- merged dataset workflow
- study citations
- 2 use-case tabs (sovereignty, vote choice)
- an `FR`/`EN` button in the top bar for page-by-page language switching

## Usage

```r
library(qesR)

# list available study codes
get_qescodes()

# detailed metadata table (DOI, documentation links, names)
get_qescodes(detailed = TRUE)

# load one study and assign object qes2018 into .GlobalEnv
qes2018 <- get_qes("qes2018")

# qes2018 is returned with variable/value labels (when available)
# and a codebook is attached:
cb <- attr(qes2018, "qes_codebook")
head(cb)

# you can also fetch the codebook directly
qes2018_codebook <- get_codebook("qes2018")
# compact layout (default): variable, label, question, n_value_labels

# wide layout: adds list-column with value labels
qes2018_codebook_wide <- get_codebook("qes2018", layout = "wide")

# long layout: one row per value label
qes2018_codebook_long <- get_codebook("qes2018", layout = "long")

# reformat an existing codebook object
format_codebook(qes2018_codebook, layout = "wide")
get_value_labels(qes2018_codebook, long = TRUE)

# alias
qes2018_codebook <- qes_codebook("qes2018")
qes2018_codebook <- get_qes_codebook("qes2018")

# codebook/support files (PDFs, questionnaires, metadata)
get_codebook_files(codebook = qes2018_codebook)
get_qes_codebook_files(codebook = qes2018_codebook)
download_codebook("qes2018", dest_dir = tempdir())

# preview first 10 rows
get_preview("qes2018", 10)

# retrieve question text from labels/codebook
get_question(qes2018, "some_variable")

# attempt fuller question recovery if metadata appears truncated
get_question(qes2018, "some_variable", full = TRUE)
# (uses `pdftotext` or `gs` when available)

# cesR-like prepared non-exhaustive dataset
decon <- get_decon("qes2022")
head(decon)

# harmonized stacked master dataset across studies
qes_master <- get_qes_master()
head(qes_master)
# includes derived age_group and harmonized education/turnout/vote fields

# inspect which source variable fed each harmonized field
head(attr(qes_master, "source_map"))
```

## Study Citations

| Year | Code | Study | Citation | Documentation |
|---|---|---|---|---|
| 2022 | `qes2022` | 2022 Quebec Election Study | Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison, 2023, "2022 Quebec Election Study", <https://doi.org/10.7910/DVN/PAQBDR>, Harvard Dataverse, V1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw== [fileUNF] | <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PAQBDR> |
| 2018 | `qes2018` | Étude électorale québécoise 2018 | Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, "Étude électorale québécoise 2018", <https://doi.org/10.5683/SP3/NWTGWS>, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/NWTGWS> |
| 2018 | `qes2018_panel` | Sondage panel sur l'élection québécoise de 2018 | Durand, Claire; Blais, André, 2023, "Sondage panel sur l'élection québécoise de 2018", <https://doi.org/10.5683/SP3/XDDMMR>, Borealis, V1, UNF:6:ECsYwSg8SlYcGPle+8FjWw== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/XDDMMR> |
| 2014 | `qes2014` | Étude électorale québécoise 2014 | Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2014", <https://doi.org/10.5683/SP3/64F7WR>, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/64F7WR> |
| 2012 | `qes2012` | Étude électorale québécoise 2012 | Bélanger, Éric; Nadeau, Richard; Henderson, Ailsa; Hepburn, Eve, 2023, "Étude électorale québécoise 2012", <https://doi.org/10.5683/SP2/WXUPXT>, Borealis, V1, UNF:6:nG192rAWV0IlYSpRg4WBaQ== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/WXUPXT> |
| 2012 | `qes2012_panel` | Sondage panel sur l'élection québécoise de 2012 | Durand, Claire; Goyder, John, 2023, "Sondage panel sur l'élection québécoise de 2012", <https://doi.org/10.5683/SP3/RKHPVL>, Borealis, V1, UNF:6:/ACwE8qVPCB013O9cweCqQ== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/RKHPVL> |
| 2007-2010 | `qes_crop_2007_2010` | Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010 | Durand, Claire, 2023, "Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010", <https://doi.org/10.5683/SP3/IRZ1PF>, Borealis, V1, UNF:6:Yaloq+G6EVBknLlAk44JoQ== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IRZ1PF> |
| 2008 | `qes2008` | Étude électorale québécoise 2008 | Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2008", <https://doi.org/10.5683/SP2/8KEYU3>, Borealis, V1, UNF:6:6wfopjsb0foTuDDWQPDfXg== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/8KEYU3> |
| 2007 | `qes2007` | Étude électorale québécoise 2007 | Bélanger, Éric; Nadeau, Richard; Crête, Jean; Stephenson, Laura; Tanguay, Brian, 2023, "Étude électorale québécoise 2007", <https://doi.org/10.5683/SP2/6XGOKA>, Borealis, V1, UNF:6:fNjQ+LF7dCVuIrjEyQuOyg== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/6XGOKA> |
| 2007 | `qes2007_panel` | Sondage panel sur l'élection québécoise de 2007 | Durand, Claire; Goyder, John, 2023, "Sondage panel sur l'élection québécoise de 2007", <https://doi.org/10.5683/SP3/NDS6VT>, Borealis, V1, UNF:6:ASjoqrxxkLm0vvSA6lc9Fw== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/NDS6VT> |
| 1998 | `qes1998` | Sondages électoraux sur les élections générales québécoises de 1998 | Durand, Claire, 2023, "Sondages électoraux sur les élections générales québécoises de 1998", <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/QFUAWG> |
