# qesR downstream impact: detail (v0.4.4 outputs)

Companion to `dev/assessment.md` §10. This file holds the full variable lists, counts and check commands. The summary tables are in the assessment.

## Scope and method

- **Package version measured:** HEAD c154985. Its `R/` code differs from d1faad6 (v0.4.4 on `main`, the version installed for the paper) only in the `assign_global` defaults, `get_question()`'s lookup environment and the local-codebook search roots (`git -c core.fileMode=false diff d1faad6 c154985 -- R/`). So data output is the same as v0.4.4. One caveat: main also searches `getwd()/codebooks` and `getwd()/inst/codebooks`. That matters only in a working directory that has those folders. The paper project has neither.
- **Calls measured:** `get_qes()` with default arguments for qes2012, qes2014, qes2018 and qes2022, and `get_qes_master()` in two builds:
  - the tracked root `qes_master.csv`, 40,606 rows, built by older code in an English-message session;
  - a fresh v0.4.4 build, 39,132 rows, built in a French-message session.
- **Reference data:** each output was compared with the original files from Dataverse (`/api/access/datafile/{id}?format=original`), read with haven: the 2012 and 2014 SPSS and Stata twins, the 2018 `.dta` and the 2022 `.dta`. The qes2007_panel and qes2018_panel `.tab` and `.sav` files were compared in the same way.
- **Session:** R 4.4.0, haven 2.5.5, fr_CA.UTF-8. `~/.Renviron` sets LANG and LC_ALL to fr_CA.UTF-8, so every R session on the owner's machine gets French messages by default. pdftotext, gs, PyPDF2 and textutil were all present.
- **Where the evidence lives:** scripts and outputs were in the session scratchpad (`impact/`, `adv/`, `xref/`, `xref_verify/`). The scratchpad is temporary, so the lists that matter are reproduced below.
- **Paper project:** `/Users/thomasgp/OJ_WelfareQuebec` was only read (grep, sed). Nothing was run, knitted or written there.
- **Disclosure:** six of the original-file downloads sent a User-Agent string that contained the owner's e-mail address to Borealis and Harvard Dataverse. Later downloads used R's default User-Agent.

**Headline.** For all four studies, the raw codes returned by `get_qes()` equal the original files value for value. The only exception is weight and timer rounding in the `.tab`, at most about 5e-8 relative. Row and column counts also match: 1505/1517/3072/1521 rows and 177/140/254/718 columns. The `get_qes()` defects are in text, labels and column types. `get_qes_master()` has substantive value errors.

**Consequence for the paper.** Fixing these bugs will change results when the paper's scripts are re-run:
- It will certainly change every `get_qes_master()`-based output.
- It will change `get_qes()` label, factor-text and type outputs.
- It will not change the raw-code article pipeline, which uses `as.numeric()` with windows of valid codes.

So:
- Release the fixes as a new version (0.5.0) with a NEWS entry for each changed output. Do not patch them silently into 0.4.4.
- Pin the qesR version the paper used: the commit SHA in an renv lockfile, or `remotes::install_github("ThomasGareau/qesR", ref = "d1faad6")`. Print `packageDescription("qesR")$RemoteSha` in the replication output.

---

## 1. Per bug: study outputs

### 1.1 D10: text reader truncates files under French messages (master only)

- **Cause:** the check at R/qes_download.R:2075-2076 matches the English warning text only: `grepl("EOF within quoted string", ...)`. In French the warning reads "Fin de fichier (EOF) dans une chaîne de caractères entre guillements", so the quote-free re-read never runs.
- **`get_qes()` for 2012, 2014, 2018 and 2022 is not affected:** all rows are read.
- **`get_qes_master()` under French messages loses rows from two panels:**

  | Study | Original rows | Read | Rows lost | Master after dedup | Last kept row |
  |---|---|---|---|---|---|
  | qes2007_panel | 2442 | 1234 | 1208 (subsample nompn 1151/83; 674B keeps 83 of 1291) | 1204 (dedup removed 30 more) | absorbs the rest of the file into one 882,838-char field |
  | qes2018_panel | 1250 | 634 | 616 | 634 | absorbs a 144,237-char field |

- **Accounts for the 1,474-row gap in §4:** tracked minus fresh is (2062 − 1204) + (1250 − 634) = 858 + 616 = 1,474, exactly the difference between the 40,606 and 39,132 row masters.
- **English messages also have a problem:** the rows are complete, but the fallback read uses `quote = ""`, so every string cell keeps literal double quotes (for example `ville` = `"TROIS-RIVIERES"`). That affects all 22 character columns and 53,724 cells in qes2007_panel, and all 3 character columns and 3,750 cells in qes2018_panel. Numeric columns match the `.sav`.
- **Check** (run in a scratch directory, in fresh sessions):
  ```r
  Sys.setenv(LANGUAGE = "fr")  # then repeat with "en"
  m <- qesR::get_qes_master(assign_global = FALSE)
  table(m$qes_code)            # qes2007_panel 1204 (fr) vs 2062 (en); qes2018_panel 634 vs 1250
  ```

### 1.2 Accented text replaced by U+FFFD (D3 consequence)

- **Cause:** the damage is already in Dataverse's ingested `.tab`. The 2018 `.tab` has 1,464 lines containing the bytes EF BF BD and the 2022 `.tab` has 1,287, with 0 correctly encoded `é` in either. The original `.dta` files have 0 U+FFFD.
- **Affected cells:**

  | Study | Column | Damaged cells | Example |
  |---|---|---|---|
  | qes2012 | q58 | 2 value labels | C1 characters, see 1.3 |
  | qes2012 | q65 | 2 value labels | C1 characters, see 1.3 |
  | qes2018 | district | 568 | `L��vis` for `Lévis` |
  | qes2018 | NM_MUNCP | 1237 | |
  | qes2018 | Q2_960 | 2 value labels | |
  | qes2018 | Q2_961 | 1 value label | |
  | qes2022 | pid_fr | 843 | `du Parti lib��ral` |
  | qes2022 | pes_mostimpissue | 576 | |
  | qes2022 | fedname | 517 | |
  | qes2022 | fr_pid_pr | 432 | |
  | qes2022 | pes_covid_employ2_5_TEXT | 26 | |
  | qes2022 | pes_emb_voteinfo_14_TEXT | 17 | |
  | qes2022 | cps_qc_vote_2018_5_TEXT | 15 | |
  | qes2022 | pes_qc_priorities2_8_TEXT | 13 | |
  | qes2022 | cps_partybest_8_TEXT | 12 | |
  | qes2022 | cps_religion_22_TEXT | 10 | |
  | qes2022 | cps_pastpartyvote_5_TEXT, pes_votechoice_5_TEXT, pes_employed_12_TEXT | 6 each | |
  | qes2022 | cps_negativevote_5_TEXT | 5 | |
  | qes2022 | cps_votechoice1_8_TEXT, pes_whymail, pes_reasonnotvote_17_TEXT, pes_race_13_TEXT | 4 each | |
  | qes2022 | cps_votesecond_5_TEXT, cps_socialmedia_9_TEXT, cps_provpid_5_TEXT | 3 each | |
  | qes2022 | cps_fedpid_5_TEXT, cps_lang_3_TEXT, pes_langhome_17_TEXT | 2 each | |
  | qes2022 | pes_contactparty_8_TEXT, pes_emb_registerhow_4_TEXT | 1 each | |

- **Totals:** qes2018 has 2 data columns with 1,805 damaged cells. qes2022 has 26 data columns with 2,517 damaged cells. qes2012 and qes2014 have no damaged data cells.
- **Master:** qes2022 `vote_choice_text` has 4 rows with U+FFFD. `province_territory` in qes_crop_2007_2010 has 7,203 rows containing a C1 character (CP850 `RESTE DU QU\u0090BEC`), in both builds. That study is outside the four targets.

### 1.3 C1 control characters in 2012 value labels (DDI route)

The DDI decodes CP1252 punctuation as C1 control characters: `…` becomes U+0085 and `–` becomes U+0096.

| Column | Codes affected | Cells whose `as_factor()` text differs | Carried into the master as |
|---|---|---|---|
| q58 | `\u0085a nation?`, `\u0085a province?` | 1,464 | `party_best_environment` (an H1 rename), 1,464 qes2012 rows |
| q65 | code 19 `gaspésie\u0096Îles-de-la-madeleine`, code 23 `hull\u0096aylmer` | 28 | `feeling_business_0_100` (an H1 rename), 28 qes2012 rows |

### 1.4 `.tab` instead of the original file; twin chosen by size (D3)

- **qes2012 gets the Stata twin** (file 425918). Its values are identical to the SPSS twin, which has the same UNF, but:
  - 174 of 177 variable labels are lowercased compared with the SPSS twin, and 107 of them are also cut at 80 characters (the SPSS labels can be up to 256).
  - Value labels are lowercased too. `as_factor()` text differs from the SPSS twin in 174 columns, 161 of them only by case. Example: `scol` has `i don't know` and `some cegep` where the SPSS twin has `I don't know` and `Some CEGEP`.
  - The package labels equal the Stata `.dta` labels in 177 of 177 columns.
- **qes2022 column types:**
  - `cps_StartDate`, `cps_EndDate`, `cps_RecordedDate`, `pes_StartDate`, `pes_EndDate` and `pes_RecordedDate` come back as character (`"2022-09-19 14:17:02.000"`). The original has POSIXct. In the three `pes_*` columns, 301 missing dates each come back as `""` rather than NA, 903 cells in total.
  - `cps_votechoice3_5_TEXT` comes back as integer. In the original it is character, with one `-99` and 1,520 `""`; the package returns -99 = 1 and NA = 1,520.
  - `pes_maildifficult_11_TEXT` comes back as logical, with `""` turned into NA in 1,521 cells.
  - 653 columns change storage from double to integer (361 labelled, 292 plain). The values are equal, but `as.character()` formatting changes. For example, `cps_income` `as_factor()` text differs in 87 cells (`1e+05` vs `100000`).
- **Weights and timers are rounded in the `.tab`,** by at most about 5e-8 relative, which is negligible: qes2012 `pond`, qes2018 `pond`, qes2022 `cps_weight_general`, `cps_weight_general_trimmed`, `pes_weight_general`, `pes_weight_general_trimmed`, `cps_time` and `pes_time`.

### 1.5 Value labels dropped when rebuilt from the DDI

The table compares the number of value labels in the original file with the number in the package (orig → pkg). Most of the dropped labels are trivial `"1" = "1"` pairs, so `as_factor()` text does not change for them.

| Study | Variables (orig → pkg) |
|---|---|
| qes2012 | q7 (13→4), q33a-g (6→4), q34a-g (6→4), q58 (4→4, text), q62aa-ai (1→0), q64 (7→6), q65 (78→78, text), q70a-f (13→4), q71 (13→4), q81 (7→4), q89 (13→4) |
| qes2014 | LANG (2→2, text), GREET (1→1), CODE1 (27→0), CODE2 (10→0), CODE3 (27→0), Q15 (13→4), Q31A-F (13→4), Q32 (13→4), Q36A (6→6), Q40 (7→4), Q41 (4→4), Q50 (13→4), Q53 (13→4), Q54 (13→4), Q57 (10→10) |
| qes2018 | q2_96_other (85→0), Q2_960 (4→3), Q2_961 (2→1), Q2_962-Q2_96I (1→0 each); plus the overrides in 1.7 |
| qes2022 | cps_age_in_years (101→0), pes_household (10→1); variable-label changes in 1.7 |

Losses that do change `as_factor()` text:
- **qes2012 q64:** code 96 is labelled `''` in the original and the package drops that label, so 20 cells show `96` instead of `''`.
- **qes2012 q62aa-q62ai:** the same mechanism affects 125 cells (6/12/18/11/11/19/14/16/18).
- **qes2018 q2_96_other:** code 1 is blank in the original (2,984 rows), and the other 84 labels are open-text answers to Q2's "other (96)" option. All 84 are lost, so 88 respondents (codes 2-85) show a bare integer.
- **qes2018 Q2_960-Q2_96I (19 columns):** the original has a blank `''` label on code 1. The package drops it, so `as_factor()` text changes from `''` to `1` in 3,069-3,072 rows per column. These are junk labels, but the text still changes.
- **No `make.unique` suffixes:** none of these four studies has a `.1`-style artefact.

### 1.6 D2: DDI taken from a different file (qes2014 only; benign)

qes2014 reads its data from the Stata twin (425915) and its DDI from the SPSS twin (425916). The values are identical. Only LANG, CODE1 and CODE3 have different label sets, and there the Stata twin's own labels are broken (all codes 0). The visible effect is that LANG gains `English` and `Français` labels, which is correct. qes2012, qes2018 and qes2022 each use the DDI of their own data file.

### 1.7 Hard-coded overrides and rebuilt variable labels (D9, K4/K5, D4)

- **qes2018:** 8 variables get value labels and French question labels that the original `.dta` lacks: q0qc, qsexe, qlangue, qscol, q5, q5a, q6 and q26. The codes were checked against the 2018 FR questionnaire and are correct, but accents are stripped (`Feminin`, `Quebec solidaire`). `as_factor()` text differs from the raw codes in 595-3,072 rows per variable.
- **qes2022:** 17 variable labels differ from the original:
  - 4 are longer: cps_age_in_years (143 characters), cps_covid_votecomf3 (131), cps_treat (105), pes_reducegap (95).
  - 1 is shorter: cps_intelection_1 (47 characters vs 79).
  - 12 are NA: pes_maildifficult_1-12.
  - Three more (cps_consent, pes_consent, cps_qc_energy) differ only by a trimmed trailing `\n`.
  - These labels probably come from PDF enrichment, so they depend on which OS tools are installed (D4).

### 1.8 K1: codebook attributes dropped

This affects the attribute only. For all four studies, `attributes(attr(d, "qes_codebook"))` is just `names`, `row.names`, `class` and `value_labels_map`, and `get_qes()` prints "Codebook/support files available: 0".

### 1.9 -99 and DK/refusal codes (no change: faithful to the source)

- **qes2022 -99 values are in the original:** 262 columns contain -99 (234 numeric, of which 189 are labelled and 45 plain integer, plus 28 character), 128,927 cells in total. That is the same set of columns, unlabelled, as in the original `.dta`. Every column, with its -99 count:

  cps_genderid_4_TEXT 1520, cps_trans 5, cps_satis_prov 2, cps_impissue_matrix 1, cps_partybest 3, cps_partybest_8_TEXT 1397, cps_interest_1 13, cps_intelection_1 15, cps_votechoice1_8_TEXT 1411, cps_votechoice2_8_TEXT 61, cps_votechoice3_5_TEXT 1, cps_votelean_8_TEXT 193, cps_votesecond 1, cps_votesecond_5_TEXT 1433, cps_negativevote_1..7 (410-1481), cps_negativevote_5_TEXT 1481, cps_partytherm_23/25/27/30/33 (45-63), cps_leadertherm_1/2/3/7/8 (29-229), cps_candtherm_23/25/27/29/30 (612-857), cps_govperf_3/4, cps_intelligent_5, cps_stronglead_1-5, cps_trustworthy_74/75/77/82, cps_cares_4, cps_ideoself_1 34, cps_ideoparty_1/3/5/7/8 (43-55), cps_partybest_issues_1-10 (14-19), cps_qc_energy, cps_qc_attach, cps_can_attach 7, cps_proveconblame, cps_refugee, cps_share, cps_treat, cps_provinequality, cps_qc_decisions_1-4, cps_valuesQC, cps_groups2_6/7/8/11/12/13/16/17/18 (7-25), cps_covid_handle_2, cps_vaccine1, cps_govresp_1-6, cps_socialmedia_1-10 (126-1205), cps_socialmedia_9_TEXT 1179, pes_identity_qc_ca, cps_fedpid 1, cps_fedpid_5_TEXT 1512, cps_provpid_5_TEXT 1506, cps_pastpartyvote 2, cps_pastpartyvote_5_TEXT 1320, cps_qc_vote_2018 4, cps_qc_vote_2018_5_TEXT 1260, cps_religion 2, cps_religion_22_TEXT 1487, cps_lang_1 1268, cps_lang_2 169, cps_lang_3 1444, cps_lang_3_TEXT 1449, cps_income 39, cps_income2 10, cps_yob 2, pes_mostimpissue 3, pes_votechoice_5_TEXT 1078, pes_q8_7_TEXT 102, pes_reasonnotvote_17_TEXT 100, pes_attention_1, pes_contact, pes_contactparty_1-9 (259-457), pes_contactparty_8_TEXT 457, pes_govtcare, pes_confidence_1/3, pes_groups_2/4/5, pes_emb_registerhow_4_TEXT 33, pes_emb_voteinfo_1-14 (452-1212), pes_emb_voteinfo_14_TEXT 1183, pes_voteoptions_1-8 (77-1203), pes_emb_info_2/3/5/6/7 (4-19), pes_embsatisfy, pes_identify_1/3, pes_nativism_1-9, pes_emb_age, pes_groupdiscrim_1-6, pes_fedpower 4, pes_qc_priorities2_8_TEXT 1194, pes_participation1-3_*, pes_feminine_1 5, pes_masculine_1 4, pes_langhome_1-17 (141-1220), pes_langhome_3_TEXT 1219, pes_langhome_17_TEXT 1193, pes_employed_12_TEXT 1206, pes_work 2, pes_covid_employ2_1-5 (163-299), pes_covid_employ2_5_TEXT 255, pes_covidwork, pes_own_1-5 (423-1153), pes_schoolkids, pes_orientation_4_TEXT 1211, pes_otherprov_1-13 (174-1220), pes_race_1-13 (61-1220), pes_race_13_TEXT 1207. Unlisted counts are 1-5.

- **In the multi-select items, -99 means "not selected".** Examples are cps_lang_*, pes_langhome_* and pes_race_*, so recoding -99 to 0 is correct there.
- **No SPSS user-missing values leak for these four studies:** the 2012 and 2014 SPSS twins declare none, and the 2018 and 2022 originals are Stata files.
- **DK and refusal codes (8/9, 98/99, 998/999) are ordinary labelled values in the originals:** 2012 has 155 columns and 21,642 cells, 2014 has 121 and 18,372, 2018 has 1 and 2, and 2022 has 37 and 3,453.
- **In the master:** -99 reaches the master as text in qes2022 `income` (39 rows) and `religion` (2).

### 1.10 Master coding errors (H1-H5, H7, weights)

Tracked and fresh builds are identical on all of these unless stated.

| Bug | Master column (study rows) | Magnitude |
|---|---|---|
| H1 name stacking | 63 renamed columns, e.g. `vote_federal_2006` (2012 q74), `feeling_david_0_100` (2012/2018 q42), `provincial_pid_item` (2018 q70), `issue_qc_status` (2012/2018 q8), `home_language` (2012 q80) | 45 populated for qes2012, 27 for qes2018, 0 for 2014/2022. `vote_federal_2006` 2012 = satisfaction with democracy; `feeling_david_0_100` 2018 = codes 2 = 1529, 98 = 954, 1 = 343; `provincial_pid_item` 2018 = home language; `home_language` 2012 = for/against (743/590) |
| H2 party_best | `party_best` (2012, 2014, 2018) | 2012 q8 = importance of voting (1505 rows). 2014 Q8 = best campaign (1517). 2018 q8 = "Quel parti était votre premier choix?" (asked if Q7 = NON), a different item, and all 367 values collapsed to "Don't know / Refused" (raw 1-4 = 306, 96 = 42, 99 = 19). 2022 `cps_partybest` = best party on an issue, also a different construct from 2014 |
| H3 interest | `political_interest` (2018) | 3012 raw 1-4 codes with 1 = "Très intéressé(e)": inverted, mean 2.09 vs 6.3-6.5 in other studies |
| H3-adjacent: ideology 2014 endpoints | `ideology` (2014) | `.coerce_scale_0_10` only recognises English "most left/right": 142 answers at 0 (55) and 10 (87) become NA; 1195 → 1053 valid. Values 1-9 match raw Q32 one to one |
| H4 ideology 2018 | `ideology` (2018) | all 3072 NA; `q36_1` has 2490 valid (84 at 0, 115 at 10) |
| H4 born_canada | `born_canada` (2018, 2014) | 2018: 138 born in another province → "No"; 304 raw `3`; 25 raw 98/99. 2014: 7 French DK texts |
| H4 language | `language` (2014, 2022) | 2014 = interview language LANG (French 1182 / English 335; mother tongue QLANG gives Français 1141); 2022 = survey UI language |
| H4 income / religion | `income`, `religion` | 2012 income empty (1505); 2014 French bracket text; 2018 raw codes incl. 99 = 326; 2022 numeric with -99 = 39. Religion 2018 raw, 96 = 39, 99 = 22, 1827 blank |
| H4 turnout / vote | `turnout`, `vote_choice` (2022) | intention (`cps_turnout` 1 = 1296 "certain to vote"; `cps_votechoice1`) although `pes_*` recall exists |
| H4 other | `party_lean`, `sovereignty`, `age_group`, `province_territory` | party_lean 2012 q13 "best stands up for Quebec", 2014 Q5 first choice; `sovereignty` ≡ `sovereignty_support`; 2018 age_group 270 NA incl. 229 aged 16-17; province "Quebec" for every row |
| H5 dedup | all columns (qes2007_panel) | 380 real respondents dropped (2442 → 2062; `quest` repeats across subsamples 674A/674B, sex agrees 51%, age 20%). Fresh French build: 1234 read, 30 more removed → 1204. The four cross-sections lose none |
| H7 dead branch | `vote_choice_text` (2018) | sourced from `q2_96_other`, which is Q2 (most important issue) "other" text, not vote text. NA in all 3072 rows only because 1.5 drops its labels. Fixing labels alone would put issue text in 88 rows; fix R/master.R:22 at the same time |
| Weights | `survey_weight` | 4 target studies mean 1.000; 2022 PES items weighted with the CPS weight; pooled weighting dominated by CROP (6.04), qes2007_panel (6.00 tracked / 6.12 fresh), qes2012_panel (6.30); qes2018_panel 1.000 / 1.041 (D10) |
| C2 | `qes_name_en` (panel rows) | cosmetic |
| H8 | none | no wave column; nothing wrong in values by itself |

**Does not change data output:** A1 (breaks bare calls instead, see 2.2), H6 (the paper does not call `get_decon()`), K3 (the 80-character Stata labels are the original), D1, D4-D8 (no drift, as the Dataverse versions are unchanged: 2012/2014/2018 V1.0, 2022 V1.1), A2-A6, C1, C3-C5, K2, K6-K7, V1-V4 and R1-R4.

---

## 2. OJ_WelfareQuebec exposure

### 2.1 Call inventory

- **Files:** 18 contain direct calls. `replication_article.Rmd` runs three of them through `code = readLines(...)`, which makes 19.
- **Calls:** `get_qes()` is called 56 times (qes2012 x13, qes2014 x13, qes2018 x14, qes2022 x16) and `get_qes_master()` 3 times.
- **Global-object pattern:**
  - 53 calls are unconditional bare `get_qes("qes20xx")` calls followed by use of the global object (`d12 <- qes2012`).
  - 2 are conditional: `if (!exists("qes2022")) suppressMessages(get_qes("qes2022"))` in build_chapter_figures.R:561 and figure_map_core_vs_appendix.Rmd:669.
  - Only analyses_exploratoires.Rmd:169 assigns the result.
  - All 3 `get_qes_master()` calls assign their result.

### 2.2 A1: the `assign_global` default

- **Installed version:** d1faad6, whose default is `assign_global = TRUE`.
- **HEAD c154985 flips the default to FALSE.** Under HEAD, the 55 bare calls fail with "object 'qes2012' not found". That changes no numbers, but:
  - `tableA1_modelsummary.R` wraps the QES block in a `tryCatch` (lines 100-257), so under HEAD the QES table and row disappear without an error. The only trace is `qes_status` in `tableA1_modelsummary.json`, written at line 881.
  - `build_fig_qes_ces_multinomial.R:78` also relies on the global object.
- **Checks:**
  ```r
  packageDescription("qesR")$RemoteSha   # expect d1faad6...
  formals(qesR::get_qes)$assign_global   # expect TRUE
  ```

### 2.3 Per script

Paths are relative to `/Users/thomasgp/OJ_WelfareQuebec`. `AR/` stands for `Revisions_QC_juillet2026/07_Article_rework/`.

**Shared QES block.** The files listed below read raw `get_qes()` columns through `as.numeric()` and windows of valid codes. The columns are:
- 2012: q25, q71, q52, sexe, langu, scol, age, agex, reven;
- 2014, 2018 and 2022: the equivalent items, for example 2022 cps_votechoice1, cps_ideoself_1, cps_qc_referendum, cps_genderid, cps_lang_2, cps_edu, cps_age_in_years and cps_income.

Every one of these columns equals the original file value for value, NA counts included. The bugs that touch them change metadata only:
- q71 and Q32 lose trivial value labels;
- langu gets the Stata twin's lowercased variable label;
- the 2018 overrides add labels to q5a, q6, q26, qsexe, qlangue and qscol;
- the cps_age_in_years label changes.

The -99 and DK codes fall outside the windows. For cps_lang_2, -99 means "not selected", so coding it 0 is correct. The cps_yob fallback is never used, because cps_age_in_years has 0 NA and no values under 18. None of these files uses weights, the encoding-damaged columns or the date columns. Re-running the v4 block verbatim on the cached outputs gives N = 918/950/1748/861 = 4477, the published N.

| Script | Live? | Exposure | Notes / check |
|---|---|---|---|
| AR/build_fig6_rework_v4.R | yes | none (data); A1 breaks it under HEAD | Identity check below; N = 4477 |
| AR/modelsummary_tables/tableA1_modelsummary.R | yes | none (data); A1 drops QES silently under HEAD | Check `qes_status` in the JSON and the docx mtime after any reinstall |
| AR/modelsummary_tables/tableA4_qes_modelsummary.R | yes | none (data); A1 breaks it under HEAD | Model N in JSON = 4477 |
| AR/replication/replication_article.Rmd | yes | inherits the three above | Knit a copy outside the project, or check existing HTML: N = 4477, Table A1 QES row present |
| AR/comments_aout/audit_modeles/audit3_prep_qes_sans_filtre_commun.R | yes | none (data); A1 | Filter is `!is.na(parti)` only; compare `table(d$year)` with the same prep on haven-read originals |
| AR/comments_aout/audit_modeles/audit3_prep_fig4_extract.R | yes | none (data); A1 | Block identical to v4 |
| AR/build_fig6_rework_v4.avant_fix_28aout.R, v3, v2, build_fig6_rework.R | no | none (data); A1 | Backups/older versions; identical block. v3's `fig6_numbers_2015.json` is still tableA4's reference |
| AR/modelsummary_tables/tableA1_modelsummary.avant_fix_28aout.R | no | none (data); A1 silent drop | Backup |
| build_english_fig6.R, fig_3_pooled_qes_ces.R | no | none (data); A1 | Same windows |
| fig_3_qes_only.R | no | none (data); A1 | **Writes the same figures/fig_3_qes_2012_2022.png/.pdf as build_fig_qes_ces_multinomial.R.** Current file mtime (Apr 10 19:51) is after both scripts were modified (18:57 and 18:19), so it is probably from this unaffected script, but this is not proven. chapitre_v2.Rmd references that figure |
| build_chapter_figures.R | no | none (data); A1 (conditional call) | Fig 3.1 bis uses raw qes2022 columns (pes_votechoice, cps_votelean, pes_reducegap, pes_privjobs, pes_othersahead, pes_womenhome, pes_familyvalues, pes_dogays, cps_qc_referendum, pes_langhome_1/2, cps_yob, cps_genderid, cps_edu, cps_borncda, cps_income), all equal to the source. Reading `cps_yob` as age (line ~620) is the script's own error |
| figure_map_core_vs_appendix.Rmd | no | none (data); A1 (conditional call) | Copy of the above; the `cps_yob` error at line 709 is its own |
| build_fig_qes_ces_multinomial.R | no | **affected** (master) + A1 at line 78 | See below |
| memo_figures_chapitre.Rmd | no | **affected** (master) | See below |
| analyses_exploratoires.Rmd | no | **affected** (master); D10 was live in its knitted PDF | See below |

**build_fig_qes_ces_multinomial.R** (`d <- get_qes_master()` at line 35)
- The master part of the model is qes2012 916, qes2014 832 and qes2022 1032 rows, 2,780 in total. It is identical in the tracked and fresh builds, with identical party splits.
- qes2018_panel rows pass the first filter at line 37: 676 in the tracked build and 185 in the fresh one. They are all then dropped at lines 71-72, because the master has no age for that study. So D10 does not reach this figure. Year 2018 comes only from the manual raw qes2018 block.
- **H3, 2014 ideology endpoints:** `lr01` spans only 0.1-0.9 for 2014. About 119 of the 142 endpoint answers belong to the four mapped parties (PLQ 54, PQ 36, QS 16, CAQ 13) before the other filters.
- **H4, language:** `francais` uses the interview language for 2014 (LANG) and the survey UI language for 2022 (cps_UserLanguage). The manual 2018 block uses mother tongue.
- **H4, income:** 2012 income is 0 non-NA and is imputed as `income_z = 0`. The 2014 bracket text is parsed correctly. The 2022 -99 values are removed by the >0 filter.
- **H4, vote:** the 2022 vote is campaign intention (`cps_votechoice1`).
- **Check:**
  ```r
  m <- qesR::get_qes_master(assign_global = FALSE)
  table(m$ideology[m$qes_code == "qes2014"])     # no 0 or 10
  table(haven::read_sav("<2014 .sav>")$Q32)      # 0 = 55, 10 = 87
  ```

**memo_figures_chapitre.Rmd** (fig2 chunk, lines 789-803)
- The chunk draws one left-right density per party, pooled over 2012, 2014 and 2022. There is no facet by year, and the inline means (`party_summ`) are pooled too.
- **H3 changes the headline.** With the 2014 endpoints restored from raw Q32 (row alignment checked), the means are:

  | Party | Current mean (n) | With 2014 endpoints (n) |
  |---|---|---|
  | PLQ | 5.89 (695) | 6.08 (749) |
  | CAQ | 5.86 (903) | 5.86 (916) |
  | PQ | 4.37 (839) | 4.36 (875) |
  | QS | 3.25 (453) | 3.18 (469) |

  The memo's "PLQ ≈ CAQ" gap is 0.03 only because of H3; with the endpoints restored it is 0.22.
- **H4:** 2018 is excluded by the author's year filter and caption ("vagues avec idéologie disponible"), but ideology is unavailable only because of the q36 bug. The 2022 vote is intention.
- **D10 and H5 do not reach this figure:** the filtered counts are 1000/904/986 in both builds.

**analyses_exploratoires.Rmd** (`load-qes` chunk, `cache = TRUE`)
- **D10 was live in the owner's output.** The knitted `analyses_exploratoires.pdf` (Mar 29), table 1.1, gives 2012 1204, 2014 1053, 2018 194, 2022 1487, which matches the fresh French-session build exactly. The English build would give 2018 = 720. "n=194" is also hard-coded in the prose at lines 369 and 663.
- **H3:** without H3, 2014 N would be 1195, not 1053.
- **2018 sections (lines 370-386, 667-685):** they show only Durand panel rows, because QES 2018 ideology is all NA (H4), and D10 cuts those rows too.
- **§1.7 sovereignty by party (lines 697-725):** it groups by `qes_year` across all 11 studies. CROP supplies 3918 of 6378 rows in 2007, 7833 of 8619 in 2008, and all rows for 2009-2010. qes1998 (vote = `intvote`), qes2012_panel and the full qes2018 (1675 rows) are included.
  - The D10 effect on the plotted shares is at most 2 points: 2007 CAQ/ADQ 0.32→0.31 and QS 0.62→0.60; 2018 CAQ/ADQ 0.30→0.31, PLQ 0.04→0.03, PQ 0.85→0.86, QS 0.56→0.57.
  - Two H4 problems are larger threats to this trend: the sovereignty wording changes (the 2007/2008 partnership wording vs independence from 2012), and intention is mixed with recall (1998 `intvote`, 2022 `cps_votechoice1`).
- **§9.2 (lines 2528-2545, "PLQ ≈ CAQ"):** the same pooled density as the memo, so the same H3 distortion applies (PLQ 5.89 vs 6.08).
- **Errors that are the script's own:** -99 counted as "Gauche" (line 472) and the PLQ/CAQ swap (line 434). -99 is in the source `.dta`, and the 80-character labels are the Stata originals.
- **Check,** in fresh sessions with `LANGUAGE = "en"` and then `"fr"`:
  ```r
  m <- qesR::get_qes_master(assign_global = FALSE)
  table(m$qes_code)
  table(m$qes_year[suppressWarnings(as.numeric(m$ideology)) %in% 0:10])
  ```
  Expect 2018 = 720 (en) vs 194 (fr), and qes2007_panel = 2062 vs 1204.

### 2.4 Identity check for the article pipeline

Run this in a scratch directory, never inside the paper project:

```r
library(qesR)
x <- get_qes("qes2022", assign_global = FALSE)
o <- haven::read_dta("<original 2022 .dta, /api/access/datafile/7449513?format=original>")
for (v in c("cps_votechoice1", "cps_ideoself_1", "cps_qc_referendum", "cps_genderid",
            "cps_lang_2", "cps_edu", "cps_age_in_years", "cps_income"))
  print(c(v, identical(as.numeric(x[[v]]), as.numeric(o[[v]]))))
# repeat for the 2012, 2014 and 2018 variable lists (originals via ?format=original);
# then confirm the pipeline gives N = 4477 (918/950/1748/861)
```
