# Open questions for the owner

State at qesR 0.7.1, harmonization spec 4.2.0 (2026-09-28). The file
has two parts:

1. **Resolved (with sources):** what the public deposits, the producers'
   own publications and the original files now settle, where each answer
   comes from, and what changed in the spec. Decisions taken in earlier
   slices are recorded there too; they stand unless the owner reopens one.
2. **Needs the owner:** only what a person has to do: requests to
   producers, to Élections Québec and to others (each with the exact text,
   the addressee and why), and decisions that only the owner can take.
   Claude sends no request and no e-mail; the owner sends them, from the
   contact forms named below.

Policy (design.md section 0.3, OD18): Claude fetches public documents
itself, with plain requests that carry no personal information; what needs
a login, a permission or a document that is not public is a request for
the owner. The conservative choice holds until an item is closed: an
undocumented weight is registered but not applied, an undocumented item is
left unmapped or graded down.

---

# Part 1. Resolved (with sources)

Unless stated otherwise, every figure below was checked on the pinned
original files (md5-verified; `data-raw/` builders and the checks of the
research of 2026-09-27). "Checked on the file" means that. Web sources were
fetched with plain requests; archived pages are Internet Archive captures
(`web.archive.org/web/<timestamp>/<url>`).

## 1.1 CROP polls 2007-2010 (`qes_crop_2007_2010`, doi:10.5683/SP3/IRZ1PF)

The Borealis deposit has one version (1.0) with two public files (the data
and the 31 KB codebook, file 341537): it cannot settle these points on its
own. CROP's reports for La Presse, Claire Durand's poll archive and the
archive of quebecpolitique.com do.

- **Fieldwork dates of each poll (was HZ5 A.4). Resolved.** `waves.csv` has
  each poll's own dates instead of the month's bounds. Sources: CROP's
  trend table in its La Presse report of May 2008, p. 17 (June 2007 to May
  2008), archived at
  `https://web.archive.org/web/20080611213943/http://www.cyberpresse.ca/assets/pdf/CP1753529.PDF`;
  CROP's reports of January 2009
  (`https://web.archive.org/web/20181001075245/http://pdf.cyberpresse.ca/lapresse/donnees0109.pdf`)
  and March 2009
  (`https://web.archive.org/web/20181001075059/http://pdf.cyberpresse.ca/lapresse/0964539%20rap%20La%20Presse%20mars09%202.pdf`);
  the archived per-poll pages of quebecpolitique.com for the other polls.
  Where sources differ, CROP's own dates are kept (quebecpolitique.com
  gives 19-29 October 2007, Durand's archive ends August 2007 on the 29th).
  The published sample sizes equal the file's in 23 of 24 polls (October
  2008: 1,000 published, 1,001 in the file). In the months with two CROP
  polls the file's poll is identified by its size and referendum figures
  (June 2007: the 14-25 June poll; November 2008: the 5-13 November poll,
  n = 1,003, the two others having no referendum item). June 2009 was
  fielded 11-18 June and published on 20 June: the deposit's "sondage le
  20 juin 2009" is the publication date. Mode: CROP-Express telephone
  omnibus, respondents 18 and over, interviews in French or English (CROP,
  January 2009 report).
- **Election that the previous-vote question `QP4`/`voteprec` refers to
  (was HZ5 A.3). Resolved.** The question names it: "Pour quel parti
  avez-vous voté aux dernières élections provinciales du 26 mars 2007?"
  (May 2008 report, Q40, whose counts equal projet 10) and "... du 8
  décembre 2008?" (January 2009 tables, Q41, whose counts equal projet 16:
  103 ADQ, 265 PLQ, 271 PQ, 39 QS, 33 PV, 2 other, 183 did not vote or
  spoiled, 104 NSP/refus). Polls 1-15 were fielded before 2008-12-08 and
  refer to 2007-03-26; polls 16-24 refer to 2008-12-08 (the ADQ share of
  QP4 falls from 18.2% in projet 15 to 9.2% in projet 16, weighted).
  Recorded in each wave's notes; the legacy notes of `turnout` and
  `vote_choice` (and `get_decon()` `turnout`, `votechoice`) now say the
  polls asked the previous vote, not the vote at the election each poll
  refers to. No previous-vote target exists yet (deferred work, 1.9).
- **Wording of the first referendum question `intvoterefa` (was HZ5 A.2).
  Resolved for 19 of the 24 polls.** "Si un référendum avait lieu
  aujourd'hui vous demandant si vous voulez que le Québec devienne un pays
  souverain, voteriez-vous Oui ou voteriez-vous Non?" (a sovereign country,
  the wording of the anchor of `sov_sovereign_country`). Stated in CROP's
  May 2008 report (Q51a; counts 397 Oui, 513 Non = projet 10), January 2009
  tables (Q43a; 362, 525 = projet 16) and March 2009 report (Q4a), whose
  series on this question, poll by poll from May 2007 to March 2009, equals
  each poll's weighted `intvoteref` after allocation of the undecided
  (projet 1-17). April 2009 (projet 18): CROP asked both a sovereign-country
  and a sovereignty-partnership question (37/63 and 42/58 after allocation,
  quebecpolitique.com); the file gives 37.4, the sovereign-country one.
  October 2009 (projet 23): Le Soleil, 2009-10-27, reports 35% yes « à une
  proposition de faire du Québec un pays souverain », the file's weighted
  yes before allocation (35.3); press only. **Not documented:** May 2009
  (projet 19: the press does not say which question gave its 42%, and the
  file's 42.4 fits either), June 2009 (20: no sovereignty result
  published), August and September 2009 and January 2010 (21, 22, 24: the
  figures match the published results, but no document states the wording
  after April 2009). Codes 3 (would not vote or spoil), 8 (NSP) and 9
  (refus) are starred in CROP's questionnaires (recorded if volunteered).
  **Not mapped yet**, because the spec's grammar cannot leave five polls
  out: one primary crosswalk row per study and target (V-S5), and a gate
  must be true routing (V-D7 fails when a closed gate has answers). The
  facts are in each wave's notes and in the legacy notes of
  `sovereignty_support` and `sovereignty`; the decision is D2 of part 2.
  The push `intvoterefb` is asked of codes 3, 8 and 9 only (checked on the
  file), so `intvoterefa` is the unpushed question, like the anchor
  (`qes2012_panel` `intvoteref` keeps 88 don't-knows); the
  `sov_sovereign_country` description now says the push is not part of the
  target.
- **Weight `XPOND` (was HZ5 A.1, CROP part). Documented, still
  `needs_review` (owner's acceptance, D1).** CROP's May 2008 report, p. 3-4:
  weighted to the 2006 Census population 18 and over (outside
  institutions) by sex, age, region and language spoken at home; the sample
  is 500 Montréal CMA, 200 Québec CMA, 300 elsewhere, so unweighted
  estimates over-represent the Québec CMA (20% of the sample, 10% of the
  population). In projet 10 the XPOND totals equal the report's population
  by region and by sex; age and home language are within 3% of the
  report's comparison table (which disagrees with its own crosstab
  headers), and the weights are not constant within cells, so
  post-stratification is inferred, not stated. XPOND's total (thousands)
  changes by block of polls: 6,117 (June 2007), 5,997 (August-November
  2007), 5,905 (January-March and May-October 2008), 5,881 (April 2008),
  6,169 (November 2008-June 2009), 6,215 (August 2009-January 2010).
- **Gender evidence.** The 1,003 missing `SEXE` are all of projet 20 (June
  2009), one poll, not two (crosswalk evidence corrected). The deposit's
  file description (file 329990) gives the reason: the sex of that poll's
  respondents was not coded because of a coding error (spec 4.0.0).
- **Wording of the vote-intention questions `intvoteprova` and
  `intvoteprov` (spec 4.0.0). Resolved.** CROP's January 2009
  questionnaire (Q2a and the push Q2b, `donnees0109.pdf`, archived above),
  its May 2008 report (Q38a/38b) and its March 2009 report (Q2a) print the
  anchor's stem ("S'il y avait des élections provinciales aujourd'hui au
  Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous
  pour...") and push, the parties and leaders read in rotation, and codes 7
  (would spoil, not vote or none) and 9 (NSP/refus) starred, recorded only
  if volunteered. The January 2009 tables equal projet 16 (first question
  105 ADQ, 308 PLQ, 278 PQ; with the push 111, 330, 297) and the May 2008
  sample counts projet 10 (108 ADQ, 301 PLQ). Both rows carry the French
  wording and `dk_offered = volunteered`; grades stay `comparable`
  (monthly omnibus polls, mostly outside a campaign, on a regionally
  stratified sample). The education ranges of `scol` are those of CROP's
  Q42 and May 2008 report (p. 5; projet 10 counts equal); its 97 system
  missing are all in projet 15 (November 2008), the same rows as `revenu`
  and `lmat`.

## 1.2 1998 panel (`qes1998`, doi:10.5683/SP2/QFUAWG)

One deposit version (1.0), six files; no CREATEC questionnaire and no
weighting report. Sources: the PIs' conference paper (Durand, Blais and
Vachon, ICSN, 1999; `https://www.mapageweb.umontreal.ca/durandc/icsndurc.pdf`),
their AAPOR 1999 slides (`https://www.mapageweb.umontreal.ca/durandc/aapor/sld004.htm`),
Durand's poll archive, and the three data files.

- **Definition of "francophone" (was HZ5 A.5, OD16). Resolved.** The pooled
  rows link one to one to the firms' files: CREATEC by `quetr`, CROP by
  `quest = quetr - 60000` (all 426; `ponderc`, `vpl`, `q7a`, `q16a`,
  `q3post` and age identical). HZ5's "no variable links its CROP rows" was
  wrong. CREATEC: all 1,057 have Q17 (mother tongue, "la première langue
  que vous avez apprise et que vous parlez toujours") = French. CROP: all
  426 have q22 (language spoken most often in the household) = French; the
  24 left out have English (11) or another language (13); all 450 were
  interviewed in French. The pooled codebook (file 332051) gives the reason:
  CREATEC had surveyed francophones only. `waves.csv` (both waves),
  `inst/COPYRIGHTS` and the legacy note of `language` now say "confirmed".
- **Sample design. Resolved.** The post-election sample is a
  non-proportional stratified sample of the pre-election respondents that
  over-represents non-disclosers and third-party supporters (ICSN paper,
  p. 5-6; AAPOR slide 4: N = 1,497, response rate 85%).
- **What each weight is (was HZ5 A.1, 1998 part). Arithmetic resolved;
  choice open (D3).** Checked on file 329987: `poids` is constant within
  each firm's pre-election intention stratum (`vpl`; CREATEC also gives
  2.616 to its 79 respondents not asked the intention): the design factor
  of the recontact. `ponder2 = ponderc x poids` (exact for CREATEC, within
  0.3% for CROP). `ponder3 = ponder2 / c`, c = 2.1656 (CREATEC) and 10.4417
  (CROP), so mean 1 within each firm. `ponderc` is each firm's own weight
  (CREATEC "Pondération principale", mean 1; CROP "à la population
  multiplié par 1000", mean 5.16; the ICSN tables say CROP's campaign polls
  are "weighted according to the 1996 Census"); it does not undo the
  over-selection (refusals stay 14.8% of CREATEC's intention; 8.1% with
  `ponder3`). `ponder3` reproduces the ICSN paper's Table 2 (16 of 18 cells
  exact, 2 off by 1 point); Table 1 does not tell `ponder3` from `poids`.
  Against this, the CREATEC data file's description (file 316121) says
  `ponderc` is to be preferred. So every 1998 weight stays `needs_review`
  with this reason in `weights.csv`; `ponder3` stays the registered
  candidate. `get_qes_master()` `survey_weight` keeps `ponderc` (the 0.4.4
  source, OD6), now with a note that it does not undo the over-selection.
- **Fieldwork (was HZ5 item 15). Already right, detail added.** CREATEC
  pre-election interviews 18-23 November (`dater`, a day number only, 224
  missing), CROP 19-23 November (codebook 332049; CROP's campaign poll of
  1,003 electors); recontact 8-13 December (CREATEC, `s_jr`) and 9-13
  December (CROP).
- **CREATEC routing (was HZ5 A.6, routing part). Resolved.** Q3 (intention)
  was asked only of those who said they would certainly or probably vote:
  the 79 not asked are 22 "probablement pas", 35 "certainement pas" and 22
  "NSP/refus" at Q1; the push Q4 went to Q3's don't know, would-spoil and
  refusal answers. The questionnaire itself is still not deposited (request
  Q1(c)).

## 1.3 Weights, modes and dates of the Léger studies

- **`qes2007` `pond` (was HZ5 A.1). Resolved: `reviewed`.** Bélanger and
  Nadeau 2009, *Le comportement électoral des Québécois* (PUM), ch. 2
  note 5 (`https://books.openedition.org/pum/9782`): sex, age, mother
  tongue and region, to the census, applied separately to the web and the
  telephone samples. Checked on the file: constant within type x sex x six
  age bands x 21 regions (`nomx`) x mother tongue (594 cells, none
  differing); each mode keeps its share; the vote is not calibrated
  (weighted PLQ 25.8, PQ 30.9, ADQ 31.6 against 33.1/28.3/30.8). Fieldwork
  2007-04-04 to 04-17 and mixed mode, from the deposit metadata (no date
  variable in the file).
- **`qes2008` `pond` and `pondx` (was HZ5 A.1). Resolved from the file;
  `needs_review` because the book says otherwise.** `pond` ("Sans taux de
  participation") is constant within sex x age x region (`reg`, 5) x
  mother tongue x education (`q77`) x reported vote (`q12a`) (680 cells,
  none differing; without `q12a`, 199 of 234 multi-respondent cells
  differ), and among party voters gives the official 2008 shares to within
  0.1 point (PLQ 42.05, PQ 35.26, ADQ 16.41, QS 3.85, PV 2.18; official
  42.08/35.17/16.37/3.78/2.17). It is calibrated on the vote: role
  `vote_calibrated`, not recommended. `pondx` ("avec taux de participation")
  reweights the nonvoters to the official turnout (57.30% weighted, 57.43%
  official), as the book's ch. 6 note 3 describes
  (`https://books.openedition.org/pum/9786`). With both weights calibrated
  on vote or turnout, `qes2008` has no recommended weight; V-S13 now allows
  that case only (design.md). The book's ch. 2 note 5 names only sex, age,
  mother tongue and region, so the method is asked of the PIs (Q2).
- **`qes2008` mode (was HZ5 A.7). Evidence recorded; stays telephone.** The
  deposit metadata says "Entrevue téléphonique" (2008-12-09 to 12-15). The
  questionnaires (files 196358, 197296) read like a web script: no LIRE /
  NE PAS LIRE (10 in the 2007 telephone script), about 50 « Je préfère ne
  pas répondre », self-reported sex with code 9, quota and « cliquer sur la
  flèche » exits, PIN and LMID panel identifiers in the English version;
  the greeting reads like a telephone one, and neither questionnaire names
  LegerWeb. Kept as the producer's metadata says until the PIs answer (Q2).
- **`qes2012` (was MANIFEST NEEDS OWNER 4, `pond` margins). Resolved.**
  Léger's technical report (file 196368): LegerWeb online panel, 1,505
  respondents, 2012-09-12 to 09-25, pretest of 24 on 09-12, median 27
  minutes; « La variable POND ... est basée sur les variables suivantes :
  SEXE, ÂGE, RÉGION et LANGUE, selon le dernier recensement ». Checked on
  the file: constant within sex x six age bands x 17 regions (`q0qc`) x
  mother tongue (264 cells); mother-tongue targets 79.1/8.3/12.6, equal to
  `qes2008`'s and not the 2011 Census (about 78-79% French and 7-8% English
  at 20+), so "the latest census" is probably 2006 (not confirmed; asked in
  Q2). The validation compares `qes2012` with the 2011 Census, the census
  before its election, which is right for the population whatever the
  weight used.

## 1.4 The Durand panels

- **2018 panel dates and modes (was MANIFEST NEEDS OWNER 4). Resolved.**
  Pre-election wave 2018-09-26 to 09-28: the deposit metadata
  (doi:10.5683/SP3/XDDMMR, `dateOfCollection`), Ipsos's report for La
  Presse and Global News of 2018-09-29
  (`https://www.ipsos.com/sites/default/files/ct/news/documents/2018-09/rapport_la_presse_global_news_29_septembre_2018.pdf`)
  and Durand and Blais's postprint of their 2020 CJPS article
  (`https://umontreal.scholaris.ca/bitstreams/0115eb27-9aa6-4916-94f1-3f4a8020cc31/download`):
  850 web-panel and 400 telephone interviews of adults eligible to vote;
  only the intention and certainty items were asked of all 1,250.
  Recontact 2018-10-12 to 10-19: deposit metadata only (Q1(e) asks for
  confirmation); 842 interviews, 592 online and 250 by telephone (Durand,
  Policy Options, February 2019). `method` is the pre-wave mode (post-wave
  members: 136 landline, 103 cell, 603 web), so at least 11 pre-wave web
  respondents answered the recontact by telephone and the recontact mode
  of each respondent is not in the file. The codebook's 75/25 split matches
  neither wave.
- **2018 panel weights (was R4). Resolved: `reviewed`.** Ipsos's report
  (Pondération: âge, sexe, région, niveau d'éducation, langue maternelle,
  census) and Durand and Blais, postprint p. 12 and Table A1 (each wave
  weighted separately; ranges 0.26-4.39 and 0.31-5.38, reproduced from the
  file). The weighted margins hit the targets in both waves: age 26/33/41,
  sex 49/51, the five `fsa_tabl` regions, French mother tongue (`s1`) 79.0,
  university (`scolU`) 32; the four education groups and the English/other
  split are not held. Their "cooperation rates" (62.5%, 69.6%) count by the
  recontact mode, not by pre-wave origin (239/400 and 603/850 in the file),
  and are not repeated in the spec.
- **2012 panel weights. Described from the file; still `needs_review`
  (D1).** `pond`/`pondam1` and `pond_post`/`pond_postam1` are two weights
  over the same cells (sex x six age bands x `reg` (4) x mother tongue
  `lmat` (3), 86 cells, none differing); `pondam1 = 0.1573 x pond` has mean
  0.9916 over the 844 (probably normalized on the larger pre-election
  sample: inference); `pond_postam1 = 0.1356 x pond_post` has mean 1;
  `pond_post` is 1.0 to 4.0 times `pond`, but its weighted margins over the
  844 are not round census-like targets (sex 47.7/52.3), so what it was
  calibrated to is unknown. `pondvote` gives the official 2012 shares
  among declared voters to within 0.2 point. Fieldwork 24-26 August and
  10-18 September 2012 (codebook, file 654292), as `waves.csv` had it.
- **2007 panel post-wave weight (was MANIFEST NEEDS OWNER 4). Described
  from the file; `needs_review` (D1).** No weight is calibrated on the
  post-wave completers. `pond`, `pondam1` and `ponderation_totale` ("bonne
  pondération après réestimation?") are functions of poll x sex x six age
  bands x region (3) x home language (2) x education (4) (391 cells; none
  differing for `ponderation_totale`, 3 for `pond`); `pond_tot_am1` is
  `ponderation_totale` rescaled to mean 1 within each poll and has values
  for 2,053 of the 2,054 post-wave members, the 391 converted refusals
  included. It is now registered as the post wave's recommended weight,
  `needs_review` (so still `NA`).

## 1.5 `qes2018` mother tongue (was F1). Resolved: not swapped.

The map (1 French, 2 English, 96 other) is right. Programmed questionnaire
(file 367181): QLANGUE Français 1, Anglais 2, Autre 96; the home-language
item Q70 has another order (Anglais 1, Français 2, ...), an independent
check. Checked on file 425914: of code 2, 401 of 447 (89.7%) speak English
most often at home and 64% were born in Quebec, 17% elsewhere in Canada; of
code 96, 84 of 187 (44.9%) speak a non-official language at home and 105
(56.1%) were born outside Canada; code 2 is the largest group in the
anglophone ridings (126 of 211) and in Pointe-Claire, Kirkland,
Côte-Saint-Luc and Westmount; 18 of the 20 open answers of code 2 are in
English. The methodological report (file 361045, p. 3) labels the same
counts English 447 and Autre 187, consistent with this (it comes from the
same producer, so it is not independent). The weighted shares are 77.1%
French, 16.2% English, 6.6% other (16.6% English at 20+), against 7.2%
English and 13.7% non-official at the 2016 Census (20+, single answers).
The English excess is sample composition: the weight calibrates French
against all other languages only (Table 14), and English spoken most often
at home is over-represented too (18.7% weighted, 12.0% in the 2016 Census,
all ages, multiple answers counted). The labelled 2018 panel shows the same
pattern (`s1`: 15.7% English weighted). The crosswalk evidence of the row
says so; the row can be signed off (D1).

## 1.6 Census benchmarks (was V1, V3; MANIFEST NEEDS OWNER 1 and 2)

- **2011 age by sex at 18+. Resolved.** The 2011 Census Profile's
  comprehensive CSV download works with a plain GET (catalogue
  98-316-XWE2011001, provinces file 101,
  `https://www12.statcan.gc.ca/census-recensement/2011/dp-pd/prof/details/download-telecharger/comprehensive/comp_download.cfm?CTLG=98-316-XWE2011001&FMT=CSV101&Lang=E&Tab=1&Geo1=PR&Code1=01&Geo2=PR&Code2=01&Data=Count&SearchText=&SearchType=Begins&SearchPR=01&B1=All&Custom=&TABID=1`,
  md5 290269093e4383386ead96eeedb11912). It has single years 15-19, so 18+
  and the six bands are exact: men 3,086,620, women 3,269,905; 693,310 /
  1,022,110 / 1,019,030 / 1,272,270 / 1,092,110 / 1,257,685. The topic-based
  table 98-311-XCB2011018 is only partly live (its Quebec filter returns
  "File not found") and is not needed.
- **2006 age by sex at 18+. Applied (provenance for the owner to accept,
  D6).** Table 97-551-XCB2006009 (Age (123) and Sex (3), 2001 and 2006,
  100% data) is gone from Statistics Canada's site; its Beyond 20/20 file
  survives in the Internet Archive
  (`https://web.archive.org/web/20130701214950id_/http://www12.statcan.gc.ca/census-recensement/2006/dp-pd/tbt/Download.cfm?PID=88984`,
  md5 899bc736d3e590a5658a05a9c00bf08a). `data-raw/build_benchmarks.R`
  reads its Quebec 2006 block at a fixed offset and checks it: the file's
  md5, the Quebec total (7,546,135) and median ages (41.0, 39.9, 41.9) of
  the 2006 Census Profile, male plus female equal to the total, each
  five-year band equal to the sum of its single years. 18+: men 2,896,890,
  women 3,100,020; 650,465 / 960,200 / 1,121,430 / 1,232,125 / 952,430 /
  1,080,290. The fallback is the population estimates 17-10-0005-01
  (adjusted for undercoverage, not census counts).
- **2006 mother tongue. Not applied (D6).** The only 2006 table found by
  age, 97-555-XCB2006019 (100% data, also from the Internet Archive),
  gives at 20+ French 4,611,975, English 436,295, other 595,395 (single
  answers), but counts institutional residents and records far more
  multiple mother tongues than the sample data or 2011 (3.3% of the
  population against 2.0%; English and French together 105,325 against
  43,335 in the 2006 Profile). So `qes2007`, `qes2008`, the 2007 panel and
  CROP still have no mother-tongue comparison.
- **What changed.** `census_margins.csv`: gender and six age bands at 18+
  for 2006 and 2011, as for 2016 and 2021 (they were gender 20+ and five
  bands from 25, private households). Mother tongue stays 20+ and education
  25+ (the tables have no 18+ cut; a custom tabulation would be the only
  way, and is not worth asking for). `validation_report.csv` re-recorded:
  the 2006 and 2011 indices of every study, and new weighted rows for
  `qes2007` (recall 7.33) and the 2018 panel (recall 5.53). The validation
  article (EN/FR) says so.

## 1.7 Élections Québec licence (was R4 and the HZ7 deferred item). Facts
settled; the choice is D5.

- Élections Québec publishes its own open-data licence, the « Licence
  d'utilisation des données ouvertes du directeur général des élections »
  (`https://www.dgeq.org/licence.html`; English
  `https://www.dgeq.org/en/licence.html`; consulted 2026-09-27). The
  earlier note that no open licence exists was wrong. It is free,
  non-exclusive and non-transferable, for any lawful purpose including
  commercial use, with reproduction, translation, compilation, public
  communication and sub-licensing. It requires a fixed notice at all times
  (quoted in `inst/COPYRIGHTS`), an indemnity, no suggestion of endorsement
  and no use of the logos; the Chief Electoral Officer may cancel it
  without giving a reason, which cancels every sub-licence. It is not a
  Creative Commons licence.
- It covers the data "available or referenced" on dgeq.org. The archives
  page (`https://www.dgeq.org/archives.html`) lists the general elections
  of 2012, 2014, 2018 and 2022: for 2014, 2018 and 2022 the same
  `resultats.json` files qesR uses; for 2012 only the candidate lists and
  the poll-level results (`resultats-bureau-vote.zip`), not the
  `gen2012-09-04/resultats.json` qesR cites. electionsquebec.qc.ca says the
  archive goes back to 2012. On a cautious reading, 1998, 2007, 2008 and
  2012 (as qesR takes it) fall under the site's terms of use
  (`https://www.electionsquebec.qc.ca/notre-institution/conditions-dutilisation/`):
  non-profit reproduction with source and copyright; written permission
  for other uses and adaptations.
- Élections Québec has no organization and no dataset on Données Québec.
  The « Atlas des élections au Québec » there (Fondation Lionel-Groulx,
  CC BY-SA 4.0) has per-riding results of 1998-2012 whose 1998 province
  totals equal Élections Québec's exactly (2007 and 2008 not checked), but
  its note does not show that the Foundation could relicense Élections
  Québec's figures, so it is not a way around the permission.
- Done: `inst/COPYRIGHTS` (section 3) and `cran-comments.md` state these
  facts; `official_results.csv`, `official_turnout.csv` and
  `validation_report.csv` stay build-ignored (they mix covered and
  uncovered years, and the licence is revocable with an indemnity).

## 1.8 Decisions taken in earlier slices (recorded; they stand unless the
owner reopens one)

- **Validation (HZ7).** Education compared as university or below (V2),
  because the census counts credentials completed and the surveys the
  level reached. Census year: the census on or before the study's latest
  year, for the whole study (V3). Gated: the weighted V-L2 indices and the
  weighted census indices, at the recorded value + 2.0 points; unweighted
  rows for information; turnout within 0-35 points; `pid_prov` agreement
  among partisans who reported a party vote (V4). The report lives in
  `inst/extdata/validation/` (V5). Findings, no change: `qes2018` weighted
  education counts incomplete university outside university (F2); its age
  cells cut across ten-year bands (F3); the V-L2 baselines 8.0, 5.6, 3.2
  and 8.0 of design section 8.3 are confirmed (F4).
- **Legacy switch (HZ6).** The legacy builders applied rows in review
  (`include_draft = TRUE`), with the grade of each cell in
  `attr(, "source_map")` (H1; superseded by spec 4.0.0: signed-off rows
  only, see below). "Did not vote / None" and "Don't know /
  Refused" of `vote_choice` are `NA` with reasons (H2). `gender` keeps
  Non-binary and Other (H3). Six age bands for the 2007 and 2012 panels,
  three for the 2018 panel (H4). `qes2012` education code 10 is
  `not_mappable`, code 8 college (H5). `get_decon()` types follow the
  targets (H6). `respondent_id` of the panels is the ID part of `qes_id`
  (H7). The 2007 panel's time-invariant items read in either wave (H8).
- **Remaining studies (HZ5).** "None" among voters is `spoiled` (item 10).
  The Parti Égalité (1998) is `other` (11). 1998 `intvote` feeds the pushed
  intention (12). CROP `intvoteprov` code 7 is `not_mappable` (13). The
  firms' own 1998 files are not harmonized, the firm being a stratum of
  `qes1998` (14). Departures from the design text: 1998 `pre` and `post`
  waves (15); poll waves and wave `*` (16); the leading column `stratum`
  (17); 1998 `sov_partnership_1995` from `q16a_crop` without the push
  (17a); weights registered per wave (17b). Item 18 (version numbering) is
  superseded by D10.
- **Spec review (2.0.0).** The `religion` gates, the 2007 panel's turnout
  from `voteoui`, the text-only notes (R1 to R5 as recorded in CHANGES.csv).
- **Review sign-off (4.0.0).** An automated double review of the 132
  crosswalk rows against the original files and documents (codes and data;
  wording and comparability; adjudicated where the two passes disagreed),
  2026-09-27, not a human review: `reviewed_by` says so on every row. 96
  rows were signed off as they were and 36 after a correction, each
  checked on the originals and the documents before it was applied and
  named in the row's `review_note` (schema version 2). 93 rows were
  `stable`; the 39 it held in review (D12) are `stable` since 4.1.0
  (below). `get_qes_master()` and `get_decon()` apply signed-off rows only (`include_draft = FALSE`):
  a column whose question is in a held row is `NA`, reason `not_reviewed`,
  cause `not_signed_off`, basis the row's `review_note`; every difference
  from 0.5.0 is explained in `dev/legacy-diff.md`. MAJOR: `qes2022`
  `cps_income` code 0 is `no_answer` (a blank sent to the follow-up
  `cps_income2`) and `cps_votechoice1_8_TEXT` is gated like its parent
  row.
- **Content sign-off apart from the weights (4.1.0, the owner's decision
  of 2026-09-28; design.md OD20).** The release rule of V-S13 that failed
  a stable row on a study-wave whose recommended weight needs review is
  removed. The 38 rows it held (D12 (a)) are `stable`: their `review_note`
  still says the review was automated, not human, and that the weight is
  tracked apart (Q1). The weights stay `needs_review` and unapplied (`NA`,
  with a message; `qes_design()` leaves their rows out; in
  `get_qes_master()` `weight_pre` and `weight_post` have reason
  `not_reviewed`, cause `weight_needs_review`). `qes2014` `QSEXE` is
  `stable` at `comparable`, the grade it had before the review, with the
  English stem the review filled; `identical` waits for a human second
  reviewer (D12 (b)). All 132 rows are `stable`.
- **OD3 lifted: `qes2022` metadata ships (qesR 0.7.1, spec 4.2.0, the
  owner's decision of 2026-09-28; design.md OD3).** The package ships the
  metadata of `qes2022` like that of the other studies, under the study's
  licence, CC BY-NC 4.0, with the attribution the licence requires; that
  content is not covered by the MIT licence (`inst/COPYRIGHTS`, section 2,
  lists the files, and a test checks the list). The dictionary holds its
  718 variables, labels, value labels, counts and missing codes, and the
  question text of 464 variables (448 of them also in French) quoted from
  its bilingual codebook (drafts, `reviewed = FALSE`); the runtime shard and
  `inst/extdata/dict/shard_rules.csv` are gone. The spec quotes its wording
  and labels (18 crosswalk rows, 65 value-map rows) and `gates.csv` and
  `expected/marginals.csv` hold its counts; the build-ignored
  `data-raw/nc/` is removed, its aggregates moved (identical) to the
  shipped files, the recorded validation report and the 0.4.4 fixture.
  D11 is resolved by this decision, Q4 becomes an optional courtesy notice,
  and `cran-comments.md` asks CRAN whether CC BY-NC 4.0 files are
  acceptable in the package (qesR can fetch them at runtime instead). The
  MIT licence is stated to cover the package code only (`inst/COPYRIGHTS`,
  `DESCRIPTION`, README, website, `?qesR-package`, `?qesR-fr`), and a
  printed `qes2022` codebook and the harmonization reference repeat the
  attribution and licence.
- **Live failure of tag v0.7.0 (the owner's items of 2026-09-29).** The
  `live` workflow failed on Ubuntu, R 4.6.1: the shipped dictionary listed
  the observed values of continuous columns with at most 50 values (the
  `qes2007_panel` weights, keyed as 15-significant-digit text such as
  `"1.43011282409"`), whose text differs across platforms and R versions.
  qesR 0.7.1 lists no value of a continuous column (a fraction, a weight or
  an id; 564 rows of `qes1998`, `qes1998_createc`, `qes1998_crop` and
  `qes2007_panel`), so every listed code is a whole number, and the tests
  check it offline and live. `live.yml` also keeps its cache at an absolute
  path (`runner.temp/qesr-cache`), since `actions/cache` rejected
  `../qesr-cache` and never saved the originals.

## 1.9 Deferred work (other slices; no owner action)

- Targets not in the spec yet: previous provincial vote (2008 `q13`, CROP
  `QP4`, whose reference election is now known), federal vote, party
  identification strength, region, the push sovereignty items, each a
  target of its own; `vote_prov_lean` (for `party_lean`); the appended
  columns `region_cma3`, `region_admin`, `income_rank`.
- `fn:` rules (none registered yet): the 1998 unpushed intention, the CROP
  and 2007 panel pushed intention without the merged code, and `qes2022`
  `language` (`fn:multiselect`).
- `income` and `religion` of `qes2018` (no value labels in the file),
  `vote_choice_text`, the English question text of `qes2008`, the stray
  "3." of `qes2014` `Q57` in the dictionary.
- `age` from the year of birth and age groups from ages as targets; the
  panel stability check of design section 8.3.
- Optionally register `weight_web` of the 2018 panel (850 web rows, label
  Weight_Web_Only), not recommended.

---

# Part 2. Needs the owner

## 2.1 Requests to send (the owner sends them; Claude sent nothing)

Each request uses the addressee's own contact channel (the "Contact"
button of the Borealis or Harvard Dataverse dataset page, or the
organization's contact form); no address is written here. None of them
blocks a release: until an answer comes, the conservative choice of part 1
holds.

**Q1. Claire Durand (Université de Montréal), dataset contact of the CROP
polls (doi:10.5683/SP3/IRZ1PF), the 1998 polls (doi:10.5683/SP2/QFUAWG),
the 2018 panel (doi:10.5683/SP3/XDDMMR) and the 2007 and 2012 panels
(doi:10.5683/SP3/NDS6VT, doi:10.5683/SP3/RKHPVL).** Contact button of the
CROP dataset page on Borealis. Why: these are the documents and facts only
the producer holds; they would let the CROP referendum item be mapped, the
1998 weight be chosen and the panels' weights be reviewed.

> Madame Durand,
>
> Je prépare qesR, un paquet R libre (licence MIT) qui télécharge et
> harmonise les Études électorales québécoises et vos dépôts de sondages
> sur Borealis. Quelques points ne sont pas documentés dans les dépôts ;
> pourriez-vous m'éclairer ?
>
> 1. Sondages CROP 2007-2010 (doi:10.5683/SP3/IRZ1PF). (a) Les
>    questionnaires ou rapports CROP-Express de mai 2009 (14-24 mai,
>    n = 1 001, projet 19), juin 2009 (11-18 juin, n = 1 003, projet 20),
>    août 2009 (projet 21), septembre 2009 (projet 22) et janvier 2010
>    (projet 24) : la question intvoterefa y portait-elle sur un « pays
>    souverain » ou sur la « souveraineté-partenariat » ? (b) Le libellé
>    anglais de cette question. (c) XPOND est-elle la pondération livrée
>    par CROP, et à quoi correspondent ses totaux de population, qui
>    changent par bloc de sondages (6 117 k, 5 997 k, 5 905 k, 5 881 k,
>    6 169 k, 6 215 k) ?
> 2. Sondages de 1998 (doi:10.5683/SP2/QFUAWG). (a) ponder3 (= ponderc x
>    poids, normalisée dans chaque firme) est-elle la bonne pondération du
>    fichier panel regroupé, pour les deux vagues ? La description du
>    fichier CREATEC (316121) recommande ponderc plutôt que poids, ponder2
>    et ponder3, mais ponderc ne corrige pas la sur-sélection des discrets ;
>    cette recommandation vise-t-elle seulement les estimations
>    transversales de CREATEC ? (b) Sur quelles marges et quel recensement
>    ponderc a-t-elle été calée, pour CREATEC et pour CROP ? (c) Le
>    questionnaire préélectoral de CREATEC (18-23 novembre 1998) et son
>    questionnaire de rappel pourraient-ils être déposés ? (d) Quels sont
>    les trois sondages préélectoraux recontactés ? Les dates d'entrevue de
>    CREATEC se séparent en 18-20 et 21-23 novembre.
> 3. Panel de 2018 (doi:10.5683/SP3/XDDMMR). (a) Pouvez-vous confirmer les
>    dates du recontact, du 12 au 19 octobre 2018 (métadonnées du dépôt) ?
>    (b) Le fichier d'Ipsos (projet 17-057727) contient-il le mode de
>    l'entrevue postélectorale de chaque répondant (592 en ligne, 250 par
>    téléphone) ? Si oui, pourrait-il être ajouté au dépôt ?
> 4. Panels de 2007 et de 2012. (a) 2012 (doi:10.5683/SP3/RKHPVL) : sur
>    quelles marges pond_post (« pondération_recensement ») a-t-elle été
>    calée, et pondam1 a-t-elle été normalisée sur l'échantillon
>    préélectoral complet ? Le questionnaire préélectoral pourrait-il être
>    déposé ? (b) 2007 (doi:10.5683/SP3/NDS6VT) : quelle pondération
>    utiliser pour la vague postélectorale, y compris les 391 refus
>    convertis (pond_tot_am1 ?) ?
>
> Je vous remercie de votre aide.
>
> Thomas Gareau-Paquette

**Q2. Éric Bélanger (McGill University), dataset contact of the QES 2007,
2008 and 2012 (doi:10.5683/SP2/6XGOKA, doi:10.5683/SP2/8KEYU3,
doi:10.5683/SP2/WXUPXT); or Richard Nadeau (Université de Montréal).**
Contact button of the QES 2008 dataset page on Borealis. Why: the 2008 mode
and weights and the 2007 web questionnaire are not deposited, and the file
contradicts the book on the 2008 weight.

> Monsieur Bélanger,
>
> Je prépare qesR, un paquet R libre (licence MIT) qui harmonise les Études
> électorales québécoises à partir de vos dépôts sur Borealis. Trois points
> ne sont pas documentés :
>
> 1. ÉEQ 2008 (doi:10.5683/SP2/8KEYU3). La note technique de Léger sur
>    l'enquête postélectorale de décembre 2008 (comme celles de 2012 et de
>    2014, fichiers 196368 et 352011) pourrait-elle être déposée ? En
>    particulier : (a) le mode de collecte : les métadonnées indiquent
>    « Entrevue téléphonique », mais les questionnaires déposés (196358,
>    197296) ressemblent à un questionnaire Web (aucune consigne
>    d'intervieweur, « Je préfère ne pas répondre », identifiants PIN et
>    LMID) ; (b) les variables et le recensement de pond et de pondx : dans
>    le fichier, pond dépend aussi de la scolarité et du vote déclaré et
>    reproduit les résultats officiels de 2008, alors que votre livre (PUM
>    2009, ch. 2, note 5) ne nomme que le sexe, l'âge, la langue maternelle
>    et la région. Si le mode est le Web, la métadonnée du dépôt pourrait
>    être corrigée.
> 2. ÉEQ 2007 (doi:10.5683/SP2/6XGOKA). Le questionnaire Web (seul le
>    questionnaire téléphonique est déposé), pour savoir si « ne sait pas »
>    était offert en ligne.
> 3. ÉEQ 2012 (doi:10.5683/SP2/WXUPXT). Le « dernier recensement » de la
>    variable POND est-il celui de 2006 ? Ses marges de langue maternelle
>    sont celles de 2008.
>
> Je vous remercie de votre aide.
>
> Thomas Gareau-Paquette

**Q3. Élections Québec (directeur général des élections du Québec),
"Demandes d'autorisation et questions",
`https://www.electionsquebec.qc.ca/nous-joindre/`.** Only if D5 chooses to
ship the official results. Why: the terms of use require written
permission for the years not on dgeq.org and for the adaptation (minor
parties summed).

> Bonjour,
>
> Je maintiens qesR, un paquet R libre (licence MIT, diffusé sur CRAN) qui
> compare les enquêtes électorales québécoises aux résultats officiels.
> J'aimerais y inclure les totaux de l'ensemble du Québec (votes par parti,
> les petits partis additionnés en une ligne ; bulletins valides, rejetés
> et déposés ; électeurs inscrits ; taux de participation) des élections
> générales du 30 novembre 1998, du 26 mars 2007, du 8 décembre 2008 et du
> 4 septembre 2012, tirés des fichiers
> https://donnees.electionsquebec.qc.ca/production/provincial/resultats/archives/gen<date>/resultats.json.
>
> 1. M'autorisez-vous à redistribuer et à adapter ces totaux dans qesR,
>    avec la mention de la source et de votre droit d'auteur ?
> 2. Ces fichiers d'archives, qui ne sont pas listés sur dgeq.org, sont-ils
>    couverts par la Licence d'utilisation des données ouvertes du
>    directeur général des élections, comme ceux de 2014, 2018 et 2022 ?
> 3. Pour 2014, 2018 et 2022, la licence est-elle compatible avec une
>    redistribution dans un paquet sous licence MIT, compte tenu des
>    clauses d'indemnisation et de résiliation ?
>
> Merci à l'avance.
>
> Thomas Gareau-Paquette

**Q4. Optional courtesy notice to Laura B. Stephenson (Western
University), dataset contact of the 2022 QES (doi:10.7910/DVN/PAQBDR), for
its authors (Mahéo, Bélanger, Stephenson, Harell).** Contact button of the
Harvard Dataverse dataset page. No longer a request for permission: the
owner lifted OD3 on 2026-09-28 (1.8), and qesR 0.7.1 ships the study's
labels, codebook wording and counts under its own licence, CC BY-NC 4.0,
with the attribution it requires (non-commercial redistribution with
attribution is what that licence allows). Nothing waits on an answer; the
notice only tells the authors, and gives them a way to object or to ask for
a different attribution.

> Dear Professor Stephenson,
>
> I maintain qesR, a free R package that downloads and harmonizes the
> Quebec Election Studies. From version 0.7.1, qesR ships metadata of the
> 2022 QES (doi:10.7910/DVN/PAQBDR): its variable and value labels, the
> question wording of its codebook in English and French, and unweighted
> counts of the answers (used to check the harmonization). These files keep
> the study's licence, CC BY-NC 4.0, with an attribution to you and your
> co-authors and a link to the licence, and they are marked as not covered
> by the package's MIT licence. No respondent data ship: the data file is
> downloaded from Harvard Dataverse at the user's request. If you would
> prefer another form of attribution, or would rather these files not be
> distributed, please tell me.
>
> Thank you,
>
> Thomas Gareau-Paquette

**Q5. Optional reading through the owner's library access (paywalled, not
fetched).** No one to write to. Durand, Blais and Vachon 2001, *POQ*
65(1):108-123 (`https://doi.org/10.1086/320041`), methodology section: does
it name the 1998 weight? Stephenson and Crête 2011, *IJPOR* 23(1):24
(QES 2007 web and telephone samples). Bélanger and Gélineau 2011, *CJPS*
44(3):529-551 (`https://doi.org/10.1017/S0008423911000461`), data section
(2008 mode and weights). The body of Bélanger and Nadeau 2009 (PUM),
chapters 2, 3 and 6.

**Q6. Conditional: Statistics Canada (statcan.gc.ca contact form).** Only
if D6 rejects the decoded Internet Archive file. Why: the 2006 tables are
no longer published.

> Bonjour, pourriez-vous me fournir en CSV, pour le Québec, les tableaux du
> Recensement de 2006 97-551-XCB2006009 (âge et sexe) et 97-555-XCB2006019
> (langue maternelle, groupes d'âge et sexe), qui ne sont plus en ligne ?
> Ils servent de repères dans un paquet R libre, sous la Licence ouverte de
> Statistique Canada. Merci.

## 2.2 Decisions only the owner can take

**D1. Sign-off.** All 132 rows are `stable` after the automated double
review of spec 4.0.0 (1.8), which is not a human review; design.md made
the owner's review the sign-off. To decide: (a) whether the automated
review stands as the sign-off for the release, or which rows the owner
reviews by hand first (setting `reviewed_by` to the reviewer and, for a
row that changes, a CHANGES.csv entry); (b) whether to accept as
`reviewed` the weights described from CROP's reports (`XPOND`, 1.1) and
from the files only (the 2007 and 2012 panels, 1.4), or keep them
`needs_review` until Q1 is answered. Since spec 4.1.0 (the owner's
decision of 2026-09-28, design.md OD20) this choice is about the weights
only: the content sign-off of a row no longer depends on its weight, and
the rows of these studies are `stable` either way. Until a weight is
accepted, its values are `NA` (in `qes_harmonize()` and in `weight_pre`
and `weight_post` of `get_qes_master()`) and `qes_design()` leaves its
rows out. Weights set to `reviewed` in the round of spec 3.0.0, on
producer documents checked on the file: `qes2007` `pond`, the 2018
panel's `weight` and `weight_rts`; the owner may revert them.

**D2. CROP sovereign-country item.** `intvoterefa` is documented as the
sovereign-country question in 19 of 24 polls (1.1) but cannot be mapped
for those only. Options: (a) wait for Q1 (current); (b) map all 24 polls
under `sov_sovereign_country` at grade `approximate`, with the five
undocumented polls named in the row's notes; (c) extend the grammar so a
pooled-polls study may have one primary row per poll wave (engine, V-S5,
column hashes, provenance and views change). Recommendation: (a), then (b)
if Q1 gets no answer.

**D3. 1998 recommended weight.** `ponder3` (the design correction, which
reproduces the PIs' published tables) against `ponderc` (the deposit's
advice, which does not correct it) (1.2). Options: wait for Q1 (current:
`ponder3` registered, `needs_review`, so 1998 estimates are unweighted);
or sign off `ponder3` now on the Table 2 evidence, keeping the producer's
contrary advice in `source_ref`. `get_qes_master()` `survey_weight` stays
`ponderc` in either case (the 0.4.4 source); change it only if the owner
prefers consistency to backward compatibility (a legacy value change).

**D4. `qes2008` weight.** Both weights are calibrated on vote or turnout,
so `qes2008` has no recommended weight and V-S13 now allows that (1.3).
Options: keep (current); or recommend `pond` anyway (it is the one weight
without the turnout adjustment), which needs V-S13 relaxed further and
makes the V-L2 recall check of `qes2008` circular (it would have to be
exempted from the gate).

**D5. Élections Québec results.** (1.7) Options: (a) keep the three files
build-ignored (current); (b) accept the dgeq.org licence for 2014, 2018
and 2022 and ship those rows in a separate file with the required notice,
the rest staying build-ignored until Q3 is answered (the revocation and
indemnity clauses probably fall short of what CRAN expects of a data
licence); (c) send Q3 and ship everything with the permission. Also
decide whether to recompute 2012 from the listed poll-level file (not
checked yet).

**D6. 2006 census benchmarks.** (1.6) (a) Accept the decoded Internet
Archive copy of Statistics Canada's 2006 file for age by sex at 18+
(applied), or revert to the undercoverage-adjusted estimates 17-10-0005-01,
or send Q6. (b) Whether to add a 2006 mother-tongue benchmark from
97-555-XCB2006019 despite its universe (institutional residents,
3.3% multiple answers against 2.0% in 2011): not applied. (c) Keep 20+
for mother tongue and 25+ for education (no 18+ cut is published).

**D7. `qes2008` mode.** Stays telephone, as the deposit says, until Q2 is
answered. If no answer comes, decide whether the questionnaire evidence
(1.3) is enough to record it as web; grades are `comparable` either way.

**D8. Vocational credentials in `education4` (was R2).** `qes2018` counts
the DEP as secondary, `qes2018_panel` its trade certificate as college, and
studies with no DEP option probably put DEP holders under technical
(college). One rule is needed for both (a map change is MAJOR): college
(the anchor's reading: move `qes2018` code 9 to 3), or `not_mappable` in
both (Statistics Canada classes trades apart from college).

**D9. `qes1998` `language` in `get_qes_master()` (was R3).** The pooled
definition is now confirmed (1.2), so the choice is only between the
constant French (current, noted as the home language for the 426 CROP
rows) and the stricter render (French for CREATEC, `NA` for CROP), which
needs a stratum-conditional render in `legacy.csv` (engine, validator,
tests).

**D10. Release numbering (was R1, V7, item 18).** The spec is 4.0.0
(MAJOR changes after 1.0.0 without a CRAN release in between). Options:
keep the history as it is, or fold 2.0.0 to 4.0.0 into one entry before the
first release. Done for the package (2026-09-28): `Version: 0.7.0`, the
development subsections of NEWS.md merged into `# qesR 0.7.0`, the tarball
built and checked with `sh data-raw/build_tarball.sh <dir> --check` (the
"large components" NOTE is gone), and the date, NOTE list and test counts
of `cran-comments.md` updated. Left to the owner: commit, push, run the
GitHub checks and win-builder (their results go to the maintainer) and
fill in their results and the spelling NOTE in `cran-comments.md`.

**D11. `qes2022` under OD3 (was R5, V6). Resolved (2026-09-28).** The
owner lifted OD3: the metadata of `qes2022` ships under CC BY-NC 4.0
(1.8), so its row count in `expected/hashes.csv`, its grade reasons, its
wording, labels and counts, and the website's aggregates of it all stay.
What remains open is CRAN's answer on CC BY-NC 4.0 files in the package
(`cran-comments.md`).

**D12. Rows held in review (spec 4.0.0; released in spec 4.1.0).** The
automated review signed 39 rows off on their content but left them in
`review`. Spec 4.1.0 (the owner's decision of 2026-09-28, design.md OD20)
makes all of them `stable`; what is left for a person:

(a) The 38 rows of `qes1998`, `qes2007_panel`, `qes2012_panel` and the
CROP polls were held only by the release rule of V-S13 (a stable row on a
study-wave whose recommended weight needs review), which is removed: the
content sign-off of a row is now separate from the review of its wave's
weight. They are `stable`, with a `review_note` that says the review was
automated (not human) and that the weight is tracked apart. No row is
held for a decision (one choice of instrument is below, with the other
decisions the review raised); their weights are D1 (b) (`qes2007_panel`
`pondam1` and `pond_tot_am1`, `qes2012_panel` `pondam1` and `pond_post`,
CROP `XPOND`) and D3 (`qes1998` `ponder3`), and Q1 asks the producer. The
rows were: `qes1998` `intvote` and `intvote2` (`vote_prov_intent_push`),
`q3post` (`vote_prov_recall`), `q1post` (`turnout_prov_recall`),
`q16a_crop` (`sov_partnership_1995`), `age` (`age_group3`, `age_group6`),
`sexe_post` (`gender`); `qes2007_panel` `intvote1`, `intvote`, `vote`,
`voteoui`, `intref1`, `age` (two rows), `sexe`, `scol`, `lmat`, `revenu`,
`interet`; `qes2012_panel` `intvoteref`, `intvoteprov1`, `intvoteprov`,
`voteprov`, `participation`, `interetrec` (documentation only), `age` (two
rows), `sexe`, `lmat`; CROP `intvoteprova`,
`intvoteprov`, `QAGE` (two rows), `SEXE`, `scol`, `lmat`, `revenu`.

(b) `qes2014` `QSEXE` (`gender`): the review raised it from `comparable` to
`identical` (the anchor's stems in both languages: "What is your
gender?" / "Quel est votre sexe?"), and design.md section 11.1, step 3,
gives an `identical` row that pools fielding languages to a second
reviewer. Spec 4.1.0 keeps the conservative grade, `comparable`, with the
English stem the review filled, and makes the row `stable`. To decide: a
human second reviewer confirms `identical` (a grade change that does not
cross the default `min_grade`, so not MAJOR under design.md section 5.11:
MINOR, with a CHANGES.csv entry) or leaves it at `comparable`.

Other decisions the review raised (no row is held for them): whether to
keep the instrument `sex_recorded` of `qes2012_panel` `sexe`, which is
inferred, or use `gender_2` as the 2018 panel does; whether to
use the `cps_income2` brackets for the `qes2022` respondents who left the
amount blank (a two-source rule the spec does not have yet; the 17 whose
follow-up says "no income" are `no_answer` for now); whether the
two-language options that `qes2012` and `qes2008` list in their
questionnaires but not in their files call for another instrument name
(`lang_first_multi`) for both.

**Held pending evidence:** none. No row is held in review (spec 4.1.0);
the weights that need review wait for Q1 (D1 (b), D3).

**D13. Rows of spec 4.3.0 held for the owner (review of 2026-09-29).** The
automated double review of the 104 rows added in spec 4.3.0 (against the
original files and documents, adjudicated where the two reviewers
disagreed; not a human review) signed 101 off (`stable`), 45 of them
corrected or completed, text in most cases. Three rows stay in `review` (their `review_note` says
why) until the owner decides:

(a) `qes2022` `cps_provpidstr` (`pid_prov_strength`): the English version
asks how strongly the respondent feels, the French one (1,164 of the
1,352 answers) how close, the wording graded `approximate` in 2007 and
2008. The review lowered the grade from `comparable` to `approximate` and
asks the owner to confirm it before sign-off (a grade that crosses the
`comparable` threshold).

(b) `qes2008` `q13` and `qes2018` `q9` (`vote_prov_prev`): respondents
under 18 at the previous election were asked and answered (5 born in 1990
in 2008, all "did not vote"; 268 born 1997-2000 in 2018, 58 of whom name
a party or another party). The reviewers asked for a gate on the year of
birth setting them to `ineligible`; a gate in the spec states the
questionnaire's routing and the data check V-D7 rejects it (they were
asked), and the spec has no rule that overrides an answer by eligibility.
To decide: add such a rule (for example an `eligible` gate kind that V-D7
does not read as routing, a MINOR schema change), or sign the rows off
with the answers kept and the caveat in their notes (as written now).

The legacy freeze (design.md section 0.5, V-S19): two signed-off rows
would have changed a legacy column, `qes2012` `pid_fed` (`federal_pid`,
`fed_pid`) and `qes2022` `lang_mother` (`language`). They are kept as in
0.7.1 (`legacy.csv` study rows of cause `legacy_frozen`); letting them fill is
an owner decision that changes `get_qes_master()`/`get_decon()` output and
needs a NEWS line.
