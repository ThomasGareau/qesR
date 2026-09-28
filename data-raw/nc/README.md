# data-raw/nc: aggregates of the 2022 Quebec Election Study (CC BY-NC 4.0)

The files in this folder are derived from the 2022 Quebec Election Study:

> Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison
> (2023). *2022 Quebec Election Study*. Harvard Dataverse, V1.1.
> <https://doi.org/10.7910/DVN/PAQBDR>

That study is licensed under Creative Commons Attribution-NonCommercial 4.0
International (CC BY-NC 4.0,
<https://creativecommons.org/licenses/by-nc/4.0/>). **These files are covered
by that licence, not by the MIT licence of qesR**, and they may not be used for
commercial purposes. When you reuse them, cite the study as above
(`qes_cite("qes2022")` gives the full citation).

What the files hold (no respondent rows):

| File | Content |
|---|---|
| `sources_qes2022_variables.csv` | variable names, types and declared missing codes |
| `sources_qes2022_values.csv` | codes, the md5 hash of each value label, and the unweighted count of each code |
| `missing_labels_qes2022.csv` | the value labels that mark item nonresponse (plain label text) |
| `gates_qes2022.csv` | unweighted counts of each answer to a question and to the question that filters it |
| `marginals_qes2022.csv` | unweighted counts of each harmonized category |
| `validation_qes2022.csv` | the qes2022 rows of the validation report (weighted and unweighted shares, differences from the official results and the census) |
| `v044_n_na_qes2022.csv` | the number of missing values of each column that `get_qes("qes2022")` returned in qesR 0.4.4 (the qes2022 part of the `n_na` column of `tests/testthat/fixtures/v044-get-qes-names.csv`, which ships without it) |

They are tracked in the source repository only for the offline and live
checks of GitHub CI (`data-raw/spec_check.R`, `data-raw/build_hashes.R`,
`tests/testthat/test-validation-live.R`, `tests/testthat/test-live-read.R`). `data-raw/` is excluded from the
package build (`.Rbuildignore`), so nothing here ships with qesR (design OD3;
`inst/COPYRIGHTS`, section 2).

The weekly live workflow (`.github/workflows/live.yml`) uploads the validation
report, `qes2022` rows included, as a workflow artifact; this file is uploaded
with it as its licence and attribution notice.
