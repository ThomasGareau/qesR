# Soft-deprecated qesR functions

Eleven functions from qesR 0.4.4 are kept as **legacy wrappers**. They
keep working, with the same arguments, and will not be removed. Each one
prints a short notice naming its replacement, once per session.

|  |  |  |
|----|----|----|
| Legacy function | Replacement | Deprecated since |
| [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md) | [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md) | 0.5.0 |
| [`get_qes_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md) | [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md) | 0.5.0 |
| [`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md) | `head(get_qes(srvy), obs)` | 0.5.0 |
| [`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md) | `qes_codebook(codebook, layout = )` | 0.5.0 |
| [`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md) | `qes_codebook(layout = "long")` | 0.5.0 |
| [`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md) | [`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md) | 0.5.0 |
| [`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md) | [`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md) | 0.5.0 |
| [`get_qes_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md) | [`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md) | 0.5.0 |
| [`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md) | `qes_download(what = "docs")` | 0.5.0 |
| [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md) | [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md) | 0.5.0 |
| [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md) | `qes_harmonize(srvy, targets = "decon")` | 0.7.0 |

## Notices

The notice is a message of class `qesR_message_deprecated`, not a
warning, so it never turns into an error under `options(warn = 2)`. It
is shown once per session for each function. `quiet = TRUE` does not
hide it; set `options(qesR.quiet_deprecated = TRUE)` to hide all of
them. Its language follows `options(qesR.lang =)` (see
[qesR-package](https://thomasgareau.github.io/qesR/reference/qesR-package.md));
the data returned never depends on the language.

## En français

Onze fonctions de qesR 0.4.4 restent disponibles comme fonctions
héritées : elles continuent de fonctionner, avec les mêmes arguments, et
ne seront pas retirées. Chacune affiche une fois par session une courte
note qui nomme la fonction qui la remplace (depuis 0.5.0, et depuis
0.7.0 pour
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md)).
La note est un message de classe `qesR_message_deprecated` ;
`quiet = TRUE` ne la masque pas, mais
`options(qesR.quiet_deprecated = TRUE)` la masque. Elle s'affiche en
français avec `options(qesR.lang = "fr")`.

## Examples

``` r
# a legacy function and its replacement give the same rows
old_rows <- get_preview("qes_demo", obs = 2)
new_rows <- head(get_qes("qes_demo", assign_global = FALSE, quiet = TRUE), 2)
identical(dim(old_rows), dim(new_rows))
#> [1] TRUE

# hide the notices of every legacy function
op <- options(qesR.quiet_deprecated = TRUE)
head(get_qescodes(), 3)
#>   index qes_survey_code get_qes_call_char
#> 1     1         qes2022         "qes2022"
#> 2     2         qes2018         "qes2018"
#> 3     3   qes2018_panel   "qes2018_panel"
options(op)
```
