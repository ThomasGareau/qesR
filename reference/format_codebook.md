# Reformat a qesR Codebook

Reformats a `qes_codebook` object into compact, wide, or long layout.

## Usage

``` r
format_codebook(codebook, layout = c("compact", "wide", "long"))
```

## Arguments

- codebook:

  A `qes_codebook` object from
  [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md).

- layout:

  One of `"compact"`, `"wide"`, or `"long"`.

## Value

A reformatted codebook data frame.
