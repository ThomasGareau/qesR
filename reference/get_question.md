# Get Survey Question Text

Returns question text from variable labels or an attached/paired
codebook, with optional full-question recovery.

## Usage

``` r
get_question(do, q, full = TRUE)
```

## Arguments

- do:

  A data.frame or the name of one in the global environment.

- q:

  Column name whose question text should be returned.

- full:

  If TRUE, try to recover full question text when metadata appears
  truncated.

## Value

A character scalar with question text, or `NA_character_` when none
exists.
