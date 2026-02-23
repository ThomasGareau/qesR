# qesR 0.4.4

- Added `get_qes_master()` for harmonized merged datasets across QES studies.
- Added merge quality controls: within-year respondent de-duplication and all-empty-row removal.
- Added broader harmonization for age, age group, education, turnout, vote choice, and weights.
- Renamed opaque legacy merged variables (e.g., `q10`, `q16`, `voteprec`) to readable names and added transparent old-to-new mapping via `variable_name_map`.
- Added variable-name mapping sidecar output (`*_variable_name_map.csv`) when saving/building merged datasets.
- Improved codebook workflows with compact/wide/long layouts and value-label extraction helpers.
- Updated researcher website use-case figures with clearer year-axis labeling, dynamic y-axis bounds, and expandable full code blocks for reproducibility.
- Added researcher website content with merged-dataset documentation, study citations, and analysis examples.

# qesR 0.4.3

- Added `get_question(..., full = TRUE)` support for richer question-text recovery.
- Improved codebook metadata parsing and handling of support files.

# qesR 0.4.2

- Expanded QES catalog entries and Dataverse file selection behavior.

# qesR 0.4.1

- Added `get_decon()` and export updates.

# qesR 0.4.0

- Added codebook retrieval and formatting helpers.
