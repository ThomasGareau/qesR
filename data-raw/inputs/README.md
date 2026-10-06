# Committed build inputs

Small public inputs are committed here so that the builders run from a clean
clone without a request. `data-raw/` is build-ignored, so none of these files
are in the package tarball.

## dataverse/: Dataverse metadata snapshots

These are the JSON of the pinned version of each of the 11 deposits in the
catalog, as returned by

    <server>/api/datasets/:persistentId/versions/<version>?persistentId=doi:<doi>

They were fetched on 2026-10-05 with plain requests (User-Agent
`qesR/0.9.1 R/4.4`, one at a time, at least 1 s apart). `index.csv` records
each file's DOI, server, pinned version (`dataset_version`), the publisher
(the installation's name, from the dataset endpoint), the URL, the licence,
the md5 of the committed file and any edit made to it.
`data-raw/build_catalog.R` reads these snapshots when `QESR_DV_JSON_DIR` is
unset, and refuses a snapshot whose md5 differs from `index.csv`.

Edit: in `10.7910_DVN_PAQBDR.json` (qes2022) the `datasetContactEmail` field
(a personal e-mail address) was removed. The catalog does not read it, and no
other byte was changed. The other ten files are verbatim.

Licence: these files are descriptive metadata. Each one is redistributed here
under the licence its deposit declares, which is the more cautious reading
whatever the platform's own metadata policy is. The platform terms pages
could not be read with a plain request: Harvard's support site returned 403,
best-practices.dataverse.org timed out, and Borealis publishes no API terms
of use.

- The 10 Borealis deposits (10.5683/...): CC0 1.0. No attribution is
  required. The source is Borealis, the Canadian Dataverse Repository.
- qes2022 (10.7910/DVN/PAQBDR, Harvard Dataverse): CC BY-NC 4.0. Attribution:
  Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison,
  2023, "2022 Quebec Election Study", https://doi.org/10.7910/DVN/PAQBDR,
  Harvard Dataverse, V1.1. The only change is the e-mail removal described
  above. The licence is http://creativecommons.org/licenses/by-nc/4.0.
  qesR's other qes2022 metadata carries the same licence (inst/COPYRIGHTS,
  section 2).

## Not committed

- The 2011 Census Profile zip (98-316-XWE2011001-101_CSV.zip, used by
  build_benchmarks.R). Statistics Canada's download page serves a Cloudflare
  JavaScript challenge to plain requests, and no copy of the real file was
  available when these snapshots were made. It is a manual download (a
  browser), listed in `data-raw/inputs.csv` with its URL and md5. If a
  Quebec-only extract is added here later, it can be committed under the
  Statistics Canada Open Licence with this attribution: "Adapted from
  Statistics Canada, 2011 Census Profile, catalogue 98-316-XWE2011001. This
  does not constitute an endorsement by Statistics Canada of this product."
- The Élections Québec results JSON. Their terms do not allow
  redistribution under the repository's licence (inst/COPYRIGHTS, section 3).
  `data-raw/fetch_inputs.R` downloads them.
- Respondent-level data, the original files and every `.rds` built from
  them. Fill a download cache with `data-raw/live_fetch.R`.
