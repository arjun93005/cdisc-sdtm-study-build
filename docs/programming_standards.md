# Programming Standards (GPP / GDP) - ZX-201

How this project applies Good Programming Practice and Good Documentation Practice, so the work could stand up to an audit or regulatory inspection (ICH E6 GCP requires that trial data handling is traceable, reproducible and documented).

## Good Programming Practice
- **Program headers**: every program states study, purpose, inputs, outputs, author and modification history.
- **One program per output**: each SDTM domain has its own program; shared logic lives in `R/utils/sdtm_utils.R` and is unit-tested (`tests/`).
- **Spec-driven, not hard-coded**: variable names, order, labels, types, lengths and codelists come from `specs/sdtm_spec/`. Changing the spec changes the output; the code cannot add undocumented variables.
- **Fail loudly**: unmapped values, uncoded terms, unknown kits, unexpected units and duplicate keys stop the program. Nothing falls through silently.
- **No manual data edits**: raw files are read-only inputs. Data issues are reported (conformance report) and explained (SDRG), never "fixed" in code.
- **Reproducibility**: seeded simulation; one command (`Rscript run_all.R`) rebuilds everything; R and package versions recorded in each logrx log.
- **Log review**: every program runs through `logrx::axecute()`; `run_all.R` scans the logs and fails on any error or warning (the R equivalent of a SAS log check for `ERROR:`/`WARNING:`/uninitialised notes).

## Good Documentation Practice
- **Single source of truth**: define.xml, define.html, the aCRF page numbers and SDRG tables are all generated from the same metadata and data, so documents cannot disagree with the datasets.
- **Versioned terminology**: the CDISC CT release used is pinned (`specs/ct/CT_VERSION.txt`) and recorded in define.xml and the SDRG.
- **Traceability**: aCRF -> SDTM variable (annotations) -> define.xml Origin (`PDFPageRef`) -> SDRG explanation.
- **Version control**: all programs and specs are kept in git; outputs are regenerated, not edited.

## Validation approach
- Production programming plus independent checks (CT check, define integrity checks, 29 conformance rules, unit tests).
- In a production setting, outputs would also be double-programmed by a second programmer (see the companion ADaM/TFL project, which demonstrates independent QC with `diffdf`).
