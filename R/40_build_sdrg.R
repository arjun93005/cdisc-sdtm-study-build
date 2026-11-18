# ==============================================================================
# Program     : R/40_build_sdrg.R
# Study       : ZX-201
# Purpose     : Build the Study Data Reviewer's Guide (SDRG) following the
#               PHUSE clinical SDRG template structure. Narrative sections are
#               written here; every table (standards, datasets, record counts,
#               conformance issues) is generated from the final data and
#               metadata so the document can never disagree with the package.
# Inputs      : output/sdtm/*.xpt, specs/sdtm_spec/datasets.csv,
#               output/metadata/conformance_report.csv, specs/ct/CT_VERSION.txt
# Outputs     : docs/SDRG.md, output/sdtm/sdrg.html
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

spec <- read_spec()
CT_VERSION <- readLines("specs/ct/CT_VERSION.txt", n = 1)
conf <- read_csv("output/metadata/conformance_report.csv", show_col_types = FALSE)
doms <- spec$datasets$Dataset
sd <- set_names(map(doms, read_sdtm), doms)
dm <- sd$DM

md_table <- function(df) {
  df <- df %>% mutate(across(everything(), ~ str_replace_all(coalesce(as.character(.x), ""), "\\|", "/")))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")), "")
}

n_scr <- nrow(dm); n_rand <- sum(!is.na(dm$ARMCD)); n_trt <- sum(!is.na(dm$RFXSTDTC))
arm_n <- dm %>% filter(!is.na(ARMCD)) %>% count(ARM)

explanations <- c(
  "ZX-002" = "A licensed MedDRA dictionary was not available for this portfolio study; coding uses an illustrative MedDRA-style hierarchy (term names only). In a real submission AELLTCD-AESOCCD would be populated from the MedDRA version listed in section 1.3.",
  "ZX-008" = "One AE (subject listed in the issue file) has an end date three days before its start date. The site was queried; the query was open at database lock and the data are submitted as collected. No imputation was applied in SDTM. In ADaM this record would be handled by the imputation rules in the SAP.",
  "ZX-012" = "EPOCH is null only where AESTDTC is a partial date (day unknown). EPOCH cannot be assigned without a complete date; SDTM keeps dates at collected precision.",
  "ZX-025" = "One Week 4 weight value was entered in pounds instead of kilograms at a US site (value is consistent with the subject's weight x 2.2). Submitted as collected; the issue is flagged for the clinical team and excluded from weight analyses per the SAP data-handling rules.",
  "ZX-029" = sprintf("The RACE (C74457) and ETHNIC (C66790) codelists are not present in the CDISC CT release pinned for this build (%s). Values follow the SDTMIG v3.4 DM assumptions and the CDASH CRF; they are documented as sponsor codelists in define.xml.", CT_VERSION)
)

ds_inv <- spec$datasets %>%
  transmute(Dataset, Label, Class,
            Records = map_int(Dataset, ~ nrow(sd[[.x]])),
            Subjects = map_chr(Dataset, ~ if ("USUBJID" %in% names(sd[[.x]])) as.character(n_distinct(sd[[.x]]$USUBJID)) else "-"),
            Structure)

issues_tbl <- conf %>% filter(Status != "PASS") %>%
  transmute(`Rule` = RuleID, Severity, Description, Findings,
            Explanation = coalesce(explanations[RuleID], "TBD"))
if (any(issues_tbl$Explanation == "TBD")) warning("Conformance findings without an SDRG explanation: ",
                                                  paste(issues_tbl$Rule[issues_tbl$Explanation == "TBD"], collapse = ", "))

md <- c(
  "# Study Data Reviewer's Guide - ZX-201 (SDTM)",
  "",
  "> **Simulated study.** All data are synthetic and were generated for a clinical-programming portfolio. Structure follows the PHUSE clinical Study Data Reviewer's Guide (cSDRG) template.",
  "",
  sprintf("*Generated %s from the final datasets and metadata by `R/40_build_sdrg.R`.*", format(Sys.Date())),
  "",
  "## 1. Introduction",
  "",
  "### 1.1 Purpose",
  "This guide gives a reviewer the context needed to use the ZX-201 SDTM tabulation datasets: standards used, protocol design as represented in the trial design domains, sponsor decisions in mapping the collected data, and an explanation of every conformance finding that remains in the final package.",
  "",
  "### 1.2 Study Data Standards and Dictionary Inventory",
  md_table(tribble(
    ~`Standard or Dictionary`, ~Version, ~Notes,
    "SDTM", "2.0", "",
    "SDTM Implementation Guide", "3.4", "--LOBXFL used instead of --BLFL",
    "CDISC Controlled Terminology", CT_VERSION, "NCI EVS release, pinned in specs/ct/CT_VERSION.txt",
    "CDASH", "aligned", "eCRF field names follow CDASH (specs/ecrf_spec.csv)",
    "Define-XML", "2.1", "define.xml + define.html",
    "MedDRA", "illustrative", "Not licensed MedDRA - see section 4",
    "WHODrug Global", "illustrative", "ATC-level class text and code only"
  )),
  "### 1.3 Source Data Used for Dataset Creation",
  "SDTM datasets were created directly from source extracts (no legacy conversion): EDC form extracts (`data/raw/raw_*.csv`), the IxRS randomisation and kit lists (unblinded at database lock), the central-laboratory vendor transfer (`central_lab_transfer.csv`) and Data Management coding exports. See `docs/data_flow_plan.md` for the full data flow.",
  "",
  "## 2. Protocol Description",
  "",
  "### 2.1 Protocol Number and Title",
  sprintf("**ZX-201** - %s", sd$TS$TSVAL[sd$TS$TSPARMCD == "TITLE"]),
  "",
  "### 2.2 Protocol Design in Relation to the Trial Design Datasets",
  "Subjects are screened for up to 14 days, randomised 1:1:1 (site-stratified blocks of 6) to placebo, ZX-101 10 mg or ZX-101 20 mg once daily for 12 weeks, and followed up 14 days after the last dose. Each arm therefore has three elements (TA):",
  "",
  md_table(sd$TA %>% select(ARMCD, ARM, TAETORD, ETCD, ELEMENT, EPOCH, TABRANCH)),
  "Planned visits (TV): ",
  "",
  md_table(sd$TV %>% select(VISITNUM, VISIT, VISITDY, TVSTRL)),
  "## 3. Subject Data Description",
  "",
  "### 3.1 Overview",
  sprintf("%d subjects were screened, %d randomised and %d dosed. Randomised by arm: %s.", n_scr, n_rand, n_trt,
          paste(sprintf("%s n=%d", arm_n$ARM, arm_n$n), collapse = "; ")),
  "",
  md_table(ds_inv),
  "### 3.2 Annotated CRF",
  "`acrf.pdf` is generated from the eCRF specification (`specs/ecrf_spec.csv`) by `R/11_annotated_crf.R`. Page 1 is a cover/contents page; each form starts on its own page, and the define.xml `PDFPageRef` values point to these physical pages. Central lab data and IxRS data are not on the CRF.",
  "",
  "### 3.3 SDTM Subject Domains - sponsor decisions",
  "",
  "- **DM** includes screen failures (ARMNRS = 'SCREEN FAILURE', null ARMCD/ARM) and one subject randomised but never dosed (ARMNRS = 'ASSIGNED, NOT TREATED'). RFSTDTC/RFENDTC = first/last dose; for the not-treated subject both are the randomisation date. German sites collect year of birth only, so BRTHDTC is a partial date and AGE is the collected age; elsewhere AGE is derived from BRTHDTC and RFICDTC.",
  "- **SUPPDM** holds RACE1/RACE2 for subjects with more than one race checked (RACE = 'MULTIPLE').",
  "- **SE / EPOCH**: elements are SCREENING (consent to first dose), TREATMENT (first dose to the day after last dose) and FOLLOW-UP. EPOCH on every observation is assigned from these boundaries; Day 1 assessments are protocol-mandated pre-dose and are SCREENING (time is compared with the first-dose time).",
  "- **EX**: one record per dispensed kit. EXTRT and EXDOSE come from the unblinded IxRS kit list joined on the kit number (EXREFID); all subjects received their randomised treatment.",
  "- **AE / CM**: partial dates are kept at collected precision; no imputation in SDTM. Ongoing events use --ENRTPT = 'ONGOING' with --ENTPT = 'END OF STUDY'.",
  "- **VS**: seated blood pressure is collected in triplicate (VSREPNUM 1-3); the mean is derived in ADaM, not SDTM. Temperature is collected in C or F (US sites) and standardised to C in VSSTRESN. Height is collected at Screening only.",
  "- **LB**: vendor test codes and units are mapped through `specs/lab_test_mapping.csv`. Vendor unit `10^3/uL` is a CDISC CT synonym, so LBORRESU uses the submission value `10^9/L`. Results reported as `<x` keep the character result in LBSTRESC with null LBSTRESN. Haemolysed potassium samples are LBSTAT = 'NOT DONE'.",
  "- **Baseline**: --LOBXFL (SDTMIG 3.4) flags the last non-missing result on or before first dose per test; for triplicate BP this is the third replicate. Analysis baseline is defined in ADaM.",
  "",
  "## 4. Data Conformance Summary",
  "",
  "### 4.1 Conformance Inputs",
  sprintf("Conformance was assessed with the study's own rule set (`R/30_conformance_checks.R`, %d rules covering structure, ISO 8601, study days, referential integrity, cross-domain safety consistency and plausibility) plus a controlled-terminology check against the pinned CT release (`R/01_ct_check.R`) and define.xml integrity checks. A production submission would additionally be run through the FDA-accepted validator (e.g. Pinnacle 21 Enterprise / CDISC CORE); those results are not available for this portfolio build.", nrow(conf)),
  "",
  sprintf("Result: **%d rules passed**, %d raised warnings and %d failed. Each non-passing rule is explained below.",
          sum(conf$Status == "PASS"), sum(conf$Status == "WARN"), sum(conf$Status == "FAIL")),
  "",
  "### 4.2 Issues Summary",
  md_table(issues_tbl),
  "## 5. Appendix - Program Inventory",
  md_table(tribble(
    ~Program, ~Purpose,
    "R/00_simulate_raw_edc.R", "Generate synthetic source extracts",
    "R/01_ct_check.R", "Validate spec codelists against CDISC CT; resolve NCI codes",
    "R/sdtm/*.R", "One program per SDTM domain (trial design, DM/SUPPDM, SV/SE, EX, AE, CM, DS, VS, LB)",
    "R/10_define_xml.R", "Define-XML 2.1 + define.html + integrity checks",
    "R/11_annotated_crf.R", "Annotated CRF (HTML + PDF)",
    "R/30_conformance_checks.R", "Conformance rules",
    "R/40_build_sdrg.R", "This document",
    "run_all.R", "Runs everything in order with logrx logs and a log scan"
  ))
)

dir.create("docs", showWarnings = FALSE)
writeLines(md, "docs/SDRG.md", useBytes = TRUE)
body <- commonmark::markdown_html(paste(md, collapse = "\n"), extensions = TRUE)
html <- paste0("<!doctype html><html lang='en'><head><meta charset='utf-8'><title>ZX-201 SDRG</title><style>",
               "body{font-family:Segoe UI,Arial,sans-serif;max-width:1050px;margin:24px auto;padding:0 16px;color:#1b1b1b;line-height:1.45}",
               "table{border-collapse:collapse;font-size:12.5px;margin:8px 0 16px}th{background:#2b5c8a;color:#fff;text-align:left}",
               "th,td{border:1px solid #c9d3dd;padding:4px 7px;vertical-align:top}h2{border-bottom:2px solid #2b5c8a;padding-bottom:3px}",
               "blockquote{background:#fff7e0;border-left:4px solid #e0a800;margin:0;padding:6px 12px}code{background:#f1f3f5;padding:1px 4px}",
               "</style></head><body>", body, "</body></html>")
writeLines(html, file.path(SDTM_DIR, "sdrg.html"), useBytes = TRUE)
message("SDRG written: docs/SDRG.md, output/sdtm/sdrg.html (", nrow(issues_tbl), " issues explained)")
