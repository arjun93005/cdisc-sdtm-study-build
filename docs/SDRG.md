# Study Data Reviewer's Guide - ZX-201 (SDTM)

> **Simulated study.** All data are synthetic and were generated for a clinical-programming portfolio. Structure follows the PHUSE clinical Study Data Reviewer's Guide (cSDRG) template.

*Generated 2026-11-18 from the final datasets and metadata by `R/40_build_sdrg.R`.*

## 1. Introduction

### 1.1 Purpose
This guide gives a reviewer the context needed to use the ZX-201 SDTM tabulation datasets: standards used, protocol design as represented in the trial design domains, sponsor decisions in mapping the collected data, and an explanation of every conformance finding that remains in the final package.

### 1.2 Study Data Standards and Dictionary Inventory
| Standard or Dictionary | Version | Notes |
|---|---|---|
| SDTM | 2.0 |  |
| SDTM Implementation Guide | 3.4 | --LOBXFL used instead of --BLFL |
| CDISC Controlled Terminology | 2026-09-25 | NCI EVS release, pinned in specs/ct/CT_VERSION.txt |
| CDASH | aligned | eCRF field names follow CDASH (specs/ecrf_spec.csv) |
| Define-XML | 2.1 | define.xml + define.html |
| MedDRA | illustrative | Not licensed MedDRA - see section 4 |
| WHODrug Global | illustrative | ATC-level class text and code only |

### 1.3 Source Data Used for Dataset Creation
SDTM datasets were created directly from source extracts (no legacy conversion): EDC form extracts (`data/raw/raw_*.csv`), the IxRS randomisation and kit lists (unblinded at database lock), the central-laboratory vendor transfer (`central_lab_transfer.csv`) and Data Management coding exports. See `docs/data_flow_plan.md` for the full data flow.

## 2. Protocol Description

### 2.1 Protocol Number and Title
**ZX-201** - A Phase II, Randomized, Double-Blind, Placebo-Controlled, Parallel-Group Study of ZX-101 in Adults with Stage 1-2 Essential Hypertension

### 2.2 Protocol Design in Relation to the Trial Design Datasets
Subjects are screened for up to 14 days, randomised 1:1:1 (site-stratified blocks of 6) to placebo, ZX-101 10 mg or ZX-101 20 mg once daily for 12 weeks, and followed up 14 days after the last dose. Each arm therefore has three elements (TA):

| ARMCD | ARM | TAETORD | ETCD | ELEMENT | EPOCH | TABRANCH |
|---|---|---|---|---|---|---|
| PBO | Placebo | 1 | SCRN | Screening | SCREENING | Randomized to Placebo |
| PBO | Placebo | 2 | PBO | Placebo | TREATMENT |  |
| PBO | Placebo | 3 | FU | Follow-up | FOLLOW-UP |  |
| ZX10 | ZX-101 10 mg | 1 | SCRN | Screening | SCREENING | Randomized to ZX-101 10 mg |
| ZX10 | ZX-101 10 mg | 2 | ZX10 | ZX-101 10 mg | TREATMENT |  |
| ZX10 | ZX-101 10 mg | 3 | FU | Follow-up | FOLLOW-UP |  |
| ZX20 | ZX-101 20 mg | 1 | SCRN | Screening | SCREENING | Randomized to ZX-101 20 mg |
| ZX20 | ZX-101 20 mg | 2 | ZX20 | ZX-101 20 mg | TREATMENT |  |
| ZX20 | ZX-101 20 mg | 3 | FU | Follow-up | FOLLOW-UP |  |

Planned visits (TV): 

| VISITNUM | VISIT | VISITDY | TVSTRL |
|---|---|---|---|
| 1 | SCREENING | -14 | Up to 14 days before Day 1 (after informed consent) |
| 2 | DAY 1 | 1 | Day 1, before first dose of study drug |
| 3 | WEEK 2 | 15 | Day 15 (window -2/+3 days) |
| 4 | WEEK 4 | 29 | Day 29 (window -2/+3 days) |
| 5 | WEEK 8 | 57 | Day 57 (window -2/+3 days) |
| 6 | WEEK 12 | 85 | Day 85 (window -2/+3 days) |
| 7 | FOLLOW-UP | 99 | 14 days after last dose (window -1/+3 days) |

## 3. Subject Data Description

### 3.1 Overview
200 subjects were screened, 180 randomised and 179 dosed. Randomised by arm: Placebo n=59; ZX-101 10 mg n=59; ZX-101 20 mg n=62.

| Dataset | Label | Class | Records | Subjects | Structure |
|---|---|---|---|---|---|
| TA | Trial Arms | TRIAL DESIGN | 9 | - | One record per planned Element per Arm |
| TE | Trial Elements | TRIAL DESIGN | 5 | - | One record per planned Element |
| TV | Trial Visits | TRIAL DESIGN | 7 | - | One record per planned Visit per Arm |
| TS | Trial Summary | TRIAL DESIGN | 35 | - | One record per trial summary parameter value |
| DM | Demographics | SPECIAL PURPOSE | 200 | 200 | One record per subject |
| SE | Subject Elements | SPECIAL PURPOSE | 555 | 200 | One record per actual Element per subject |
| SV | Subject Visits | SPECIAL PURPOSE | 1257 | 200 | One record per actual visit per subject |
| CM | Concomitant/Prior Medications | INTERVENTIONS | 164 | 115 | One record per recorded medication occurrence per subject |
| EX | Exposure | INTERVENTIONS | 530 | 179 | One record per constant dosing interval per subject |
| AE | Adverse Events | EVENTS | 284 | 148 | One record per adverse event per subject |
| DS | Disposition | EVENTS | 580 | 200 | One record per disposition status or protocol milestone per subject |
| LB | Laboratory Test Results | FINDINGS | 6606 | 200 | One record per lab test per visit per subject |
| VS | Vital Signs | FINDINGS | 11457 | 200 | One record per vital sign measurement per replicate per visit per subject |
| SUPPDM | Supplemental Qualifiers for DM | RELATIONSHIP | 14 | 7 | One record per supplemental qualifier per related parent domain record |

### 3.2 Annotated CRF
`acrf.pdf` is generated from the eCRF specification (`specs/ecrf_spec.csv`) by `R/11_annotated_crf.R`. Page 1 is a cover/contents page; each form starts on its own page, and the define.xml `PDFPageRef` values point to these physical pages. Central lab data and IxRS data are not on the CRF.

### 3.3 SDTM Subject Domains - sponsor decisions

- **DM** includes screen failures (ARMNRS = 'SCREEN FAILURE', null ARMCD/ARM) and one subject randomised but never dosed (ARMNRS = 'ASSIGNED, NOT TREATED'). RFSTDTC/RFENDTC = first/last dose; for the not-treated subject both are the randomisation date. German sites collect year of birth only, so BRTHDTC is a partial date and AGE is the collected age; elsewhere AGE is derived from BRTHDTC and RFICDTC.
- **SUPPDM** holds RACE1/RACE2 for subjects with more than one race checked (RACE = 'MULTIPLE').
- **SE / EPOCH**: elements are SCREENING (consent to first dose), TREATMENT (first dose to the day after last dose) and FOLLOW-UP. EPOCH on every observation is assigned from these boundaries; Day 1 assessments are protocol-mandated pre-dose and are SCREENING (time is compared with the first-dose time).
- **EX**: one record per dispensed kit. EXTRT and EXDOSE come from the unblinded IxRS kit list joined on the kit number (EXREFID); all subjects received their randomised treatment.
- **AE / CM**: partial dates are kept at collected precision; no imputation in SDTM. Ongoing events use --ENRTPT = 'ONGOING' with --ENTPT = 'END OF STUDY'.
- **VS**: seated blood pressure is collected in triplicate (VSREPNUM 1-3); the mean is derived in ADaM, not SDTM. Temperature is collected in C or F (US sites) and standardised to C in VSSTRESN. Height is collected at Screening only.
- **LB**: vendor test codes and units are mapped through `specs/lab_test_mapping.csv`. Vendor unit `10^3/uL` is a CDISC CT synonym, so LBORRESU uses the submission value `10^9/L`. Results reported as `<x` keep the character result in LBSTRESC with null LBSTRESN. Haemolysed potassium samples are LBSTAT = 'NOT DONE'.
- **Baseline**: --LOBXFL (SDTMIG 3.4) flags the last non-missing result on or before first dose per test; for triplicate BP this is the third replicate. Analysis baseline is defined in ADaM.

## 4. Data Conformance Summary

### 4.1 Conformance Inputs
Conformance was assessed with the study's own rule set (`R/30_conformance_checks.R`, 29 rules covering structure, ISO 8601, study days, referential integrity, cross-domain safety consistency and plausibility) plus a controlled-terminology check against the pinned CT release (`R/01_ct_check.R`) and define.xml integrity checks. A production submission would additionally be run through the FDA-accepted validator (e.g. Pinnacle 21 Enterprise / CDISC CORE); those results are not available for this portfolio build.

Result: **24 rules passed**, 4 raised warnings and 1 failed. Each non-passing rule is explained below.

### 4.2 Issues Summary
| Rule | Severity | Description | Findings | Explanation |
|---|---|---|---|---|
| ZX-002 | Warning | SDTMIG-expected MedDRA code variables present (AELLTCD, AEPTCD, AEHLTCD, AEHLGTCD, AEBDSYCD, AESOCCD) | 1 | A licensed MedDRA dictionary was not available for this portfolio study; coding uses an illustrative MedDRA-style hierarchy (term names only). In a real submission AELLTCD-AESOCCD would be populated from the MedDRA version listed in section 1.3. |
| ZX-008 | Error | Start date/time on or before end date/time | 1 | One AE (subject listed in the issue file) has an end date three days before its start date. The site was queried; the query was open at database lock and the data are submitted as collected. No imputation was applied in SDTM. In ADaM this record would be handled by the imputation rules in the SAP. |
| ZX-012 | Warning | EPOCH populated (from TA) whenever the observation date is complete | 10 | EPOCH is null only where AESTDTC is a partial date (day unknown). EPOCH cannot be assigned without a complete date; SDTM keeps dates at collected precision. |
| ZX-025 | Warning | Plausibility: weight change > 20% from baseline (VSLOBXFL) record | 1 | One Week 4 weight value was entered in pounds instead of kilograms at a US site (value is consistent with the subject's weight x 2.2). Submitted as collected; the issue is flagged for the clinical team and excluded from weight analyses per the SAP data-handling rules. |
| ZX-029 | Warning | Codelists and terms exist in the pinned CDISC CT release | 2 | The RACE (C74457) and ETHNIC (C66790) codelists are not present in the CDISC CT release pinned for this build (2026-09-25). Values follow the SDTMIG v3.4 DM assumptions and the CDASH CRF; they are documented as sponsor codelists in define.xml. |

## 5. Appendix - Program Inventory
| Program | Purpose |
|---|---|
| R/00_simulate_raw_edc.R | Generate synthetic source extracts |
| R/01_ct_check.R | Validate spec codelists against CDISC CT; resolve NCI codes |
| R/sdtm/*.R | One program per SDTM domain (trial design, DM/SUPPDM, SV/SE, EX, AE, CM, DS, VS, LB) |
| R/10_define_xml.R | Define-XML 2.1 + define.html + integrity checks |
| R/11_annotated_crf.R | Annotated CRF (HTML + PDF) |
| R/30_conformance_checks.R | Conformance rules |
| R/40_build_sdrg.R | This document |
| run_all.R | Runs everything in order with logrx logs and a log scan |

