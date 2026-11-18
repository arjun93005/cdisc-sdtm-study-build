# ==============================================================================
# Program     : R/01_ct_check.R
# Study       : ZX-201
# Purpose     : Validate every CDISC-controlled codelist in the SDTM spec
#               against the pinned NCI EVS CT release and resolve NCI C-codes
#               (codelist and term level) for define.xml.
#                 - term not in CT, non-extensible codelist  -> ERROR
#                 - term not in CT, extensible codelist      -> NOTE (sponsor extension)
#                 - codelist not in pinned CT release        -> WARNING (explained in SDRG)
# Inputs      : specs/sdtm_spec/codelists.csv, specs/ct/SDTM_Terminology.txt
# Outputs     : output/metadata/codelists_resolved.csv
#               output/metadata/ct_check_report.csv
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

ct   <- load_ct()
spec <- read_spec()
dir.create("output/metadata", recursive = TRUE, showWarnings = FALSE)

cl <- spec$codelists %>%
  left_join(ct$codelists %>% select(CDISCCodelist = short_name, NCICodelistCode = codelist_code,
                                    CTExtensible = extensible),
            by = "CDISCCodelist") %>%
  left_join(ct$terms %>% select(CDISCCodelist = short_name, Term = value, NCITermCode = term_code,
                                NCIPreferredTerm = nci_pt),
            by = c("CDISCCodelist", "Term"))

report <- bind_rows(
  cl %>% filter(!is.na(CDISCCodelist), is.na(NCICodelistCode)) %>% distinct(CodelistID, CDISCCodelist) %>%
    transmute(Severity = "WARNING", CodelistID, Term = NA_character_,
              Message = sprintf("Codelist %s is not present in the pinned CT release; values follow SDTMIG v3.4 assumptions", CDISCCodelist)),
  cl %>% filter(!is.na(NCICodelistCode), Term != "*", is.na(NCITermCode), CTExtensible == "No") %>%
    transmute(Severity = "ERROR", CodelistID, Term, Message = "Term not in non-extensible CDISC codelist"),
  cl %>% filter(!is.na(NCICodelistCode), Term != "*", is.na(NCITermCode), CTExtensible == "Yes") %>%
    transmute(Severity = "NOTE", CodelistID, Term, Message = "Sponsor extension of extensible CDISC codelist"),
  cl %>% filter(!is.na(NCICodelistCode), !is.na(Extensible), Extensible != CTExtensible) %>% distinct(CodelistID, CTExtensible) %>%
    transmute(Severity = "ERROR", CodelistID, Term = NA_character_,
              Message = paste("Spec 'Extensible' differs from CT:", CTExtensible))
)

write_csv(cl, "output/metadata/codelists_resolved.csv", na = "")
write_csv(report, "output/metadata/ct_check_report.csv", na = "")

message(sprintf("CT check vs %s: %d codelists, %d terms resolved; %d ERROR, %d WARNING, %d NOTE",
                readLines("specs/ct/CT_VERSION.txt", n = 1),
                n_distinct(cl$CodelistID), sum(!is.na(cl$NCITermCode)),
                sum(report$Severity == "ERROR"), sum(report$Severity == "WARNING"), sum(report$Severity == "NOTE")))
if (nrow(report)) print(as.data.frame(report), right = FALSE)
if (any(report$Severity == "ERROR")) stop("Controlled terminology errors - fix the spec before proceeding")
