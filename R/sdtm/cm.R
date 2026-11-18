# ==============================================================================
# Program     : R/sdtm/cm.R
# Study       : ZX-201
# Purpose     : Create SDTM CM (Concomitant/Prior Medications).
# Inputs      : data/raw/raw_cm.csv, coding_cm_whodrug_style.csv, output/sdtm/dm.xpt
# Outputs     : output/sdtm/cm.xpt
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

dm     <- read_sdtm("DM")
coding <- read_raw("coding_cm_whodrug_style")
if (anyDuplicated(coding$VERBATIM_UPPER)) stop("Coding export has duplicate verbatim keys")

cm <- read_raw("raw_cm") %>%
  mutate(VERBATIM_UPPER = toupper(str_squish(CMTRT))) %>%
  left_join(coding, by = "VERBATIM_UPPER")
if (any(is.na(cm$DECOD))) stop("Uncoded CM verbatim terms: ", paste(unique(cm$CMTRT[is.na(cm$DECOD)]), collapse = "; "))

cm <- cm %>%
  mutate(
    STUDYID  = STUDYID,
    DOMAIN   = "CM",
    USUBJID  = paste(STUDYID, SUBJID, sep = "-"),
    CMDECOD  = DECOD, CMCLAS = ATC_TEXT, CMCLASCD = ATC_CODE,
    CMDOSE   = as.numeric(CMDSTXT),
    CMDOSFRQ = ct_map(CMFREQ, c("Once daily" = "QD", "Twice daily" = "BID", "As needed" = "PRN"), "CMDOSFRQ"),
    CMROUTE  = ct_map(CMROUTE, c(Oral = "ORAL"), "CMROUTE"),
    CMSTDTC  = raw_to_iso(CMSTDAT),
    CMENDTC  = raw_to_iso(CMENDAT),
    CMENRTPT = if_else(CMONGO == "Yes", "ONGOING", NA_character_),
    CMENTPT  = if_else(CMONGO == "Yes", "END OF STUDY", NA_character_)
  ) %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(CMSTDY = study_day(CMSTDTC, RFSTDTC), CMENDY = study_day(CMENDTC, RFSTDTC)) %>%
  derive_seq("CMSEQ", c("CMTRT", "CMSTDTC", "CMSPID")) %>%
  select(STUDYID, DOMAIN, USUBJID, CMSEQ, CMSPID, CMTRT, CMDECOD, CMCLAS, CMCLASCD, CMINDC, CMDOSE,
         CMDOSU, CMDOSFRQ, CMROUTE, CMSTDTC, CMENDTC, CMSTDY, CMENDY, CMENRTPT, CMENTPT)
write_domain(cm, "CM")
