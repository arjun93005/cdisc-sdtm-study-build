# ==============================================================================
# Program     : R/sdtm/ae.R
# Study       : ZX-201
# Purpose     : Create SDTM AE (Adverse Events) from the AE log form and the
#               Data Management coding export.
# Inputs      : data/raw/raw_ae.csv, coding_ae_meddra_style.csv, output/sdtm/dm.xpt
# Outputs     : output/sdtm/ae.xpt
# Author      : arjun93005
# History     : v1.0  Initial version
# Notes       : Partial dates are kept at collected precision (no imputation
#               in SDTM; imputation is an ADaM activity).
# ==============================================================================
source("R/utils/sdtm_utils.R")

dm     <- read_sdtm("DM")
coding <- read_raw("coding_ae_meddra_style")
yn     <- c(Yes = "Y", No = "N")
if (anyDuplicated(coding$VERBATIM_UPPER)) stop("Coding export has duplicate verbatim keys")

ae <- read_raw("raw_ae") %>%
  mutate(VERBATIM_UPPER = toupper(str_squish(AETERM))) %>%
  left_join(coding, by = "VERBATIM_UPPER")

uncoded <- ae %>% filter(is.na(PT))
if (nrow(uncoded)) stop("Uncoded AE verbatim terms (send to DM for coding): ",
                        paste(unique(uncoded$AETERM), collapse = "; "))

ae <- ae %>%
  mutate(
    STUDYID  = STUDYID,
    DOMAIN   = "AE",
    USUBJID  = paste(STUDYID, SUBJID, sep = "-"),
    AELLT    = LLT, AEDECOD = PT, AEHLT = HLT, AEHLGT = HLGT, AEBODSYS = SOC, AESOC = SOC,
    AESEV    = toupper(AESEV),
    AESER    = ct_map(AESER, yn, "AESER"),
    AEACN    = toupper(AEACN),
    AEREL    = toupper(AEREL),
    AEOUT    = toupper(AEOUT),
    AESDTH   = ct_map(AESDTH, yn, "AESDTH"),
    AESHOSP  = ct_map(AESHOSP, yn, "AESHOSP"),
    AESTDTC  = raw_to_iso(AESTDAT),
    AEENDTC  = raw_to_iso(AEENDAT),
    AEENRTPT = if_else(AEONGO == "Yes", "ONGOING", NA_character_),
    AEENTPT  = if_else(AEONGO == "Yes", "END OF STUDY", NA_character_)
  ) %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(EPOCH  = derive_epoch(USUBJID, AESTDTC, dm),
         AESTDY = study_day(AESTDTC, RFSTDTC),
         AEENDY = study_day(AEENDTC, RFSTDTC)) %>%
  derive_seq("AESEQ", c("AESTDTC", "AEDECOD", "AESPID")) %>%
  select(STUDYID, DOMAIN, USUBJID, AESEQ, AESPID, AETERM, AELLT, AEDECOD, AEHLT, AEHLGT, AEBODSYS,
         AESOC, AESEV, AESER, AEACN, AEREL, AEOUT, AESDTH, AESHOSP, EPOCH, AESTDTC, AEENDTC,
         AESTDY, AEENDY, AEENRTPT, AEENTPT)
write_domain(ae, "AE")
