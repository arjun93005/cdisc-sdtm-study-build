# ==============================================================================
# Program     : R/sdtm/ex.R
# Study       : ZX-201
# Purpose     : Create SDTM EX (Exposure) from the Study Drug Administration
#               form and the unblinded IxRS kit list.
# Inputs      : data/raw/raw_ex.csv, ixrs_kit_list.csv, output/sdtm/dm.xpt
# Outputs     : output/sdtm/ex.xpt
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

dm   <- read_sdtm("DM")
kits <- read_raw("ixrs_kit_list")

ex <- read_raw("raw_ex") %>%
  left_join(kits, by = "KITNO") %>%
  mutate(
    STUDYID  = STUDYID,
    DOMAIN   = "EX",
    USUBJID  = paste(STUDYID, SUBJID, sep = "-"),
    EXREFID  = KITNO,
    EXTRT    = if_else(TREATMENT == "Placebo", "PLACEBO", "ZX-101"),
    EXDOSE   = as.numeric(EXTABS) * as.numeric(STRENGTH_MG),
    EXDOSU   = "mg",
    EXDOSFRM = "TABLET",
    EXDOSFRQ = ct_map(EXFREQ, c("Once daily" = "QD"), "EXDOSFRQ"),
    EXROUTE  = ct_map(EXROUTE, c(Oral = "ORAL"), "EXROUTE"),
    EXSTDTC  = raw_to_iso(EXSTDAT, EXSTTIM),
    EXENDTC  = raw_to_iso(EXENDAT)
  )
if (any(is.na(ex$TREATMENT))) stop("Kit numbers not found in IxRS kit list: ",
                                   paste(ex$KITNO[is.na(ex$TREATMENT)], collapse = ", "))

ex <- ex %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(EPOCH  = derive_epoch(USUBJID, EXSTDTC, dm),
         EXSTDY = study_day(EXSTDTC, RFSTDTC),
         EXENDY = study_day(EXENDTC, RFSTDTC)) %>%
  derive_seq("EXSEQ", c("EXSTDTC")) %>%
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXREFID, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXDOSFRQ,
         EXROUTE, EPOCH, EXSTDTC, EXENDTC, EXSTDY, EXENDY)
write_domain(ex, "EX")
