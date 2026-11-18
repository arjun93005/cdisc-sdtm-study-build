# ==============================================================================
# Program     : R/sdtm/ds.R
# Study       : ZX-201
# Purpose     : Create SDTM DS (Disposition): protocol milestones (informed
#               consent, randomisation) and the end-of-study disposition event.
# Inputs      : data/raw/raw_dm.csv, raw_ds.csv, ixrs_randomization.csv, output/sdtm/dm.xpt
# Outputs     : output/sdtm/ds.xpt
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

dm <- read_sdtm("DM")

ic <- read_raw("raw_dm") %>%
  transmute(SUBJID, DSTERM = "INFORMED CONSENT OBTAINED", DSDECOD = DSTERM,
            DSCAT = "PROTOCOL MILESTONE", DSSTDTC = raw_to_iso(ICDAT), predose = TRUE)

rnd <- read_raw("ixrs_randomization") %>%
  transmute(SUBJID, DSTERM = "RANDOMIZED", DSDECOD = DSTERM,
            DSCAT = "PROTOCOL MILESTONE", DSSTDTC = RANDDT, predose = TRUE)

reason_map <- c("Adverse event" = "ADVERSE EVENT", "Death" = "DEATH", "Lack of efficacy" = "LACK OF EFFICACY",
                "Lost to follow-up" = "LOST TO FOLLOW-UP", "Screen failure" = "SCREEN FAILURE",
                "Withdrawal by subject" = "WITHDRAWAL BY SUBJECT")
eos <- read_raw("raw_ds") %>%
  mutate(DSDECOD = if_else(DSCOMPL == "Yes", "COMPLETED", ct_map(DSREAS, reason_map, "DSREAS"))) %>%
  transmute(SUBJID,
            DSTERM = coalesce(toupper(DSSPEC), DSDECOD),
            DSDECOD, DSCAT = "DISPOSITION EVENT", DSSTDTC = raw_to_iso(DSSTDAT), predose = FALSE)

ds <- bind_rows(ic, rnd, eos) %>%
  mutate(STUDYID = STUDYID, DOMAIN = "DS", USUBJID = paste(STUDYID, SUBJID, sep = "-")) %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(EPOCH = derive_epoch(USUBJID, DSSTDTC, dm, predose = predose),
         DSSTDY = study_day(DSSTDTC, RFSTDTC)) %>%
  derive_seq("DSSEQ", c("DSSTDTC", "DSCAT")) %>%
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC, DSSTDY)
write_domain(ds, "DS")
