# ==============================================================================
# Program     : R/sdtm/lb.R
# Study       : ZX-201
# Purpose     : Create SDTM LB (Laboratory Test Results) from the central-lab
#               vendor transfer: map vendor test codes to CDISC CT, convert
#               conventional units to SI, derive LBNRIND and LBLOBXFL.
# Inputs      : data/raw/central_lab_transfer.csv, specs/lab_test_mapping.csv,
#               specs/visit_schedule.csv, output/sdtm/dm.xpt, output/sdtm/sv.xpt
# Outputs     : output/sdtm/lb.xpt
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

dm  <- read_sdtm("DM")
sv  <- read_sdtm("SV")
tv  <- read_csv("specs/visit_schedule.csv", show_col_types = FALSE)
map <- read_csv("specs/lab_test_mapping.csv", show_col_types = FALSE)

raw <- read_raw("central_lab_transfer")

unmapped <- setdiff(unique(raw$TEST_CODE), map$VENDOR_CODE)
if (length(unmapped)) stop("Vendor test codes without mapping: ", paste(unmapped, collapse = ", "))
unit_issue <- raw %>% left_join(map, by = c("TEST_CODE" = "VENDOR_CODE")) %>% filter(UNITS != CONV_UNIT)
if (nrow(unit_issue)) stop("Unexpected vendor units for: ", paste(unique(unit_issue$TEST_CODE), collapse = ", "))

fmt_si <- function(x, dec) ifelse(is.na(x), NA_character_, sprintf(paste0("%.", dec, "f"), round(x, dec)))

lb <- raw %>%
  left_join(map, by = c("TEST_CODE" = "VENDOR_CODE")) %>%
  mutate(
    STUDYID  = STUDYID,
    DOMAIN   = "LB",
    USUBJID  = paste(STUDYID, PATIENT, sep = "-"),
    FOLDER   = if_else(VISIT_CODE == "UNSCH", "Unscheduled", tv$FOLDER[match(VISIT_CODE, tv$LAB_VISIT_CODE)]),
    LBREFID  = ACCESSION,
    LBORRES  = RESULT,
    LBORRESU = if_else(is.na(RESULT), NA_character_, CONV_UNIT_CT),   # vendor unit -> CT submission value
    LBORNRLO = RANGE_LOW,
    LBORNRHI = RANGE_HIGH,
    num      = suppressWarnings(as.numeric(RESULT)),
    lt       = str_detect(coalesce(RESULT, ""), "^<"),
    lt_val   = if_else(lt, as.numeric(str_remove(RESULT, "^<")) * FACTOR, NA_real_),
    LBSTRESN = round(num * FACTOR, SI_DECIMALS),
    LBSTRESC = case_when(lt ~ paste0("<", fmt_si(lt_val, SI_DECIMALS)),
                         TRUE ~ fmt_si(LBSTRESN, SI_DECIMALS)),
    LBSTRESU = if_else(is.na(RESULT), NA_character_, SI_UNIT),
    LBSTNRLO = round(as.numeric(RANGE_LOW) * FACTOR, SI_DECIMALS),
    LBSTNRHI = round(as.numeric(RANGE_HIGH) * FACTOR, SI_DECIMALS),
    LBNRIND  = case_when(lt & lt_val <= LBSTNRLO ~ "LOW",
                         is.na(LBSTRESN) ~ NA_character_,
                         LBSTRESN < LBSTNRLO ~ "LOW",
                         LBSTRESN > LBSTNRHI ~ "HIGH",
                         TRUE ~ "NORMAL"),
    LBSTAT   = if_else(is.na(RESULT), "NOT DONE", NA_character_),
    LBREASND = if_else(is.na(RESULT), str_remove(RESULT_COMMENT, " - TEST CANCELLED$"), NA_character_),
    LBNAM    = "CENTRAL LABORATORY (SIMULATED VENDOR)",
    LBDTC    = iso_datetime(COLL_DATE, COLL_TIME)
  )

lb <- map_visits(lb, "LBDTC", sv, tv) %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(EPOCH = derive_epoch(USUBJID, LBDTC, dm, predose = FOLDER %in% c("Screening", "Day 1")),
         LBDY  = study_day(LBDTC, RFSTDTC)) %>%
  derive_lobxfl("LB", dm) %>%
  derive_seq("LBSEQ", c("LBCAT", "LBTESTCD", "VISITNUM")) %>%
  select(STUDYID, DOMAIN, USUBJID, LBSEQ, LBREFID, LBTESTCD, LBTEST, LBCAT, LBORRES, LBORRESU,
         LBORNRLO, LBORNRHI, LBSTRESC, LBSTRESN, LBSTRESU, LBSTNRLO, LBSTNRHI, LBNRIND, LBSTAT,
         LBREASND, LBNAM, LBSPEC, LBLOBXFL, VISITNUM, VISIT, VISITDY, EPOCH, LBDTC, LBDY)
write_domain(lb, "LB")
