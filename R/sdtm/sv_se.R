# ==============================================================================
# Program     : R/sdtm/sv_se.R
# Study       : ZX-201
# Purpose     : Create SDTM SV (Subject Visits) and SE (Subject Elements).
# Inputs      : data/raw/raw_sv.csv, specs/visit_schedule.csv, output/sdtm/dm.xpt
# Outputs     : output/sdtm/sv.xpt, output/sdtm/se.xpt
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

dm  <- read_sdtm("DM")
tv  <- read_csv("specs/visit_schedule.csv", show_col_types = FALSE)
raw_sv <- read_raw("raw_sv")

# --- SV ---------------------------------------------------------------------------
sv <- raw_sv %>%
  mutate(USUBJID = paste(STUDYID, SUBJID, sep = "-"), SVSTDTC = raw_to_iso(VISDAT)) %>%
  left_join(tv %>% select(FOLDER, VISITNUM, VISIT, VISITDY), by = "FOLDER") %>%
  arrange(USUBJID, SVSTDTC, VISITNUM)

# Unscheduled visits: preceding planned VISITNUM + 0.01, 0.02 ...
sv <- sv %>% group_by(USUBJID) %>%
  mutate(prev_planned = cummax(coalesce(VISITNUM, 0))) %>%
  group_by(USUBJID, prev_planned) %>%
  mutate(k = cumsum(FOLDER == "Unscheduled")) %>% ungroup() %>%
  mutate(
    VISITNUM = if_else(FOLDER == "Unscheduled", prev_planned + k / 100, VISITNUM),
    VISIT    = if_else(FOLDER == "Unscheduled", sprintf("UNSCHEDULED %.2f", VISITNUM), VISIT),
    SVUPDES  = if_else(FOLDER == "Unscheduled", "UNSCHEDULED ASSESSMENT", NA_character_)
  )

sv <- sv %>%
  mutate(STUDYID = STUDYID, DOMAIN = "SV", SVENDTC = SVSTDTC,
         EPOCH = derive_epoch(USUBJID, SVSTDTC, dm, predose = FOLDER %in% c("Screening", "Day 1"))) %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(SVSTDY = study_day(SVSTDTC, RFSTDTC), SVENDY = study_day(SVENDTC, RFSTDTC)) %>%
  select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, EPOCH, SVSTDTC, SVENDTC, SVSTDY, SVENDY, SVUPDES)

dups <- sv %>% count(USUBJID, VISITNUM) %>% filter(n > 1)
if (nrow(dups)) stop("Duplicate SV visits: ", paste(dups$USUBJID, collapse = ", "))
write_domain(sv, "SV")

# --- SE ---------------------------------------------------------------------------
day_after <- function(dtc) format(dtc_date(dtc) + 1)
elements <- c(SCRN = "Screening", PBO = "Placebo", ZX10 = "ZX-101 10 mg", ZX20 = "ZX-101 20 mg", FU = "Follow-up")

se_scrn <- dm %>% transmute(USUBJID, ETCD = "SCRN", TAETORD = 1, EPOCH = "SCREENING",
                            SESTDTC = RFICDTC, SEENDTC = coalesce(RFXSTDTC, RFPENDTC))
se_trt  <- dm %>% filter(!is.na(RFXSTDTC)) %>%
  transmute(USUBJID, ETCD = ACTARMCD, TAETORD = 2, EPOCH = "TREATMENT",
            SESTDTC = RFXSTDTC, SEENDTC = day_after(RFXENDTC))
se_fu   <- dm %>% filter(!is.na(RFXSTDTC), substr(RFPENDTC, 1, 10) > substr(RFXENDTC, 1, 10)) %>%
  transmute(USUBJID, ETCD = "FU", TAETORD = 3, EPOCH = "FOLLOW-UP",
            SESTDTC = day_after(RFXENDTC), SEENDTC = RFPENDTC)

se <- bind_rows(se_scrn, se_trt, se_fu) %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(STUDYID = STUDYID, DOMAIN = "SE", ELEMENT = unname(elements[ETCD]),
         SESTDY = study_day(SESTDTC, RFSTDTC), SEENDY = study_day(SEENDTC, RFSTDTC)) %>%
  derive_seq("SESEQ", "TAETORD") %>%
  select(STUDYID, DOMAIN, USUBJID, SESEQ, ETCD, ELEMENT, TAETORD, EPOCH, SESTDTC, SEENDTC, SESTDY, SEENDY)
write_domain(se, "SE")
