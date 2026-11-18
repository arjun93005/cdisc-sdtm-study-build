# ==============================================================================
# Program     : R/sdtm/vs.R
# Study       : ZX-201
# Purpose     : Create SDTM VS (Vital Signs): transpose the horizontal CRF
#               (one row per visit) to one record per test per replicate,
#               standardise temperature to C, derive VSLOBXFL.
# Inputs      : data/raw/raw_vs.csv, output/sdtm/dm.xpt, output/sdtm/sv.xpt
# Outputs     : output/sdtm/vs.xpt
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

dm <- read_sdtm("DM")
sv <- read_sdtm("SV")
tv <- read_csv("specs/visit_schedule.csv", show_col_types = FALSE)

tests <- tribble(
  ~field,    ~VSTESTCD, ~VSTEST,                    ~VSORRESU,   ~VSREPNUM, ~pos,
  "SYSBP_1", "SYSBP",   "Systolic Blood Pressure",  "mmHg",      1,         TRUE,
  "SYSBP_2", "SYSBP",   "Systolic Blood Pressure",  "mmHg",      2,         TRUE,
  "SYSBP_3", "SYSBP",   "Systolic Blood Pressure",  "mmHg",      3,         TRUE,
  "DIABP_1", "DIABP",   "Diastolic Blood Pressure", "mmHg",      1,         TRUE,
  "DIABP_2", "DIABP",   "Diastolic Blood Pressure", "mmHg",      2,         TRUE,
  "DIABP_3", "DIABP",   "Diastolic Blood Pressure", "mmHg",      3,         TRUE,
  "PULSE",   "PULSE",   "Pulse Rate",               "beats/min", NA,        TRUE,
  "TEMP",    "TEMP",    "Temperature",              NA,          NA,        FALSE,
  "WEIGHT",  "WEIGHT",  "Weight",                   "kg",        NA,        FALSE,
  "HEIGHT",  "HEIGHT",  "Height",                   "cm",        NA,        FALSE
)

raw <- read_raw("raw_vs") %>%
  mutate(USUBJID = paste(STUDYID, SUBJID, sep = "-"), VSDTC = raw_to_iso(VSDAT, VSTIM))

long <- raw %>%
  pivot_longer(all_of(tests$field), names_to = "field", values_to = "VSORRES") %>%
  inner_join(tests, by = "field") %>%
  # HEIGHT is only collected at Screening: no record at other visits
  filter(!(VSTESTCD == "HEIGHT" & is.na(VSORRES))) %>%
  mutate(VSORRESU = coalesce(VSORRESU, TEMPU),
         bp = VSTESTCD %in% c("SYSBP", "DIABP"),
         not_done = bp & !is.na(BPND))

# BP not done: one NOT DONE record per test (not per replicate)
long <- long %>% filter(!(not_done & VSREPNUM > 1)) %>%
  mutate(VSREPNUM = if_else(not_done, NA_real_, VSREPNUM),
         VSORRES  = if_else(not_done, NA_character_, VSORRES),
         VSORRESU = if_else(not_done, NA_character_, VSORRESU),
         VSSTAT   = if_else(not_done, "NOT DONE", NA_character_),
         VSREASND = if_else(not_done, toupper(BPREASND), NA_character_))

vs <- long %>%
  mutate(
    STUDYID  = STUDYID,
    DOMAIN   = "VS",
    VSPOS    = if_else(pos, ct_map(VSPOS, c(Sitting = "SITTING"), "VSPOS"), NA_character_),
    num      = suppressWarnings(as.numeric(VSORRES)),
    VSSTRESN = case_when(VSTESTCD == "TEMP" & VSORRESU == "F" ~ round((num - 32) * 5 / 9, 1),
                         TRUE ~ num),
    VSSTRESU = case_when(is.na(VSSTRESN) ~ NA_character_, VSTESTCD == "TEMP" ~ "C", TRUE ~ VSORRESU),
    VSSTRESC = if_else(is.na(VSSTRESN), NA_character_, as.character(VSSTRESN))
  )

# --- Visit (planned via folder; unscheduled via SV date) -------------------------
vs <- map_visits(vs, "VSDTC", sv, tv)

vs <- vs %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(EPOCH = derive_epoch(USUBJID, VSDTC, dm, predose = FOLDER %in% c("Screening", "Day 1")),
         VSDY  = study_day(VSDTC, RFSTDTC)) %>%
  derive_lobxfl("VS", dm) %>%
  derive_seq("VSSEQ", c("VISITNUM", "VSTESTCD", "VSREPNUM")) %>%
  select(STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSPOS, VSORRES, VSORRESU, VSSTRESC,
         VSSTRESN, VSSTRESU, VSSTAT, VSREASND, VSREPNUM, VSLOBXFL, VISITNUM, VISIT, VISITDY,
         EPOCH, VSDTC, VSDY)
write_domain(vs, "VS")
