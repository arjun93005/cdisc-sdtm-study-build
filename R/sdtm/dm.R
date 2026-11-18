# ==============================================================================
# Program     : R/sdtm/dm.R
# Study       : ZX-201
# Purpose     : Create SDTM DM (Demographics) and SUPPDM.
# Inputs      : data/raw/raw_dm.csv, raw_ex.csv, raw_ds.csv, raw_sv.csv,
#               ixrs_randomization.csv, ixrs_kit_list.csv
# Outputs     : output/sdtm/dm.xpt, output/sdtm/suppdm.xpt
# Spec        : specs/sdtm_spec/variables.csv (Dataset = DM, SUPPDM)
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

raw_dm <- read_raw("raw_dm")
raw_ex <- read_raw("raw_ex")
raw_ds <- read_raw("raw_ds")
raw_sv <- read_raw("raw_sv")
rand   <- read_raw("ixrs_randomization")
kits   <- read_raw("ixrs_kit_list")

arm_lookup <- c("Placebo" = "PBO", "ZX-101 10 mg" = "ZX10", "ZX-101 20 mg" = "ZX20")

# --- Reference dates from exposure --------------------------------------------
ex_ref <- raw_ex %>%
  mutate(st = raw_to_iso(EXSTDAT, EXSTTIM), en = raw_to_iso(EXENDAT)) %>%
  left_join(kits, by = "KITNO") %>%
  group_by(SUBJID) %>%
  summarise(RFXSTDTC = min(st), RFXENDTC = max(en),
            act_trt = paste(sort(unique(TREATMENT)), collapse = " + "), .groups = "drop")

# --- End of participation: latest of DS date and last visit -------------------
last_contact <- bind_rows(
  raw_ds %>% transmute(SUBJID, d = raw_to_iso(DSSTDAT)),
  raw_sv %>% transmute(SUBJID, d = raw_to_iso(VISDAT))
) %>% group_by(SUBJID) %>% summarise(RFPENDTC = max(d, na.rm = TRUE), .groups = "drop")

screening_dt <- raw_sv %>% filter(FOLDER == "Screening") %>%
  transmute(SUBJID, DMDTC = raw_to_iso(VISDAT))

# --- Race: checkbox -> RACE / MULTIPLE ----------------------------------------
race_map <- c(RACE_AMIND = "AMERICAN INDIAN OR ALASKA NATIVE", RACE_ASIAN = "ASIAN",
              RACE_BLACK = "BLACK OR AFRICAN AMERICAN", RACE_NHPI = "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
              RACE_WHITE = "WHITE")
races_long <- raw_dm %>% select(SUBJID, starts_with("RACE_")) %>%
  pivot_longer(-SUBJID, names_to = "box", values_to = "chk") %>%
  filter(chk == "Checked") %>%
  mutate(race = race_map[box]) %>%
  arrange(SUBJID, race)
race_one <- races_long %>% group_by(SUBJID) %>%
  summarise(RACE = if (n() > 1) "MULTIPLE" else first(race), n_race = n(), .groups = "drop")

# --- Build DM -------------------------------------------------------------------
dm <- raw_dm %>%
  left_join(race_one, by = "SUBJID") %>%
  left_join(rand, by = "SUBJID") %>%
  left_join(ex_ref, by = "SUBJID") %>%
  left_join(last_contact, by = "SUBJID") %>%
  left_join(screening_dt, by = "SUBJID") %>%
  left_join(raw_ds %>% select(SUBJID, DTHDAT), by = "SUBJID") %>%
  mutate(
    STUDYID  = STUDYID,
    DOMAIN   = "DM",
    USUBJID  = paste(STUDYID, SUBJID, sep = "-"),
    RFICDTC  = raw_to_iso(ICDAT),
    randomised = !is.na(RANDNO),
    treated  = !is.na(RFXSTDTC),
    RFSTDTC  = case_when(treated ~ RFXSTDTC, randomised ~ RANDDT, TRUE ~ NA_character_),
    RFENDTC  = case_when(treated ~ RFXENDTC, randomised ~ RANDDT, TRUE ~ NA_character_),
    DTHDTC   = raw_to_iso(DTHDAT),
    DTHFL    = if_else(!is.na(DTHDTC), "Y", NA_character_),
    BRTHDTC  = raw_to_iso(BRTHDAT),
    AGE      = if_else(is_complete_date(BRTHDTC),
                       as.integer(floor(as.numeric(dtc_date(RFICDTC) - dtc_date(BRTHDTC)) / 365.25)),
                       as.integer(AGE)),
    AGEU     = if_else(!is.na(AGE), "YEARS", NA_character_),
    SEX      = ct_map(SEX, c(Male = "M", Female = "F"), "SEX"),
    ETHNIC   = ct_map(ETHNIC, c("Hispanic or Latino" = "HISPANIC OR LATINO",
                                "Not Hispanic or Latino" = "NOT HISPANIC OR LATINO",
                                "Not reported" = "NOT REPORTED"), "ETHNIC"),
    ARMCD    = unname(arm_lookup[TRT_ASSIGNED]),
    ARM      = TRT_ASSIGNED,
    ACTARMCD = if_else(treated, unname(arm_lookup[act_trt]), NA_character_),
    ACTARM   = if_else(treated, act_trt, NA_character_),
    ARMNRS   = case_when(!randomised ~ "SCREEN FAILURE",
                         !treated ~ "ASSIGNED, NOT TREATED",
                         TRUE ~ NA_character_),
    ACTARMUD = NA_character_,
    COUNTRY  = case_when(str_starts(SITEID, "1") ~ "USA", str_starts(SITEID, "2") ~ "DEU",
                         str_starts(SITEID, "3") ~ "IND"),
    DMDY     = study_day(DMDTC, RFSTDTC)
  )

# Every dosed subject must have received only their randomised treatment.
mismatch <- dm %>% filter(treated, ACTARMCD != ARMCD | is.na(ACTARMCD))
if (nrow(mismatch)) stop("Planned vs actual arm mismatch for: ", paste(mismatch$USUBJID, collapse = ", "))

dm_out <- dm %>% select(STUDYID, DOMAIN, USUBJID, SUBJID, RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC,
                        RFICDTC, RFPENDTC, DTHDTC, DTHFL, SITEID, BRTHDTC, AGE, AGEU, SEX, RACE, ETHNIC,
                        ARMCD, ARM, ACTARMCD, ACTARM, ARMNRS, ACTARMUD, COUNTRY, DMDTC, DMDY)
write_domain(dm_out, "DM")

# --- SUPPDM: individual races for RACE = MULTIPLE -------------------------------
suppdm <- races_long %>%
  semi_join(race_one %>% filter(n_race > 1), by = "SUBJID") %>%
  group_by(SUBJID) %>% mutate(QNAM = paste0("RACE", row_number())) %>% ungroup() %>%
  transmute(STUDYID, RDOMAIN = "DM", USUBJID = paste(STUDYID, SUBJID, sep = "-"),
            IDVAR = NA_character_, IDVARVAL = NA_character_, QNAM,
            QLABEL = paste("Race", sub("RACE", "", QNAM)), QVAL = race,
            QORIG = "Collected", QEVAL = NA_character_)
write_domain(suppdm, "SUPPDM")
