# ==============================================================================
# Program     : R/00_simulate_raw_edc.R
# Study       : ZX-201
# Purpose     : Generate synthetic raw source data in the layouts a sponsor
#               actually receives -- EDC form extracts (CDASH field names,
#               DD-MON-YYYY text dates, partial dates, checkbox race), an IxRS
#               randomisation/kit file, and a central-lab vendor transfer in
#               conventional US units -- including realistic data issues that
#               the SDTM programs and conformance checks must handle.
# Inputs      : none (seeded, fully reproducible)
# Outputs     : data/raw/*.csv
# Author      : arjun93005
# History     : v1.0  Initial version
# Notes       : ALL DATA ARE SIMULATED. No real subjects. MedDRA/WHODrug-style
#               coding below is illustrative only (no licensed dictionary used).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr); library(tibble); library(readr)
})

set.seed(20250214)
raw_dir <- file.path("data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

MON <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
edc_date <- function(d) {
  out <- sprintf("%02d-%s-%04d", as.integer(format(d, "%d")), MON[as.integer(format(d, "%m"))],
                 as.integer(format(d, "%Y")))
  out[is.na(d)] <- NA_character_
  out
}
edc_partial <- function(d, unknown = c("day", "month")) {
  unknown <- match.arg(unknown)
  if (unknown == "day") sprintf("UN-%s-%04d", MON[as.integer(format(d, "%m"))], as.integer(format(d, "%Y")))
  else sprintf("UN-UNK-%04d", as.integer(format(d, "%Y")))
}
hhmm <- function(m) sprintf("%02d:%02d", m %/% 60, m %% 60)
wcsv <- function(df, name) write_csv(df, file.path(raw_dir, paste0(name, ".csv")), na = "")
pick <- function(x, p) sample(x, 1, prob = p)

# ------------------------------------------------------------------------------
# 1. Sites and screened subjects
# ------------------------------------------------------------------------------
sites <- tribble(
  ~SITEID, ~COUNTRY, ~TEMPU, ~n,
  "101", "USA", "F", 40,
  "102", "USA", "F", 35,
  "103", "USA", "F", 30,
  "201", "DEU", "C", 35,   # German sites collect birth YEAR only (data-privacy)
  "202", "DEU", "C", 30,
  "301", "IND", "C", 30
)
subj <- sites[rep(seq_len(nrow(sites)), sites$n), c("SITEID", "COUNTRY", "TEMPU")] %>%
  group_by(SITEID) %>% mutate(SUBJID = sprintf("%s-%03d", SITEID, row_number())) %>% ungroup()
N <- nrow(subj)

subj <- subj %>% mutate(
  icdt   = as.Date("2025-01-06") + sample(0:165, N, replace = TRUE),
  sex    = sample(c("Male", "Female"), N, TRUE, prob = c(.53, .47)),
  age_t  = pmin(pmax(round(rnorm(N, 57, 10)), 18), 75),
  brthdt = icdt - round((age_t + runif(N, .05, .95)) * 365.25),
  race   = map_chr(COUNTRY, ~ switch(.x,
             USA = pick(c("WHITE","BLACK","ASIAN","AMIND","NHPI","MULTI"), c(.64,.21,.07,.02,.01,.05)),
             DEU = pick(c("WHITE","BLACK","ASIAN","MULTI"), c(.91,.03,.04,.02)),
             IND = "ASIAN")),
  ethnic = map_chr(COUNTRY, ~ switch(.x,
             USA = pick(c("Hispanic or Latino","Not Hispanic or Latino"), c(.16,.84)),
             DEU = pick(c("Not Hispanic or Latino","Not reported"), c(.92,.08)),
             IND = "Not Hispanic or Latino")),
  sbp0   = pmax(rnorm(N, 153, 9), 142),
  dbp0   = rnorm(N, 93, 6),
  hr0    = rnorm(N, 74, 8),
  ht     = round(ifelse(sex == "Male", rnorm(N, 176, 7), rnorm(N, 163, 6))),
  wt0    = round(ifelse(sex == "Male", rnorm(N, 88, 13), rnorm(N, 74, 12)), 1),
  tempc  = rnorm(N, 36.7, .25)
)
subj$age_true <- as.integer(floor(as.numeric(subj$icdt - subj$brthdt) / 365.25))

# Screen failures (n = 20): BP criterion, renal exclusion, consent withdrawn
sf_idx <- sample(seq_len(N), 20)
sf_reason <- c(rep("BP", 12), rep("RENAL", 5), rep("WC", 3))
subj$sf <- FALSE; subj$sf[sf_idx] <- TRUE
subj$sf_reason <- NA_character_; subj$sf_reason[sf_idx] <- sf_reason
subj$sbp0[sf_idx[sf_reason == "BP"]] <- runif(12, 126, 138.5)

# ------------------------------------------------------------------------------
# 2. Randomisation (IxRS): site-stratified blocks of 6, 1:1:1
# ------------------------------------------------------------------------------
arms <- c(PBO = "Placebo", ZX10 = "ZX-101 10 mg", ZX20 = "ZX-101 20 mg")
dose_mg <- c(PBO = 0, ZX10 = 10, ZX20 = 20)
subj$armcd <- NA_character_
for (s in unique(subj$SITEID)) {
  idx <- which(subj$SITEID == s & !subj$sf)
  idx <- idx[order(subj$icdt[idx])]
  blocks <- unlist(replicate(ceiling(length(idx) / 6), sample(rep(names(arms), 2)), simplify = FALSE))
  subj$armcd[idx] <- blocks[seq_along(idx)]
}
subj <- subj %>% mutate(d1 = if_else(sf, as.Date(NA), icdt + sample(7:14, N, TRUE)))

rand <- subj %>% filter(!sf) %>% arrange(d1, SUBJID) %>%
  mutate(RANDNO = sprintf("R%04d", row_number()))

# ------------------------------------------------------------------------------
# 3. Disposition
# ------------------------------------------------------------------------------
reasons <- c("Lack of efficacy", "Withdrawal by subject", "Lost to follow-up", "Adverse event")
p_disc <- c(PBO = .11, ZX10 = .08, ZX20 = .13)
p_reason <- list(PBO = c(.45, .30, .15, .10), ZX10 = c(.15, .35, .25, .25), ZX20 = c(.05, .25, .15, .55))

rand <- rand %>% mutate(
  treated   = TRUE,
  disc      = runif(n()) < p_disc[armcd],
  reason    = map_chr(armcd, ~ sample(reasons, 1, prob = p_reason[[.x]])),
  disc_day  = sample(12:80, n(), TRUE)
)
rand$reason[!rand$disc] <- "Completed"
# one randomised-but-never-dosed subject (withdrew consent on Day 1, pre-dose)
nt <- which(rand$armcd == "PBO" & !rand$disc)[3]
rand$treated[nt] <- FALSE; rand$disc[nt] <- TRUE
rand$reason[nt] <- "Withdrawal by subject"; rand$disc_day[nt] <- 1
# one death (fatal MI, ZX-101 10 mg, study day 48)
dth <- which(rand$armcd == "ZX10" & !rand$disc)[5]
rand$disc[dth] <- TRUE; rand$reason[dth] <- "Death"; rand$disc_day[dth] <- 48

rand <- rand %>% mutate(
  disc_dt = if_else(disc, d1 + disc_day - 1, as.Date(NA)),
  dose_tm = sample(9 * 60 + 0:120, n(), TRUE)          # first dose ~09:00-11:00
)

# ------------------------------------------------------------------------------
# 4. Visits (SV source: "Visit Date" form)
# ------------------------------------------------------------------------------
plan <- tibble(FOLDER = c("Screening", "Day 1", "Week 2", "Week 4", "Week 8", "Week 12"),
               pday = c(NA, 1, 15, 29, 57, 85))

visits_rand <- rand %>% select(SUBJID, d1, disc, disc_dt, treated) %>%
  crossing(plan %>% filter(FOLDER != "Screening")) %>%
  mutate(vdt = d1 + pday - 1 + if_else(pday == 1, 0L, sample(-2:3, n(), TRUE))) %>%
  filter(!disc | vdt <= disc_dt) %>%
  filter(!(FOLDER %in% c("Week 2", "Week 8") & runif(n()) < .02))   # missed visits

last_dose <- visits_rand %>% filter(FOLDER == "Week 12") %>% select(SUBJID, wk12 = vdt)
rand <- rand %>% left_join(last_dose, by = "SUBJID") %>%
  mutate(lastdose = case_when(!treated ~ as.Date(NA),
                              disc ~ pmax(d1, disc_dt - sample(0:2, n(), TRUE)),
                              TRUE ~ wk12),
         # completers whose Week 12 visit was not recorded still dose to Day 85
         lastdose = if_else(treated & !disc & is.na(lastdose), d1 + 84, lastdose))

fu <- rand %>% filter(!disc) %>%
  transmute(SUBJID, FOLDER = "Follow-up", vdt = lastdose + 14 + sample(-1:3, n(), TRUE))
unsch <- rand %>% filter(treated, runif(n()) < .08) %>%
  transmute(SUBJID, FOLDER = "Unscheduled",
            vdt = d1 + sample(20:70, n(), TRUE)) %>%
  left_join(rand %>% select(SUBJID, disc_dt), by = "SUBJID") %>%
  filter(is.na(disc_dt) | vdt < disc_dt) %>% select(-disc_dt)

visits <- bind_rows(
  subj %>% transmute(SUBJID, FOLDER = "Screening", vdt = icdt),
  visits_rand %>% select(SUBJID, FOLDER, vdt),
  fu, unsch
) %>% arrange(SUBJID, vdt)

raw_sv <- visits %>% transmute(SUBJID, FOLDER, VISDAT = edc_date(vdt))
wcsv(raw_sv, "raw_sv")

# ------------------------------------------------------------------------------
# 5. DM form
# ------------------------------------------------------------------------------
race_boxes <- function(r) {
  sel <- switch(r, MULTI = sample(list(c("WHITE","BLACK"), c("WHITE","ASIAN"), c("WHITE","AMIND")), 1)[[1]], r)
  b <- setNames(rep("", 5), c("RACE_AMIND","RACE_ASIAN","RACE_BLACK","RACE_NHPI","RACE_WHITE"))
  b[paste0("RACE_", sel)] <- "Checked"
  as.list(b)
}
raw_dm <- subj %>%
  mutate(BRTHDAT = if_else(COUNTRY == "DEU", edc_partial(brthdt, "month"), edc_date(brthdt)),
         AGE = if_else(COUNTRY == "DEU", as.character(age_true), NA_character_)) %>%
  transmute(SITEID, SUBJID, ICDAT = edc_date(icdt), BRTHDAT, AGE, SEX = sex, ETHNIC = ethnic, race) %>%
  bind_cols(map_dfr(.$race, race_boxes)) %>% select(-race)
wcsv(raw_dm, "raw_dm")

# ------------------------------------------------------------------------------
# 6. IxRS: randomisation + kit list (unblinded at database lock)
# ------------------------------------------------------------------------------
wcsv(rand %>% transmute(SUBJID, RANDNO, RANDDT = format(d1), TRT_ASSIGNED = arms[armcd]), "ixrs_randomization")

# ------------------------------------------------------------------------------
# 7. EX form: one record per dispensed kit (dispensed Day 1, Week 4, Week 8)
# ------------------------------------------------------------------------------
disp <- visits_rand %>% filter(treated, FOLDER %in% c("Day 1", "Week 4", "Week 8")) %>%
  select(SUBJID, FOLDER, start = vdt) %>%
  left_join(rand %>% select(SUBJID, armcd, lastdose, dose_tm), by = "SUBJID") %>%
  filter(start <= lastdose) %>%
  arrange(SUBJID, start) %>% group_by(SUBJID) %>%
  mutate(end = pmin(lead(start) - 1, lastdose, na.rm = TRUE)) %>% ungroup() %>%
  mutate(KITNO = sprintf("K%05d", sample(10000:99999, n())))

kits <- disp %>% transmute(KITNO, TREATMENT = arms[armcd], STRENGTH_MG = dose_mg[armcd]) %>%
  bind_rows(tibble(KITNO = sprintf("K%05d", 1:30), TREATMENT = rep(arms, 10),
                   STRENGTH_MG = rep(dose_mg, 10))) %>%   # unused kits also in the kit list
  arrange(KITNO)
wcsv(kits, "ixrs_kit_list")

raw_ex <- disp %>% group_by(SUBJID) %>%
  mutate(EXSTTIM = if_else(row_number() == 1, hhmm(dose_tm), NA_character_)) %>% ungroup() %>%
  transmute(SUBJID, FOLDER, KITNO, EXSTDAT = edc_date(start), EXSTTIM, EXENDAT = edc_date(end),
            EXTABS = "1", EXFREQ = "Once daily", EXROUTE = "Oral")
wcsv(raw_ex, "raw_ex")

# ------------------------------------------------------------------------------
# 8. Vital signs form (triplicate seated BP; temperature in site units)
# ------------------------------------------------------------------------------
delta <- c(PBO = -4, ZX10 = -10, ZX20 = -15)
subj <- subj %>% left_join(rand %>% select(SUBJID, armcd2 = armcd, d1r = d1, lastdose, treated, dose_tm),
                           by = "SUBJID") %>%
  mutate(resp = rnorm(n(), 1, .45))

vs_src <- visits %>% left_join(subj, by = "SUBJID") %>%
  mutate(
    day   = as.integer(vdt - d1r) + 1L,
    on_trt = !is.na(treated) & treated & FOLDER %in% c("Week 2","Week 4","Week 8","Week 12","Unscheduled"),
    eff   = case_when(on_trt ~ delta[armcd2] * resp * pmin(1, (day - 1) / 28),
                      FOLDER == "Follow-up" ~ .3 * delta[armcd2] * resp,
                      TRUE ~ 0),
    vtime = if_else(FOLDER == "Day 1", dose_tm - sample(30:75, n(), TRUE), sample(450:690, n(), TRUE))
  )
bp <- function(mu, sd) round(mu + rnorm(length(mu), 0, sd))
raw_vs <- vs_src %>% mutate(
  nd = runif(n()) < .01,
  SYSBP_1 = bp(sbp0 + eff, 5), SYSBP_2 = bp(sbp0 + eff, 5), SYSBP_3 = bp(sbp0 + eff, 5),
  DIABP_1 = bp(dbp0 + eff * .55, 3.5), DIABP_2 = bp(dbp0 + eff * .55, 3.5), DIABP_3 = bp(dbp0 + eff * .55, 3.5),
  PULSE = bp(hr0 + if_else(on_trt & armcd2 == "ZX20", 2, 0), 5),
  t = tempc + rnorm(n(), 0, .2),
  TEMP = if_else(TEMPU == "F", sprintf("%.1f", t * 9 / 5 + 32), sprintf("%.1f", t)),
  WEIGHT = sprintf("%.1f", wt0 + rnorm(n(), 0, .8)),
  HEIGHT = if_else(FOLDER == "Screening", as.character(ht), NA_character_)
) %>%
  mutate(across(c(SYSBP_1:DIABP_3), ~ if_else(nd, NA_character_, as.character(.x))),
         BPND = if_else(nd, "Not done", NA_character_),
         BPREASND = if_else(nd, "Equipment failure", NA_character_)) %>%
  transmute(SUBJID, FOLDER, VSDAT = edc_date(vdt), VSTIM = hhmm(vtime), VSPOS = "Sitting",
            SYSBP_1, DIABP_1, SYSBP_2, DIABP_2, SYSBP_3, DIABP_3, BPND, BPREASND,
            PULSE = as.character(PULSE), TEMP, TEMPU, WEIGHT, HEIGHT)
# data issue: a Week 4 weight entered in lb instead of kg at a US site
oops <- which(raw_vs$FOLDER == "Week 4" & str_starts(raw_vs$SUBJID, "101"))[2]
raw_vs$WEIGHT[oops] <- sprintf("%.1f", as.numeric(raw_vs$WEIGHT[oops]) * 2.2046)
wcsv(raw_vs, "raw_vs")
vs_times <- vs_src %>% select(SUBJID, FOLDER, vdt, vtime)

# ------------------------------------------------------------------------------
# 9. Central lab transfer (vendor layout, conventional units)
# ------------------------------------------------------------------------------
lab_tests <- tribble(
  ~TEST_CODE, ~TEST_NAME, ~BATTERY, ~UNITS, ~LOW_M, ~HIGH_M, ~LOW_F, ~HIGH_F, ~mean, ~sd, ~dec,
  "SGPT", "ALT (SGPT)",      "CHEM", "U/L",      7,   56,   7,   56,   24,  10,  0,
  "SGOT", "AST (SGOT)",      "CHEM", "U/L",      10,  40,   10,  40,   23,  7,   0,
  "CREA", "Creatinine",      "CHEM", "mg/dL",    0.74, 1.35, 0.59, 1.04, NA, .13, 2,
  "POT",  "Potassium",       "CHEM", "mEq/L",    3.5, 5.1,  3.5, 5.1,  4.3, .35, 1,
  "SOD",  "Sodium",          "CHEM", "mEq/L",    136, 145,  136, 145,  140, 2.2, 0,
  "GLU",  "Glucose, fasting","CHEM", "mg/dL",    70,  99,   70,  99,   97,  14,  0,
  "HGB",  "Hemoglobin",      "HEME", "g/dL",     13.2, 16.6, 11.6, 15.0, NA, 1,  1,
  "WBC",  "WBC Count",       "HEME", "10^3/uL",  3.4, 9.6,  3.4, 9.6,  6.6, 1.5, 1,
  "PLT",  "Platelet Count",  "HEME", "10^3/uL",  135, 317,  157, 371,  245, 50,  0
)
vend_visit <- c("Screening" = "SCRN", "Day 1" = "DAY1", "Week 4" = "WK4", "Week 12" = "WK12", "Unscheduled" = "UNSCH")
k_shift <- c(PBO = 0, ZX10 = .12, ZX20 = .30)

lab_vis <- visits %>% filter(FOLDER %in% names(vend_visit)) %>%
  filter(FOLDER != "Unscheduled" | runif(n()) < .5) %>%
  left_join(vs_times, by = c("SUBJID", "FOLDER", "vdt")) %>%
  left_join(subj %>% select(SUBJID, SITEID, sex, armcd2, treated, sf_reason), by = "SUBJID") %>%
  mutate(ACCESSION = sprintf("A%07d", sample(1e6:9e6, n())),
         ctime = coalesce(vtime, 480L) + 10L)

raw_lb <- lab_vis %>% crossing(lab_tests) %>%
  mutate(
    on_trt = !is.na(treated) & treated & FOLDER %in% c("Week 4", "Week 12", "Unscheduled"),
    mu = case_when(TEST_CODE == "CREA" & sex == "Male" ~ .98, TEST_CODE == "CREA" ~ .78,
                   TEST_CODE == "HGB" & sex == "Male" ~ 14.9, TEST_CODE == "HGB" ~ 13.3,
                   TRUE ~ mean),
    mu = mu + if_else(TEST_CODE == "POT" & on_trt, k_shift[armcd2], 0),
    mu = if_else(TEST_CODE == "CREA" & FOLDER == "Screening" & !is.na(sf_reason) & sf_reason == "RENAL", 2.1, mu),
    val = if_else(TEST_CODE %in% c("SGPT", "SGOT"), exp(rnorm(n(), log(mu), .35)), rnorm(n(), mu, sd)),
    val = pmax(val, 0),
    RESULT = sprintf(paste0("%.", dec, "f"), round(val, dec)),
    RESULT = if_else(TEST_CODE == "SGPT" & val < 6, "<6", RESULT),
    RANGE_LOW = if_else(sex == "Male", LOW_M, LOW_F), RANGE_HIGH = if_else(sex == "Male", HIGH_M, HIGH_F),
    hemo = TEST_CODE == "POT" & runif(n()) < .012,
    RESULT = if_else(hemo, NA_character_, RESULT),
    RESULT_COMMENT = if_else(hemo, "SPECIMEN HEMOLYZED - TEST CANCELLED", NA_character_),
    num = suppressWarnings(as.numeric(RESULT)),
    ABN_FLAG = case_when(is.na(num) ~ NA_character_, num < RANGE_LOW ~ "L", num > RANGE_HIGH ~ "H", TRUE ~ "")
  ) %>%
  transmute(STUDY = "ZX201", SITE = SITEID, PATIENT = SUBJID, VISIT_CODE = vend_visit[FOLDER], ACCESSION,
            COLL_DATE = format(vdt), COLL_TIME = hhmm(ctime), BATTERY, TEST_CODE, TEST_NAME, RESULT, UNITS,
            RANGE_LOW, RANGE_HIGH, ABN_FLAG, RESULT_COMMENT) %>%
  arrange(PATIENT, COLL_DATE, BATTERY, TEST_CODE)
wcsv(raw_lb, "central_lab_transfer")

# ------------------------------------------------------------------------------
# 10. Adverse events (+ illustrative MedDRA-style coding export)
# ------------------------------------------------------------------------------
dict <- tribble(
  ~PT, ~HLT, ~HLGT, ~SOC, ~w_PBO, ~w_ZX10, ~w_ZX20, ~drug_rel, ~verbatims,
  "Headache", "Headaches NEC", "Headaches", "Nervous system disorders", 8, 9, 10, TRUE, "Headache|HEADACHE|Head ache|Mild headache",
  "Dizziness", "Neurological signs and symptoms NEC", "Neurological disorders NEC", "Nervous system disorders", 3, 7, 12, TRUE, "Dizziness|Dizzyness|Feeling dizzy",
  "Fatigue", "Asthenic conditions", "General system disorders NEC", "General disorders and administration site conditions", 4, 5, 7, TRUE, "Fatigue|Tiredness|Tired",
  "Nausea", "Nausea and vomiting symptoms", "Gastrointestinal signs and symptoms", "Gastrointestinal disorders", 3, 3, 4, FALSE, "Nausea|Nauseous",
  "Oedema peripheral", "Oedema NEC", "General system disorders NEC", "General disorders and administration site conditions", 1, 3, 6, TRUE, "Peripheral edema|Ankle edema|Swelling of ankles",
  "Cough", "Coughing and associated symptoms", "Respiratory disorders NEC", "Respiratory, thoracic and mediastinal disorders", 2, 2, 3, FALSE, "Cough|Dry cough",
  "Nasopharyngitis", "Upper respiratory tract infections", "Infections - pathogen unspecified", "Infections and infestations", 6, 6, 6, FALSE, "Common cold|Nasopharyngitis|Cold",
  "Hyperkalaemia", "Potassium imbalance", "Electrolyte and fluid balance conditions", "Metabolism and nutrition disorders", .5, 2, 4, TRUE, "Hyperkalemia|High potassium",
  "Back pain", "Musculoskeletal and connective tissue pain and discomfort", "Musculoskeletal and connective tissue disorders NEC", "Musculoskeletal and connective tissue disorders", 3, 3, 3, FALSE, "Back pain|Low back pain|Backache",
  "Diarrhoea", "Diarrhoea (excl infective)", "Gastrointestinal motility and defaecation conditions", "Gastrointestinal disorders", 2, 3, 3, FALSE, "Diarrhea|Loose stools",
  "Hypotension", "Vascular hypotensive disorders", "Decreased and nonspecific blood pressure disorders and shock", "Vascular disorders", .3, 1, 3, TRUE, "Hypotension|Low blood pressure",
  "Upper respiratory tract infection", "Upper respiratory tract infections", "Infections - pathogen unspecified", "Infections and infestations", 4, 4, 4, FALSE, "URTI|Upper respiratory infection",
  "Arthralgia", "Joint related signs and symptoms", "Joint disorders", "Musculoskeletal and connective tissue disorders", 2, 2, 2, FALSE, "Joint pain|Arthralgia",
  "Insomnia", "Disturbances in initiating and maintaining sleep", "Sleep disorders and disturbances", "Psychiatric disorders", 2, 2, 2, FALSE, "Insomnia|Difficulty sleeping",
  "Rash", "Rashes, eruptions and exanthems NEC", "Epidermal and dermal conditions", "Skin and subcutaneous tissue disorders", 1, 1.5, 2, TRUE, "Rash|Skin rash",
  "Hypertension", "Vascular hypertensive disorders NEC", "Vascular hypertensive disorders", "Vascular disorders", 4, 1.5, .5, FALSE, "Worsening hypertension|Uncontrolled blood pressure",
  "Pneumonia", "Lower respiratory tract and lung infections", "Infections - pathogen unspecified", "Infections and infestations", .4, .4, .4, FALSE, "Pneumonia|Community acquired pneumonia",
  "Syncope", "Disturbances in consciousness NEC", "Neurological disorders NEC", "Nervous system disorders", .2, .5, 1.2, TRUE, "Syncope|Fainting episode",
  "Myocardial infarction", "Ischaemic coronary artery disorders", "Coronary artery disorders", "Cardiac disorders", 0, 0, 0, FALSE, "Acute myocardial infarction"
)
serious_pts <- c("Pneumonia", "Syncope", "Myocardial infarction")
ae_coding <- dict %>% select(PT, HLT, HLGT, SOC, verbatims) %>%
  separate_rows(verbatims, sep = "\\|") %>%
  transmute(VERBATIM_UPPER = toupper(verbatims), LLT = PT, PT, HLT, HLGT, SOC,
            DICTIONARY = "MedDRA-style illustrative coding (not licensed MedDRA)") %>%
  distinct(VERBATIM_UPPER, .keep_all = TRUE)
lambda <- c(PBO = 1.2, ZX10 = 1.5, ZX20 = 1.9)

trt_subj <- rand %>% filter(treated) %>%
  mutate(end_obs = coalesce(disc_dt, lastdose + 14))
ae_list <- list()
for (i in seq_len(nrow(trt_subj))) {
  s <- trt_subj[i, ]
  span <- as.integer(s$end_obs - s$d1) + 1
  n_ae <- rpois(1, lambda[s$armcd] * span / 99)
  w <- dict[[paste0("w_", s$armcd)]]
  if (n_ae > 0) {
    pts <- sample(dict$PT, n_ae, replace = TRUE, prob = w)
    ae_list[[length(ae_list) + 1]] <- tibble(SUBJID = s$SUBJID, PT = pts,
      start = s$d1 + sample(0:(span - 1), n_ae, TRUE), armcd = s$armcd, end_obs = s$end_obs, kind = "random")
  }
  if (s$reason == "Adverse event") {
    pt <- sample(c("Dizziness", "Oedema peripheral", "Hypotension", "Hyperkalaemia", "Rash"), 1)
    ae_list[[length(ae_list) + 1]] <- tibble(SUBJID = s$SUBJID, PT = pt,
      start = s$disc_dt - sample(1:5, 1), armcd = s$armcd, end_obs = s$end_obs, kind = "disc")
  }
  if (s$reason == "Death") {
    ae_list[[length(ae_list) + 1]] <- tibble(SUBJID = s$SUBJID, PT = "Myocardial infarction",
      start = s$disc_dt - 1, armcd = s$armcd, end_obs = s$end_obs, kind = "death")
  }
}
# pre-treatment AEs during screening (not treatment-emergent)
pre <- rand %>% filter(runif(n()) < .06) %>%
  mutate(PT = sample(c("Headache", "Nasopharyngitis", "Back pain"), n(), TRUE),
         start = icdt + sample(1:5, n(), TRUE), end_obs = d1 + 30, kind = "pre") %>%
  filter(start < d1) %>%
  select(SUBJID, PT, start, armcd, end_obs, kind)
ae <- bind_rows(ae_list, pre) %>% left_join(dict %>% select(PT, drug_rel, verbatims), by = "PT") %>%
  mutate(
    AETERM = map_chr(verbatims, ~ sample(str_split(.x, "\\|")[[1]], 1)),
    ser = PT %in% serious_pts,
    dur = ceiling(rexp(n(), 1 / 7)),
    end = start + dur,
    ongoing = end > end_obs & kind != "death",
    end = case_when(kind == "death" ~ start + 1, ongoing ~ as.Date(NA), TRUE ~ end),
    AESEV = case_when(kind == "death" ~ "Severe",
                      ser ~ sample(c("Moderate", "Severe"), n(), TRUE),
                      TRUE ~ sample(c("Mild", "Moderate", "Severe"), n(), TRUE, prob = c(.62, .31, .07))),
    AESER = if_else(ser, "Yes", "No"),
    AEREL = map2_chr(drug_rel, armcd, ~ sample(c("Not related", "Unlikely related", "Possibly related", "Related"), 1,
               prob = if (.x) c(.25, .2, .35, .2) else c(.6, .25, .12, .03))),
    AEREL = if_else(kind == "pre", "Not related", AEREL),
    AEACN = case_when(kind == "pre" ~ "Not applicable", kind == "disc" ~ "Drug withdrawn",
                      kind == "death" ~ "Not applicable", TRUE ~ "Dose not changed"),
    AEOUT = case_when(kind == "death" ~ "Fatal", ongoing ~ "Not recovered/not resolved",
                      TRUE ~ sample(c("Recovered/resolved", "Recovering/resolving"), n(), TRUE, prob = c(.93, .07))),
    AESHOSP = if_else(ser, "Yes", "No"),
    AESDTH = if_else(kind == "death", "Yes", "No")
  ) %>% arrange(SUBJID, start) %>% group_by(SUBJID) %>% mutate(AELINE = row_number()) %>% ungroup()

partial <- runif(nrow(ae)) < .04 & ae$kind == "random"
raw_ae <- ae %>% mutate(
  AESTDAT = if_else(partial, edc_partial(start, "day"), edc_date(start)),
  AEENDAT = edc_date(end),
  AEONGO = if_else(ongoing, "Yes", "No")
)
# data issue: one AE whose end date precedes its start (open query at lock)
bad <- which(!is.na(raw_ae$AEENDAT) & !partial & ae$kind == "random")[7]
raw_ae$AEENDAT[bad] <- edc_date(ae$start[bad] - 3)
raw_ae <- raw_ae %>% transmute(SUBJID, AESPID = as.character(AELINE), AETERM, AESTDAT, AEENDAT, AEONGO,
                               AESEV, AESER, AESHOSP, AESDTH, AEREL, AEACN, AEOUT)
wcsv(raw_ae, "raw_ae")
wcsv(ae_coding, "coding_ae_meddra_style")

# ------------------------------------------------------------------------------
# 11. Concomitant medications (+ illustrative WHODrug/ATC-style coding)
# ------------------------------------------------------------------------------
cm_dict <- tribble(
  ~VERBATIM_UPPER, ~DECOD, ~ATC_TEXT, ~ATC_CODE,
  "TYLENOL", "PARACETAMOL", "ANILIDES", "N02BE",
  "PARACETAMOL", "PARACETAMOL", "ANILIDES", "N02BE",
  "ACETAMINOPHEN", "PARACETAMOL", "ANILIDES", "N02BE",
  "IBUPROFEN", "IBUPROFEN", "PROPIONIC ACID DERIVATIVES", "M01AE",
  "ADVIL", "IBUPROFEN", "PROPIONIC ACID DERIVATIVES", "M01AE",
  "ATORVASTATIN", "ATORVASTATIN", "HMG COA REDUCTASE INHIBITORS", "C10AA",
  "LIPITOR", "ATORVASTATIN", "HMG COA REDUCTASE INHIBITORS", "C10AA",
  "METFORMIN", "METFORMIN", "BIGUANIDES", "A10BA",
  "OMEPRAZOLE", "OMEPRAZOLE", "PROTON PUMP INHIBITORS", "A02BC",
  "CETIRIZINE", "CETIRIZINE", "PIPERAZINE DERIVATIVES", "R06AE",
  "ASPIRIN 81MG", "ACETYLSALICYLIC ACID", "PLATELET AGGREGATION INHIBITORS EXCL. HEPARIN", "B01AC"
)
prior_pool <- tribble(
  ~p, ~names, ~CMINDC, ~CMDSTXT, ~CMFREQ,
  .25, "Atorvastatin|Lipitor", "Hypercholesterolemia", "20", "Once daily",
  .15, "Metformin", "Type 2 diabetes", "500", "Twice daily",
  .10, "Omeprazole", "GERD", "20", "Once daily",
  .15, "Aspirin 81mg", "CV prophylaxis", "81", "Once daily",
  .08, "Cetirizine", "Seasonal allergy", "10", "Once daily"
)
cm_prior <- map_dfr(seq_len(nrow(prior_pool)), function(k) {
  pp <- prior_pool[k, ]
  who <- subj %>% filter(!sf, runif(n()) < pp$p)
  tibble(SUBJID = who$SUBJID,
         CMTRT = map_chr(seq_len(nrow(who)), ~ sample(str_split(pp$names, "\\|")[[1]], 1)),
         CMINDC = pp$CMINDC, CMDSTXT = pp$CMDSTXT, CMDOSU = "mg", CMFREQ = pp$CMFREQ,
         st = who$icdt - sample(200:3000, nrow(who), TRUE))
}) %>% mutate(CMSTDAT = if_else(runif(n()) < .5, edc_partial(st, "month"), edc_partial(st, "day")),
              CMENDAT = NA_character_, CMONGO = "Yes")
cm_ae <- ae %>% filter(PT %in% c("Headache", "Back pain"), runif(n()) < .6) %>%
  transmute(SUBJID,
            CMTRT = if_else(PT == "Headache", sample(c("Tylenol", "Paracetamol", "Acetaminophen"), n(), TRUE),
                            sample(c("Ibuprofen", "Advil"), n(), TRUE)),
            CMINDC = PT, CMDSTXT = if_else(PT == "Headache", "500", "400"), CMDOSU = "mg", CMFREQ = "As needed",
            CMSTDAT = edc_date(start), CMENDAT = edc_date(start + sample(1:4, n(), TRUE)), CMONGO = "No")
raw_cm <- bind_rows(cm_prior %>% select(-st), cm_ae) %>% mutate(CMROUTE = "Oral") %>%
  arrange(SUBJID) %>% group_by(SUBJID) %>% mutate(CMSPID = as.character(row_number())) %>% ungroup() %>%
  select(SUBJID, CMSPID, CMTRT, CMINDC, CMDSTXT, CMDOSU, CMFREQ, CMROUTE, CMSTDAT, CMENDAT, CMONGO)
wcsv(raw_cm, "raw_cm")
wcsv(cm_dict %>% mutate(DICTIONARY = "WHODrug-style illustrative coding (not licensed WHODrug)"), "coding_cm_whodrug_style")

# ------------------------------------------------------------------------------
# 12. End of Study form (DS)
# ------------------------------------------------------------------------------
sf_text <- c(BP = "Did not meet inclusion criterion 3 (seated SBP 140-179 mmHg)",
             RENAL = "Met exclusion criterion 6 (serum creatinine above protocol limit)",
             WC = "Subject withdrew consent before randomisation")
raw_ds <- bind_rows(
  subj %>% filter(sf) %>%
    transmute(SUBJID, DSCOMPL = "No", DSREAS = "Screen failure", DSSPEC = sf_text[sf_reason],
              DSSTDAT = edc_date(icdt + sample(2:9, n(), TRUE)), DTHDAT = NA_character_),
  rand %>% transmute(SUBJID,
                     DSCOMPL = if_else(disc, "No", "Yes"),
                     DSREAS = if_else(disc, reason, NA_character_),
                     DSSPEC = case_when(reason == "Death" ~ "Fatal acute myocardial infarction",
                                        !treated ~ "Withdrew consent on Day 1 before first dose",
                                        TRUE ~ NA_character_),
                     DSSTDAT = edc_date(coalesce(disc_dt, lastdose + 14)),
                     DTHDAT = if_else(reason == "Death", edc_date(disc_dt), NA_character_))
)
# completers: DS date = actual follow-up visit date
raw_ds <- raw_ds %>% left_join(fu %>% transmute(SUBJID, fudat = edc_date(vdt)), by = "SUBJID") %>%
  mutate(DSSTDAT = if_else(DSCOMPL == "Yes" & !is.na(fudat), fudat, DSSTDAT)) %>% select(-fudat) %>%
  arrange(SUBJID)
wcsv(raw_ds, "raw_ds")

message("Raw data written to ", raw_dir, ": ",
        paste(sprintf("%s (%d)", c("dm", "sv", "vs", "lb", "ae", "cm", "ex", "ds"),
                      c(nrow(raw_dm), nrow(raw_sv), nrow(raw_vs), nrow(raw_lb), nrow(raw_ae),
                        nrow(raw_cm), nrow(raw_ex), nrow(raw_ds))), collapse = ", "))
