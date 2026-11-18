# ==============================================================================
# Program     : R/sdtm/trial_design.R
# Study       : ZX-201
# Purpose     : Create trial design domains TA, TE, TV and TS from the protocol
#               (specs/trial_design, specs/visit_schedule.csv). TSPARM and
#               TSVALCD are looked up in the pinned CDISC CT file so they can
#               never drift from the published terminology.
# Inputs      : specs/trial_design/ts_source.csv, specs/visit_schedule.csv,
#               specs/ct/SDTM_Terminology.txt, output/sdtm/dm.xpt
# Outputs     : output/sdtm/ta.xpt, te.xpt, tv.xpt, ts.xpt
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

dm <- read_sdtm("DM")
ct <- load_ct()
CT_VERSION <- readLines(file.path("specs", "ct", "CT_VERSION.txt"), n = 1)

arms <- tibble(ARMCD = c("PBO", "ZX10", "ZX20"), ARM = c("Placebo", "ZX-101 10 mg", "ZX-101 20 mg"))

# --- TE -----------------------------------------------------------------------------
te <- tribble(
  ~ETCD,  ~ELEMENT,       ~TESTRL,                                ~TEENRL,                                          ~TEDUR,
  "SCRN", "Screening",    "Informed consent",                     "First dose of study drug",                       NA,
  "PBO",  "Placebo",      "First dose of placebo",                "1 day after last dose of study drug",            "P12W",
  "ZX10", "ZX-101 10 mg", "First dose of ZX-101 10 mg",           "1 day after last dose of study drug",            "P12W",
  "ZX20", "ZX-101 20 mg", "First dose of ZX-101 20 mg",           "1 day after last dose of study drug",            "P12W",
  "FU",   "Follow-up",    "1 day after last dose of study drug",  "Follow-up visit (14 days after last dose)",      "P14D"
) %>% mutate(STUDYID = STUDYID, DOMAIN = "TE", .before = 1)
write_domain(te, "TE")

# --- TA -----------------------------------------------------------------------------
ta <- arms %>%
  crossing(TAETORD = 1:3) %>%
  mutate(STUDYID = STUDYID, DOMAIN = "TA",
         ETCD = case_when(TAETORD == 1 ~ "SCRN", TAETORD == 2 ~ ARMCD, TRUE ~ "FU"),
         ELEMENT = te$ELEMENT[match(ETCD, te$ETCD)],
         TABRANCH = if_else(TAETORD == 1, paste("Randomized to", ARM), NA_character_),
         TATRANS = NA_character_,
         EPOCH = c("SCREENING", "TREATMENT", "FOLLOW-UP")[TAETORD]) %>%
  select(STUDYID, DOMAIN, ARMCD, ARM, TAETORD, ETCD, ELEMENT, TABRANCH, TATRANS, EPOCH)
write_domain(ta, "TA")

# --- TV -----------------------------------------------------------------------------
tv <- read_csv("specs/visit_schedule.csv", show_col_types = FALSE) %>%
  transmute(STUDYID = STUDYID, DOMAIN = "TV", VISITNUM, VISIT, VISITDY, TVSTRL, TVENRL)
write_domain(tv, "TV")

# --- TS -----------------------------------------------------------------------------
derived <- c(
  "{ACTSUB}" = as.character(sum(!is.na(dm$ARMCD))),
  "{SSTDTC}" = min(dm$RFICDTC),
  "{SENDTC}" = max(dm$RFPENDTC)
)
ts_src <- read_csv("specs/trial_design/ts_source.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  mutate(TSVAL = coalesce(unname(derived[TSVAL]), TSVAL))

tsparm_name <- ct$terms %>% filter(short_name == "TSPARM") %>% select(term_code, TSPARM = value)
tsparm <- ct$terms %>% filter(short_name == "TSPARMCD") %>%
  select(TSPARMCD = value, term_code) %>%
  left_join(tsparm_name, by = "term_code") %>% select(-term_code)

missing_parm <- setdiff(ts_src$TSPARMCD, tsparm$TSPARMCD)
if (length(missing_parm)) stop("TSPARMCD not in CDISC CT: ", paste(missing_parm, collapse = ", "))

# TSVALCD: for CT-controlled values, the NCI code of the term in the codelist
# associated with the parameter (e.g. TPHASE -> codelist TPHASE).
param_codelist <- c(TPHASE = "TPHASE", STYPE = "STYPE", TTYPE = "TTYPE", INTMODEL = "INTMODEL",
                    TBLIND = "TBLIND", TCNTRL = "TCNTRL", RANDOM = "NY", ADDON = "NY",
                    HLTSUBJI = "NY", TINDTP = "TINDTP", SEXPOP = "SEXPOP")
ts <- ts_src %>%
  left_join(tsparm, by = "TSPARMCD") %>%
  mutate(cl = unname(param_codelist[TSPARMCD])) %>%
  left_join(ct$terms %>% select(cl = short_name, TSVAL = value, TSVALCD = term_code), by = c("cl", "TSVAL")) %>%
  mutate(TSVCDVER = if_else(TSVCDREF == "CDISC CT", CT_VERSION, TSVCDVER))

bad_ct <- ts %>% filter(TSVCDREF == "CDISC CT", is.na(TSVALCD))
if (nrow(bad_ct)) stop("TS values not found in CT: ", paste(bad_ct$TSPARMCD, bad_ct$TSVAL, collapse = "; "))

ts <- ts %>%
  group_by(TSPARMCD) %>% mutate(TSSEQ = row_number()) %>% ungroup() %>%
  mutate(STUDYID = STUDYID, DOMAIN = "TS", TSGRPID = NA_character_) %>%
  select(STUDYID, DOMAIN, TSSEQ, TSGRPID, TSPARMCD, TSPARM, TSVAL, TSVALNF, TSVALCD, TSVCDREF, TSVCDVER)
write_domain(ts, "TS")
