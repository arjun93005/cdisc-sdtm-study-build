# ==============================================================================
# Program     : R/30_conformance_checks.R
# Study       : ZX-201
# Purpose     : Independent conformance and data-quality checks on the final
#               SDTM package, modelled on the kinds of rules run by the FDA
#               validator / CDISC CORE (structure, referential integrity,
#               ISO 8601, study day, cross-domain consistency, plausibility).
#               Rule IDs are study-internal (ZX-nnn); each cites the SDTMIG
#               section or principle it implements. Every non-PASS rule must
#               be explained in the SDRG section 4.
# Inputs      : output/sdtm/*.xpt, specs/sdtm_spec/*.csv, output/metadata/ct_check_report.csv
# Outputs     : output/metadata/conformance_report.csv  (one row per rule)
#               output/metadata/conformance_issues.csv  (one row per finding)
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

spec <- read_spec()
doms <- spec$datasets$Dataset[file.exists(file.path(SDTM_DIR, paste0(tolower(spec$datasets$Dataset), ".xpt")))]
sd   <- set_names(map(doms, read_sdtm), doms)
dm <- sd$DM; ta <- sd$TA; tv <- sd$TV

rules <- tibble(RuleID = character(), Domain = character(), Severity = character(), Description = character(), Reference = character())
issues <- tibble(RuleID = character(), Domain = character(), USUBJID = character(), Detail = character())

rule <- function(id, domain, severity, description, reference, found = NULL) {
  rules <<- add_row(rules, RuleID = id, Domain = domain, Severity = severity, Description = description, Reference = reference)
  if (!is.null(found) && nrow(found)) {
    issues <<- bind_rows(issues, found %>% transmute(RuleID = id, Domain = coalesce(.data$Domain, domain),
                                                     USUBJID = as.character(USUBJID), Detail = as.character(Detail)))
  }
}
f <- function(df, detail, domain = NA_character_) {
  if (!nrow(df)) return(tibble(Domain = character(), USUBJID = character(), Detail = character()))
  tibble(Domain = domain, USUBJID = if ("USUBJID" %in% names(df)) df$USUBJID else NA_character_, Detail = detail)
}
subject_doms <- setdiff(doms, c("TA", "TE", "TV", "TS"))
iso_re <- "^[0-9]{4}(-[0-9]{2}(-[0-9]{2}(T[0-9]{2}:[0-9]{2}(:[0-9]{2})?)?)?)?$"

# --- Structure -----------------------------------------------------------------------
rule("ZX-001", "ALL", "Error", "Required (Req) variables are never null", "SDTMIG 4.1.5",
     map_dfr(doms, function(d) {
       req <- spec$variables %>% filter(Dataset == d, Core == "Req") %>% pull(Variable)
       map_dfr(req, ~ { n <- sum(is.na(sd[[d]][[.x]])); if (n) tibble(Domain = d, USUBJID = NA_character_, Detail = sprintf("%s.%s null in %d records", d, .x, n)) })
     }))
rule("ZX-002", "AE", "Warning", "SDTMIG-expected MedDRA code variables present (AELLTCD, AEPTCD, AEHLTCD, AEHLGTCD, AEBDSYCD, AESOCCD)",
     "SDTMIG 6.2 AE",
     { miss <- setdiff(c("AELLTCD", "AEPTCD", "AEHLTCD", "AEHLGTCD", "AEBDSYCD", "AESOCCD"), names(sd$AE))
       if (length(miss)) tibble(Domain = "AE", USUBJID = NA_character_, Detail = paste("Not present:", paste(miss, collapse = ", "))) })
rule("ZX-003", "ALL", "Error", "Names <= 8 characters, labels <= 40, character values <= 200 bytes", "FDA Technical Conformance Guide 4.1.2",
     map_dfr(doms, function(d) {
       x <- sd[[d]]
       bind_rows(
         tibble(v = names(x)) %>% filter(nchar(v) > 8) %>% transmute(Domain = d, USUBJID = NA_character_, Detail = paste("Name too long:", v)),
         tibble(v = names(x), l = map_chr(x, ~ coalesce(attr(.x, "label"), ""))) %>% filter(nchar(l) > 40) %>%
           transmute(Domain = d, USUBJID = NA_character_, Detail = paste("Label too long:", v)),
         tibble(v = names(x)[map_lgl(x, is.character)]) %>% rowwise() %>%
           filter(max(c(0, nchar(x[[v]], type = "bytes")), na.rm = TRUE) > 200) %>% ungroup() %>%
           transmute(Domain = d, USUBJID = NA_character_, Detail = paste("Value > 200 bytes:", v)))
     }))
rule("ZX-004", "ALL", "Error", "--SEQ unique within USUBJID", "SDTMIG 4.1.2.8",
     map_dfr(subject_doms, function(d) {
       sq <- grep("SEQ$", names(sd[[d]]), value = TRUE)
       if (!length(sq)) return(NULL)
       sd[[d]] %>% count(USUBJID, s = .data[[sq]]) %>% filter(n > 1) %>% transmute(Domain = d, USUBJID, Detail = paste(sq, "=", s, "duplicated"))
     }))
rule("ZX-005", "ALL", "Error", "Every USUBJID exists in DM", "SDTMIG 5.2",
     map_dfr(setdiff(subject_doms, "DM"), ~ sd[[.x]] %>% distinct(USUBJID) %>% anti_join(dm, by = "USUBJID") %>%
               transmute(Domain = .x, USUBJID, Detail = "USUBJID not in DM")))
rule("ZX-006", "ALL", "Error", "No duplicate records on the define.xml key variables", "Define-XML 2.1 KeySequence",
     map_dfr(doms, function(d) {
       k <- str_split(spec$datasets$Keys[spec$datasets$Dataset == d], ",\\s*")[[1]]
       sd[[d]] %>% count(across(all_of(k))) %>% filter(n > 1) %>%
         transmute(Domain = d, USUBJID = if ("USUBJID" %in% k) USUBJID else NA_character_, Detail = paste("Duplicate key:", paste(k, collapse = ", ")))
     }))

# --- Dates and study days -------------------------------------------------------------
rule("ZX-007", "ALL", "Error", "All --DTC values are valid ISO 8601", "SDTMIG 4.4.1",
     map_dfr(subject_doms, function(d) {
       map_dfr(grep("DTC$", names(sd[[d]]), value = TRUE), function(v) {
         x <- sd[[d]]; bad <- !is.na(x[[v]]) & !grepl(iso_re, x[[v]])
         if (any(bad)) tibble(Domain = d, USUBJID = x$USUBJID[bad], Detail = paste(v, "=", x[[v]][bad]))
       })
     }))
start_end <- list(AE = c("AESTDTC", "AEENDTC"), CM = c("CMSTDTC", "CMENDTC"), EX = c("EXSTDTC", "EXENDTC"),
                  SE = c("SESTDTC", "SEENDTC"), SV = c("SVSTDTC", "SVENDTC"))
rule("ZX-008", "ALL", "Error", "Start date/time on or before end date/time", "SDTMIG 4.4",
     map_dfr(names(start_end), function(d) {
       s <- start_end[[d]]; x <- sd[[d]]
       bad <- is_complete_date(x[[s[1]]]) & is_complete_date(x[[s[2]]]) &
         substr(x[[s[1]]], 1, 10) > substr(x[[s[2]]], 1, 10)
       if (any(bad)) tibble(Domain = d, USUBJID = x$USUBJID[bad], Detail = sprintf("%s=%s > %s=%s", s[1], x[[s[1]]][bad], s[2], x[[s[2]]][bad]))
     }))
rule("ZX-009", "ALL", "Error", "--DY consistent with --DTC and DM.RFSTDTC (and never 0)", "SDTMIG 4.4.4",
     map_dfr(setdiff(subject_doms, c("DM", "SUPPDM")), function(d) {
       x <- sd[[d]] %>% left_join(dm %>% select(USUBJID, RFSTDTC_ = RFSTDTC), by = "USUBJID")
       map_dfr(grep("DY$", names(sd[[d]]), value = TRUE), function(dy) {
         dtc <- sub("DY$", "DTC", dy); if (!dtc %in% names(x)) return(NULL)
         exp <- study_day(x[[dtc]], x$RFSTDTC_)
         bad <- !(is.na(exp) & is.na(x[[dy]])) & (is.na(exp) | is.na(x[[dy]]) | exp != x[[dy]] | x[[dy]] == 0)
         if (any(bad)) tibble(Domain = d, USUBJID = x$USUBJID[bad], Detail = paste(dy, "inconsistent"))
       })
     }))
rule("ZX-010", "AE", "Warning", "AE start date not before informed consent", "Protocol: AEs collected from consent",
     sd$AE %>% left_join(dm %>% select(USUBJID, RFICDTC), by = "USUBJID") %>%
       filter(is_complete_date(AESTDTC), substr(AESTDTC, 1, 10) < RFICDTC) %>% f("AESTDTC before RFICDTC", "AE"))

# --- Visits / epochs / arms -------------------------------------------------------------
vis_doms <- intersect(c("SV", "VS", "LB"), subject_doms)
rule("ZX-011", "ALL", "Error", "VISITNUM-VISIT one-to-one across domains; planned visits match TV", "SDTMIG 4.4.5",
     { v <- map_dfr(vis_doms, ~ sd[[.x]] %>% distinct(VISITNUM, VISIT) %>% mutate(Domain = .x))
       bind_rows(
         v %>% distinct(VISITNUM, VISIT) %>% count(VISITNUM) %>% filter(n > 1) %>% transmute(Domain = "ALL", USUBJID = NA_character_, Detail = paste("VISITNUM", VISITNUM, "has >1 VISIT")),
         v %>% filter(VISITNUM %% 1 == 0) %>% anti_join(tv, by = c("VISITNUM", "VISIT")) %>% transmute(Domain, USUBJID = NA_character_, Detail = paste("Planned visit not in TV:", VISIT))) })
rule("ZX-012", "ALL", "Warning", "EPOCH populated (from TA) whenever the observation date is complete", "SDTMIG 4.1.4.9",
     map_dfr(intersect(c("AE", "DS", "EX", "LB", "VS", "SV"), subject_doms), function(d) {
       x <- sd[[d]]; dtc <- intersect(c(paste0(d, "STDTC"), paste0(d, "DTC"), "SVSTDTC"), names(x))[1]
       bad <- (is_complete_date(x[[dtc]]) & is.na(x$EPOCH)) | (!is.na(x$EPOCH) & !x$EPOCH %in% ta$EPOCH)
       nopart <- !is_complete_date(x[[dtc]]) & is.na(x$EPOCH)
       bind_rows(if (any(bad)) tibble(Domain = d, USUBJID = x$USUBJID[bad], Detail = "EPOCH missing or not in TA"),
                 if (any(nopart)) tibble(Domain = d, USUBJID = x$USUBJID[nopart], Detail = paste("EPOCH null:", dtc, "is a partial date")))
     }))
rule("ZX-013", "DM", "Error", "ARMCD/ARM and ACTARMCD/ACTARM pairs exist in TA", "SDTMIG 5.2 DM",
     bind_rows(dm %>% filter(!is.na(ARMCD)) %>% anti_join(distinct(ta, ARMCD, ARM), by = c("ARMCD", "ARM")) %>% f("ARMCD/ARM not in TA"),
               dm %>% filter(!is.na(ACTARMCD)) %>% anti_join(distinct(ta, ACTARMCD = ARMCD, ACTARM = ARM), by = c("ACTARMCD", "ACTARM")) %>% f("ACTARMCD/ACTARM not in TA")))
rule("ZX-014", "DM", "Error", "ARMNRS populated if and only if ARMCD or ACTARMCD is null", "SDTMIG 5.2 DM assumption",
     dm %>% filter(xor(!is.na(ARMNRS), is.na(ARMCD) | is.na(ACTARMCD))) %>% f("ARMNRS inconsistent with ARMCD/ACTARMCD"))
rule("ZX-015", "DM", "Error", "Subjects with RFXSTDTC have EX records and RFXSTDTC = earliest EXSTDTC", "SDTMIG 5.2 DM",
     dm %>% filter(!is.na(RFXSTDTC)) %>%
       left_join(sd$EX %>% group_by(USUBJID) %>% summarise(first = min(EXSTDTC)), by = "USUBJID") %>%
       filter(is.na(first) | first != RFXSTDTC) %>% f("RFXSTDTC does not match EX"))

# --- Cross-domain safety consistency ----------------------------------------------------
dth_ae <- sd$AE %>% filter(AESDTH == "Y") %>% distinct(USUBJID)
rule("ZX-016", "DM", "Error", "DTHFL = 'Y' iff DTHDTC populated; consistent with AESDTH and DS DEATH", "SDTMIG 5.2 / 6.2",
     bind_rows(dm %>% filter(xor(coalesce(DTHFL == "Y", FALSE), !is.na(DTHDTC))) %>% f("DTHFL vs DTHDTC"),
               dth_ae %>% anti_join(dm %>% filter(DTHFL == "Y"), by = "USUBJID") %>% f("AESDTH = Y but DTHFL not Y"),
               sd$DS %>% filter(DSDECOD == "DEATH") %>% anti_join(dm %>% filter(DTHFL == "Y"), by = "USUBJID") %>% f("DS DEATH but DTHFL not Y")))
rule("ZX-017", "AE", "Error", "AEOUT = 'FATAL' iff AESDTH = 'Y'", "SDTMIG 6.2 AE",
     sd$AE %>% filter(xor(coalesce(AEOUT == "FATAL", FALSE), coalesce(AESDTH == "Y", FALSE))) %>% f("AEOUT vs AESDTH", "AE"))
rule("ZX-018", "AE", "Error", "Serious AEs have at least one seriousness criterion = 'Y'", "SDTMIG 6.2 AE assumption 5",
     sd$AE %>% filter(AESER == "Y", !(coalesce(AESDTH, "N") == "Y" | coalesce(AESHOSP, "N") == "Y")) %>% f("AESER = Y without criterion", "AE"))
rule("ZX-019", "DS", "Error", "Exactly one DISPOSITION EVENT record per subject", "SDTMIG 6.2 DS",
     dm %>% select(USUBJID) %>% left_join(sd$DS %>% filter(DSCAT == "DISPOSITION EVENT") %>% count(USUBJID), by = "USUBJID") %>%
       filter(coalesce(n, 0L) != 1) %>% f("Number of disposition events != 1", "DS"))
rule("ZX-020", "SUPPDM", "Error", "RACE = 'MULTIPLE' iff SUPPDM RACE1/RACE2 present", "SDTMIG 5.2 DM assumption 7",
     { sp <- sd$SUPPDM %>% filter(QNAM %in% c("RACE1", "RACE2")) %>% distinct(USUBJID)
       bind_rows(dm %>% filter(RACE == "MULTIPLE") %>% anti_join(sp, by = "USUBJID") %>% f("MULTIPLE without SUPPDM"),
                 sp %>% anti_join(dm %>% filter(RACE == "MULTIPLE"), by = "USUBJID") %>% f("SUPPDM race without MULTIPLE")) })

# --- Findings --------------------------------------------------------------------------
for (d in intersect(c("VS", "LB"), subject_doms)) {
  x <- sd[[d]]; p <- d
  rule(sprintf("ZX-02%d", if (d == "VS") 1 else 2), d, "Error", sprintf("At most one %sLOBXFL = 'Y' per subject, test (and position)", p), "SDTMIG 3.4 --LOBXFL",
       x %>% filter(.data[[paste0(p, "LOBXFL")]] == "Y") %>% count(USUBJID, t = .data[[paste0(p, "TESTCD")]]) %>% filter(n > 1) %>% f("Multiple LOBXFL", d))
}
rule("ZX-023", "VS/LB", "Error", "One standard unit per test; --ORRES populated xor --STAT = 'NOT DONE'", "SDTMIG 4.5.1",
     bind_rows(
       map_dfr(intersect(c("VS", "LB"), subject_doms), function(d) {
         x <- sd[[d]]; p <- d
         bind_rows(
           x %>% filter(!is.na(.data[[paste0(p, "STRESU")]])) %>% distinct(t = .data[[paste0(p, "TESTCD")]], u = .data[[paste0(p, "STRESU")]]) %>%
             count(t) %>% filter(n > 1) %>% transmute(Domain = d, USUBJID = NA_character_, Detail = paste(t, "has >1 standard unit")),
           x %>% filter(!xor(!is.na(.data[[paste0(p, "ORRES")]]), coalesce(.data[[paste0(p, "STAT")]] == "NOT DONE", FALSE))) %>%
             transmute(Domain = d, USUBJID, Detail = "ORRES/STAT inconsistent"))
       })))
rule("ZX-024", "LB", "Error", "LBNRIND consistent with LBSTRESN and the standard reference range", "SDTMIG 6.3 LB",
     sd$LB %>% filter(!is.na(LBSTRESN)) %>%
       mutate(exp = case_when(LBSTRESN < LBSTNRLO ~ "LOW", LBSTRESN > LBSTNRHI ~ "HIGH", TRUE ~ "NORMAL")) %>%
       filter(exp != LBNRIND) %>% f("LBNRIND inconsistent", "LB"))
rule("ZX-025", "VS", "Warning", "Plausibility: weight change > 20% from baseline (VSLOBXFL) record", "Data review (sponsor)",
     sd$VS %>% filter(VSTESTCD == "WEIGHT") %>%
       left_join(sd$VS %>% filter(VSTESTCD == "WEIGHT", VSLOBXFL == "Y") %>% select(USUBJID, base = VSSTRESN), by = "USUBJID") %>%
       filter(abs(VSSTRESN - base) / base > .2) %>%
       mutate(Detail = sprintf("%s WEIGHT %s kg vs baseline %s kg", VISIT, VSORRES, base)) %>%
       transmute(Domain = "VS", USUBJID, Detail))
rule("ZX-026", "VS/LB", "Error", "Every findings visit date has a matching SV record", "SDTMIG 5.3 SV",
     map_dfr(intersect(c("VS", "LB"), subject_doms), function(d) {
       x <- sd[[d]] %>% transmute(USUBJID, VISITNUM, dt = substr(.data[[paste0(d, "DTC")]], 1, 10)) %>% distinct()
       x %>% anti_join(sd$SV, by = c("USUBJID", "VISITNUM", "dt" = "SVSTDTC")) %>% transmute(Domain = d, USUBJID, Detail = paste("No SV for VISITNUM", VISITNUM, "on", dt))
     }))
rule("ZX-027", "SE", "Error", "Subject elements are contiguous (SEENDTC = next SESTDTC) and in TA order", "SDTMIG 5.3 SE",
     sd$SE %>% arrange(USUBJID, TAETORD) %>% group_by(USUBJID) %>%
       mutate(nxt = lead(SESTDTC)) %>% ungroup() %>% filter(!is.na(nxt), SEENDTC != nxt) %>% f("Gap/overlap between elements", "SE"))
ts_req <- c("TITLE", "TPHASE", "STYPE", "SSTDTC", "SDTMVER", "SDTIGVER", "PLANSUB", "RANDOM", "TBLIND", "TCNTRL", "AGEMIN", "SEXPOP")
rule("ZX-028", "TS", "Error", "TS contains the parameters required for submission", "FDA Study Data TCG 4.1.1.3",
     tibble(p = setdiff(ts_req, sd$TS$TSPARMCD)) %>% transmute(Domain = "TS", USUBJID = NA_character_, Detail = paste("Missing", p)))

# --- Controlled terminology (from R/01_ct_check.R) ------------------------------------------
ctr <- read_csv("output/metadata/ct_check_report.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
rule("ZX-029", "ALL", "Warning", "Codelists and terms exist in the pinned CDISC CT release", "SDTMIG 4.3",
     ctr %>% filter(Severity != "NOTE") %>% transmute(Domain = "ALL", USUBJID = NA_character_, Detail = paste(CodelistID, Message)))

# --- Report ---------------------------------------------------------------------------
summary <- rules %>% left_join(issues %>% count(RuleID, name = "Findings"), by = "RuleID") %>%
  left_join(issues %>% group_by(RuleID) %>% summarise(Example = paste(head(paste(coalesce(USUBJID, ""), Detail), 2), collapse = " | ")), by = "RuleID") %>%
  mutate(Findings = coalesce(Findings, 0L), Status = if_else(Findings == 0, "PASS", if_else(Severity == "Error", "FAIL", "WARN")))
dir.create("output/metadata", showWarnings = FALSE)
write_csv(summary, "output/metadata/conformance_report.csv", na = "")
write_csv(issues, "output/metadata/conformance_issues.csv", na = "")
message(sprintf("Conformance: %d rules - %d PASS, %d WARN, %d FAIL (%d findings)", nrow(summary),
                sum(summary$Status == "PASS"), sum(summary$Status == "WARN"), sum(summary$Status == "FAIL"), nrow(issues)))
print(as.data.frame(summary %>% filter(Status != "PASS") %>% select(RuleID, Status, Findings, Example)), right = FALSE)
