# ==============================================================================
# Program     : R/utils/sdtm_utils.R
# Study       : ZX-201
# Purpose     : Shared, unit-tested helper functions used by every SDTM program
#               (ISO 8601 conversion, study day, sequence numbers, EPOCH,
#               last-observation-before-exposure flag, CT mapping, spec-driven
#               attribute assignment and XPT export).
# Inputs      : none (sourced by other programs)
# Outputs     : none
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
})

STUDYID <- "ZX-201"
SPEC_DIR <- file.path("specs", "sdtm_spec")
SDTM_DIR <- file.path("output", "sdtm")
RAW_DIR  <- file.path("data", "raw")

# ------------------------------------------------------------------------------
# Dates
# ------------------------------------------------------------------------------
.MONTHS <- c(JAN = "01", FEB = "02", MAR = "03", APR = "04", MAY = "05", JUN = "06",
             JUL = "07", AUG = "08", SEP = "09", OCT = "10", NOV = "11", DEC = "12")

#' Convert an EDC date string ("DD-MON-YYYY", with UN/UNK for unknown parts)
#' and optional "HH:MM" time into an ISO 8601 --DTC value.
#'
#' Partial dates are truncated to the most precise known component, as
#' required by SDTMIG section 4.4 ("UN-MAR-2025" -> "2025-03",
#' "UN-UNK-2025" -> "2025"). A time is only appended to a complete date.
#' Unparseable input returns NA (and is caught by the conformance checks).
raw_to_iso <- function(date, time = NULL) {
  date <- toupper(str_trim(as.character(date)))
  out <- rep(NA_character_, length(date))
  ok <- !is.na(date) & date != ""
  out[ok] <- vapply(strsplit(date[ok], "-", fixed = TRUE), function(p) {
    if (length(p) != 3 || !grepl("^[0-9]{4}$", p[3])) return(NA_character_)
    yy <- p[3]
    if (p[2] %in% c("UN", "UNK")) return(yy)
    mm <- unname(.MONTHS[p[2]])
    if (is.na(mm)) return(NA_character_)
    if (p[1] %in% c("UN", "UNK")) return(paste(yy, mm, sep = "-"))
    if (!grepl("^[0-9]{1,2}$", p[1])) return(NA_character_)
    iso <- sprintf("%s-%s-%02d", yy, mm, as.integer(p[1]))
    if (is.na(as.Date(iso, format = "%Y-%m-%d"))) NA_character_ else iso
  }, character(1))

  if (!is.null(time)) {
    time <- str_trim(as.character(time))
    has_t <- !is.na(out) & nchar(out) == 10 & !is.na(time) &
      grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", time)
    hh <- sprintf("%02d", as.integer(sub(":.*", "", time[has_t])))
    out[has_t] <- paste0(out[has_t], "T", hh, ":", sub(".*:", "", time[has_t]))
  }
  out
}

#' Combine an ISO date (already YYYY-MM-DD) with an HH:MM time.
iso_datetime <- function(date, time) {
  ifelse(!is.na(date) & !is.na(time) & time != "", paste0(date, "T", time), date)
}

#' TRUE when the --DTC value contains a complete date (YYYY-MM-DD).
is_complete_date <- function(dtc) !is.na(dtc) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", dtc)

#' Date part of an ISO 8601 value as Date (NA when partial).
dtc_date <- function(dtc) {
  out <- rep(as.Date(NA), length(dtc))
  ok <- is_complete_date(dtc)
  out[ok] <- as.Date(substr(dtc[ok], 1, 10))
  out
}

#' SDTM study day (SDTMIG 4.4.4): no day 0; day 1 is the reference date.
#' Only derived when both the event date and the reference date are complete.
study_day <- function(dtc, refdtc) {
  d <- dtc_date(dtc)
  r <- dtc_date(refdtc)
  diff <- as.integer(d - r)
  ifelse(is.na(diff), NA_integer_, ifelse(diff >= 0, diff + 1L, diff))
}

# ------------------------------------------------------------------------------
# Record-level derivations
# ------------------------------------------------------------------------------

#' --SEQ: unique, sequential within USUBJID after sorting by the dataset keys.
derive_seq <- function(df, seqvar, sort_keys) {
  df %>%
    arrange(USUBJID, across(all_of(sort_keys))) %>%
    group_by(USUBJID) %>%
    mutate(!!seqvar := as.numeric(row_number())) %>%
    ungroup()
}

#' EPOCH, consistent with the SE (subject elements) boundaries:
#'   SCREENING : informed consent  -> first dose (exclusive)
#'   TREATMENT : first dose        -> day after last dose (exclusive)
#'   FOLLOW-UP : day after last dose onwards
#' When the observation and first dose both carry a time, datetimes are
#' compared. A date-only observation on the first-dose date is TREATMENT
#' unless `predose` is TRUE (protocol-mandated pre-dose assessments).
#' Subjects never dosed (screen failures, randomised-not-treated) are
#' SCREENING throughout. Partial dates get no EPOCH.
derive_epoch <- function(usubjid, dtc, dm, predose = FALSE) {
  predose <- rep_len(predose, length(dtc))
  ref <- dm[match(usubjid, dm$USUBJID), c("RFXSTDTC", "RFXENDTC")]
  fd <- ref$RFXSTDTC; ld <- dtc_date(ref$RFXENDTC)
  od <- dtc_date(dtc); fdd <- dtc_date(fd)
  both_time <- nchar(dtc) >= 16 & !is.na(fd) & nchar(fd) >= 16
  on_or_after_first <- ifelse(both_time, dtc >= fd,
                              od > fdd | (od == fdd & !predose))
  case_when(
    is.na(od)                    ~ NA_character_,
    is.na(fdd)                   ~ "SCREENING",
    !on_or_after_first           ~ "SCREENING",
    !is.na(ld) & od > ld         ~ "FOLLOW-UP",
    TRUE                         ~ "TREATMENT"
  )
}

#' --LOBXFL (SDTMIG 3.4): "Y" on the last non-missing result on or before
#' first exposure (RFXSTDTC), within subject and test (plus `by`).
derive_lobxfl <- function(df, prefix, dm, by = character()) {
  dtc <- paste0(prefix, "DTC"); res <- paste0(prefix, "ORRES"); tst <- paste0(prefix, "TESTCD")
  flag <- paste0(prefix, "LOBXFL")
  ord <- intersect(c(dtc, paste0(prefix, "REPNUM")), names(df))
  key <- df %>%
    mutate(.row = row_number()) %>%
    left_join(dm %>% select(USUBJID, RFXSTDTC), by = "USUBJID") %>%
    filter(!is.na(.data[[res]]), .data[[res]] != "", !is.na(RFXSTDTC),
           is_complete_date(.data[[dtc]]),
           (nchar(.data[[dtc]]) >= 16 & .data[[dtc]] <= RFXSTDTC) |
             (nchar(.data[[dtc]]) < 16 & substr(.data[[dtc]], 1, 10) <= substr(RFXSTDTC, 1, 10))) %>%
    arrange(USUBJID, across(all_of(c(tst, by))), across(all_of(ord))) %>%
    group_by(USUBJID, across(all_of(c(tst, by)))) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    pull(.row)
  df[[flag]] <- NA_character_
  df[[flag]][key] <- "Y"
  df
}

#' Attach VISITNUM / VISIT / VISITDY to findings records.
#' Planned visits: EDC folder -> TV. Unscheduled: matched to the SV
#' unscheduled visit on the same date (error if no SV visit exists).
#' Requires columns USUBJID, FOLDER and the --DTC variable `dtc_var`.
map_visits <- function(df, dtc_var, sv, tv) {
  d <- substr(df[[dtc_var]], 1, 10)
  planned_num <- tv$VISITNUM[match(df$FOLDER, tv$FOLDER)]
  uns <- sv %>% filter(VISITNUM %% 1 != 0)
  uns_num <- uns$VISITNUM[match(paste(df$USUBJID, d), paste(uns$USUBJID, uns$SVSTDTC))]
  df$VISITNUM <- if_else(df$FOLDER == "Unscheduled", uns_num, planned_num)
  miss <- df %>% filter(is.na(VISITNUM))
  if (nrow(miss)) stop("Records without a matching visit: ", paste(unique(miss$USUBJID), collapse = ", "))
  df %>%
    left_join(sv %>% distinct(USUBJID, VISITNUM, VISIT), by = c("USUBJID", "VISITNUM")) %>%
    left_join(tv %>% select(VISITNUM, VISITDY), by = "VISITNUM")
}

# ------------------------------------------------------------------------------
# Controlled terminology
# ------------------------------------------------------------------------------

CT_FILE <- file.path("specs", "ct", "SDTM_Terminology.txt")

#' Load the pinned NCI EVS CDISC SDTM CT file.
#' Returns list(codelists = one row per codelist, terms = one row per term).
load_ct <- function(path = CT_FILE) {
  ct <- read_tsv(path, col_types = cols(.default = "c"), quote = "", show_col_types = FALSE)
  names(ct) <- c("code", "codelist_code", "extensible", "codelist_name", "value",
                 "synonyms", "definition", "nci_pt")
  list(
    codelists = ct %>% filter(is.na(codelist_code)) %>%
      transmute(codelist_code = code, short_name = value, codelist_name, extensible),
    terms = ct %>% filter(!is.na(codelist_code)) %>%
      left_join(ct %>% filter(is.na(codelist_code)) %>% select(codelist_code = code, short_name = value),
                by = "codelist_code") %>%
      select(short_name, codelist_code, term_code = code, value, synonyms, nci_pt)
  )
}

#' Map raw collected values to submission values using a named lookup.
#' Stops (fails the program) on any unmapped non-missing value so that
#' new CRF responses can never silently fall through to SDTM.
ct_map <- function(x, lookup, what = deparse(substitute(x))) {
  key <- toupper(str_trim(as.character(x)))
  names(lookup) <- toupper(names(lookup))
  out <- unname(lookup[key])
  bad <- unique(x[!is.na(x) & x != "" & is.na(out)])
  if (length(bad)) stop(sprintf("Unmapped value(s) for %s: %s", what, paste(bad, collapse = ", ")))
  out[is.na(x) | x == ""] <- NA_character_
  out
}

# ------------------------------------------------------------------------------
# Specification-driven finishing
# ------------------------------------------------------------------------------

read_spec <- function() {
  list(
    datasets  = read_csv(file.path(SPEC_DIR, "datasets.csv"), show_col_types = FALSE),
    variables = read_csv(file.path(SPEC_DIR, "variables.csv"), show_col_types = FALSE,
                         col_types = cols(.default = "c", Order = "i", Length = "i")),
    codelists = read_csv(file.path(SPEC_DIR, "codelists.csv"), show_col_types = FALSE,
                         col_types = cols(.default = "c")),
    value_level = read_csv(file.path(SPEC_DIR, "value_level.csv"), show_col_types = FALSE,
                           col_types = cols(.default = "c"))
  )
}

check_codelist <- function(values, codelist_id, spec, what) {
  terms <- spec$codelists %>% filter(CodelistID == codelist_id)
  if (!nrow(terms) || all(terms$Term == "*")) return(invisible())   # external dictionaries
  bad <- setdiff(unique(na.omit(values)), terms$Term)
  if (length(bad)) {
    msg <- sprintf("%s values not in codelist %s: %s", what, codelist_id, paste(bad, collapse = ", "))
    if (isTRUE(terms$Extensible[1] == "No")) stop(msg) else warning(msg, call. = FALSE)
  }
}

#' Apply the metadata specification to a domain and write the SAS V5
#' transport file (output/sdtm/<domain>.xpt):
#'   * every spec variable must exist (Req/Exp missing -> error)
#'   * no undocumented variables may exist
#'   * variable order, labels, types and lengths come from the spec
#'   * text lengths are set to the maximum observed length (FDA SDTCG)
#'   * records sorted by the spec key variables
#'   * codelist-controlled variables are checked against the spec codelist
write_domain <- function(df, domain, spec = read_spec()) {
  vars <- spec$variables %>% filter(Dataset == domain) %>% arrange(Order)
  dsmeta <- spec$datasets %>% filter(Dataset == domain)
  if (!nrow(vars)) stop("No variable metadata for ", domain)

  missing <- setdiff(vars$Variable, names(df))
  if (length(missing)) stop(domain, ": variables in spec but not in data: ", paste(missing, collapse = ", "))
  extra <- setdiff(names(df), vars$Variable)
  if (length(extra)) stop(domain, ": undocumented variables in data: ", paste(extra, collapse = ", "))

  df <- df[, vars$Variable]

  # types
  for (i in seq_len(nrow(vars))) {
    v <- vars$Variable[i]
    if (vars$DataType[i] %in% c("integer", "float")) {
      df[[v]] <- as.numeric(df[[v]])
    } else {
      df[[v]] <- as.character(df[[v]])
      df[[v]][!is.na(df[[v]]) & df[[v]] == ""] <- NA_character_
    }
  }

  # codelist conformance (non-extensible -> error, extensible -> warning)
  cl <- vars %>% filter(!is.na(Codelist), Codelist != "")
  for (i in seq_len(nrow(cl))) {
    check_codelist(df[[cl$Variable[i]]], cl$Codelist[i], spec, paste0(domain, ".", cl$Variable[i]))
  }
  vlm <- spec$value_level %>% filter(Dataset == domain, !is.na(Codelist))
  for (i in seq_len(nrow(vlm))) {
    sel <- df[[vlm$WhereVariable[i]]] %in% vlm$WhereValue[i]
    check_codelist(df[[vlm$Variable[i]]][sel], vlm$Codelist[i], spec,
                   sprintf("%s.%s [%s = %s]", domain, vlm$Variable[i], vlm$WhereVariable[i], vlm$WhereValue[i]))
  }

  # sort by keys
  keys <- str_split(dsmeta$Keys, ",\\s*")[[1]]
  df <- df %>% arrange(across(all_of(intersect(keys, names(df)))))

  # labels / lengths (attributes read by haven::write_xpt)
  for (i in seq_len(nrow(vars))) {
    v <- vars$Variable[i]
    attr(df[[v]], "label") <- vars$Label[i]
    if (is.character(df[[v]])) {
      len <- max(c(1L, nchar(df[[v]], type = "bytes")), na.rm = TRUE)
      if (len > 200) stop(domain, ".", v, " exceeds 200 characters")
      attr(df[[v]], "width") <- len
    }
  }

  dir.create(SDTM_DIR, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(SDTM_DIR, paste0(tolower(domain), ".xpt"))
  haven::write_xpt(df, path, version = 5, name = toupper(domain), label = dsmeta$Label)
  message(sprintf("%-7s %5d records, %2d variables -> %s", domain, nrow(df), ncol(df), path))
  invisible(df)
}

read_sdtm <- function(domain) {
  haven::read_xpt(file.path(SDTM_DIR, paste0(tolower(domain), ".xpt"))) %>%
    mutate(across(where(is.character), ~ na_if(.x, "")))
}

read_raw <- function(name) {
  read_csv(file.path(RAW_DIR, paste0(name, ".csv")), col_types = cols(.default = "c"),
           na = c("", "NA"), show_col_types = FALSE)
}
