# ==============================================================================
# Program     : run_all.R
# Study       : ZX-201
# Purpose     : Run the complete SDTM build in dependency order. Each program is
#               executed with logrx::axecute(), which writes a timestamped,
#               auditable log (session info, packages and functions used,
#               messages, warnings, errors) to logs/. A final log scan - the R
#               equivalent of a SAS log check - fails the run if any program
#               produced an error or an unexpected warning.
# Usage       : Rscript run_all.R            (from the project root)
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================

programs <- c(
  "R/00_simulate_raw_edc.R",     # source data (simulated)
  "R/01_ct_check.R",             # controlled terminology vs pinned CT
  "R/sdtm/dm.R",                 # DM, SUPPDM (reference dates first)
  "R/sdtm/sv_se.R",              # SV, SE
  "R/sdtm/trial_design.R",       # TA, TE, TV, TS
  "R/sdtm/ex.R",
  "R/sdtm/ae.R",
  "R/sdtm/cm.R",
  "R/sdtm/ds.R",
  "R/sdtm/vs.R",
  "R/sdtm/lb.R",
  "R/10_define_xml.R",           # define.xml / define.html
  "R/11_annotated_crf.R",        # acrf.html / acrf.pdf
  "R/30_conformance_checks.R",   # conformance report
  "R/40_build_sdrg.R"            # SDRG
)

dir.create("logs", showWarnings = FALSE)
unlink(list.files("logs", full.names = TRUE))
for (p in programs) {
  message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), p))
  logrx::axecute(p, log_name = paste0(tools::file_path_sans_ext(basename(p)), ".log"),
                 log_path = "logs", quit_on_error = TRUE)
}

# --- Log scan ----------------------------------------------------------------------
# Known, documented warnings that are allowed (each is explained in the SDRG).
allowed <- c()
scan <- do.call(rbind, lapply(list.files("logs", pattern = "\\.log$", full.names = TRUE), function(f) {
  x <- readLines(f, warn = FALSE)
  sec <- function(name) {
    i <- grep(paste0("^", name, ":"), x)
    if (!length(i)) return(character())
    out <- character()
    for (j in seq(i[1] + 1, length(x))) {
      if (grepl("^[A-Z][A-Za-z ]+:$", x[j]) || grepl("^-{10,}", x[j])) break
      if (nzchar(trimws(x[j]))) out <- c(out, trimws(x[j]))
    }
    out
  }
  data.frame(log = basename(f), errors = length(sec("Errors")), warnings = length(sec("Warnings")))
}))
print(scan, row.names = FALSE)
if (sum(scan$errors) > 0 || sum(scan$warnings) > length(allowed)) {
  stop("Log check failed: review logs/ before using the outputs")
}
message("Log check clean: ", nrow(scan), " programs, 0 errors, 0 warnings")
