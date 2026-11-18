# ==============================================================================
# Program     : R/11_annotated_crf.R
# Study       : ZX-201
# Purpose     : Render the annotated CRF (aCRF) from the eCRF specification:
#               one form per page, each field shown as it appears to sites,
#               with SDTM annotations in boxes following the CDISC
#               Metadata Submission Guidelines conventions (domain banner per
#               form, variable annotations beside fields, [NOT SUBMITTED] for
#               collected-but-not-tabulated fields). Printed to PDF so that
#               define.xml PDFPageRef page numbers are physical pages.
# Inputs      : specs/ecrf_spec.csv
# Outputs     : output/sdtm/acrf.html, output/sdtm/acrf.pdf
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")

ecrf <- read_csv("specs/ecrf_spec.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  mutate(FormOrder = as.integer(FormOrder), FieldOrder = as.integer(FieldOrder)) %>%
  arrange(FormOrder, FieldOrder)
esc <- function(s) { s <- coalesce(s, ""); s <- gsub("&", "&amp;", s); s <- gsub("<", "&lt;", s); gsub(">", "&gt;", s) }

domain_names <- c(DM = "Demographics", DS = "Disposition", SV = "Subject Visits", VS = "Vital Signs",
                  EX = "Exposure", AE = "Adverse Events", CM = "Concomitant/Prior Medications")
domain_col <- c(DM = "#bfe3ff", DS = "#ffe9b3", SV = "#d9f2d0", VS = "#ffd6e7", EX = "#e3d9ff",
                AE = "#ffd9c2", CM = "#d0f0f0")

widget <- function(type, responses) {
  opts <- if (is.na(responses)) character() else str_split(responses, "\\s*\\|\\s*")[[1]]
  radios <- paste0("&#9675; ", esc(opts), collapse = "&nbsp;&nbsp; ")
  if (str_detect(type, "^Date")) "<span class='box'>DD</span>-<span class='box w3'>MON</span>-<span class='box w4'>YYYY</span>"
  else if (str_detect(type, "^Time")) "<span class='box'>HH</span>:<span class='box'>MM</span>"
  else if (str_detect(type, "^Decimal \\+ Radio")) paste0("<span class='box w4'></span> &nbsp;", radios)
  else if (str_detect(type, "^Radio")) radios
  else if (str_detect(type, "^Checkbox")) paste0("&#9744; ", esc(opts), collapse = "<br>")
  else if (str_detect(type, "^Dropdown")) paste0("<span class='box w8'>&#9662; select</span><div class='opts'>",
                                                 esc(paste(opts, collapse = " / ")), "</div>")
  else if (str_detect(type, "^Pre-filled")) paste0("<i>", esc(paste(opts, collapse = " / ")), "</i>")
  else if (str_detect(type, "x3")) "<span class='box w4'></span> <span class='box w4'></span> <span class='box w4'></span>"
  else "<span class='box w8'></span>"
}

annotation <- function(a, dom) {
  ns <- str_detect(a, "^\\[NOT SUBMITTED\\]")
  sprintf("<div class='ann %s' style='background:%s'>%s</div>",
          if (ns) "ns" else "", if (ns) "#eeeeee" else domain_col[[dom]], esc(a))
}

forms <- ecrf %>% distinct(FormOrder, FormOID, FormName, Domain)
toc <- paste0("<tr><td>", forms$FormName, "</td><td>", forms$Domain, " (", domain_names[forms$Domain], ")</td><td>",
              forms$FormOrder + 1L, "</td></tr>", collapse = "")

pages <- map_chr(seq_len(nrow(forms)), function(i) {
  f <- forms[i, ]
  fl <- ecrf %>% filter(FormOID == f$FormOID)
  doms <- unique(str_extract_all(paste(fl$SDTMAnnotation, collapse = " "), "\\b(DM|DS|SV|VS|EX|AE|CM|SUPPDM)\\.")[[1]])
  doms <- unique(c(f$Domain, sub("\\.$", "", doms)))
  doms <- doms[doms %in% names(domain_names)]
  banner <- paste0(sprintf("<span class='dom' style='background:%s'>%s (%s)</span>", domain_col[doms], doms, domain_names[doms]),
                   collapse = " ")
  rows <- paste0(map_chr(seq_len(nrow(fl)), function(j) {
    r <- fl[j, ]
    sprintf("<tr><td class='q'><b>%s</b><div class='oid'>%s &middot; CDASH %s%s</div></td><td class='w'>%s</td><td class='a'>%s</td></tr>",
            esc(r$Question), esc(r$FieldOID), esc(r$CDASHVariable),
            if (!is.na(r$EditCheck)) paste0(" &middot; check: ", esc(r$EditCheck)) else "",
            widget(r$ControlType, r$Responses), annotation(r$SDTMAnnotation, f$Domain))
  }), collapse = "")
  sprintf("<section class='page'><div class='hdr'><span>ZX-201 &middot; Annotated CRF</span><span>Page %d</span></div><h2>%s</h2><div class='banner'>%s</div><table class='crf'><colgroup><col style='width:34%%'><col style='width:30%%'><col style='width:36%%'></colgroup><tr><th>Question</th><th>Response</th><th>SDTM annotation</th></tr>%s</table><div class='ftr'>Form %s &middot; version 1.0 &middot; SIMULATED STUDY</div></section>",
          f$FormOrder + 1L, esc(f$FormName), banner, rows, f$FormOID)
})

html <- c(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'><title>ZX-201 Annotated CRF</title><style>",
  "@page{size:A4;margin:12mm}body{font-family:Arial,Helvetica,sans-serif;font-size:11px;color:#111;margin:0}",
  ".page{page-break-after:always;padding:0 4mm}.page:last-child{page-break-after:auto}",
  ".hdr{display:flex;justify-content:space-between;color:#666;border-bottom:1px solid #999;padding-bottom:3px;margin-bottom:8px}",
  ".ftr{color:#888;margin-top:8px;font-size:9px}h1{font-size:20px}h2{font-size:15px;margin:6px 0}",
  ".banner{margin:4px 0 8px}.dom{display:inline-block;border:1px solid #333;padding:3px 8px;font-weight:bold;margin-right:6px}",
  "table.crf{border-collapse:collapse;width:100%;table-layout:fixed}table.crf th{background:#333;color:#fff;text-align:left;padding:4px}",
  "table.crf td{border:1px solid #bbb;padding:5px;vertical-align:top}.oid{color:#777;font-size:9px;margin-top:2px}",
  ".box{display:inline-block;border:1px solid #444;min-width:18px;height:14px;padding:0 2px;color:#aaa;font-size:9px;text-align:center}",
  ".w3{min-width:28px}.w4{min-width:36px}.w8{min-width:110px;text-align:left}.opts{color:#666;font-size:9px;margin-top:2px}",
  ".ann{border:1px solid #333;padding:3px 5px;font-family:Consolas,monospace;font-size:10px}.ann.ns{color:#555;font-style:italic}",
  "table.toc{border-collapse:collapse;width:70%}table.toc td,table.toc th{border:1px solid #bbb;padding:4px 8px;text-align:left}",
  "</style></head><body>",
  "<section class='page'><div class='hdr'><span>ZX-201 &middot; Annotated CRF</span><span>Page 1</span></div>",
  "<h1>ZX-201 Annotated Case Report Form</h1>",
  "<p>A Phase II, Randomized, Double-Blind, Placebo-Controlled, Parallel-Group Study of ZX-101 in Adults with Stage 1-2 Essential Hypertension.<br><b>Simulated study for portfolio purposes.</b></p>",
  "<p>Annotations follow the CDISC Metadata Submission Guidelines: a domain banner at the top of each form, the SDTM variable(s) each field populates beside the field, and <i>[NOT SUBMITTED]</i> for fields collected but not tabulated. Central laboratory data (LB) are received electronically and do not appear on the CRF; randomisation (DM.ARMCD/ARM) comes from IxRS.</p>",
  "<table class='toc'><tr><th>Form</th><th>Domain</th><th>Page</th></tr>", toc, "</table></section>",
  pages, "</body></html>")
dir.create(SDTM_DIR, recursive = TRUE, showWarnings = FALSE)
html_path <- normalizePath(file.path(SDTM_DIR, "acrf.html"), mustWork = FALSE)
writeLines(html, html_path, useBytes = TRUE)

# --- print to PDF with a headless Chromium browser (Edge or Chrome) ------------------
browsers <- c("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
              "C:/Program Files/Google/Chrome/Application/chrome.exe",
              Sys.which(c("chromium", "google-chrome", "microsoft-edge")))
browser <- browsers[nzchar(browsers) & file.exists(browsers)][1]
pdf_path <- normalizePath(file.path(SDTM_DIR, "acrf.pdf"), mustWork = FALSE)
if (!is.na(browser)) {
  url <- paste0("file:///", gsub("\\\\", "/", html_path))
  system2(browser, c("--headless", "--disable-gpu", "--no-pdf-header-footer",
                     paste0("--print-to-pdf=", shQuote(pdf_path, type = "cmd")), shQuote(url, type = "cmd")),
          stdout = FALSE, stderr = FALSE)
  n_pages <- length(grepRaw("/Type\\s*/Page[^s]", readBin(pdf_path, "raw", file.size(pdf_path)), all = TRUE))
  expected <- nrow(forms) + 1L
  message(sprintf("acrf.pdf written: %d pages (expected %d)", n_pages, expected))
  if (n_pages != expected) stop("aCRF page count differs from the page numbers used in define.xml")
} else {
  warning("No headless Chromium browser found: acrf.html written, acrf.pdf not produced")
}
