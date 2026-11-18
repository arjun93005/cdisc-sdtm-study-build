# ==============================================================================
# Program     : R/10_define_xml.R
# Study       : ZX-201
# Purpose     : Generate Define-XML v2.1 for the SDTM package from the metadata
#               specification, the CT-resolved codelists and the final XPT
#               files, plus a human-readable define.html.
#               Self-checks (written to output/metadata/define_check.csv):
#                 * every OID referenced is defined, every OID defined is used
#                 * ItemRefs match the variables present in each XPT
#                 * define Length >= maximum observed length
# Inputs      : specs/sdtm_spec/*.csv, specs/ecrf_spec.csv,
#               output/metadata/codelists_resolved.csv, output/sdtm/*.xpt
# Outputs     : output/sdtm/define.xml, output/sdtm/define.html,
#               output/metadata/define_check.csv
# Author      : arjun93005
# History     : v1.0  Initial version
# ==============================================================================
source("R/utils/sdtm_utils.R")
suppressPackageStartupMessages(library(xml2))

spec  <- read_spec()
cls   <- read_csv("output/metadata/codelists_resolved.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
ecrf  <- read_csv("specs/ecrf_spec.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
CT_VERSION <- readLines("specs/ct/CT_VERSION.txt", n = 1)

# aCRF: page 1 = cover/TOC, each form starts on its own page
form_page <- ecrf %>% distinct(FormOrder, FormName) %>%
  mutate(page = as.integer(FormOrder) + 1L) %>% { setNames(.$page, .$FormName) }

datasets <- spec$datasets %>% filter(file.exists(file.path(SDTM_DIR, paste0(tolower(Dataset), ".xpt"))))
data <- set_names(map(datasets$Dataset, read_sdtm), datasets$Dataset)

# --- actual lengths -----------------------------------------------------------------
actual_len <- function(x) if (is.character(x)) max(c(1L, nchar(x, type = "bytes")), na.rm = TRUE) else 8L
vars <- spec$variables %>% filter(Dataset %in% datasets$Dataset) %>%
  rowwise() %>%
  mutate(ActualLength = actual_len(data[[Dataset]][[Variable]])) %>% ungroup() %>%
  mutate(DefLength = case_when(DataType %in% c("integer", "float") ~ 8L,
                               DataType %in% c("date", "datetime") ~ NA_integer_,
                               TRUE ~ ActualLength),
         ItemOID = paste("IT", Dataset, Variable, sep = "."),
         MethodOID = if_else(Origin == "Derived" & !is.na(Method), paste("MT", Dataset, Variable, sep = "."), NA_character_),
         CommentOID = if_else(!is.na(Comment), paste("COM", Dataset, Variable, sep = "."), NA_character_))
vlm <- spec$value_level %>% filter(Dataset %in% datasets$Dataset) %>%
  mutate(ItemOID = paste("IT", Dataset, Variable, WhereValue, sep = "."),
         WCOID = paste("WC", Dataset, WhereVariable, WhereValue, sep = "."),
         VLOID = paste("VL", Dataset, Variable, sep = "."),
         MethodOID = if_else(!is.na(Method), paste("MT", Dataset, Variable, WhereValue, sep = "."), NA_character_))
vars <- vars %>% mutate(ValueListOID = if_else(paste(Dataset, Variable) %in% paste(vlm$Dataset, vlm$Variable),
                                               paste("VL", Dataset, Variable, sep = "."), NA_character_))
used_cl <- unique(c(na.omit(vars$Codelist), na.omit(vlm$Codelist)))

# --- helpers ------------------------------------------------------------------------
add_desc <- function(node, text) {
  d <- xml_add_child(node, "Description")
  xml_add_child(d, "TranslatedText", text, "xml:lang" = "en")
  invisible(node)
}
add_origin <- function(node, origin, source, pages) {
  o <- xml_add_child(node, "def:Origin", Type = origin, Source = source)
  if (origin == "Collected" && !is.na(pages)) {
    pg <- unname(form_page[str_split(pages, ";")[[1]]])
    pg <- pg[!is.na(pg)]
    if (length(pg)) {
      dr <- xml_add_child(o, "def:DocumentRef", leafID = "LF.ACRF")
      xml_add_child(dr, "def:PDFPageRef", PageRefs = paste(sort(unique(pg)), collapse = " "), Type = "PhysicalRef")
    }
  }
  invisible(o)
}

# --- document -----------------------------------------------------------------------
doc <- xml_new_root("ODM",
  xmlns = "http://www.cdisc.org/ns/odm/v1.3",
  "xmlns:def" = "http://www.cdisc.org/ns/def/v2.1",
  "xmlns:xlink" = "http://www.w3.org/1999/xlink",
  ODMVersion = "1.3.2", FileType = "Snapshot", FileOID = "ZX-201.SDTM.DEFINE",
  CreationDateTime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  Originator = "ZX-201 Clinical Programming", SourceSystem = "R",
  SourceSystemVersion = paste(R.version$major, R.version$minor, sep = "."),
  "def:Context" = "Submission")
study <- xml_add_child(doc, "Study", OID = "ZX-201")
gv <- xml_add_child(study, "GlobalVariables")
xml_add_child(gv, "StudyName", "ZX-201")
xml_add_child(gv, "StudyDescription", "A Phase II, Randomized, Double-Blind, Placebo-Controlled, Parallel-Group Study of ZX-101 in Adults with Stage 1-2 Essential Hypertension (SIMULATED DATA)")
xml_add_child(gv, "ProtocolName", "ZX-201")
mdv <- xml_add_child(study, "MetaDataVersion", OID = "MDV.ZX-201.SDTMIG.3.4", Name = "ZX-201 SDTM",
                     Description = "SDTM data definitions for study ZX-201", "def:DefineVersion" = "2.1.0")

stds <- xml_add_child(mdv, "def:Standards")
xml_add_child(stds, "def:Standard", OID = "STD.SDTMIG", Name = "SDTMIG", Type = "IG", Version = "3.4", Status = "Final")
xml_add_child(stds, "def:Standard", OID = "STD.CT", Name = "CDISC/NCI", Type = "CT", PublishingSet = "SDTM",
              Version = CT_VERSION, Status = "Final")
acrf <- xml_add_child(mdv, "def:AnnotatedCRF"); xml_add_child(acrf, "def:DocumentRef", leafID = "LF.ACRF")
supp <- xml_add_child(mdv, "def:SupplementalDoc"); xml_add_child(supp, "def:DocumentRef", leafID = "LF.SDRG")

# ValueListDef + WhereClauseDef
for (vl in unique(vlm$VLOID)) {
  v <- vlm %>% filter(VLOID == vl)
  node <- xml_add_child(mdv, "def:ValueListDef", OID = vl)
  for (i in seq_len(nrow(v))) {
    ir <- xml_add_child(node, "ItemRef", ItemOID = v$ItemOID[i], OrderNumber = i, Mandatory = "No")
    if (!is.na(v$MethodOID[i])) xml_set_attr(ir, "MethodOID", v$MethodOID[i])
    xml_add_child(ir, "def:WhereClauseRef", WhereClauseOID = v$WCOID[i])
  }
}
for (i in seq_len(nrow(vlm))) {
  wc <- xml_add_child(mdv, "def:WhereClauseDef", OID = vlm$WCOID[i])
  rc <- xml_add_child(wc, "RangeCheck", Comparator = "EQ", SoftHard = "Soft",
                      "def:ItemOID" = paste("IT", vlm$Dataset[i], vlm$WhereVariable[i], sep = "."))
  xml_add_child(rc, "CheckValue", vlm$WhereValue[i])
}

# ItemGroupDef
class_order <- c("TRIAL DESIGN", "SPECIAL PURPOSE", "INTERVENTIONS", "EVENTS", "FINDINGS", "RELATIONSHIP")
datasets <- datasets %>% arrange(match(Class, class_order), Dataset)
for (k in seq_len(nrow(datasets))) {
  d <- datasets[k, ]
  ig <- xml_add_child(mdv, "ItemGroupDef", OID = paste0("IG.", d$Dataset), Domain = sub("^SUPP", "", d$Dataset),
                      Name = d$Dataset, SASDatasetName = d$Dataset, Repeating = d$Repeating,
                      IsReferenceData = d$IsReferenceData, Purpose = d$Purpose, "def:Structure" = d$Structure,
                      "def:StandardOID" = "STD.SDTMIG", "def:ArchiveLocationID" = paste0("LF.", d$Dataset))
  if (!is.na(d$Comment)) xml_set_attr(ig, "def:CommentOID", paste0("COM.", d$Dataset))
  add_desc(ig, d$Label)
  keys <- str_split(d$Keys, ",\\s*")[[1]]
  v <- vars %>% filter(Dataset == d$Dataset) %>% arrange(Order)
  for (i in seq_len(nrow(v))) {
    ir <- xml_add_child(ig, "ItemRef", ItemOID = v$ItemOID[i], OrderNumber = v$Order[i],
                        Mandatory = if_else(v$Core[i] == "Req", "Yes", "No"))
    if (v$Variable[i] %in% keys) xml_set_attr(ir, "KeySequence", match(v$Variable[i], keys))
    if (!is.na(v$MethodOID[i])) xml_set_attr(ir, "MethodOID", v$MethodOID[i])
  }
  xml_add_child(ig, "def:Class", Name = d$Class)
  lf <- xml_add_child(ig, "def:leaf", ID = paste0("LF.", d$Dataset), "xlink:href" = paste0(tolower(d$Dataset), ".xpt"))
  xml_add_child(lf, "def:title", paste0(tolower(d$Dataset), ".xpt"))
}

# ItemDef (variable level, then value level)
for (i in seq_len(nrow(vars))) {
  v <- vars[i, ]
  it <- xml_add_child(mdv, "ItemDef", OID = v$ItemOID, Name = v$Variable, DataType = v$DataType)
  if (!is.na(v$DefLength)) xml_set_attr(it, "Length", v$DefLength)
  xml_set_attr(it, "SASFieldName", v$Variable)
  if (!is.na(v$CommentOID)) xml_set_attr(it, "def:CommentOID", v$CommentOID)
  add_desc(it, v$Label)
  if (!is.na(v$Codelist)) xml_add_child(it, "CodeListRef", CodeListOID = v$Codelist)
  add_origin(it, v$Origin, v$Source, v$Pages)
  if (!is.na(v$ValueListOID)) xml_add_child(it, "def:ValueListRef", ValueListOID = v$ValueListOID)
}
for (i in seq_len(nrow(vlm))) {
  v <- vlm[i, ]
  it <- xml_add_child(mdv, "ItemDef", OID = v$ItemOID, Name = v$Variable, DataType = v$DataType)
  if (v$DataType %in% c("text", "integer", "float")) xml_set_attr(it, "Length", v$Length)
  xml_set_attr(it, "SASFieldName", v$Variable)
  add_desc(it, v$Description)
  if (!is.na(v$Codelist)) xml_add_child(it, "CodeListRef", CodeListOID = v$Codelist)
  add_origin(it, v$Origin, v$Source, v$Pages)
}

# CodeList
for (cid in used_cl) {
  c1 <- cls %>% filter(CodelistID == cid) %>% arrange(as.integer(Order))
  node <- xml_add_child(mdv, "CodeList", OID = cid, Name = c1$Name[1], DataType = c1$DataType[1])
  if (!is.na(c1$NCICodelistCode[1])) xml_set_attr(node, "def:StandardOID", "STD.CT")
  if (all(c1$Term == "*")) {
    if (!is.na(c1$Dictionary[1])) {
      xml_add_child(node, "ExternalCodeList", Dictionary = c1$Dictionary[1], Version = c1$Version[1])
    } else {   # e.g. TSPARMCD: enumerate values actually used
      dom <- if (cid == "CL.TSPARMCD") "TSPARMCD" else "TSPARM"
      for (val in sort(unique(data$TS[[dom]]))) xml_add_child(node, "EnumeratedItem", CodedValue = val)
    }
  } else {
    has_decode <- any(!is.na(c1$Decode))
    for (j in seq_len(nrow(c1))) {
      item <- if (has_decode) {
        x <- xml_add_child(node, "CodeListItem", CodedValue = c1$Term[j], OrderNumber = j)
        add_desc_dec <- xml_add_child(x, "Decode"); xml_add_child(add_desc_dec, "TranslatedText", coalesce(c1$Decode[j], c1$Term[j]), "xml:lang" = "en")
        x
      } else xml_add_child(node, "EnumeratedItem", CodedValue = c1$Term[j], OrderNumber = j)
      if (!is.na(c1$NCITermCode[j])) xml_add_child(item, "Alias", Context = "nci:ExtCodeID", Name = c1$NCITermCode[j])
      else if (!is.na(c1$NCICodelistCode[j])) xml_set_attr(item, "def:ExtendedValue", "Yes")
    }
  }
  if (!is.na(c1$NCICodelistCode[1])) xml_add_child(node, "Alias", Context = "nci:ExtCodeID", Name = c1$NCICodelistCode[1])
}

# MethodDef
meth <- bind_rows(vars %>% filter(!is.na(MethodOID)) %>% transmute(MethodOID, Name = paste("Algorithm to derive", paste(Dataset, Variable, sep = ".")), Method),
                  vlm %>% filter(!is.na(MethodOID)) %>% transmute(MethodOID, Name = paste("Algorithm to derive", paste(Dataset, Variable, sep = "."), "for", WhereValue), Method))
for (i in seq_len(nrow(meth))) add_desc(xml_add_child(mdv, "MethodDef", OID = meth$MethodOID[i], Name = meth$Name[i], Type = "Computation"), meth$Method[i])

# CommentDef
coms <- bind_rows(datasets %>% filter(!is.na(Comment)) %>% transmute(OID = paste0("COM.", Dataset), Comment),
                  vars %>% filter(!is.na(CommentOID)) %>% transmute(OID = CommentOID, Comment))
for (i in seq_len(nrow(coms))) add_desc(xml_add_child(mdv, "def:CommentDef", OID = coms$OID[i]), coms$Comment[i])

# leaves for documents
for (lf in list(c("LF.ACRF", "acrf.pdf", "Annotated Case Report Form"),
                c("LF.SDRG", "sdrg.html", "Study Data Reviewer's Guide"))) {
  l <- xml_add_child(mdv, "def:leaf", ID = lf[1], "xlink:href" = lf[2])
  xml_add_child(l, "def:title", lf[3])
}

write_xml(doc, file.path(SDTM_DIR, "define.xml"), options = c("format", "as_xml"))

# ==============================================================================
# Self-checks
# ==============================================================================
x <- read_xml(file.path(SDTM_DIR, "define.xml"))
ns <- xml_ns(x)
# namespace-agnostic access: element by local name, attribute with or without def: prefix
nodes <- function(tag) xml_find_all(x, sprintf("//*[local-name()='%s']", tag))
get_attr <- function(n, a) coalesce(xml_attr(n, a), xml_attr(n, paste0("def:", a), ns = ns))
defined <- function(tag) get_attr(nodes(tag), "OID")
refs <- function(tag, attr) unique(na.omit(get_attr(nodes(sub("^//", "", tag)), attr)))
chk <- tibble(Check = character(), Result = character(), Detail = character())
add_chk <- function(check, bad) {
  chk <<- add_row(chk, Check = check, Result = if (length(bad)) "FAIL" else "PASS",
                  Detail = if (length(bad)) paste(head(bad, 10), collapse = ", ") else "")
}
add_chk("ItemRef/@ItemOID resolve to ItemDef", setdiff(refs("//ItemRef", "ItemOID"), defined("ItemDef")))
add_chk("CodeListRef resolve to CodeList", setdiff(refs("//CodeListRef", "CodeListOID"), defined("CodeList")))
add_chk("MethodOID resolve to MethodDef", setdiff(refs("//ItemRef", "MethodOID"), defined("MethodDef")))
add_chk("WhereClauseRef resolve to WhereClauseDef", setdiff(refs("//WhereClauseRef", "WhereClauseOID"), defined("WhereClauseDef")))
add_chk("ValueListRef resolve to ValueListDef", setdiff(refs("//ValueListRef", "ValueListOID"), defined("ValueListDef")))
add_chk("CommentOID resolve to CommentDef", setdiff(c(refs("//ItemDef", "CommentOID"), refs("//ItemGroupDef", "CommentOID")), defined("CommentDef")))
add_chk("leafID resolve to def:leaf", setdiff(c(refs("//DocumentRef", "leafID"), refs("//ItemGroupDef", "ArchiveLocationID")),
                                              xml_attr(nodes("leaf"), "ID")))
add_chk("Every ItemDef is referenced", setdiff(defined("ItemDef"), refs("//ItemRef", "ItemOID")))
add_chk("Every CodeList is referenced", setdiff(defined("CodeList"), refs("//CodeListRef", "CodeListOID")))
for (d in datasets$Dataset) {
  in_def <- xml_attr(xml_find_all(x, sprintf("//*[local-name()='ItemGroupDef'][@Name='%s']/*[local-name()='ItemRef']", d)), "ItemOID") %>%
    sub(paste0("IT.", d, "."), "", ., fixed = TRUE)
  add_chk(paste(d, "variables in define match XPT"), c(setdiff(names(data[[d]]), in_def), setdiff(in_def, names(data[[d]]))))
}
too_short <- vars %>% filter(!is.na(DefLength), DefLength < ActualLength) %>% transmute(paste(Dataset, Variable, sep = "."))
add_chk("Define Length >= observed length", too_short[[1]])
dir.create("output/metadata", showWarnings = FALSE)
write_csv(chk, "output/metadata/define_check.csv")
message(sprintf("define.xml: %d datasets, %d ItemDefs, %d CodeLists, %d MethodDefs, %d VLM items; self-checks %d/%d PASS",
                nrow(datasets), length(defined("ItemDef")), length(defined("CodeList")), length(defined("MethodDef")),
                nrow(vlm), sum(chk$Result == "PASS"), nrow(chk)))
if (any(chk$Result == "FAIL")) { print(as.data.frame(chk %>% filter(Result == "FAIL"))); stop("define.xml self-checks failed") }

# ==============================================================================
# define.html - readable rendering of the same metadata (no XSL dependency)
# ==============================================================================
esc <- function(s) { s <- coalesce(as.character(s), ""); s <- gsub("&", "&amp;", s); s <- gsub("<", "&lt;", s); gsub(">", "&gt;", s) }
tbl <- function(df) {
  paste0("<table><thead><tr>", paste0("<th>", esc(names(df)), "</th>", collapse = ""), "</tr></thead><tbody>",
         paste0(apply(df, 1, function(r) paste0("<tr>", paste0("<td>", esc(r), "</td>", collapse = ""), "</tr>")), collapse = ""),
         "</tbody></table>")
}
origin_txt <- function(o, s, p) ifelse(o == "Collected" & !is.na(p),
  paste0(o, " (", s, ") - aCRF p. ", map_chr(p, ~ paste(na.omit(form_page[str_split(.x, ";")[[1]]]), collapse = ", "))),
  paste0(o, " (", s, ")"))
html <- c(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'><title>ZX-201 SDTM Define</title>",
  "<style>body{font-family:Segoe UI,Arial,sans-serif;margin:24px;color:#1b1b1b}h1{font-size:22px}h2{font-size:17px;margin-top:32px;border-bottom:2px solid #2b5c8a}",
  "table{border-collapse:collapse;width:100%;font-size:12px;margin:8px 0}th{background:#2b5c8a;color:#fff;text-align:left}th,td{border:1px solid #c9d3dd;padding:3px 6px;vertical-align:top}",
  "tr:nth-child(even) td{background:#f4f7fa}.meta{color:#555;font-size:13px}</style></head><body>",
  "<h1>ZX-201 - SDTM Data Definition (Define-XML 2.1 rendering)</h1>",
  sprintf("<p class='meta'>SDTMIG 3.4 &middot; CDISC CT %s &middot; generated %s &middot; SIMULATED DATA</p>", CT_VERSION, format(Sys.time(), "%Y-%m-%d %H:%M")),
  "<h2>Datasets</h2>",
  tbl(datasets %>% transmute(Dataset, Description = Label, Class, Structure, Keys,
                             Records = map_int(Dataset, ~ nrow(data[[.x]])), Location = paste0(tolower(Dataset), ".xpt"))),
  unlist(map(datasets$Dataset, function(d) c(
    sprintf("<h2 id='%s'>%s - %s</h2>", d, d, esc(datasets$Label[datasets$Dataset == d])),
    tbl(vars %>% filter(Dataset == d) %>% arrange(Order) %>%
          transmute(Variable, Label, Type = DataType, Length = DefLength, Core, `Controlled Terms` = Codelist,
                    Origin = origin_txt(Origin, Source, Pages), `Derivation / Comment` = coalesce(Method, Comment)))
  ))),
  "<h2>Value-Level Metadata</h2>",
  tbl(vlm %>% transmute(Variable = paste(Dataset, Variable, sep = "."), Where = paste(WhereVariable, "=", WhereValue),
                        Type = DataType, Length, `Controlled Terms` = Codelist, Origin, Description = coalesce(Method, Description))),
  "<h2>Controlled Terminology</h2>",
  tbl(cls %>% filter(CodelistID %in% used_cl) %>%
        transmute(Codelist = CodelistID, Name, `NCI Codelist` = NCICodelistCode, Term = if_else(Term == "*", paste("External:", Dictionary, Version), Term),
                  Decode, `NCI Term` = NCITermCode)),
  "</body></html>")
writeLines(html, file.path(SDTM_DIR, "define.html"), useBytes = TRUE)
message("define.html written")
