# ==============================================================================
# fetch_wrts_export.R
# ------------------------------------------------------------------------------
# Downloads ONE WA Ecology WRTS export for the wr-applications app — no browser,
# no clicking. It replays what the Water Right Record Search page does:
#   1. GET the search page (picks up the form + its hidden __VIEWSTATE fields)
#   2. POST the "Combined Tab Search" with our criteria
#   3. POST "Export Search Result" -> .xlsx
#
# QUERY (General Search tab):
#   Phase  = Change Application, New Application   (both selected in one list)
#   Status = Active
#
# Adapted from permit-events/pipeline/fetch_wrts_exports.R — the form helpers
# below are unchanged from that working script; only the criteria differ.
#
# Run from the project root (the folder with app.R):
#   source("pipeline/fetch_wrts_export.R")
# File is saved to pipeline/raw/WRTS_applications_YYYYMMDD.xlsx
#
# REQUIRED PACKAGES: httr2, xml2, stringr, readxl, curl
# ==============================================================================

library(httr2)
library(xml2)
library(stringr)
library(readxl)

# ==============================================================================
# SETTINGS
# ==============================================================================

RAW_DIR    <- "pipeline/raw"
SEARCH_URL <- paste0("https://appswr.ecology.wa.gov/waterrighttrackingsystem/",
                     "WaterRights/WaterRightSearch.aspx")
USER_AGENT <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
                    "(KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36")
TIMEOUT_S  <- 600     # the search can take ~40 s in a browser

# Form-field name prefixes (ASP.NET control names)
P   <- "ctl00$MainContentPlaceHolder$"
GEN <- paste0(P, "mainTabContainer$tabPanelGeneralSearch$")
EVT <- paste0(P, "mainTabContainer$tabPanelEventSearch$")

# Option VALUES the site uses (codes, not the on-screen labels).
# Confirmed on the live search page 10/16/2026:
#   "Change Application" = ChangeApplication,  "New Application" = NewApp
STATUS_ACTIVE <- "Active"
PHASE_TYPES   <- c("ChangeApplication", "NewApp")

# ==============================================================================
# FORM HELPERS  (unchanged from permit-events)
# ------------------------------------------------------------------------------
# Form data is kept as a 2-column data frame (name, value) because ASP.NET
# multi-select lists send the SAME name once per selected value.
# ==============================================================================

collect_fields <- function(doc) {
  out <- list()
  add <- function(n, v) out[[length(out) + 1]] <<- c(name = n, value = v)

  for (el in xml_find_all(doc, ".//input[@name]")) {
    if (!is.na(xml_attr(el, "disabled"))) next
    type <- tolower(xml_attr(el, "type", default = "text"))
    nm   <- xml_attr(el, "name")
    val  <- xml_attr(el, "value", default = "")
    if (type %in% c("submit", "button", "image", "reset", "file")) next
    if (type %in% c("checkbox", "radio")) {
      if (!is.na(xml_attr(el, "checked"))) add(nm, if (val == "") "on" else val)
    } else add(nm, val)
  }
  for (el in xml_find_all(doc, ".//select[@name]")) {
    if (!is.na(xml_attr(el, "disabled"))) next
    nm   <- xml_attr(el, "name")
    opts <- xml_find_all(el, ".//option")
    if (length(opts) == 0) next
    vals <- xml_attr(opts, "value", default = NA)
    vals <- ifelse(is.na(vals), xml_text(opts), vals)
    sel  <- !is.na(xml_attr(opts, "selected"))
    multi <- !is.na(xml_attr(el, "multiple"))
    if (any(sel)) for (v in vals[sel]) add(nm, v)
    else if (!multi) add(nm, vals[1])
  }
  for (el in xml_find_all(doc, ".//textarea[@name]")) add(xml_attr(el, "name"), xml_text(el))

  if (length(out) == 0) return(data.frame(name = character(), value = character()))
  as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
}

set_field <- function(f, name, values) {
  f <- f[f$name != name, , drop = FALSE]
  if (length(values) == 0) return(f)
  rbind(f, data.frame(name = name, value = values, stringsAsFactors = FALSE))
}

update_fields <- function(f, newer) {
  for (n in unique(newer$name)) f <- set_field(f, n, newer$value[newer$name == n])
  f
}

encode_form <- function(f) {
  paste(paste0(curl::curl_escape(f$name), "=", curl::curl_escape(f$value)), collapse = "&")
}

# Microsoft AJAX "delta" response: length|type|id|content| repeated
parse_delta <- function(txt) {
  out <- list(); i <- 1; n <- nchar(txt)
  while (i <= n) {
    p1 <- regexpr("|", substr(txt, i, n), fixed = TRUE) + i - 1
    len <- as.integer(substr(txt, i, p1 - 1))
    p2 <- regexpr("|", substr(txt, p1 + 1, n), fixed = TRUE) + p1
    typ <- substr(txt, p1 + 1, p2 - 1)
    p3 <- regexpr("|", substr(txt, p2 + 1, n), fixed = TRUE) + p2
    id  <- substr(txt, p2 + 1, p3 - 1)
    content <- substr(txt, p3 + 1, p3 + len)
    out[[length(out) + 1]] <- list(type = typ, id = id, content = content)
    i <- p3 + len + 2
  }
  out
}

build_search_fields <- function(fields) {
  fields |>
    set_field(paste0(P, "scriptManager"), paste0(P, "scriptManager|", P, "btnSearch")) |>
    set_field("__ASYNCPOST", "true") |>
    set_field(paste0(P, "btnSearch"), "Combined Tab Search")
}

read_search_result <- function(delta_txt) {
  delta  <- parse_delta(delta_txt)
  hidden <- do.call(rbind, lapply(Filter(function(d) d$type == "hiddenField", delta),
                                  function(d) data.frame(name = d$id, value = d$content)))
  panels <- Filter(function(d) d$type == "updatePanel", delta)
  panel_html <- paste(vapply(panels, `[[`, "", "content"), collapse = "\n")
  err <- Filter(function(d) d$type == "error" || str_detect(d$id, "errorUpdatePanel"), delta)
  err_txt <- str_squish(xml_text(read_html(paste0(
    "<div>", paste(vapply(err, `[[`, "", "content"), collapse = ""), "</div>"))))

  n_found <- str_match(panel_html, "([\\d,]+) Water Right Records? were found")[, 2]
  if (is.na(n_found)) stop("Search returned no result count. Error panel says: '", err_txt, "'")
  list(n_found = as.integer(str_remove_all(n_found, ",")),
       hidden = hidden,
       panel_doc = read_html(paste0("<form>", panel_html, "</form>")))
}

build_export_fields <- function(fields, res) {
  btn <- xml_find_first(res$panel_doc, ".//input[@type='submit'][@value='Export Search Result']")
  if (is.na(btn)) stop("Export button not found in the search results.")
  fields |>
    update_fields(res$hidden) |>
    update_fields(collect_fields(res$panel_doc)) |>
    set_field("ctl00_MainContentPlaceHolder_tabcontainerSearchResults_ClientState",
              '{"ActiveTabIndex":0,"TabEnabledState":[true],"TabWasLoadedOnceState":[true]}') |>
    set_field(paste0(EVT, "dtrEventDate$ctlDateFrom$txtDate"), character()) |>
    set_field(paste0(EVT, "dtrEventDate$ctlDateTo$txtDate"),   character()) |>
    set_field(xml_attr(btn, "name"), xml_attr(btn, "value"))
}

base_req <- function(url, jar) {
  request(url) |>
    req_user_agent(USER_AGENT) |>
    req_headers(`Accept-Language` = "en-US,en;q=0.9") |>
    req_cookie_preserve(jar) |>
    req_timeout(TIMEOUT_S) |>
    req_retry(max_tries = 3, backoff = ~ 10)
}

# Select values in a WRTS multi-select list and set its "clicked" marker
set_list <- function(f, list_name, values) {
  f |>
    set_field(paste0(list_name, "$DDList"), values) |>
    set_field(paste0(list_name, "$hidMddClicked"),
              paste0(gsub("$", "_", list_name, fixed = TRUE), "_"))
}

# Tab containers' state: which tab is active; the first tab counts as loaded
set_tab_state <- function(f, active_tab) {
  f |>
    set_field("ctl00_MainContentPlaceHolder_mainTabContainer_ClientState",
              sprintf(paste0('{"ActiveTabIndex":%d,"TabEnabledState":[true,true,true,true,true],',
                             '"TabWasLoadedOnceState":[true,false,false,false,false]}'), active_tab)) |>
    set_field(paste0("ctl00_MainContentPlaceHolder_mainTabContainer_tabPanelSavedSearches_",
                     "savedSearchesTabContainer_ClientState"),
              paste0('{"ActiveTabIndex":0,"TabEnabledState":[true,true,true,true],',
                     '"TabWasLoadedOnceState":[true,false,false,false]}'))
}

# ==============================================================================
# CRITERIA — Phase = Change Application + New Application; Status = Active
# ==============================================================================

application_criteria <- function(f, page) {
  f |>
    set_list(paste0(GEN, "ddlDocStatus"), STATUS_ACTIVE) |>
    set_list(paste0(GEN, "ddlDocType"),   PHASE_TYPES) |>
    set_tab_state(active_tab = 0)            # General Search tab
}

# ==============================================================================
# ONE QUERY = GET page -> async Combined search -> Export
# ==============================================================================

run_wrts_query <- function(label, set_criteria, out_file) {
  message("\n== ", label, " query ==")
  jar <- tempfile(fileext = ".cookies")

  # ---- 1. GET the search page ----------------------------------------------
  t0   <- Sys.time()
  resp <- base_req(SEARCH_URL, jar) |> req_perform()
  html <- resp_body_string(resp)
  if (!str_detect(html, "__VIEWSTATE")) {
    stop("The Ecology site did not return the search form (no __VIEWSTATE). ",
         "It may have served a bot-check page. First 300 characters:\n",
         substr(html, 1, 300))
  }
  page   <- read_html(html)
  fields <- collect_fields(xml_find_first(page, "//form"))
  message("  page loaded (", nrow(fields), " form fields)")

  # ---- 2. Set criteria and run the Combined Tab Search (AJAX post) ----------
  fields <- set_criteria(fields, page)
  resp <- base_req(SEARCH_URL, jar) |>
    req_headers(`X-MicrosoftAjax` = "Delta=true", `X-Requested-With` = "XMLHttpRequest",
                Origin = "https://appswr.ecology.wa.gov", Referer = SEARCH_URL) |>
    req_body_raw(encode_form(build_search_fields(fields)),
                 type = "application/x-www-form-urlencoded; charset=UTF-8") |>
    req_perform()
  res <- read_search_result(resp_body_string(resp))
  message("  search done in ", round(difftime(Sys.time(), t0, units = "secs")),
          " s: ", format(res$n_found, big.mark = ","), " records found")

  # ---- 3. Export Search Result (full post-back -> .xlsx) ---------------------
  export <- build_export_fields(fields, res)
  resp <- base_req(SEARCH_URL, jar) |>
    req_headers(Origin = "https://appswr.ecology.wa.gov", Referer = SEARCH_URL) |>
    req_body_raw(encode_form(export), type = "application/x-www-form-urlencoded") |>
    req_perform()

  ctype <- resp_content_type(resp)
  if (!str_detect(ctype, "spreadsheet|officedocument|excel")) {
    stop("Export did not return an Excel file (got '", ctype, "').")
  }
  writeBin(resp_body_raw(resp), out_file)

  # ---- 4. Quick check: row count in the file vs the site's count -----------
  sheets <- excel_sheets(out_file)
  sh <- sheets[str_detect(sheets, regex("print", ignore_case = TRUE))][1]
  if (is.na(sh)) sh <- sheets[1]
  peek <- read_excel(out_file, sheet = sh, col_names = FALSE, col_types = "text",
                     .name_repair = "minimal")
  ids  <- unlist(peek[, 2])
  n_rows <- sum(str_detect(ids, "^\\d+$"), na.rm = TRUE)   # real WR Doc IDs only
  message("  saved ", out_file, " (", round(file.size(out_file) / 1024), " KB, ",
          format(n_rows, big.mark = ","), " rows)",
          if (n_rows != res$n_found) "  <-- row count differs from site count, CHECK" else "")

  invisible(list(file = out_file, n_found = res$n_found, n_rows = n_rows))
}

# ==============================================================================
# RUN
# ==============================================================================

dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)
stamp <- format(Sys.Date(), "%Y%m%d")

ap <- run_wrts_query("APPLICATIONS", application_criteria,
                     file.path(RAW_DIR, paste0("WRTS_applications_", stamp, ".xlsx")))

cat("\n================ WRTS DOWNLOAD SUMMARY ================\n")
cat("Applications export :", basename(ap$file), "|", ap$n_found, "found |", ap$n_rows, "rows\n")
cat("Next step           : source(\"pipeline/geocode_records.R\")\n")
cat("=======================================================\n")
