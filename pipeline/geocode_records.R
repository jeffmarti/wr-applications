# ==============================================================================
# geocode_records.R
# ------------------------------------------------------------------------------
# Reads the WRTS applications export straight from the .xlsx ("Print Version"
# sheet — no manual save-as-CSV step) and geocodes each record to its PLSS
# section / quarter / quarter-quarter centroid.
#
# Same logic as the WA Water Rights PLSS Geocoder app and the permit-events
# pipeline: TRS parsing, BLM CadNSDI lookup (Section, then Protracted Block
# fallback), quarter / quarter-quarter placement.
#
# Input : newest pipeline/raw/WRTS_applications_*.xlsx
# Output: pipeline/work/applications_geocoded_YYYYMMDD.csv
#         pipeline/work/applications_geocode_failed_YYYYMMDD.csv
#
# CACHE: pipeline/cache/section_bbox_cache.csv
#   Copied from permit-events (15,500+ sections already looked up). One row per
#   PLSS section: its bounding box. Sections BLM has no match for are
#   remembered too ("NONE") and re-checked after RECHECK_NONE_DAYS.
#
# Run from the project root:  source("pipeline/geocode_records.R")
# REQUIRED PACKAGES: dplyr, readr, stringr, httr2, readxl
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(httr2)
library(readxl)

# ==============================================================================
# SETTINGS
# ==============================================================================

RAW_DIR    <- "pipeline/raw"
WORK_DIR   <- "pipeline/work"
CACHE_FILE <- "pipeline/cache/section_bbox_cache.csv"

RECHECK_NONE_DAYS <- 90     # re-ask BLM about "no match" sections after this many days
PAUSE_S           <- 0.5    # polite pause between BLM requests

BLM_URL <- "https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/2/query"

# ==============================================================================
# READING THE WRTS EXPORT (from permit-events/join_events_phases.R)
# ==============================================================================

# "WR Doc ID" -> "wrdocid"
norm <- function(x) str_to_lower(str_remove_all(x, "[^A-Za-z0-9]"))

# Excel dates read as text come through as serial numbers ("44561").
# Convert those to m/d/Y text; leave real text dates ("12/31/2021") alone.
fix_excel_dates <- function(x) {
  is_serial <- !is.na(x) & str_detect(x, "^\\d{5}(\\.\\d+)?$")
  x[is_serial] <- format(as.Date(as.numeric(x[is_serial]), origin = "1899-12-30"),
                         "%m/%d/%Y")
  x
}

# Read one WRTS export: "Print Version" sheet, header row found automatically,
# every column as text, Ecology's criteria footer rows dropped.
read_wrts_export <- function(path) {
  sheets <- excel_sheets(path)
  sheet  <- sheets[str_detect(sheets, regex("print", ignore_case = TRUE))][1]
  if (is.na(sheet)) sheet <- sheets[1]

  peek <- read_excel(path, sheet = sheet, col_names = FALSE, n_max = 40,
                     col_types = "text", .name_repair = "minimal")
  hdr_row <- which(apply(peek, 1, function(r) any(norm(r) == "wrdocid")))[1]
  if (is.na(hdr_row)) {
    stop("No 'WR Doc ID' header found in the first 40 rows of ", basename(path),
         " (sheet '", sheet, "').", call. = FALSE)
  }

  df <- read_excel(path, sheet = sheet, skip = hdr_row - 1,
                   col_types = "text", .name_repair = "unique_quiet")

  df <- df |>
    filter(if_any(everything(), ~ !is.na(.x) & .x != "")) |>
    select(!(starts_with("...") & where(~ all(is.na(.x)))))

  # Ecology appends the search criteria BELOW the data. Real WR Doc IDs are
  # all digits — keep only those rows.
  id_col  <- names(df)[norm(names(df)) == "wrdocid"][1]
  is_real <- str_detect(str_trim(df[[id_col]]), "^\\d+$") %in% TRUE
  if (any(!is_real)) {
    message("  dropped ", sum(!is_real), " non-record row(s) (criteria footer / blanks)")
  }
  df <- df[is_real, ]

  df |> mutate(across(matches("date", ignore.case = TRUE), fix_excel_dates))
}

# ==============================================================================
# GEOCODING HELPERS (unchanged from permit-events/geocode_records.R)
# ==============================================================================

build_frstdivid <- function(twp_num, twp_dir, rng_num, rng_dir, sec_num, div_type = "SN") {
  paste0("WA33",
         str_pad(twp_num, 3, "left", "0"), "0", twp_dir,
         str_pad(rng_num, 3, "left", "0"), "0", rng_dir,
         "0", div_type,
         str_pad(sec_num, 2, "left", "0"), "0")
}

# Returns c(xmin, ymin, xmax, ymax) if found, "none" if BLM has no such
# division, NULL if the request itself failed (not cached; retried next run)
query_bbox <- function(fid) {
  resp <- tryCatch(
    request(BLM_URL) |>
      req_url_query(where = sprintf("FRSTDIVID = '%s'", fid), outFields = "FRSTDIVID",
                    returnGeometry = "true", outSR = "4326", f = "geojson") |>
      req_timeout(30) |>
      req_retry(max_tries = 3, backoff = ~ 2) |>
      req_perform(),
    error = function(e) { message("  API error for ", fid, ": ", conditionMessage(e)); NULL })
  if (is.null(resp)) return(NULL)
  gj <- tryCatch(resp_body_json(resp, check_type = FALSE, simplifyVector = FALSE),
                 error = function(e) NULL)
  if (is.null(gj) || !is.null(gj$error)) return(NULL)
  if (length(gj$features) == 0) return("none")
  xy <- unlist(lapply(gj$features, function(f) f$geometry$coordinates))
  xs <- xy[c(TRUE, FALSE)]; ys <- xy[c(FALSE, TRUE)]
  c(xmin = min(xs), ymin = min(ys), xmax = max(xs), ymax = max(ys))
}

lookup_section <- function(twp_num, twp_dir, rng_num, rng_dir, sec_num) {
  for (dt in c("SN", "PB")) {
    r <- query_bbox(build_frstdivid(twp_num, twp_dir, rng_num, rng_dir, sec_num, dt))
    if (is.null(r)) return(list(status = "error"))
    if (!identical(r, "none")) return(list(status = "ok", div_type = dt, bbox = r))
    Sys.sleep(0.3)
  }
  list(status = "none")
}

quarter_frac <- function(dir) {
  switch(dir,
         "NE" = c(x = 0.75, y = 0.75), "NW" = c(x = 0.25, y = 0.75),
         "SE" = c(x = 0.75, y = 0.25), "SW" = c(x = 0.25, y = 0.25),
         c(x = 0.50, y = 0.50))
}

qq_point <- function(xmin, ymin, xmax, ymax, q, qq) {
  dx <- xmax - xmin; dy <- ymax - ymin
  has_q  <- !is.na(q)  && q  != ""
  has_qq <- !is.na(qq) && qq != ""
  if (!has_q) return(c((xmin + xmax) / 2, (ymin + ymax) / 2))
  qf <- quarter_frac(q)
  qx0 <- xmin + dx * (qf[["x"]] - 0.25); qx1 <- xmin + dx * (qf[["x"]] + 0.25)
  qy0 <- ymin + dy * (qf[["y"]] - 0.25); qy1 <- ymin + dy * (qf[["y"]] + 0.25)
  if (!has_qq) return(c((qx0 + qx1) / 2, (qy0 + qy1) / 2))
  qqf <- quarter_frac(qq)
  c(qx0 + (qx1 - qx0) * qqf[["x"]], qy0 + (qy1 - qy0) * qqf[["y"]])
}

empty_cache <- function() {
  tibble(plss_key = character(), div_type = character(), xmin = double(),
         ymin = double(), xmax = double(), ymax = double(), fetched = as.Date(character()))
}

# ==============================================================================
# STEP 0 — Load the cache
# ==============================================================================

dir.create(dirname(CACHE_FILE), showWarnings = FALSE, recursive = TRUE)
dir.create(WORK_DIR, showWarnings = FALSE, recursive = TRUE)
cache <- if (file.exists(CACHE_FILE)) {
  read_csv(CACHE_FILE, col_types = "ccddddD", show_col_types = FALSE)
} else empty_cache()

# ==============================================================================
# STEP 1 — Read the newest export and parse TRS
# ==============================================================================

in_file <- list.files(RAW_DIR, pattern = "^WRTS_applications_.*\\.xlsx$", full.names = TRUE)
if (length(in_file) == 0) stop("No WRTS_applications_*.xlsx in ", RAW_DIR,
                               ". Run fetch_wrts_export.R first.")
in_file <- in_file[which.max(file.mtime(in_file))]
message("Geocoding: ", basename(in_file))

wr <- read_wrts_export(in_file)
if (!"TRS" %in% names(wr)) stop("The export has no TRS column. Columns are:\n  ",
                                paste(names(wr), collapse = " | "))
for (cl in c("Q", "QQ")) if (!cl %in% names(wr)) wr[[cl]] <- NA_character_

message("Columns in export: ", paste(names(wr), collapse = " | "))

wr <- wr |>
  mutate(
    twp_num  = str_match(TRS, "T([0-9.]+)[NS]")[, 2] |> as.numeric() |> round(),
    twp_dir  = str_match(TRS, "T[0-9.]+([NS])")[, 2],
    rng_num  = str_match(TRS, "R([0-9.]+)[EW]")[, 2] |> as.numeric() |> round(),
    rng_dir  = str_match(TRS, "R[0-9.]+([EW])")[, 2],
    sec_num  = str_extract(TRS, "(?<=/)[0-9]+$") |> as.integer(),
    plss_key = if_else(is.na(twp_num) | is.na(rng_num) | is.na(sec_num) |
                         is.na(twp_dir) | is.na(rng_dir), NA_character_,
                       paste0("T", twp_num, twp_dir, "_R", rng_num, rng_dir, "_S", sec_num))
  )

secs <- wr |> filter(!is.na(plss_key)) |>
  distinct(plss_key, twp_num, twp_dir, rng_num, rng_dir, sec_num)

# ==============================================================================
# STEP 2 — Look up sections not already cached
# ==============================================================================

stale_none <- cache |> filter(div_type == "NONE", fetched < Sys.Date() - RECHECK_NONE_DAYS)

# Rows marked as found (SN / PB) but with no bounding box can't place a point.
# The cache copied from permit-events has ~300 of these (all dated 10/12/2026,
# the day the geocoder app's section_cache.rds was imported). Treat them as
# not cached so BLM is asked again; the good answer then replaces them.
broken <- cache |> filter(div_type != "NONE", is.na(xmin) | is.na(ymin) |
                            is.na(xmax) | is.na(ymax))
if (nrow(broken) > 0) message(nrow(broken), " cache rows have no bounding box - ",
                              "they will be re-queried if needed")

cache  <- cache |> anti_join(bind_rows(stale_none, broken), by = "plss_key")
to_get <- secs |> anti_join(cache, by = "plss_key")

message(nrow(secs), " unique sections | ", nrow(secs) - nrow(to_get), " cached | ",
        nrow(to_get), " to query from BLM")

n_err <- 0
if (nrow(to_get) > 0) {
  for (i in seq_len(nrow(to_get))) {
    s <- to_get[i, ]
    r <- lookup_section(s$twp_num, s$twp_dir, s$rng_num, s$rng_dir, s$sec_num)
    if (r$status == "error") { n_err <- n_err + 1; next }
    cache <- bind_rows(cache, tibble(
      plss_key = s$plss_key,
      div_type = if (r$status == "ok") r$div_type else "NONE",
      xmin = if (r$status == "ok") r$bbox[["xmin"]] else NA_real_,
      ymin = if (r$status == "ok") r$bbox[["ymin"]] else NA_real_,
      xmax = if (r$status == "ok") r$bbox[["xmax"]] else NA_real_,
      ymax = if (r$status == "ok") r$bbox[["ymax"]] else NA_real_,
      fetched = Sys.Date()))
    if (i %% 25 == 0 || i == nrow(to_get)) {
      message("  ", i, " / ", nrow(to_get), " queried")
      write_csv(cache, CACHE_FILE)                              # save progress
    }
    Sys.sleep(PAUSE_S)
  }
}
write_csv(cache |> arrange(plss_key), CACHE_FILE)

# ==============================================================================
# STEP 3 — Place each record, split into geocoded / failed
# ==============================================================================

wr <- wr |> left_join(cache |> select(-fetched), by = "plss_key")

pts <- mapply(function(x0, y0, x1, y1, q, qq) {
  if (is.na(x0)) return(c(NA_real_, NA_real_))
  qq_point(x0, y0, x1, y1, q, qq)
}, wr$xmin, wr$ymin, wr$xmax, wr$ymax, wr$Q, wr$QQ)

if (nrow(wr) == 0) pts <- matrix(numeric(0), nrow = 2)

wr <- wr |>
  mutate(lon = pts[1, ], lat = pts[2, ],
         fail_reason = case_when(
           !is.na(lat)                        ~ NA_character_,
           is.na(TRS) | TRS == ""             ~ "No TRS in WRTS",
           is.na(plss_key)                    ~ "TRS could not be read",
           TRUE                               ~ "No BLM section match")) |>
  select(-plss_key, -div_type, -xmin, -ymin, -xmax, -ymax,
         -twp_num, -twp_dir, -rng_num, -rng_dir, -sec_num) |>
  relocate(lon, lat, .after = TRS)

geocoded <- wr |> filter(!is.na(lat)) |> select(-fail_reason)
failed   <- wr |> filter(is.na(lat))

stamp    <- format(Sys.Date(), "%Y%m%d")
out_ok   <- file.path(WORK_DIR, paste0("applications_geocoded_", stamp, ".csv"))
out_fail <- file.path(WORK_DIR, paste0("applications_geocode_failed_", stamp, ".csv"))
write_csv(geocoded, out_ok,   na = "")
write_csv(failed,   out_fail, na = "")

# ==============================================================================
# STEP 4 — Summary
# ==============================================================================

cat("\n================ GEOCODING SUMMARY ================\n")
cat("Records in export           :", nrow(wr), "\n")
cat("Geocoded                    :", nrow(geocoded),
    sprintf("(%.1f%%)", 100 * nrow(geocoded) / max(nrow(wr), 1)), "\n")
cat("Failed                      :", nrow(failed), "\n")
if (nrow(failed) > 0) print(table(failed$fail_reason))
cat("BLM requests that errored   :", n_err, if (n_err > 0) " (will retry next run)" else "", "\n")
cat("Sections in cache           :", nrow(cache), "\n")
cat("Wrote                       :", basename(out_ok), "|", basename(out_fail), "\n")
cat("===================================================\n")
