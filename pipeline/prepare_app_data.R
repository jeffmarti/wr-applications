# ==============================================================================
# prepare_app_data.R
# ------------------------------------------------------------------------------
# Turns the geocoded + failed CSVs into the compact files the app reads, and
# downloads the WRIA and county boundary layers (once) so the published app
# never has to fetch anything at startup.
#
# Input : newest pipeline/work/applications_geocoded_*.csv
#         newest pipeline/work/applications_geocode_failed_*.csv
# Output: data/applications.rds   list(records, unmapped, info)
#         data/boundaries.rds     list(wria, county)
#
# Run from the project root:  source("pipeline/prepare_app_data.R")
# REQUIRED PACKAGES: dplyr, readr, stringr, sf
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(sf)

# ==============================================================================
# SETTINGS
# ==============================================================================

WORK_DIR <- "pipeline/work"
OUT_DIR  <- "data"

# "Data current as of" date. NULL = take it from the geocoded file name.
DATA_DATE <- NULL

# Which export column drives the app's time slider (and the KMZ time stamps).
# Starting with the priority date, same as the Geocoder KMZ. If the export
# turns out to have an application-received date (check the column list
# printed by geocode_records.R), put that column's name here instead.
SLIDER_DATE_COL <- "Priority Date Claim First Use"

# Boundary layers rarely change. FALSE = reuse data/boundaries.rds if present.
REFRESH_BOUNDARIES <- FALSE

WRIA_GEOJSON_URL <- paste0(
  "https://services.arcgis.com/6lCKYNJLvwTXqrmp/arcgis/rest/services/",
  "ECY/FeatureServer/11/query?outFields=*&where=1%3D1&f=geojson"
)
COUNTY_GEOJSON_URL <- paste0(
  "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/",
  "State_County/MapServer/11/query?where=STATE%3D'53'",
  "&outFields=NAME&f=geojson"
)

# ==============================================================================
# WRIA NAME LOOKUP (same as the low-flow-wr app -> labels like "17 — Quilcene-Snow")
# ==============================================================================

wria_names <- tribble(
  ~wria_num, ~wria_name,
  1L,  "Nooksack",               2L,  "San Juan",
  3L,  "Lower Skagit-Samish",    4L,  "Upper Skagit",
  5L,  "Stillaguamish",          6L,  "Island",
  7L,  "Snohomish",              8L,  "Cedar-Sammamish",
  9L,  "Duwamish-Green",        10L,  "Puyallup-White",
  11L, "Nisqually",             12L,  "Chambers-Clover",
  13L, "Deschutes",             14L,  "Kennedy-Goldsborough",
  15L, "Kitsap",                16L,  "Skokomish-Dosewallips",
  17L, "Quilcene-Snow",         18L,  "Elwha-Dungeness",
  19L, "Lyre-Hoko",             20L,  "Soleduc",
  21L, "Queets-Quinault",       22L,  "Lower Chehalis",
  23L, "Upper Chehalis",        24L,  "Willapa",
  25L, "Grays-Elochoman",       26L,  "Cowlitz",
  27L, "Lewis",                 28L,  "Salmon-Washougal",
  29L, "Wind-White Salmon",     30L,  "Klickitat",
  31L, "Rock-Glade",            32L,  "Walla Walla",
  33L, "Lower Snake",           34L,  "Palouse",
  35L, "Middle Snake",          36L,  "Esquatzel Coulee",
  37L, "Lower Yakima",          38L,  "Naches",
  39L, "Upper Yakima",          40L,  "Alkali-Squilchuck",
  41L, "Lower Crab",            42L,  "Grand Coulee",
  43L, "Upper Crab-Wilson",     44L,  "Moses Coulee",
  45L, "Wenatchee",             46L,  "Entiat",
  47L, "Chelan",                48L,  "Methow",
  49L, "Okanogan",              50L,  "Foster",
  51L, "Nespelem",              52L,  "Sanpoil",
  53L, "Lower Lake Roosevelt",  54L,  "Lower Spokane",
  55L, "Little Spokane",        56L,  "Hangman",
  57L, "Middle Spokane",        58L,  "Middle Lake Roosevelt",
  59L, "Colville",              60L,  "Kettle",
  61L, "Upper Lake Roosevelt",  62L,  "Pend Oreille"
)

# ==============================================================================
# HELPERS
# ==============================================================================

col_or_na <- function(df, name) {
  if (name %in% names(df)) df[[name]] else rep(NA_character_, nrow(df))
}

parse_date_flex <- function(x) {
  x <- na_if(str_trim(x), "")
  d <- as.Date(x, format = "%m/%d/%Y")
  iso <- is.na(d) & !is.na(x)
  d[iso] <- as.Date(x[iso], format = "%Y-%m-%d")
  d
}

newest <- function(pattern) {
  f <- list.files(WORK_DIR, pattern = pattern, full.names = TRUE)
  if (length(f) == 0) return(NULL)
  f[which.max(file.mtime(f))]
}

# Same as permit-events: points sharing a location are spread around a small
# ring (radius grows with stack size, capped ~400 m) so each can be clicked.
spread_stacked <- function(df) {
  df |>
    group_by(lon, lat) |>
    mutate(n_stack = n(), k = row_number()) |>
    ungroup() |>
    mutate(
      r_m      = if_else(n_stack > 1, pmin(80 + 15 * n_stack, 400), 0),
      ang      = 2 * pi * (k - 1) / n_stack,
      lat_sprd = lat + (r_m * sin(ang)) / 111320,
      lon_sprd = lon + (r_m * cos(ang)) / (111320 * cos(lat * pi / 180))
    ) |>
    select(-k, -r_m, -ang)
}

fetch_boundary <- function(url, label) {
  message("Downloading ", label, " ...")
  tryCatch({
    lyr <- st_read(url, quiet = TRUE) |> st_make_valid()
    lyr <- lyr |>
      st_transform(32610) |>
      st_simplify(dTolerance = 50, preserveTopology = TRUE) |>
      st_transform(4326)
    message("  ", label, ": ", nrow(lyr), " features")
    lyr
  }, error = function(e) {
    message("  WARNING - could not download ", label, ": ", e$message)
    NULL
  })
}

# One cleaned table, used for BOTH mapped and unmapped records
clean_records <- function(raw) {
  tibble(
    wr_doc_id    = col_or_na(raw, "WR Doc ID"),
    doc_number   = col_or_na(raw, "Record Document Number"),
    holder       = col_or_na(raw, "Person or Organization"),
    phase        = col_or_na(raw, "Phase"),
    stage        = col_or_na(raw, "Stage"),
    priority_raw = col_or_na(raw, "Priority Date Claim First Use"),
    purpose      = col_or_na(raw, "Purpose of Use"),
    qi           = col_or_na(raw, "Qi"),
    uom          = col_or_na(raw, "UOM"),
    qa           = col_or_na(raw, "Qa"),
    source_name  = col_or_na(raw, "First Source Name Listed"),
    WRIA         = col_or_na(raw, "WRIA"),
    county       = col_or_na(raw, "County"),
    trs          = col_or_na(raw, "TRS"),
    q            = col_or_na(raw, "Q"),
    qq           = col_or_na(raw, "QQ"),
    lon          = suppressWarnings(as.numeric(col_or_na(raw, "lon"))),
    lat          = suppressWarnings(as.numeric(col_or_na(raw, "lat"))),
    fail_reason  = col_or_na(raw, "fail_reason")
  ) |>
    mutate(
      priority_date = parse_date_flex(priority_raw),
      slider_date   = parse_date_flex(col_or_na(raw, SLIDER_DATE_COL)),
      # New vs Change. Anything unexpected is kept and reported in the summary.
      app_type = case_when(
        str_detect(phase, regex("^New Application$",    ignore_case = TRUE)) ~ "New Application",
        str_detect(phase, regex("^Change Application$", ignore_case = TRUE)) ~ "Change Application",
        TRUE ~ coalesce(phase, "Unknown")
      ),
      # Same rule as the low-flow-wr app: WRTS lists "Groundwater" as the
      # source name for wells; anything else is a named surface source.
      source_type = case_when(
        is.na(source_name) | str_trim(source_name) == "" ~ NA_character_,
        str_to_lower(str_trim(source_name)) == "groundwater" ~ "Groundwater",
        TRUE ~ "Surface Water"
      ),
      wria_num = suppressWarnings(as.integer(as.numeric(WRIA)))
    ) |>
    left_join(wria_names, by = "wria_num") |>
    mutate(wria_label = if_else(is.na(wria_num), NA_character_,
                                paste0(wria_num, " — ",
                                       coalesce(wria_name, paste0("WRIA ", wria_num)))))
}

# ==============================================================================
# STEP 1 — Read the geocoded + failed CSVs
# ==============================================================================

geo_file  <- newest("^applications_geocoded_.*\\.csv$")
fail_file <- newest("^applications_geocode_failed_.*\\.csv$")
if (is.null(geo_file)) stop("No applications_geocoded_*.csv in ", WORK_DIR,
                            ". Run geocode_records.R first.")
message("Geocoded file: ", basename(geo_file))
message("Failed file  : ", if (is.null(fail_file)) "(none)" else basename(fail_file))

read_chr <- function(f) read_csv(f, col_types = cols(.default = "c"),
                                 na = c("", "NA"), show_col_types = FALSE)

geo  <- clean_records(read_chr(geo_file))
fail <- if (is.null(fail_file)) clean_records(read_chr(geo_file)[0, ]) else
  clean_records(read_chr(fail_file))

# ==============================================================================
# STEP 2 — Split: mapped (has lon/lat AND a priority date) vs unmapped
# ------------------------------------------------------------------------------
# The cumulative slider needs a date, so a geocoded record with no priority
# date can't be placed in time. It goes to the "Not Mapped" list with a reason
# instead of being silently dropped.
# ==============================================================================

if (!SLIDER_DATE_COL %in% names(read_chr(geo_file)))
  stop("SLIDER_DATE_COL '", SLIDER_DATE_COL, "' is not a column in the export.")

no_date <- geo |> filter(is.na(slider_date)) |>
  mutate(fail_reason = paste("No", SLIDER_DATE_COL, "in WRTS"))

records <- geo |>
  filter(!is.na(lon), !is.na(lat), !is.na(slider_date)) |>
  select(-fail_reason) |>
  spread_stacked()

unmapped <- bind_rows(fail, no_date) |>
  arrange(wria_num, doc_number)

outside <- sum(records$lon < -125 | records$lon > -116.5 |
               records$lat < 45.4 | records$lat > 49.1)

# ==============================================================================
# STEP 3 — Data-currency info + save
# ==============================================================================

if (is.null(DATA_DATE)) {
  stamp <- str_extract(basename(geo_file), "20\\d{6}")
  DATA_DATE <- if (!is.na(stamp)) as.Date(stamp, "%Y%m%d") else as.Date(file.mtime(geo_file))
}

info <- list(
  data_date   = DATA_DATE,
  prepared_at = Sys.time(),
  source_file = basename(geo_file),
  n_total     = nrow(records) + nrow(unmapped),
  n_mapped    = nrow(records),
  n_unmapped  = nrow(unmapped),
  slider_col  = SLIDER_DATE_COL
)

dir.create(OUT_DIR, showWarnings = FALSE)
saveRDS(list(records = records, unmapped = unmapped, info = info),
        file.path(OUT_DIR, "applications.rds"))

# ==============================================================================
# STEP 4 — Boundary layers (WRIA + county), downloaded once
# ==============================================================================

bnd_file <- file.path(OUT_DIR, "boundaries.rds")

# Also re-download if an earlier run saved a file with a layer missing
bnd_incomplete <- file.exists(bnd_file) &&
  with(readRDS(bnd_file), is.null(wria) || is.null(county))

if (REFRESH_BOUNDARIES || !file.exists(bnd_file) || bnd_incomplete) {
  wria_sf   <- fetch_boundary(WRIA_GEOJSON_URL,   "WRIA boundaries")
  county_sf <- fetch_boundary(COUNTY_GEOJSON_URL, "WA county boundaries")

  if (!is.null(wria_sf)) {
    nr_col <- names(wria_sf)[str_detect(names(wria_sf),
                                        regex("NR$|NUM$|NUMBER$", ignore_case = TRUE))][1]
    nm_col <- names(wria_sf)[str_detect(names(wria_sf),
                                        regex("NM$|NAME$", ignore_case = TRUE))][1]
    if (is.na(nr_col)) stop("Could not find the WRIA number column in the WRIA layer.")
    wria_sf <- wria_sf |>
      mutate(WRIA = as.character(as.integer(.data[[nr_col]])),
             wria_display = paste0("WRIA ", WRIA, " — ",
                                   if (!is.na(nm_col)) .data[[nm_col]] else "")) |>
      select(WRIA, wria_display)
  }
  if (!is.null(county_sf)) county_sf <- county_sf |> select(NAME)

  # Never replace a good layer with a failed download
  old <- if (file.exists(bnd_file)) readRDS(bnd_file) else list(wria = NULL, county = NULL)
  saveRDS(list(wria   = if (is.null(wria_sf))   old$wria   else wria_sf,
               county = if (is.null(county_sf)) old$county else county_sf), bnd_file)
} else {
  message("Reusing existing ", bnd_file, " (set REFRESH_BOUNDARIES <- TRUE to re-download)")
}

bnd <- readRDS(bnd_file)

# ==============================================================================
# STEP 5 — Validation summary (check these each refresh)
# ==============================================================================

cat("\n================ APP DATA SUMMARY ================\n")
cat("Data current as of          :", format(DATA_DATE, "%m/%d/%Y"), "\n")
cat("Records in export           :", info$n_total, "\n")
cat("Mapped                      :", nrow(records), "\n")
cat("Not mapped                  :", nrow(unmapped), "\n")
if (nrow(unmapped) > 0) print(table(unmapped$fail_reason, useNA = "ifany"))
cat("Application type (mapped):\n"); print(table(records$app_type, useNA = "ifany"))
unexpected <- setdiff(unique(records$app_type), c("New Application", "Change Application"))
if (length(unexpected) > 0) cat("  <-- CHECK unexpected phase value(s):",
                                paste(unexpected, collapse = ", "), "\n")
cat("Source type (mapped):\n"); print(table(records$source_type, useNA = "ifany"))
if (nrow(records) > 0) {
  cat("Slider dates range          :", format(min(records$slider_date), "%m/%d/%Y"), "to",
      format(max(records$slider_date), "%m/%d/%Y"), "\n")
  cat("Slider dates by type (earliest / median / latest):\n")
  print(records |> group_by(app_type) |>
          summarise(earliest = min(slider_date), median = median(slider_date),
                    latest = max(slider_date), .groups = "drop"))
  n_old_chg <- sum(records$app_type == "Change Application" &
                   records$slider_date < as.Date("1990-01-01"))
  if (n_old_chg > 0) cat("  NOTE:", n_old_chg, "change applications dated before 1990 -",
                         "likely the ORIGINAL right's priority date, not the filing date\n")
}
cat("Points outside WA bounds    :", outside, if (outside > 0) "  <-- CHECK" else "", "\n")
cat("WRIA polygons               :", if (is.null(bnd$wria)) "MISSING" else nrow(bnd$wria),
    " (expect 62)\n")
cat("County polygons             :", if (is.null(bnd$county)) "MISSING" else nrow(bnd$county),
    " (expect 39)\n")
cat("==================================================\n")
