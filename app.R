# ==============================================================================
# app.R - WA Water Right Applications: Active New & Change Applications (Shiny)
# ==============================================================================
# Maps every ACTIVE New Application and Change Application in Ecology's Water
# Rights Tracking System (WRTS), geocoded to its PLSS section / quarter /
# quarter-quarter. A cumulative time slider shows the applications dated on or
# before the slider date, and the map can be exported to KMZ for Google Earth.
#
# DATA is prepared BEFORE publication by pipeline/run_pipeline.R (weekly, on
# GitHub Actions) - users never upload anything. The app reads only:
#   data/applications.rds   records, unmapped records, "data current as of" info
#   data/boundaries.rds     WRIA + county polygons
# PLSS townships/sections are drawn live from BLM's CadNSDI WMS service.
#
# FOLDER LAYOUT (RStudio project folder = the GitHub repo jeffmarti/wr-applications):
#   app.R                          <- this file             (deploy)
#   data/applications.rds                                    (deploy)
#   data/boundaries.rds                                      (deploy)
#   manifest.json                  <- for Posit Connect Cloud (deploy)
#   pipeline/                      <- refresh scripts + cache (do NOT deploy)
#   .github/workflows/refresh-data.yml
#   .Renviron                      <- CARTO_API_KEY=...  (do NOT commit to GitHub)
#
# REQUIRED PACKAGES:
# install.packages(c("shiny","leaflet","dplyr","readr","sf","DT","plotly",
#                    "htmltools","purrr","zip"))
# ==============================================================================

library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(DT)
library(plotly)
library(htmltools)
library(purrr)
library(zip)

# ==============================================================================
# SETTINGS
# ==============================================================================

APP_DATA_FILE <- "data/applications.rds"
BOUNDARY_FILE <- "data/boundaries.rds"

TYPE_NEW <- "New Application"
TYPE_CHG <- "Change Application"
TYPE_ORDER <- c(TYPE_NEW, TYPE_CHG)

# Map colours - the SAME colours as the PLSS Geocoder KMZ styles:
#   New Application    KML ff0000ff -> #FF0000 (red)
#   Change Application KML ffe6d8ad -> #ADD8E6 (light blue)
# Light blue is pale on the Light basemap, so map markers get a dark outline.
COL_NEW     <- "#FF0000"
COL_CHG     <- "#ADD8E6"
COL_OUTLINE <- "#1f2a44"
KML_COL_NEW <- "ff0000ff"
KML_COL_CHG <- "ffe6d8ad"
TYPE_COLS   <- c(COL_NEW, COL_CHG)   # in TYPE_ORDER

COL_WRIA   <- "#1a3a5c"
COL_COUNTY <- "#6a0dad"

MIN_ZOOM   <- 6
WA_VIEW    <- c(lng1 = -124.8, lat1 = 45.5, lng2 = -116.9, lat2 = 49.0)
WA_MAXBNDS <- c(lng1 = -127.5, lat1 = 44.3, lng2 = -114.5, lat2 = 50.3)

# CARTO basemap key - read from .Renviron locally, and from the Variables
# section of this app's settings on Posit Connect Cloud. Never typed here.
CARTO_KEY <- Sys.getenv("CARTO_API_KEY")
message("CARTO_API_KEY ", if (nzchar(CARTO_KEY))
  paste0("found (", nchar(CARTO_KEY), " characters)") else
    "NOT found - Light basemap will show CARTO's watermark")
CARTO_ATTRIB <- paste0(
  "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors, ",
  "&copy; <a href='https://carto.com/attributions'>CARTO</a>"
)

BLM_PLSS_WMS <- paste0("https://gis.blm.gov/arcgis/services/Cadastral/",
                       "BLM_Natl_PLSS_CadNSDI/MapServer/WMSServer")

WRTS_RECORD_URL <- paste0(
  "https://appswr.ecology.wa.gov/waterrighttrackingsystem/WaterRights/",
  "WaterRightRecord.aspx?waRecId="
)

# ==============================================================================
# LOAD PREPARED DATA (once, at startup)
# ==============================================================================

if (!file.exists(APP_DATA_FILE)) {
  stop("Missing ", APP_DATA_FILE, ". Run pipeline/run_pipeline.R first.")
}
app_data  <- readRDS(APP_DATA_FILE)
RECORDS   <- app_data$records
UNMAPPED  <- app_data$unmapped
DATA_INFO <- app_data$info

boundaries <- if (file.exists(BOUNDARY_FILE)) readRDS(BOUNDARY_FILE) else
  list(wria = NULL, county = NULL)
WRIA_SF   <- boundaries$wria
COUNTY_SF <- boundaries$county

wria_tbl       <- RECORDS |> distinct(wria_num, wria_label) |> filter(!is.na(wria_num)) |>
  arrange(wria_num)
WRIA_CHOICES   <- setNames(as.character(wria_tbl$wria_num), wria_tbl$wria_label)
COUNTY_CHOICES <- sort(unique(na.omit(RECORDS$county)))
# "Not Recorded" = no First Source Name Listed in WRTS (source_type is NA)
SOURCE_NA      <- "Not Recorded"
SOURCE_CHOICES <- c("Surface Water", "Groundwater", SOURCE_NA)

# Slider: Jan 1 of the earliest year -> data date (cumulative, starts at the end)
SLIDER_MIN <- as.Date(format(min(RECORDS$slider_date), "%Y-01-01"))
SLIDER_MAX <- max(DATA_INFO$data_date, max(RECORDS$slider_date))

DATA_DATE_TXT <- format(DATA_INFO$data_date, "%m/%d/%Y")
SLIDER_LABEL  <- if (identical(DATA_INFO$slider_col, "Priority Date Claim First Use"))
  "priority date" else tolower(DATA_INFO$slider_col)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

esc <- function(x) if_else(is.na(x) | x == "", "&mdash;", htmlEscape(x))

fmt_date <- function(d) if_else(is.na(d), "&mdash;", format(d, "%m-%d-%Y"))

# The popup rows Jeff specified (same layout as the low-flow-wr app)
popup_fields <- function(df) {
  list(
    "Water Right No." = paste0("<a href='", WRTS_RECORD_URL, df$wr_doc_id,
                               "' target='_blank'>", esc(df$doc_number), "</a>"),
    "Holder"          = esc(df$holder),
    "Purpose"         = esc(df$purpose),
    "Priority Date"   = fmt_date(df$priority_date),
    "Diversion Rate"  = if_else(is.na(df$qi), "&mdash;",
                                paste(htmlEscape(df$qi), coalesce(df$uom, ""))),
    "Annual Vol."     = paste0(esc(df$qa), " ac-ft"),
    "Water Source"    = esc(df$source_name),
    "Source Type"     = esc(coalesce(df$source_type, SOURCE_NA)),
    "WRIA"            = esc(df$wria_label),
    "County"          = esc(df$county),
    "QQ Q / TRS"      = esc(trimws(gsub("\\s+", " ",
                                        paste(coalesce(df$qq, ""), coalesce(df$q, ""), coalesce(df$trs, ""))))),
    "Phase / Stage"   = paste0(esc(df$phase), " / ", esc(df$stage))
  )
}

build_popups <- function(df) {
  f <- popup_fields(df)
  rows <- map2(names(f), f, function(lbl, val) paste0(
    "<tr><td style='padding:3px 8px 3px 0;color:#52514e;white-space:nowrap;",
    "border-bottom:1px solid #eee;'><b>", lbl, "</b></td>",
    "<td style='padding:3px 0;border-bottom:1px solid #eee;'>", val, "</td></tr>"))
  paste0("<div style='font-family:Arial;font-size:12px;min-width:240px;max-width:330px;'>",
         "<table style='border-collapse:collapse;width:100%;'>",
         reduce(rows, paste0), "</table></div>")
}

# ==============================================================================
# KMZ BUILDER
# ------------------------------------------------------------------------------
# From the PLSS Geocoder (WRIA / phase folders, CDATA popup tables, Ecology
# record link, failed-records CSV bundled in the KMZ) and the low-flow-wr app
# (fixed popup field list, filtered-data export with progress message).
#
# Time: each placemark gets <TimeSpan><begin> = its slider date and no end, so
# in Google Earth it appears at that date and stays visible afterwards -
# the same CUMULATIVE behaviour as this app's slider. (A <TimeStamp> would
# instead show it only near that one date.)
# ==============================================================================

kml_styles <- paste0(
  '  <Style id="style_new_application">\n',
  '    <IconStyle><color>', KML_COL_NEW, '</color><scale>1.0</scale>\n',
  '      <Icon><href>http://maps.google.com/mapfiles/kml/shapes/placemark_square.png</href></Icon>\n',
  '    </IconStyle>\n',
  '    <LabelStyle><scale>0.6</scale></LabelStyle>\n',
  '  </Style>\n',
  '  <Style id="style_change_application">\n',
  '    <IconStyle><color>', KML_COL_CHG, '</color><scale>1.0</scale>\n',
  '      <Icon><href>http://maps.google.com/mapfiles/kml/shapes/placemark_square.png</href></Icon>\n',
  '    </IconStyle>\n',
  '    <LabelStyle><scale>0.6</scale></LabelStyle>\n',
  '  </Style>'
)

kml_description <- function(df) {
  f <- popup_fields(df)
  f[["Water Right No."]] <- esc(df$doc_number)    # link goes above the table instead
  rows <- map2(names(f), f, function(lbl, val)
    paste0("<tr><td><b>", lbl, "</b></td><td>", val, "</td></tr>"))
  paste0("<![CDATA[",
         "<a href='", WRTS_RECORD_URL, df$wr_doc_id, "'>View Water Right Record</a><br/><br/>",
         "<table border='1' cellpadding='4' cellspacing='0' ",
         "style='font-size:12px;font-family:Arial'>",
         reduce(rows, paste0), "</table>]]>")
}

kml_placemarks <- function(df, indent) {
  if (nrow(df) == 0) return(character())
  style <- if_else(df$app_type == TYPE_NEW, "#style_new_application", "#style_change_application")
  paste0(
    indent, "<Placemark>\n",
    indent, "  <name>", htmlEscape(df$doc_number), "</name>\n",
    indent, "  <styleUrl>", style, "</styleUrl>\n",
    indent, "  <TimeSpan><begin>", format(df$slider_date, "%Y-%m-%d"), "</begin></TimeSpan>\n",
    indent, "  <description>", kml_description(df), "</description>\n",
    indent, "  <Point><coordinates>", df$lon, ",", df$lat, ",0</coordinates></Point>\n",
    indent, "</Placemark>"
  )
}

kml_folder <- function(name, inner, indent) {
  if (length(inner) == 0) return(character())
  paste0(indent, "<Folder>\n", indent, "  <name>", htmlEscape(name), "</name>\n",
         paste(inner, collapse = "\n"), "\n", indent, "</Folder>")
}

# hierarchy: "wria_type" (WRIA -> application type) or "type_wria"
build_kmz <- function(df, hierarchy = "wria_type", unmapped = NULL, title = "") {
  df <- df |> arrange(wria_num, app_type, doc_number)
  wria_name_of <- function(w) coalesce(df$wria_label[match(w, df$wria_num)], "WRIA unknown")
  
  folders <- if (hierarchy == "wria_type") {
    map_chr(unique(df$wria_num), function(w) {
      grp   <- df[df$wria_num %in% w, ]
      inner <- unlist(map(TYPE_ORDER, function(t)
        kml_folder(t, kml_placemarks(grp[grp$app_type == t, ], "        "), "    ")))
      kml_folder(wria_name_of(w), inner, "  ")
    })
  } else {
    unlist(map(TYPE_ORDER, function(t) {
      grp   <- df[df$app_type == t, ]
      inner <- unlist(map(unique(grp$wria_num), function(w)
        kml_folder(wria_name_of(w), kml_placemarks(grp[grp$wria_num %in% w, ], "        "), "    ")))
      kml_folder(t, inner, "  ")
    }))
  }
  
  kml_doc <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2">\n',
    '<Document>\n',
    '  <name>', htmlEscape(title), '</name>\n',
    '  <description><![CDATA[Active water right applications from WA Ecology WRTS, ',
    'data current as of ', DATA_DATE_TXT, '. Red = New Application | ',
    'Light blue = Change Application. Generated ', format(Sys.Date(), "%m/%d/%Y"), '.]]></description>\n',
    '  <LookAt>\n',
    '    <longitude>-120.5</longitude><latitude>47.4</latitude>\n',
    '    <altitude>0</altitude><range>800000</range>\n',
    if (nrow(df) > 0) paste0(
      '    <gx:TimeSpan><begin>', format(min(df$slider_date), "%Y-%m-%d"),
      '</begin><end>', format(max(df$slider_date), "%Y-%m-%d"), '</end></gx:TimeSpan>\n') else "",
    '  </LookAt>\n',
    kml_styles, "\n",
    paste(folders, collapse = "\n"), "\n",
    '</Document>\n</kml>'
  )
  
  dir <- tempfile("kmz_"); dir.create(dir)
  kml_path <- file.path(dir, "doc.kml")
  writeLines(kml_doc, kml_path, useBytes = TRUE)
  files <- kml_path
  if (!is.null(unmapped) && nrow(unmapped) > 0) {
    um_path <- file.path(dir, "not_mapped.csv")
    write_csv(unmapped_export(unmapped), um_path, na = "")
    files <- c(files, um_path)
  }
  kmz_path <- tempfile(fileext = ".kmz")
  zip::zip(zipfile = kmz_path, files = files, mode = "cherry-pick")
  kmz_path
}

# Plain table layout used by the CSV downloads
records_export <- function(df) {
  df |>
    arrange(slider_date) |>
    transmute(
      `Application Type`       = app_type,
      `Record Document Number` = doc_number,
      `WR Doc ID`              = wr_doc_id,
      `Ecology Record URL`     = paste0(WRTS_RECORD_URL, wr_doc_id),
      `Person or Organization` = holder,
      `Purpose of Use`         = purpose,
      `Priority Date`          = format(priority_date, "%m/%d/%Y"),
      Qi = qi, UOM = uom, Qa = qa,
      `Water Source`           = source_name,
      `Source Type`            = coalesce(source_type, SOURCE_NA),
      WRIA = wria_label, County = county,
      Phase = phase, Stage = stage,
      TRS = trs, Q = q, QQ = qq,
      lon = round(lon, 6), lat = round(lat, 6)
    )
}

unmapped_export <- function(df) {
  df |>
    transmute(
      `Reason not mapped`      = fail_reason,
      `Application Type`       = app_type,
      `Record Document Number` = doc_number,
      `WR Doc ID`              = wr_doc_id,
      `Ecology Record URL`     = paste0(WRTS_RECORD_URL, wr_doc_id),
      `Person or Organization` = holder,
      `Purpose of Use`         = purpose,
      `Priority Date`          = priority_raw,
      `Water Source`           = source_name,
      WRIA = wria_label, County = county,
      TRS = trs, Q = q, QQ = qq,
      Phase = phase, Stage = stage
    )
}

legend_html <- paste0(
  "<div style='background:white;padding:8px 10px;border-radius:4px;",
  "font-family:Arial;font-size:12px;box-shadow:0 1px 4px rgba(0,0,0,.3);'>",
  "<div style='font-weight:bold;margin-bottom:4px;'>Active applications</div>",
  "<div><svg width='14' height='14'><circle cx='7' cy='7' r='5.5' fill='", COL_NEW,
  "' stroke='", COL_OUTLINE, "' stroke-width='1'/></svg> New Application</div>",
  "<div><svg width='14' height='14'><circle cx='7' cy='7' r='5.5' fill='", COL_CHG,
  "' stroke='", COL_OUTLINE, "' stroke-width='1'/></svg> Change Application</div>",
  "</div>"
)

# ==============================================================================
# CSS - navy-to-blue banner, matching the other apps in the suite
# ==============================================================================

app_css <- "
  .app-banner {
    background: linear-gradient(90deg, #0b2545 0%, #13315c 45%, #1d6fb8 100%);
    color: white; padding: 14px 20px; margin: 0 -15px 15px -15px;
  }
  .app-banner h2 { margin: 0; font-size: 22px; font-weight: 600; }
  .app-banner p  { margin: 2px 0 0 0; font-size: 13px; opacity: .85; }
  .stat-row { display: flex; gap: 12px; margin-bottom: 10px; flex-wrap: wrap; }
  .stat-tile {
    flex: 1; min-width: 150px; background: #fcfcfb; border: 1px solid #e3e2de;
    border-radius: 6px; padding: 8px 12px;
  }
  .stat-tile .num { font-size: 26px; font-weight: 600; color: #0b0b0b; line-height: 1.1; }
  .stat-tile .lbl { font-size: 12px; color: #52514e; }
  .play-row { display: flex; gap: 6px; align-items: center; margin-top: -6px; margin-bottom: 10px; }
  .play-row .form-group { margin-bottom: 0; }
  .navbar { margin-bottom: 0; }
  .data-note { font-size: 12px; color: #52514e; margin-bottom: 4px; }
  .dl-btn { width: 100%; margin-bottom: 6px; text-align: left; }
"

# ==============================================================================
# UI
# ==============================================================================

ui <- function(request) {
  navbarPage(
    title = "WR Applications",
    header = tags$head(tags$style(HTML(app_css))),
    
    # ---- Tab 1: Map ---------------------------------------------------------
    tabPanel(
      "Map",
      div(class = "app-banner",
          h2("WA Water Right Applications \u2014 Active New & Change Applications"),
          p("Pending applications in Ecology's Water Rights Tracking System",
            HTML("&nbsp;&middot;&nbsp;"), "Data current as of ", strong(DATA_DATE_TXT))),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          h4("Time slider"),
          helpText(style = "margin-top:-4px;",
                   paste0("Shows applications with a ", SLIDER_LABEL,
                          " on or before the slider date (cumulative).")),
          sliderInput("thru", NULL, min = SLIDER_MIN, max = SLIDER_MAX, value = SLIDER_MAX,
                      step = 1, timeFormat = "%m/%d/%Y", ticks = FALSE, width = "100%"),
          div(class = "play-row",
              actionButton("play", "Play", icon = icon("play"), class = "btn-sm"),
              actionButton("go_end", "All", class = "btn-sm"),
              selectInput("play_step", NULL, width = "120px",
                          choices = c("1 month" = "1 month", "1 year" = "1 year",
                                      "5 years" = "5 years"),
                          selected = "1 year")),
          
          hr(),
          h4("Filters"),
          checkboxGroupInput("type_show", "Application type",
                             choices = TYPE_ORDER, selected = TYPE_ORDER),
          checkboxGroupInput("source_show", "Source type",
                             choices = SOURCE_CHOICES, selected = SOURCE_CHOICES),
          selectizeInput("wria_sel", "WRIA", choices = WRIA_CHOICES, multiple = TRUE,
                         options = list(placeholder = "All WRIAs (map zooms to selection)")),
          selectizeInput("county_sel", "County", choices = COUNTY_CHOICES, multiple = TRUE,
                         options = list(placeholder = "All counties")),
          checkboxInput("spread", "Spread stacked points (same location)", value = TRUE),
          
          hr(),
          h4("Map overlays"),
          checkboxInput("show_wria",   "WRIA boundaries",       value = TRUE),
          checkboxInput("show_county", "County boundaries",     value = FALSE),
          checkboxInput("show_plss",   "PLSS / TRS grid (BLM)", value = FALSE),
          helpText(style = "font-size:11px;margin-top:-6px;",
                   "Zoom to level 10+ for section lines to render."),
          
          hr(),
          h4("Download"),
          radioButtons("kmz_hierarchy", "KMZ folder grouping",
                       choices = c("WRIA \u2192 Application type" = "wria_type",
                                   "Application type \u2192 WRIA" = "type_wria"),
                       selected = "wria_type"),
          downloadButton("dl_kmz_shown", "KMZ \u2014 as shown on map",
                         icon = icon("earth-americas"), class = "btn-sm btn-primary dl-btn"),
          downloadButton("dl_kmz_all", "KMZ \u2014 all active applications",
                         icon = icon("earth-americas"), class = "btn-sm dl-btn"),
          downloadButton("dl_csv_shown", "CSV \u2014 as shown on map",
                         icon = icon("file-csv"), class = "btn-sm dl-btn"),
          helpText(style = "font-size:11px;",
                   "\u201CAs shown\u201D uses the current slider date and filters.",
                   "\u201CAll\u201D ignores them and also bundles the not-mapped",
                   "records as a CSV inside the KMZ. Open KMZ files in",
                   "Google Earth Pro for the time slider and popups."),
          
          hr(),
          h4("Data"),
          uiOutput("data_info")
        ),
        
        mainPanel(
          width = 9,
          uiOutput("stat_tiles"),
          leafletOutput("map", height = 560),
          br(),
          h4(paste0("Applications per year of ", SLIDER_LABEL, " (current filters, incl. not mapped)"),
             style = "margin-top:4px;"),
          plotlyOutput("timeline", height = 260)
        )
      )
    ),
    
    # ---- Tab 2: Table -------------------------------------------------------
    tabPanel(
      "Table",
      br(),
      p("Records currently shown on the map (slider date and filters applied)."),
      DTOutput("table")
    ),
    
    # ---- Tab 3: Not mapped --------------------------------------------------
    tabPanel(
      "Not Mapped",
      br(),
      p("Active applications that could not be placed on the map. Common causes:",
        "no TRS recorded in WRTS, a TRS that has no matching BLM PLSS section",
        "(e.g. Donation Land Claims, typos), or no ", SLIDER_LABEL, " to place it in time."),
      downloadButton("dl_unmapped", "Download not-mapped records (CSV)", class = "btn-sm"),
      br(), br(),
      DTOutput("unmapped_table")
    ),
    
    # ---- Tab 4: About -------------------------------------------------------
    tabPanel(
      "About",
      fluidRow(column(
        width = 8, offset = 1,
        br(),
        h3("About this app"),
        
        h4("What this app shows"),
        p("Each point is a water right application with Status = Active and Phase = ",
          strong("New Application"), " (red) or ", strong("Change Application"),
          " (light blue) in the Department of Ecology's Water Rights Tracking System (WRTS)."),
        
        h4("Time slider"),
        p("The slider is cumulative: it shows every application whose ", SLIDER_LABEL,
          " falls on or before the slider date. Press ", strong("Play"),
          " to watch applications accumulate, or ", strong("All"), " to show everything.",
          "Only applications that are still active today are included, so this is not",
          "a history of everything ever filed: applications already decided or withdrawn",
          "do not appear at any slider date."),
        p("Some applications have been pending for decades; the oldest active application",
          "in the state dates to 1936."),
        
        h4("Summary tiles and bar chart"),
        p("The tiles and the bar chart count ", strong("all"), " active applications that match",
          "the filters, including those that could not be mapped. The tiles count applications",
          "dated on or before the slider date (not-mapped records with no date are always counted).",
          "Each bar is one year, New stacked on Change; years after the slider date are faded."),
        
        h4("Source type"),
        p(strong("Groundwater"), " = WRTS lists \u201CGroundwater\u201D as the first source;",
          strong("Surface Water"), " = a named surface source;",
          strong("Not Recorded"), " = no source name is recorded in WRTS."),
        
        h4("Locations"),
        p("Points are placed at the PLSS section, quarter, or quarter-quarter centroid from",
          "each record's TRS description, so locations are approximate. Records sharing a",
          "location are arranged in a small circle so each can be selected; uncheck ",
          em("Spread stacked points"), " to show true centroids. Records that could not be",
          "mapped are listed on the ", strong("Not Mapped"), " tab."),
        
        h4("KMZ downloads"),
        p("KMZ files open in Google Earth Pro (desktop). Placemarks are grouped in folders by",
          "WRIA and application type, each popup links to the Ecology record, and Google",
          "Earth's time slider works the same cumulative way as this app's slider."),
        
        h4("Map overlays"),
        p("Basemaps include the USGS National Map Hydro layer (streams and water bodies).",
          "WRIA boundaries: WA Department of Ecology. County boundaries: US Census Bureau",
          "TIGERweb. PLSS grid: drawn live from the",
          tags$a("BLM National PLSS CadNSDI service",
                 href = "https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer",
                 target = "_blank", .noWS = "after"), "."),
        
        h4("Data source and currency"),
        p("Data is sourced from the WRTS ",
          tags$a("Water Right Record Search",
                 href = "https://appswr.ecology.wa.gov/waterrighttrackingsystem/WaterRights/WaterRightSearch.aspx",
                 target = "_blank"),
          " (Phase = New Application or Change Application; Status = Active) and geocoded",
          "against the BLM CadNSDI PLSS layer. ",
          strong(paste0("Data is current as of ", DATA_DATE_TXT, ".")),
          " Data is refreshed weekly."),
        
        h4("Companion app"),
        p(tags$a("Water Right Application Waiting Times",
                 href = "https://waterwater-wr-app-data.share.connect.posit.cloud/",
                 target = "_blank"),
          " \u2014 how long water right applications have been waiting for a decision,",
          "a natural companion to this map of the applications themselves."),
        
        h4("Questions and comments"),
        p(tags$a("jeffjmarti@gmail.com", href = "mailto:jeffjmarti@gmail.com")),
        br()
      ))
    )
  )
}

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # --------------------------------------------------------------------------
  # Data panel in the sidebar
  # --------------------------------------------------------------------------
  
  output$data_info <- renderUI({
    i <- DATA_INFO
    div(class = "data-note",
        strong(paste("Data current as of", DATA_DATE_TXT)), br(),
        format(i$n_mapped, big.mark = ","), " applications mapped", br(),
        if (i$n_unmapped > 0)
          tagList(format(i$n_unmapped, big.mark = ","), " not mapped (see Not Mapped tab)", br()),
        "Source: WA Ecology WRTS")
  })
  
  # --------------------------------------------------------------------------
  # Play / All buttons
  # --------------------------------------------------------------------------
  
  playing <- reactiveVal(FALSE)
  stop_play <- function() {
    playing(FALSE)
    updateActionButton(session, "play", label = "Play", icon = icon("play"))
  }
  
  observeEvent(input$play, {
    # Pressing Play at the end starts again from the beginning
    if (!playing() && as.Date(input$thru) >= SLIDER_MAX) {
      updateSliderInput(session, "thru", value = SLIDER_MIN)
    }
    playing(!playing())
    updateActionButton(session, "play",
                       label = if (playing()) "Pause" else "Play",
                       icon  = icon(if (playing()) "pause" else "play"))
  })
  
  observeEvent(input$go_end, {
    stop_play()
    updateSliderInput(session, "thru", value = SLIDER_MAX)
  })
  
  observe({
    req(playing())
    invalidateLater(900)
    isolate({
      req(input$thru)
      nxt <- seq(as.Date(input$thru), by = input$play_step, length.out = 2)[2]
      if (nxt >= SLIDER_MAX) {
        updateSliderInput(session, "thru", value = SLIDER_MAX)
        stop_play()
      } else {
        updateSliderInput(session, "thru", value = nxt)
      }
    })
  })
  
  # --------------------------------------------------------------------------
  # Filtering
  #   base_filtered(): attribute filters only (independent of the date)
  #   shown():         + slider date (cumulative)
  # --------------------------------------------------------------------------
  
  thru <- reactive({
    req(input$thru)
    as.Date(input$thru)
  })
  
  # Same attribute filters for mapped (RECORDS) and not-mapped (UNMAPPED) records
  apply_filters <- function(df) {
    df <- df |> filter(app_type %in% input$type_show,
                       coalesce(source_type, SOURCE_NA) %in% input$source_show)
    if (length(input$wria_sel)   > 0) df <- df |> filter(as.character(wria_num) %in% input$wria_sel)
    if (length(input$county_sel) > 0) df <- df |> filter(county %in% input$county_sel)
    df
  }
  
  base_filtered <- reactive(apply_filters(RECORDS))
  
  # Mapped records on the map: attribute filters + slider date
  shown <- reactive({
    base_filtered() |> filter(slider_date <= thru())
  })
  
  # Not-mapped records under the same filters. Records with no date can't be
  # placed in time, so they are always counted.
  unmapped_filtered <- reactive(apply_filters(UNMAPPED))
  unmapped_thru <- reactive({
    unmapped_filtered() |> filter(is.na(slider_date) | slider_date <= thru())
  })
  
  # Mapped + not mapped together - used by the tiles and the bar chart
  all_filtered <- reactive({
    bind_rows(base_filtered(), unmapped_filtered()) |> filter(!is.na(slider_date))
  })
  
  # --------------------------------------------------------------------------
  # Stat tiles
  # --------------------------------------------------------------------------
  
  # Tiles count mapped AND not-mapped applications (same filters + slider date)
  output$stat_tiles <- renderUI({
    um     <- unmapped_thru()
    df     <- bind_rows(shown(), um)
    n_new  <- sum(df$app_type == TYPE_NEW)
    n_chg  <- sum(df$app_type == TYPE_CHG)
    yr_ago <- seq(thru(), by = "-1 year", length.out = 2)[2]
    n_last <- sum(df$slider_date > yr_ago & df$slider_date <= thru(), na.rm = TRUE)
    
    tile <- function(num, lbl, swatch = NULL) {
      div(class = "stat-tile",
          div(class = "num", format(num, big.mark = ",")),
          div(class = "lbl", if (!is.null(swatch)) HTML(swatch), lbl))
    }
    sw <- function(col) paste0("<span style='color:", col,
                               ";text-shadow:0 0 1px #1f2a44;'>&#9679;</span> ")
    
    div(class = "stat-row",
        tile(nrow(df), paste("Applications through", format(thru(), "%m/%d/%Y"))),
        tile(n_new, "New applications", sw(COL_NEW)),
        tile(n_chg, "Change applications", sw(COL_CHG)),
        tile(nrow(um), "Not mapped (counted, not on map)"),
        tile(n_last, paste0("With a ", SLIDER_LABEL, " in the 12 months before the slider date")))
  })
  
  # --------------------------------------------------------------------------
  # Map - base, boundaries and PLSS drawn once; points redrawn via proxy.
  #   basemap (200) < PLSS grid (350) < boundaries (410) < points (420)
  # --------------------------------------------------------------------------
  
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = MIN_ZOOM,
                                          maxBoundsViscosity = 1.0)) |>
      addMapPane("plss",       zIndex = 350) |>
      addMapPane("boundaries", zIndex = 410) |>
      addMapPane("points",     zIndex = 420)
    
    m <- if (nzchar(CARTO_KEY)) {
      m |> addTiles(
        urlTemplate = paste0("https://basemaps.cartocdn.com/rastertiles/light_all/",
                             "{z}/{x}/{y}.png?key=", CARTO_KEY),
        attribution = CARTO_ATTRIB, group = "Light")
    } else {
      m |> addProviderTiles(providers$CartoDB.Positron, group = "Light")
    }
    
    m <- m |>
      addTiles(
        urlTemplate = paste0("https://basemap.nationalmap.gov/arcgis/rest/services/",
                             "USGSHydroCached/MapServer/tile/{z}/{y}/{x}"),
        attribution = "USGS The National Map | Hydro Cached",
        options = tileOptions(maxNativeZoom = 16), group = "USGS Hydro") |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") |>
      addWMSTiles(
        baseUrl = BLM_PLSS_WMS,
        layers  = "1,2",
        options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                 opacity = 0.80, version = "1.3.0", pane = "plss"),
        attribution = "PLSS grid: BLM National CadNSDI",
        group = "PLSS grid"
      )
    
    if (!is.null(WRIA_SF)) {
      m <- m |> addPolygons(
        data = WRIA_SF, group = "WRIA boundaries",
        color = COL_WRIA, weight = 1.5, fillColor = COL_WRIA, fillOpacity = 0.04,
        label = ~wria_display,
        labelOptions = labelOptions(style = list("font-size" = "11px", "font-weight" = "bold"),
                                    sticky = TRUE),
        highlightOptions = highlightOptions(color = "#2e6da4", weight = 2.5,
                                            fillOpacity = 0.10, bringToFront = FALSE),
        options = pathOptions(pane = "boundaries")
      )
    }
    if (!is.null(COUNTY_SF)) {
      m <- m |> addPolygons(
        data = COUNTY_SF, group = "County boundaries",
        color = COL_COUNTY, weight = 1.5, fillColor = COL_COUNTY, fillOpacity = 0.03,
        label = ~paste(NAME, "County"),
        labelOptions = labelOptions(style = list("font-size" = "11px"), sticky = TRUE),
        highlightOptions = highlightOptions(color = COL_COUNTY, weight = 2.5,
                                            fillOpacity = 0.08, bringToFront = FALSE),
        options = pathOptions(pane = "boundaries")
      )
    }
    
    m |>
      addLayersControl(baseGroups = c("Light", "USGS Hydro", "Topographic", "Imagery"),
                       position = "topleft",
                       options = layersControlOptions(collapsed = TRUE)) |>
      fitBounds(WA_VIEW[["lng1"]], WA_VIEW[["lat1"]], WA_VIEW[["lng2"]], WA_VIEW[["lat2"]]) |>
      setMaxBounds(WA_MAXBNDS[["lng1"]], WA_MAXBNDS[["lat1"]],
                   WA_MAXBNDS[["lng2"]], WA_MAXBNDS[["lat2"]]) |>
      hideGroup(c("County boundaries", "PLSS grid")) |>
      addControl(HTML(legend_html), position = "bottomright") |>
      addScaleBar(position = "bottomleft")
  })
  
  # input$map_zoom first appears once the map has drawn -> "map is ready"
  map_ready <- reactiveVal(FALSE)
  observeEvent(input$map_zoom, map_ready(TRUE), once = TRUE)
  
  observe({
    req(map_ready())
    proxy <- leafletProxy("map")
    toggle <- function(p, show, group) if (isTRUE(show)) showGroup(p, group) else hideGroup(p, group)
    proxy |>
      toggle(input$show_wria,   "WRIA boundaries") |>
      toggle(input$show_county, "County boundaries") |>
      toggle(input$show_plss,   "PLSS grid")
  })
  
  observeEvent(input$wria_sel, {
    req(map_ready())
    proxy <- leafletProxy("map") |> clearGroup("WRIA selected")
    sel <- input$wria_sel
    if (length(sel) > 0 && !is.null(WRIA_SF)) {
      sel_sf <- WRIA_SF[WRIA_SF$WRIA %in% sel, ]
      if (nrow(sel_sf) > 0) {
        bb <- st_bbox(sel_sf)
        proxy |>
          addPolylines(data = sel_sf, group = "WRIA selected", color = COL_WRIA,
                       weight = 3.5, opacity = 0.9,
                       options = pathOptions(pane = "boundaries", interactive = FALSE)) |>
          flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
    } else {
      proxy |> flyToBounds(WA_VIEW[["lng1"]], WA_VIEW[["lat1"]],
                           WA_VIEW[["lng2"]], WA_VIEW[["lat2"]])
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Points
  observe({
    req(map_ready())
    df <- shown()
    use_spread <- isTRUE(input$spread)
    
    proxy <- leafletProxy("map") |> clearGroup("points")
    if (nrow(df) == 0) return()
    
    df <- df |>
      arrange(desc(app_type == TYPE_CHG)) |>          # new (red) drawn on top
      mutate(x = if (use_spread) lon_sprd else lon,
             y = if (use_spread) lat_sprd else lat,
             fill_col = if_else(app_type == TYPE_NEW, COL_NEW, COL_CHG))
    
    proxy |>
      addCircleMarkers(
        data = df, lng = ~x, lat = ~y, group = "points",
        radius = 6, color = COL_OUTLINE, weight = 1, opacity = 0.9,
        fillColor = ~fill_col, fillOpacity = 0.95,
        popup = build_popups(df),
        label = ~paste0(doc_number, " \u2014 ", coalesce(holder, "(no name)")),
        options = pathOptions(pane = "points")
      )
  })
  
  # --------------------------------------------------------------------------
  # Stacked bar chart - applications per year (mapped + not mapped, current
  # filters), New stacked on Change. Years after the slider date are faded,
  # and a dotted line marks the slider date. When the slider moves, only the
  # fading and the line change (plotlyProxy, no full redraw, no flicker).
  # --------------------------------------------------------------------------
  
  thru_shape <- function(d) {
    list(list(type = "line", x0 = d, x1 = d, yref = "paper", y0 = 0, y1 = 1,
              line = list(color = "#52514e", width = 1.5, dash = "dot")))
  }
  
  # 1 for years up to the slider year, faded after it
  bar_opacity <- function(years, d) {
    ifelse(years <= as.integer(format(d, "%Y")), 1, 0.25)
  }
  
  chart_years <- reactiveVal(NULL)
  
  output$timeline <- renderPlotly({
    df <- all_filtered()
    req(nrow(df) > 0)
    d0 <- isolate(thru())
    
    yr  <- as.integer(format(df$slider_date, "%Y"))
    yrs <- seq(min(yr), max(yr))
    chart_years(yrs)
    cnt   <- function(t) as.integer(table(factor(yr[df$app_type == t], levels = yrs)))
    x_mid <- as.Date(paste0(yrs, "-07-01"))          # bar centred mid-year
    bar_w <- 0.82 * 365.25 * 86400000                # bar width in ms (date axis)
    op    <- bar_opacity(yrs, d0)
    
    plot_ly(x = x_mid) |>
      add_bars(y = cnt(TYPE_CHG), name = TYPE_CHG, width = bar_w, customdata = yrs,
               marker = list(color = COL_CHG, opacity = op,
                             line = list(color = COL_OUTLINE, width = 0.5)),
               hovertemplate = "%{customdata}: %{y:,} change<extra></extra>") |>
      add_bars(y = cnt(TYPE_NEW), name = TYPE_NEW, width = bar_w, customdata = yrs,
               marker = list(color = COL_NEW, opacity = op,
                             line = list(color = COL_OUTLINE, width = 0.5)),
               hovertemplate = "%{customdata}: %{y:,} new<extra></extra>") |>
      layout(
        barmode   = "stack",
        hovermode = "x unified",
        xaxis = list(title = paste("Year of", SLIDER_LABEL), showgrid = FALSE,
                     color = "#52514e", type = "date"),
        yaxis = list(title = "Applications", gridcolor = "#ebeae6",
                     zeroline = FALSE, color = "#52514e", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.18, traceorder = "reversed"),
        shapes = thru_shape(d0),
        margin = list(l = 55, r = 20, t = 10, b = 40),
        plot_bgcolor = "#fcfcfb", paper_bgcolor = "#ffffff"
      ) |>
      config(displayModeBar = FALSE)
  })
  
  observeEvent(thru(), {
    yrs <- chart_years()
    req(yrs)
    op <- as.list(bar_opacity(yrs, thru()))
    plotlyProxy("timeline", session) |>
      plotlyProxyInvoke("restyle", list(marker.opacity = list(op, op)), list(0, 1)) |>
      plotlyProxyInvoke("relayout", list(shapes = thru_shape(thru())))
  }, ignoreInit = TRUE)
  
  # --------------------------------------------------------------------------
  # Tables
  # --------------------------------------------------------------------------
  
  output$table <- renderDT({
    datatable(records_export(shown()) |> select(-`Ecology Record URL`, -lon, -lat),
              rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$unmapped_table <- renderDT({
    df <- unmapped_export(UNMAPPED) |>
      mutate(`Record Document Number` = paste0(
        "<a href='", `Ecology Record URL`, "' target='_blank'>",
        htmlEscape(`Record Document Number`), "</a>")) |>
      select(-`Ecology Record URL`)
    datatable(df, escape = FALSE, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # --------------------------------------------------------------------------
  # Downloads
  # --------------------------------------------------------------------------
  
  stamp <- function() format(DATA_INFO$data_date, "%Y%m%d")
  
  output$dl_kmz_shown <- downloadHandler(
    filename = function() paste0("WR_Applications_thru_", format(thru(), "%Y%m%d"),
                                 "_data_", stamp(), ".kmz"),
    content = function(file) {
      df <- shown()
      validate(need(nrow(df) > 0, "No records to export. Adjust the slider or filters."))
      withProgress(message = "Building KMZ file...", value = 0.5, {
        file.copy(build_kmz(df, input$kmz_hierarchy,
                            title = paste0("Active WR applications through ",
                                           format(thru(), "%m/%d/%Y"))), file)
      })
    }
  )
  
  output$dl_kmz_all <- downloadHandler(
    filename = function() paste0("WR_Applications_all_data_", stamp(), ".kmz"),
    content = function(file) {
      withProgress(message = "Building KMZ file...", value = 0.5, {
        file.copy(build_kmz(RECORDS, input$kmz_hierarchy, unmapped = UNMAPPED,
                            title = paste0("All active WR applications (data ",
                                           DATA_DATE_TXT, ")")), file)
      })
    }
  )
  
  output$dl_csv_shown <- downloadHandler(
    filename = function() paste0("WR_Applications_thru_", format(thru(), "%Y%m%d"),
                                 "_data_", stamp(), ".csv"),
    content = function(file) write_csv(records_export(shown()), file, na = "")
  )
  
  output$dl_unmapped <- downloadHandler(
    filename = function() paste0("WR_Applications_not_mapped_", stamp(), ".csv"),
    content = function(file) write_csv(unmapped_export(UNMAPPED), file, na = "")
  )
}

# ==============================================================================
# RUN THE APP
# ==============================================================================

shinyApp(ui = ui, server = server)