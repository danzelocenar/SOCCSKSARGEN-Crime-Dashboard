# ============================================================
# SOCCSKSARGEN CRIME INTERACTIVE DASHBOARD
# ============================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(sf)
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(viridis)
library(DT)
library(shinyWidgets)
library(htmlwidgets)

# ============================================================
# DATA LOADING
# All heavy spatial objects are pre-built RDS files.
# No st_union / st_transform / st_centroid runs at startup.
# ============================================================

# ── Crime dataset ─────────────────────────────────────────────
crime_raw <- read_excel("CRIME DATASET CLEANED.xlsx")
gc()  # free memory immediately after loading

crime_clean <- crime_raw %>%
  rename(
    Province = PROVINCE,
    Year     = `YEAR COMMITTED`,
    Month    = `NUM MONTH COMMITTED`,
    Category = `OFFENSE CATEGORY`
  ) %>%
  mutate(
    Province = str_trim(Province),
    Province = case_when(
      str_detect(Province, regex("SOUTH COTABATO", ignore_case = TRUE)) ~ "South Cotabato",
      str_detect(Province, regex("SULTAN KUDARAT", ignore_case = TRUE)) ~ "Sultan Kudarat",
      str_detect(Province, regex("SARANGANI",      ignore_case = TRUE)) ~ "Sarangani",
      str_detect(Province, regex("COTABATO",       ignore_case = TRUE)) ~ "Cotabato",
      str_detect(Province, regex("GENERAL SANTOS", ignore_case = TRUE)) ~ "General Santos City",
      TRUE ~ Province
    ),
    Category = str_trim(Category),
    Category = case_when(
      str_detect(Category, regex("Index Crimes - 8FC",                 ignore_case = TRUE)) ~ "Index Crimes-8FC",
      str_detect(Category, regex("Non-Index Crimes - Special Complex", ignore_case = TRUE)) ~ "Non-Index: Special Complex",
      str_detect(Category, regex("Non-Index Crimes - Special Laws",    ignore_case = TRUE)) ~ "Non-Index: Special Laws",
      str_detect(Category, regex("PSI",                                ignore_case = TRUE)) ~ "Non-Index: PSI",
      str_detect(Category, regex("Non-Index Crimes - Other",           ignore_case = TRUE)) ~ "Non-Index: Other",
      TRUE ~ Category
    ),
    Province = as.factor(Province),
    Category = as.factor(Category),
    Year     = as.integer(Year),
    Month    = as.integer(Month)
  ) %>%
  filter(
    !is.na(Province), !is.na(Year),
    !is.na(Month),    !is.na(Category),
    Year >= 2015, Year <= 2025,
    Month >= 1,   Month <= 12
  )

# Drop raw import — no longer needed
rm(crime_raw)
gc()

# ── Model results (graceful fallback if file missing) ─────────
bhm_rr <- tryCatch(
  read_excel("Objective1_BHM_Results.xlsx", sheet = "Relative_Risk"),
  error = function(e) NULL
)
gc()

bst_temporal <- tryCatch(
  read_excel("Objective2_BST_Results.xlsx", sheet = "Temporal_RW_Effects"),
  error = function(e) NULL
)
gc()

cp_results <- tryCatch(
  read_excel("Objective3_CPA_Results.xlsx", sheet = "CP_Model_Comparison"),
  error = function(e) NULL
)

prov_cp <- tryCatch(
  read_excel("Objective3_CPA_Results.xlsx", sheet = "Province_CP_Results"),
  error = function(e) NULL
)
gc()

# ── Spatial objects — loaded from pre-built RDS (NO heavy computation) ───
# These were created by deploy.R using ms_simplify + st_union + st_transform
# Running them here on shinyapps.io would cause "signal: killed" (OOM).
region_muni  <- readRDS("region_muni.rds")
region_sf    <- readRDS("region_sf.rds")
centroids_df <- readRDS("centroids_df.rds")
gc()

# ── Constants ─────────────────────────────────────────────────
all_years      <- sort(unique(crime_clean$Year))
all_categories <- c("All Categories", levels(crime_clean$Category))
all_provinces  <- c("All Provinces",  levels(crime_clean$Province))
all_months     <- c("All Months", month.name)

best_cp_year <- if (!is.null(cp_results))
  cp_results$CP_Year[which.min(cp_results$WAIC)] else NULL

total_crimes  <- nrow(crime_clean)
peak_year_val <- crime_clean %>%
  count(Year) %>% slice_max(n, n = 1) %>% pull(Year)
peak_prov_val <- crime_clean %>%
  count(Province, sort = TRUE) %>% slice(1) %>% pull(Province)

prov_colors <- c(
  "Cotabato"            = "#E63946",
  "South Cotabato"      = "#2196F3",
  "Sultan Kudarat"      = "#FF9800",
  "Sarangani"           = "#4CAF50",
  "General Santos City" = "#9C27B0"
)

# ── save-map builder — NOT called at startup, only inside downloadHandler ──
# FIX: removed "save_sf_data <- build_save_sf()" from global scope.
# Building a joined sf object at startup was wasting ~100-200 MB on boot.
# It is now built lazily inside the downloadHandler only when the user
# actually clicks "Save as HTML File".
build_save_sf <- function() {
  totals <- crime_clean %>%
    group_by(Province) %>%
    summarise(Total = n(), .groups = "drop")
  sf_out <- region_sf %>%
    left_join(totals, by = "Province")
  if (!is.null(bhm_rr))
    sf_out <- sf_out %>%
    left_join(bhm_rr %>%
                dplyr::select(Province, RR, RR_lower, RR_upper, Risk),
              by = "Province")
  if (!is.null(prov_cp))
    sf_out <- sf_out %>%
    left_join(prov_cp %>%
                dplyr::select(Province, Best_CP_Year,
                              RR_Change, Significant),
              by = "Province")
  sf_out
}

# ── Vectorised popup builder for save map ────────────────────
build_save_popup <- function(sf_obj) {
  has_rr <- "RR" %in% names(sf_obj)
  has_cp <- "Best_CP_Year" %in% names(sf_obj)
  
  mapply(function(prov, total, rr, rr_lo, rr_hi, risk,
                  cp_yr, rr_chg, sig) {
    rr_html <- if (has_rr && !is.na(rr)) paste0(
      "<br><br>",
      "<b style='color:#94a3b8;font-size:11px;'>RELATIVE RISK (BHM)</b><br>",
      "<span style='font-size:20px;font-weight:700;color:",
      ifelse(rr > 1, "#ef4444", "#10b981"), ";'>",
      round(rr, 3), "</span>",
      " <span style='color:#94a3b8;font-size:11px;'>", risk, "</span>",
      "<br><span style='color:#64748b;font-size:11px;'>95% CI: [",
      round(rr_lo, 3), ", ", round(rr_hi, 3), "]</span>"
    ) else ""
    
    cp_html <- if (has_cp && !is.na(cp_yr)) paste0(
      "<br><br>",
      "<b style='color:#94a3b8;font-size:11px;'>CHANGE-POINT YEAR</b><br>",
      "<span style='font-size:20px;font-weight:700;color:#10b981;'>",
      cp_yr, "</span>",
      "<br><span style='color:#64748b;font-size:11px;'>",
      "Crime change: ", round((rr_chg - 1) * 100, 1), "% | ",
      "Significant: ", sig, "</span>"
    ) else ""
    
    paste0(
      "<div style='font-family:Inter,sans-serif;background:#1e293b;",
      "color:#e2e8f0;padding:14px;border-radius:8px;min-width:240px;'>",
      "<b style='font-size:15px;color:#3b82f6;'>", prov, "</b><br><br>",
      "<b style='color:#94a3b8;font-size:11px;'>TOTAL CRIMES (2015-2025)</b><br>",
      "<span style='font-size:26px;font-weight:700;color:#f97316;'>",
      formatC(total, format = "d", big.mark = ","), "</span>",
      rr_html, cp_html, "</div>"
    )
  },
  sf_obj$Province,
  sf_obj$Total,
  if (has_rr) sf_obj$RR          else rep(NA, nrow(sf_obj)),
  if (has_rr) sf_obj$RR_lower    else rep(NA, nrow(sf_obj)),
  if (has_rr) sf_obj$RR_upper    else rep(NA, nrow(sf_obj)),
  if (has_rr) sf_obj$Risk        else rep(NA, nrow(sf_obj)),
  if (has_cp) sf_obj$Best_CP_Year else rep(NA, nrow(sf_obj)),
  if (has_cp) sf_obj$RR_Change   else rep(NA, nrow(sf_obj)),
  if (has_cp) sf_obj$Significant  else rep(NA, nrow(sf_obj)),
  SIMPLIFY = TRUE
  )
}

# ============================================================
# CUSTOM CSS
# ============================================================
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
  * { font-family: 'Inter', sans-serif !important; }
  body, .wrapper            { background-color: #0f172a !important; }
  .content-wrapper          { background-color: #0f172a !important; }
  .main-header .navbar,
  .main-header .logo        { background: linear-gradient(135deg,#1e3a5f 0%,#0f172a 100%) !important;
                               border-bottom: 2px solid #3b82f6 !important; }
  .main-header .logo        { font-size:15px !important; font-weight:700 !important;
                               color:#e2e8f0 !important; letter-spacing:0.5px !important; }
  .main-sidebar             { background: linear-gradient(180deg,#1e293b 0%,#0f172a 100%) !important;
                               border-right: 1px solid #1e3a5f !important; }
  .sidebar-menu > li > a   { color:#94a3b8 !important; border-left:3px solid transparent !important;
                               transition:all 0.3s ease !important; font-size:13px !important; }
  .sidebar-menu > li.active > a,
  .sidebar-menu > li > a:hover { color:#e2e8f0 !important;
                                  background:rgba(59,130,246,0.15) !important;
                                  border-left:3px solid #3b82f6 !important; }
  .filter-label             { color:#94a3b8; font-size:11px; font-weight:600;
                               text-transform:uppercase; letter-spacing:1px;
                               margin-bottom:4px; margin-top:10px; display:block; }
  .box                      { background:#1e293b !important; border:1px solid #334155 !important;
                               border-radius:12px !important;
                               box-shadow:0 4px 20px rgba(0,0,0,0.3) !important; }
  .box-header               { background:transparent !important; border-bottom:1px solid #334155 !important;
                               color:#e2e8f0 !important; font-weight:600 !important; font-size:13px !important; }
  .box-header .box-title    { color:#e2e8f0 !important; }
  .small-box                { border-radius:12px !important; overflow:hidden !important;
                               box-shadow:0 4px 20px rgba(0,0,0,0.3) !important; }
  .small-box h3             { font-size:24px !important; font-weight:700 !important; }
  .small-box p              { font-size:11px !important; font-weight:600 !important;
                               text-transform:uppercase !important; letter-spacing:0.5px !important; }
  .small-box .icon          { font-size:55px !important; top:15px !important; opacity:0.25 !important; }
  .info-box                 { border-radius:12px !important; background:#1e293b !important;
                               border:1px solid #334155 !important;
                               box-shadow:0 4px 20px rgba(0,0,0,0.3) !important; }
  .info-box-icon            { border-radius:12px 0 0 12px !important; }
  .info-box-content .info-box-text   { color:#94a3b8 !important; font-size:11px !important;
                                        text-transform:uppercase !important; letter-spacing:0.5px !important; }
  .info-box-content .info-box-number { color:#e2e8f0 !important; font-size:20px !important;
                                        font-weight:700 !important; }
  .irs--shiny .irs-bar      { background:#3b82f6 !important;
                               border-top:1px solid #3b82f6 !important;
                               border-bottom:1px solid #3b82f6 !important; }
  .irs--shiny .irs-handle   { background:#3b82f6 !important; border:2px solid white !important; }
  .irs--shiny .irs-from,
  .irs--shiny .irs-to,
  .irs--shiny .irs-single   { background:#3b82f6 !important; font-size:11px !important; }
  .irs--shiny .irs-line     { background:#334155 !important; }
  .irs--shiny .irs-min,
  .irs--shiny .irs-max      { color:#64748b !important; font-size:10px !important; }
  ::-webkit-scrollbar       { width:6px; }
  ::-webkit-scrollbar-track { background:#0f172a; }
  ::-webkit-scrollbar-thumb { background:#334155; border-radius:3px; }
  .event-badge              { display:inline-block; padding:3px 8px; border-radius:20px;
                               font-size:10px; font-weight:600; text-transform:uppercase;
                               letter-spacing:0.5px; margin:2px; }
  .badge-red    { background:#dc2626; color:white; }
  .badge-blue   { background:#2563eb; color:white; }
  .badge-green  { background:#16a34a; color:white; }
  .badge-orange { background:#d97706; color:white; }
  .story-card   { background:linear-gradient(135deg,#1e3a5f 0%,#1e293b 100%);
                   border:1px solid #3b82f6; border-radius:12px;
                   padding:16px; margin-bottom:10px; }
  .story-card h4 { color:#3b82f6; font-size:13px; font-weight:700;
                    margin:0 0 8px 0; text-transform:uppercase; letter-spacing:0.5px; }
  .story-card p  { color:#cbd5e1; font-size:12px; line-height:1.7; margin:0; }
  hr             { border-color:#1e3a5f !important; margin:8px 0 !important; }
  table.dataTable                    { color:#e2e8f0 !important; }
  table.dataTable thead th           { background-color:#1e293b !important;
                                        color:#3b82f6 !important;
                                        border-bottom:1px solid #334155 !important; }
  table.dataTable tbody tr           { background-color:#0f172a !important; }
  table.dataTable tbody tr:hover     { background-color:#1e293b !important; }
  table.dataTable tbody td           { border-color:#1e3a5f !important; }
  .dataTables_wrapper .dataTables_filter input,
  .dataTables_wrapper .dataTables_length select { background-color:#1e293b !important;
                                                   color:#e2e8f0 !important;
                                                   border:1px solid #334155 !important;
                                                   border-radius:6px !important; }
  .dataTables_wrapper .dataTables_info,
  .dataTables_wrapper .dataTables_paginate        { color:#94a3b8 !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button { color:#94a3b8 !important;
                                                               background:#1e293b !important;
                                                               border:1px solid #334155 !important;
                                                               border-radius:4px !important; }
  .dataTables_wrapper .dataTables_paginate .paginate_button.current { background:#3b82f6 !important;
                                                                        color:white !important;
                                                                        border-color:#3b82f6 !important; }
  .dt-buttons .btn { background:#1e3a5f !important; color:#e2e8f0 !important;
                      border:1px solid #334155 !important; border-radius:6px !important;
                      font-size:11px !important; }
"

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin  = "black",
  title = "SOCCSKSARGEN Crime Dashboard",
  
  dashboardHeader(
    title      = "SOCCSKSARGEN Crime Analysis",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 270,
    tags$head(tags$style(HTML(custom_css))),
    
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",          tabName = "overview",    icon = icon("home")),
      menuItem("Crime Map",         tabName = "map",         icon = icon("map")),
      menuItem("Crime Trends",      tabName = "trends",      icon = icon("chart-line")),
      menuItem("Crime by Category", tabName = "category",    icon = icon("tags")),
      menuItem("Risk Analysis",     tabName = "risk",        icon = icon("shield-alt")),
      menuItem("What Changed?",     tabName = "changepoint", icon = icon("bolt")),
      menuItem("Data Table",        tabName = "data",        icon = icon("table"))
    ),
    
    tags$div(
      style = "padding: 8px 15px;",
      
      tags$span("Year (Map Slider)", class = "filter-label"),
      sliderInput(
        "selected_year", NULL,
        min = min(all_years), max = max(all_years),
        value = max(all_years), step = 1, sep = "",
        animate = animationOptions(
          interval    = 1800, loop = TRUE,
          playButton  = icon("play-circle"),
          pauseButton = icon("pause-circle")
        ),
        width = "100%"
      ),
      
      tags$span("Year Range (Charts)", class = "filter-label"),
      sliderInput(
        "year_range", NULL,
        min   = min(all_years), max = max(all_years),
        value = c(min(all_years), max(all_years)),
        step  = 1, sep = "", width = "100%"
      ),
      
      tags$span("Crime Category", class = "filter-label"),
      pickerInput(
        "category", NULL,
        choices  = all_categories,
        selected = "All Categories",
        options  = pickerOptions(style = "btn-sm"),
        width    = "100%"
      ),
      
      tags$span("Province", class = "filter-label"),
      pickerInput(
        "province", NULL,
        choices  = all_provinces,
        selected = "All Provinces",
        options  = pickerOptions(style = "btn-sm"),
        width    = "100%"
      ),
      
      tags$span("Month", class = "filter-label"),
      pickerInput(
        "month_filter", NULL,
        choices  = all_months,
        selected = "All Months",
        options  = pickerOptions(style = "btn-sm"),
        width    = "100%"
      ),
      
      br(),
      actionBttn(
        "reset", "Reset All Filters",
        style = "bordered", color = "primary",
        size  = "sm", block = TRUE,
        icon  = icon("undo")
      ),
      
      br(),
      downloadButton(
        "save_html",
        label = "Save as HTML File",
        class = "btn-sm btn-block",
        style = "width:100%; background:#1e3a5f; color:#e2e8f0;
                 border:1px solid #334155; border-radius:6px;
                 font-size:11px; margin-top:4px;"
      )
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      
      # ════════════════════════════════════════════════════
      # TAB 1: OVERVIEW
      # ════════════════════════════════════════════════════
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("ov_total",    width = 3),
          valueBoxOutput("ov_peak",     width = 3),
          valueBoxOutput("ov_province", width = 3),
          valueBoxOutput("ov_cp",       width = 3)
        ),
        fluidRow(
          column(
            width = 4,
            tags$div(class = "story-card",
                     tags$h4("About This Study"),
                     tags$p("This dashboard presents crime patterns across
                      the 5 provinces and city of SOCCSKSARGEN
                      (Region XII), Philippines, from 2015 to 2025.
                      Navigate through the tabs to explore crime
                      hotspots, trends, and major structural changes.")
            ),
            tags$div(class = "story-card",
                     tags$h4("Major Events in the Region"),
                     tags$p(
                       tags$span("2016",    class = "event-badge badge-red"),
                       " Anti-Drug Campaign begins", tags$br(),
                       tags$span("2017-19", class = "event-badge badge-orange"),
                       " Martial Law in Mindanao", tags$br(),
                       tags$span("2020-21", class = "event-badge badge-blue"),
                       " COVID-19 Pandemic", tags$br(),
                       tags$span("2022+",   class = "event-badge badge-green"),
                       " Post-Pandemic Recovery"
                     )
            ),
            tags$div(class = "story-card",
                     tags$h4("How to Use This Dashboard"),
                     tags$p("Use the sidebar filters to explore specific years,
                      provinces, and crime categories. The Crime Map tab
                      has a Play button to animate crime patterns year
                      by year. Click any province on any map for details.")
            )
          ),
          column(
            width = 5,
            box(
              title = "Crime Hotspot Map (2015-2025)",
              status = "primary", solidHeader = TRUE,
              width = NULL, height = 420,
              leafletOutput("overview_map", height = "360px")
            )
          ),
          column(
            width = 3,
            box(title = "Crime Share by Province",
                status = "info", solidHeader = TRUE,
                width = NULL, height = 200,
                plotlyOutput("ov_prov_pie", height = "150px")),
            box(title = "Crime Share by Category",
                status = "warning", solidHeader = TRUE,
                width = NULL, height = 200,
                plotlyOutput("ov_cat_bar", height = "150px"))
          )
        )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 2: CRIME MAP
      # ════════════════════════════════════════════════════
      tabItem(
        tabName = "map",
        fluidRow(
          valueBoxOutput("map_count",  width = 3),
          valueBoxOutput("map_year",   width = 3),
          valueBoxOutput("map_top",    width = 3),
          valueBoxOutput("map_change", width = 3)
        ),
        fluidRow(
          column(
            width = 9,
            box(
              title = uiOutput("map_header"),
              status = "primary", solidHeader = TRUE,
              width = NULL, height = 580,
              radioGroupButtons(
                "map_var", label = NULL,
                choices   = c("Crime Count" = "count",
                              "Relative Risk" = "rr",
                              "Change-Point Year" = "cp"),
                selected  = "count",
                size      = "sm", status = "primary", justified = TRUE
              ),
              br(),
              leafletOutput("crime_map", height = "470px")
            )
          ),
          column(
            width = 3,
            box(title = "Province Ranking",
                status = "info", solidHeader = TRUE,
                width = NULL, height = 290,
                plotlyOutput("map_prov_bar", height = "240px")),
            box(title = "Category Split",
                status = "warning", solidHeader = TRUE,
                width = NULL, height = 270,
                plotlyOutput("map_cat_pie", height = "220px"))
          )
        )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 3: CRIME TRENDS
      # ════════════════════════════════════════════════════
      tabItem(
        tabName = "trends",
        fluidRow(
          box(title = "How Has Crime Changed Over Time?",
              status = "primary", solidHeader = TRUE, width = 12,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Total crime count per year. The green line marks the identified change-point year."),
              plotlyOutput("trend_regional", height = "280px"))
        ),
        fluidRow(
          box(title = "Crime Trend Per Province",
              status = "info", solidHeader = TRUE, width = 8,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Click province names in the legend to show or hide them."),
              plotlyOutput("trend_province", height = "320px")),
          box(title = "Which Month Has Most Crime?",
              status = "warning", solidHeader = TRUE, width = 4,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Darker color = more crime incidents."),
              plotlyOutput("trend_monthly", height = "320px"))
        )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 4: CRIME BY CATEGORY
      # ════════════════════════════════════════════════════
      tabItem(
        tabName = "category",
        fluidRow(
          box(title = "What Types of Crime Are Most Common?",
              status = "primary", solidHeader = TRUE, width = 6,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Hover over bars for exact numbers."),
              plotlyOutput("cat_total_bar", height = "320px")),
          box(title = "Crime Category Share",
              status = "info", solidHeader = TRUE, width = 6,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Proportion of each crime type in the selected period."),
              plotlyOutput("cat_donut", height = "320px"))
        ),
        fluidRow(
          box(title = "How Did Each Crime Type Change Per Year?",
              status = "success", solidHeader = TRUE, width = 12,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Click legend items to show or hide categories."),
              plotlyOutput("cat_trend", height = "300px"))
        )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 5: RISK ANALYSIS
      # ════════════════════════════════════════════════════
      tabItem(
        tabName = "risk",
        fluidRow(
          column(width = 12,
                 tags$div(class = "story-card",
                          tags$h4("What is Relative Risk?"),
                          tags$p("Relative Risk (RR) tells us which provinces have higher or lower
                      crime compared to the regional average. RR > 1 means higher-than-average
                      risk. RR < 1 means lower-than-average risk. These values come from the
                      Bayesian Hierarchical Model (BHM) fitted in this study.")
                 )
          )
        ),
        fluidRow(
          box(title = "Crime Risk Map — Which Province is Riskiest?",
              status = "danger", solidHeader = TRUE, width = 7,
              leafletOutput("risk_map", height = "380px")),
          box(title = "Province Risk Ranking",
              status = "warning", solidHeader = TRUE, width = 5,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Red = above average risk. Green = below average risk.
                      Error bars show the uncertainty range."),
              plotlyOutput("risk_bar", height = "350px"))
        ),
        fluidRow(
          box(title = "How Did Crime Risk Change Each Year?",
              status = "primary", solidHeader = TRUE, width = 12,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "From the Bayesian Spatio-Temporal Model (BST). Red = above-average. Green = below-average."),
              plotlyOutput("risk_temporal", height = "250px"))
        )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 6: WHAT CHANGED?
      # ════════════════════════════════════════════════════
      tabItem(
        tabName = "changepoint",
        fluidRow(
          infoBoxOutput("cp_box_year",   width = 4),
          infoBoxOutput("cp_box_change", width = 4),
          infoBoxOutput("cp_box_sig",    width = 4)
        ),
        fluidRow(column(width = 12, uiOutput("cp_story_card"))),
        fluidRow(
          box(title = "Crime Trend Before and After the Change",
              status = "primary", solidHeader = TRUE, width = 8,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "The green dashed line marks the year when crime patterns significantly changed."),
              plotlyOutput("cp_trend", height = "300px")),
          box(title = "Did Each Province Change Differently?",
              status = "info", solidHeader = TRUE, width = 4,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Green = statistically significant change-point. Gray = not significant."),
              plotlyOutput("cp_prov_bar", height = "300px"))
        ),
        fluidRow(
          box(title = "How Much Did Crime Change After the Break?",
              status = "warning", solidHeader = TRUE, width = 12,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Red = crime increased after the change-point. Green = crime decreased."),
              plotlyOutput("cp_pct_change", height = "260px"))
        )
      ),
      
      # ════════════════════════════════════════════════════
      # TAB 7: DATA TABLE
      # ════════════════════════════════════════════════════
      tabItem(
        tabName = "data",
        fluidRow(
          box(title = "Browse the Crime Data",
              status = "primary", solidHeader = TRUE, width = 12,
              tags$p(style = "color:#94a3b8;font-size:12px;margin-bottom:8px;",
                     "Use the search boxes below each column to filter.
                      Use the buttons above to download the data."),
              DTOutput("data_tbl"))
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ── Dark plotly theme ────────────────────────────────────
  dark_plotly <- function(p) {
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font  = list(family = "Inter, sans-serif", color = "#94a3b8", size = 11),
      xaxis = list(gridcolor = "#1e3a5f", zerolinecolor = "#334155",
                   tickfont  = list(color = "#94a3b8")),
      yaxis = list(gridcolor = "#1e3a5f", zerolinecolor = "#334155",
                   tickfont  = list(color = "#94a3b8")),
      legend = list(font = list(color = "#94a3b8"),
                    bgcolor = "rgba(0,0,0,0)", bordercolor = "#334155")
    )
  }
  
  # ── Reset ────────────────────────────────────────────────
  observeEvent(input$reset, {
    updateSliderInput(session, "year_range",
                      value = c(min(all_years), max(all_years)))
    updateSliderInput(session, "selected_year",
                      value = max(all_years))
    updatePickerInput(session, "category",     selected = "All Categories")
    updatePickerInput(session, "province",     selected = "All Provinces")
    updatePickerInput(session, "month_filter", selected = "All Months")
    updateRadioGroupButtons(session, "map_var", selected = "count")
  })
  
  # ── Save HTML via downloadHandler ────────────────────────
  # FIX: save_sf_data is now built HERE on demand, not at global startup.
  # This saves ~100-200 MB of RAM on boot, preventing "signal: killed".
  output$save_html <- downloadHandler(
    filename = function() "SOCCSKSARGEN_Crime_Interactive_Map.html",
    content  = function(file) {
      save_sf_data <- build_save_sf()   # built lazily — only when user clicks download
      pal_save     <- colorNumeric("YlOrRd", domain = save_sf_data$Total)
      popups       <- build_save_popup(save_sf_data)
      
      map_save <- leaflet(save_sf_data) %>%
        addProviderTiles(providers$CartoDB.DarkMatterNoLabels,
                         group = "Dark Map") %>%
        addProviderTiles(providers$CartoDB.Positron,
                         group = "Light Map") %>%
        addProviderTiles(providers$Esri.WorldImagery,
                         group = "Satellite") %>%
        addPolygons(
          fillColor        = ~pal_save(Total),
          fillOpacity      = 0.80,
          color            = "#0f172a", weight = 2,
          group            = "Crime Count",
          popup            = popups,
          highlightOptions = highlightOptions(
            weight = 3, color = "#3b82f6",
            fillOpacity = 0.95, bringToFront = TRUE
          ),
          label = ~paste0(Province, ": ",
                          formatC(Total, format = "d", big.mark = ","),
                          " crimes")
        ) %>%
        addLegend(pal = pal_save, values = ~Total,
                  title = "Total Crimes (2015-2025)",
                  position = "bottomright", opacity = 0.85) %>%
        addLabelOnlyMarkers(
          data = centroids_df, lng = ~cx, lat = ~cy,
          label = ~Province,
          labelOptions = labelOptions(
            noHide = TRUE, textOnly = TRUE,
            style  = list("color" = "white", "font-weight" = "bold",
                          "font-size" = "12px",
                          "text-shadow" = "0 0 6px #0f172a,0 0 6px #0f172a")
          )
        ) %>%
        addLayersControl(
          baseGroups    = c("Dark Map", "Light Map", "Satellite"),
          overlayGroups = c("Crime Count"),
          options       = layersControlOptions(collapsed = FALSE),
          position      = "topright"
        ) %>%
        addScaleBar(position = "bottomleft") %>%
        addMiniMap(tiles = providers$CartoDB.Positron,
                   toggleDisplay = TRUE, minimized = FALSE,
                   width = 150, height = 150) %>%
        addControl(
          html = paste0(
            "<div style='background:#1e293b;color:#e2e8f0;padding:12px 16px;",
            "border-radius:10px;border:1px solid #3b82f6;",
            "font-family:Inter,sans-serif;box-shadow:0 4px 20px rgba(0,0,0,0.4);",
            "max-width:280px;'>",
            "<b style='font-size:14px;color:#3b82f6;'>SOCCSKSARGEN Crime Analysis</b><br>",
            "<span style='font-size:11px;color:#94a3b8;'>",
            "Bayesian Spatio-Temporal Modeling<br>2015-2025 | Region XII, Philippines</span>",
            if (!is.null(best_cp_year)) paste0(
              "<br><span style='font-size:11px;color:#10b981;font-weight:600;'>",
              "Change-Point Identified: ", best_cp_year, "</span>") else "",
            "<br><span style='font-size:10px;color:#64748b;'>",
            "Click any province for details</span></div>"
          ),
          position = "topleft"
        ) %>%
        setView(lng = 124.7, lat = 6.4, zoom = 8)
      
      saveWidget(map_save, file = file,
                 selfcontained = TRUE,
                 title = "SOCCSKSARGEN Crime Interactive Map (2015-2025)")
    }
  )
  
  # ── Reactive filtered data ───────────────────────────────
  fdata <- reactive({
    d <- crime_clean %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2])
    if (input$category != "All Categories")
      d <- d %>% filter(Category == input$category)
    if (input$province != "All Provinces")
      d <- d %>% filter(Province == input$province)
    if (input$month_filter != "All Months") {
      mn <- which(month.name == input$month_filter)
      d  <- d %>% filter(Month == mn)
    }
    d
  })
  
  yr_data <- reactive({
    d <- crime_clean %>% filter(Year == input$selected_year)
    if (input$category != "All Categories")
      d <- d %>% filter(Category == input$category)
    d %>% group_by(Province) %>%
      summarise(Count = n(), .groups = "drop")
  })
  
  # ── OVERVIEW value boxes ─────────────────────────────────
  output$ov_total <- renderValueBox({
    valueBox(formatC(total_crimes, format = "d", big.mark = ","),
             "Total Crime Incidents (2015-2025)",
             icon = icon("exclamation-triangle"), color = "red")
  })
  output$ov_peak <- renderValueBox({
    valueBox(peak_year_val, "Year with Most Crimes",
             icon = icon("calendar-alt"), color = "orange")
  })
  output$ov_province <- renderValueBox({
    valueBox(as.character(peak_prov_val), "Highest Crime Province",
             icon = icon("map-marker-alt"), color = "blue")
  })
  output$ov_cp <- renderValueBox({
    valueBox(ifelse(is.null(best_cp_year), "N/A", best_cp_year),
             "Year Crime Pattern Changed",
             icon = icon("bolt"), color = "green")
  })
  
  # ── OVERVIEW map ─────────────────────────────────────────
  output$overview_map <- renderLeaflet({
    totals <- crime_clean %>%
      group_by(Province) %>%
      summarise(Total = n(), .groups = "drop")
    sf_ov <- region_sf %>% left_join(totals, by = "Province")
    pal   <- colorNumeric("YlOrRd", domain = sf_ov$Total)
    
    leaflet(sf_ov) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      addPolygons(
        fillColor        = ~pal(Total), fillOpacity = 0.8,
        color            = "#1e3a5f",   weight = 2,
        popup = ~paste0(
          "<div style='font-family:Inter;background:#1e293b;",
          "color:#e2e8f0;padding:12px;border-radius:8px;'>",
          "<b style='font-size:14px;color:#3b82f6;'>", Province, "</b><br>",
          "<span style='font-size:22px;font-weight:700;color:#f97316;'>",
          formatC(Total, format = "d", big.mark = ","),
          "</span><span style='color:#94a3b8;font-size:12px;'> total crimes</span>",
          "</div>"
        ),
        highlightOptions = highlightOptions(
          weight = 3, color = "#3b82f6",
          fillOpacity = 0.95, bringToFront = TRUE)
      ) %>%
      addLabelOnlyMarkers(
        data = centroids_df, lng = ~cx, lat = ~cy,
        label = ~Province,
        labelOptions = labelOptions(
          noHide = TRUE, textOnly = TRUE,
          style  = list("color" = "white", "font-weight" = "bold",
                        "font-size" = "11px"))
      ) %>%
      addLegend(pal = pal, values = ~Total, title = "Total Crimes",
                position = "bottomright", opacity = 0.8) %>%
      setView(124.7, 6.4, zoom = 8)
  })
  
  output$ov_prov_pie <- renderPlotly({
    d <- crime_clean %>% count(Province, name = "n")
    plot_ly(d, labels = ~Province, values = ~n, type = "pie", hole = 0.5,
            marker    = list(colors = unname(prov_colors[as.character(d$Province)])),
            textinfo  = "percent", showlegend = FALSE,
            hovertemplate = "<b>%{label}</b><br>%{value:,}<extra></extra>") %>%
      dark_plotly() %>%
      layout(margin = list(t = 5, b = 5, l = 5, r = 5))
  })
  
  output$ov_cat_bar <- renderPlotly({
    d <- crime_clean %>% count(Category, sort = TRUE) %>%
      mutate(Category = str_wrap(Category, 20))
    plot_ly(d, x = ~n, y = ~reorder(Category, n), type = "bar",
            orientation = "h",
            marker = list(color = "#3b82f6",
                          line  = list(color = "#1e3a5f", width = 1)),
            hovertemplate = "<b>%{y}</b><br>%{x:,}<extra></extra>") %>%
      dark_plotly() %>%
      layout(xaxis  = list(title = ""), yaxis = list(title = ""),
             margin = list(l = 120, t = 5, b = 20, r = 10))
  })
  
  # ── MAP value boxes ──────────────────────────────────────
  output$map_count <- renderValueBox({
    valueBox(formatC(nrow(fdata()), format = "d", big.mark = ","),
             "Incidents in Selection",
             icon = icon("exclamation-circle"), color = "red")
  })
  output$map_year <- renderValueBox({
    valueBox(input$selected_year, "Map Year",
             icon = icon("calendar"), color = "blue")
  })
  output$map_top <- renderValueBox({
    top <- yr_data() %>% slice_max(Count, n = 1) %>% pull(Province)
    valueBox(as.character(top[1]), "Highest in Selected Year",
             icon = icon("map-marker"), color = "orange")
  })
  output$map_change <- renderValueBox({
    valueBox(ifelse(is.null(best_cp_year), "N/A",
                    paste("CP:", best_cp_year)),
             "Change-Point Year", icon = icon("bolt"), color = "green")
  })
  
  output$map_header <- renderUI({
    v <- switch(input$map_var,
                "count" = "Crime Count",
                "rr"    = "Relative Risk (BHM)",
                "cp"    = "Change-Point Year")
    paste(v, "-", input$selected_year, "|", input$category)
  })
  
  # ── CRIME MAP (base) ─────────────────────────────────────
  output$crime_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      setView(124.7, 6.4, zoom = 8)
  })
  
  observe({
    sf_yr <- region_sf %>%
      left_join(yr_data(), by = "Province") %>%
      mutate(Count = replace_na(Count, 0))
    var <- input$map_var
    
    if (var == "rr" && !is.null(bhm_rr)) {
      sf_yr <- sf_yr %>%
        left_join(bhm_rr %>%
                    dplyr::select(Province, RR, RR_lower, RR_upper, Risk),
                  by = "Province")
      vals    <- sf_yr$RR
      pal     <- colorNumeric(c("#4CAF50", "#FFF9C4", "#E53935"), domain = c(0.5, 2))
      leg_ttl <- "Relative Risk"
      popups  <- mapply(function(prov, rr, lo, hi, risk) paste0(
        "<div style='background:#1e293b;color:#e2e8f0;padding:12px;border-radius:8px;'>",
        "<b style='color:#3b82f6;'>", prov, "</b><br>",
        "Relative Risk: <b>", round(rr, 3), "</b><br>",
        "95% CI: [", round(lo, 3), ", ", round(hi, 3), "]<br>",
        "Risk Level: <b>", risk, "</b></div>"
      ), sf_yr$Province, sf_yr$RR, sf_yr$RR_lower, sf_yr$RR_upper, sf_yr$Risk,
      SIMPLIFY = TRUE)
      
    } else if (var == "cp" && !is.null(prov_cp)) {
      sf_yr <- sf_yr %>%
        left_join(prov_cp %>%
                    dplyr::select(Province, Best_CP_Year, RR_Change, Significant),
                  by = "Province")
      vals    <- sf_yr$Best_CP_Year
      pal     <- colorNumeric("plasma", domain = range(vals, na.rm = TRUE))
      leg_ttl <- "Change-Point Year"
      popups  <- mapply(function(prov, cp, chg, sig) paste0(
        "<div style='background:#1e293b;color:#e2e8f0;padding:12px;border-radius:8px;'>",
        "<b style='color:#3b82f6;'>", prov, "</b><br>",
        "Change-Point Year: <b>", cp, "</b><br>",
        "Crime Change: <b>", round((chg - 1) * 100, 1), "%</b><br>",
        "Significant: <b>", sig, "</b></div>"
      ), sf_yr$Province, sf_yr$Best_CP_Year, sf_yr$RR_Change, sf_yr$Significant,
      SIMPLIFY = TRUE)
      
    } else {
      vals    <- sf_yr$Count
      pal     <- colorNumeric(
        c("#0f172a", "#1d4ed8", "#f97316", "#dc2626"),
        domain = range(vals, na.rm = TRUE))
      leg_ttl <- paste("Crimes in", input$selected_year)
      popups  <- mapply(function(prov, cnt) paste0(
        "<div style='background:#1e293b;color:#e2e8f0;padding:12px;border-radius:8px;'>",
        "<b style='color:#3b82f6;font-size:14px;'>", prov, "</b><br>",
        "<span style='font-size:22px;font-weight:700;color:#f97316;'>",
        formatC(cnt, format = "d", big.mark = ","), "</span><br>",
        "<span style='color:#94a3b8;font-size:11px;'>crimes in ",
        input$selected_year, "</span></div>"
      ), sf_yr$Province, sf_yr$Count, SIMPLIFY = TRUE)
    }
    
    leafletProxy("crime_map") %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        data = region_muni, fillColor = "transparent",
        color = "#1e3a5f", weight = 0.8, group = "Municipalities"
      ) %>%
      addPolygons(
        data             = sf_yr,
        fillColor        = ~pal(vals), fillOpacity = 0.80,
        color            = "#0f172a", weight = 2,
        popup            = popups,
        popupOptions     = popupOptions(maxWidth = 280),
        highlightOptions = highlightOptions(
          weight = 3, color = "#3b82f6",
          fillOpacity = 0.95, bringToFront = TRUE),
        label = ~paste0(Province, ": ",
                        formatC(Count, format = "d", big.mark = ","))
      ) %>%
      addLabelOnlyMarkers(
        data = centroids_df, lng = ~cx, lat = ~cy,
        label = ~Province,
        labelOptions = labelOptions(
          noHide = TRUE, textOnly = TRUE,
          style  = list("color" = "white", "font-weight" = "bold",
                        "font-size" = "11px",
                        "text-shadow" = "0 0 4px #0f172a,0 0 4px #0f172a"))
      ) %>%
      addLegend(pal = pal, values = vals,
                title = leg_ttl, position = "bottomright", opacity = 0.85)
  })
  
  # ── MAP side charts ──────────────────────────────────────
  output$map_prov_bar <- renderPlotly({
    d <- fdata() %>% count(Province, sort = TRUE)
    plot_ly(d, x = ~n, y = ~reorder(Province, n), type = "bar",
            orientation = "h",
            marker = list(color = unname(prov_colors[as.character(d$Province)])),
            hovertemplate = "<b>%{y}</b><br>%{x:,}<extra></extra>") %>%
      dark_plotly() %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""),
             margin = list(l = 130, t = 5, b = 20, r = 10))
  })
  
  output$map_cat_pie <- renderPlotly({
    d    <- fdata() %>% count(Category, sort = TRUE)
    cols <- c("#3b82f6", "#f97316", "#10b981", "#8b5cf6", "#ef4444")
    plot_ly(d, labels = ~Category, values = ~n, type = "pie", hole = 0.55,
            marker = list(colors = cols[seq_len(nrow(d))]),
            textinfo = "percent", showlegend = FALSE,
            hovertemplate = "<b>%{label}</b><br>%{value:,}<extra></extra>") %>%
      dark_plotly() %>%
      layout(margin = list(t = 5, b = 5, l = 5, r = 5))
  })
  
  # ── TREND charts ─────────────────────────────────────────
  output$trend_regional <- renderPlotly({
    d <- fdata() %>% count(Year, name = "Count")
    p <- plot_ly(d, x = ~Year, y = ~Count, type = "scatter",
                 mode = "lines+markers", fill = "tozeroy",
                 fillcolor = "rgba(59,130,246,0.1)",
                 line   = list(color = "#3b82f6", width = 2.5),
                 marker = list(color = "#f97316", size = 8),
                 name   = "Crime Count",
                 hovertemplate = "<b>%{x}</b><br>%{y:,} crimes<extra></extra>")
    if (!is.null(best_cp_year) &&
        best_cp_year >= min(d$Year) &&
        best_cp_year <= max(d$Year)) {
      p <- p %>%
        add_segments(x = best_cp_year, xend = best_cp_year,
                     y = 0, yend = max(d$Count, na.rm = TRUE) * 1.05,
                     line = list(color = "#10b981", width = 2, dash = "dash"),
                     name = paste("Change-Point:", best_cp_year)) %>%
        add_annotations(x = best_cp_year, y = max(d$Count, na.rm = TRUE),
                        text     = paste0("<b>CP: ", best_cp_year, "</b>"),
                        showarrow = FALSE,
                        font     = list(color = "#10b981", size = 11),
                        xanchor  = "left")
    }
    p %>% dark_plotly() %>%
      layout(xaxis = list(title = "", tickmode = "linear", dtick = 1),
             yaxis = list(title = "Total Crimes"),
             hovermode = "x unified", showlegend = TRUE)
  })
  
  output$trend_province <- renderPlotly({
    d     <- fdata() %>% count(Province, Year, name = "Count")
    provs <- levels(crime_clean$Province)
    p     <- plot_ly()
    for (pv in provs) {
      dd <- d %>% filter(Province == pv)
      if (nrow(dd) == 0) next
      p <- p %>%
        add_trace(data = dd, x = ~Year, y = ~Count,
                  type = "scatter", mode = "lines+markers",
                  name   = as.character(pv),
                  line   = list(color = prov_colors[pv], width = 2),
                  marker = list(color = prov_colors[pv], size = 6),
                  hovertemplate = paste0(
                    "<b>", pv, "</b><br>%{x}: %{y:,} crimes<extra></extra>"))
    }
    p %>% dark_plotly() %>%
      layout(xaxis = list(title = "", tickmode = "linear", dtick = 1),
             yaxis = list(title = "Crime Count"),
             legend = list(orientation = "h", y = -0.2),
             hovermode = "x unified")
  })
  
  output$trend_monthly <- renderPlotly({
    d <- fdata() %>% count(Month, Year, name = "Count") %>%
      mutate(Mo = month.abb[Month])
    plot_ly(d,
            x = ~factor(Mo, levels = month.abb),
            y = ~factor(Year), z = ~Count,
            type = "heatmap",
            colorscale = list(c(0, "#0f172a"), c(0.5, "#1d4ed8"), c(1, "#dc2626")),
            hovertemplate = "<b>%{y} - %{x}</b><br>%{z:,} crimes<extra></extra>",
            showscale = TRUE) %>%
      dark_plotly() %>%
      layout(xaxis  = list(title = "Month"),
             yaxis  = list(title = ""),
             margin = list(l = 50, t = 10, b = 40, r = 20))
  })
  
  # ── CATEGORY charts ──────────────────────────────────────
  output$cat_total_bar <- renderPlotly({
    d    <- fdata() %>% count(Category, sort = TRUE)
    cols <- c("#3b82f6", "#f97316", "#10b981", "#8b5cf6", "#ef4444")
    plot_ly(d, x = ~n, y = ~reorder(Category, n), type = "bar",
            orientation = "h",
            marker = list(color = cols[seq_len(nrow(d))],
                          line  = list(color = "#0f172a", width = 1)),
            text         = ~formatC(n, format = "d", big.mark = ","),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>%{x:,} crimes<extra></extra>") %>%
      dark_plotly() %>%
      layout(xaxis  = list(title = ""), yaxis = list(title = ""),
             margin = list(l = 160, t = 10, b = 20, r = 60))
  })
  
  output$cat_donut <- renderPlotly({
    d    <- fdata() %>% count(Category, sort = TRUE)
    cols <- c("#3b82f6", "#f97316", "#10b981", "#8b5cf6", "#ef4444")
    plot_ly(d, labels = ~Category, values = ~n, type = "pie", hole = 0.55,
            marker = list(colors = cols[seq_len(nrow(d))],
                          line   = list(color = "#0f172a", width = 2)),
            textinfo = "label+percent",
            hovertemplate = "<b>%{label}</b><br>%{value:,} crimes (%{percent})<extra></extra>") %>%
      dark_plotly() %>%
      layout(showlegend = FALSE, margin = list(t = 20, b = 20, l = 20, r = 20))
  })
  
  output$cat_trend <- renderPlotly({
    d    <- fdata() %>% count(Category, Year, name = "Count")
    cats <- levels(crime_clean$Category)
    cols <- c("#3b82f6", "#f97316", "#10b981", "#8b5cf6", "#ef4444")
    p    <- plot_ly()
    for (i in seq_along(cats)) {
      dd <- d %>% filter(Category == cats[i])
      if (nrow(dd) == 0) next
      p <- p %>%
        add_trace(data = dd, x = ~Year, y = ~Count,
                  type = "scatter", mode = "lines+markers",
                  name   = cats[i],
                  line   = list(color = cols[i], width = 2),
                  marker = list(color = cols[i], size = 5),
                  hovertemplate = paste0(
                    "<b>", cats[i], "</b><br>%{x}: %{y:,}<extra></extra>"))
    }
    p %>% dark_plotly() %>%
      layout(xaxis = list(title = "", tickmode = "linear", dtick = 1),
             yaxis = list(title = "Crime Count"),
             legend = list(orientation = "h", y = -0.25),
             hovermode = "x unified")
  })
  
  # ── RISK charts ──────────────────────────────────────────
  output$risk_map <- renderLeaflet({
    if (is.null(bhm_rr)) {
      return(leaflet() %>%
               addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
               addControl(
                 "<div style='background:#1e293b;color:#94a3b8;
                              padding:12px;border-radius:8px;'>
                  Run BHM model first to see risk map</div>",
                 position = "topright") %>%
               setView(124.7, 6.4, zoom = 8))
    }
    sf_rr <- region_sf %>% left_join(bhm_rr, by = "Province")
    pal   <- colorNumeric(c("#4CAF50", "#FFF9C4", "#E53935"), domain = c(0.5, 2))
    
    popups_rr <- mapply(function(prov, rr, lo, hi, risk) paste0(
      "<div style='background:#1e293b;color:#e2e8f0;",
      "padding:14px;border-radius:8px;min-width:220px;'>",
      "<b style='color:#3b82f6;font-size:14px;'>", prov, "</b><br><br>",
      "<span style='font-size:28px;font-weight:700;color:",
      ifelse(rr > 1, "#ef4444", "#10b981"), ";'>", round(rr, 3), "</span>",
      "<span style='color:#94a3b8;'> RR</span>",
      "<br>95% CI: [", round(lo, 3), ", ", round(hi, 3), "]<br>",
      "<b style='color:", ifelse(risk == "High Risk", "#ef4444",
                                 ifelse(risk == "Low Risk", "#10b981", "#f97316")),
      ";'>", risk, "</b></div>"
    ), sf_rr$Province, sf_rr$RR, sf_rr$RR_lower, sf_rr$RR_upper, sf_rr$Risk,
    SIMPLIFY = TRUE)
    
    leaflet(sf_rr) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      addPolygons(
        fillColor        = ~pal(RR), fillOpacity = 0.80,
        color            = "#0f172a", weight = 2,
        popup            = popups_rr,
        highlightOptions = highlightOptions(
          weight = 3, color = "#3b82f6",
          fillOpacity = 0.95, bringToFront = TRUE),
        label = ~paste0(Province, " (RR=", round(RR, 3), ")")
      ) %>%
      addLabelOnlyMarkers(
        data = centroids_df, lng = ~cx, lat = ~cy,
        label = ~Province,
        labelOptions = labelOptions(
          noHide = TRUE, textOnly = TRUE,
          style  = list("color" = "white", "font-weight" = "bold",
                        "font-size" = "11px"))
      ) %>%
      addLegend(pal = pal, values = ~RR, title = "Relative Risk",
                position = "bottomright", opacity = 0.85,
                labFormat = labelFormat(transform = function(x) round(x, 2))) %>%
      setView(124.7, 6.4, zoom = 8)
  })
  
  output$risk_bar <- renderPlotly({
    if (is.null(bhm_rr)) {
      return(plotly_empty() %>% dark_plotly() %>%
               layout(title = list(text = "Run BHM model first",
                                   font = list(color = "#94a3b8"))))
    }
    d <- bhm_rr %>% arrange(RR)
    plot_ly(d, x = ~RR, y = ~reorder(Province, RR),
            type = "scatter", mode = "markers",
            error_x = list(type = "data", symmetric = FALSE,
                           arrayminus = ~RR - RR_lower,
                           array      = ~RR_upper - RR,
                           color = "#64748b", thickness = 2),
            marker = list(
              color = ifelse(d$Risk == "High Risk", "#ef4444",
                             ifelse(d$Risk == "Low Risk", "#10b981", "#f97316")),
              size  = 14,
              line  = list(color = "white", width = 1.5)),
            hovertemplate = "<b>%{y}</b><br>RR: %{x:.4f}<extra></extra>") %>%
      add_segments(x = 1, xend = 1, y = 0, yend = nrow(d) + 0.5,
                   line = list(color = "#64748b", dash = "dot", width = 1.5),
                   name = "Average (RR=1)") %>%
      dark_plotly() %>%
      layout(xaxis = list(title = "Relative Risk (RR)"),
             yaxis = list(title = ""), showlegend = FALSE,
             margin = list(l = 130, t = 10, b = 40, r = 20))
  })
  
  output$risk_temporal <- renderPlotly({
    if (is.null(bst_temporal)) {
      return(plotly_empty() %>% dark_plotly() %>%
               layout(title = list(text = "Run BST model first",
                                   font = list(color = "#94a3b8"))))
    }
    plot_ly(bst_temporal) %>%
      add_ribbons(x = ~Year, ymin = ~RR_temp_lo, ymax = ~RR_temp_hi,
                  fillcolor = "rgba(59,130,246,0.15)",
                  line      = list(color = "transparent"),
                  name = "95% Credible Interval", hoverinfo = "skip") %>%
      add_trace(x = ~Year, y = ~RR_temp, type = "scatter",
                mode = "lines+markers",
                line   = list(color = "#3b82f6", width = 2.5),
                marker = list(
                  color = ~ifelse(RR_temp > 1, "#ef4444", "#10b981"),
                  size  = 10, line = list(color = "white", width = 1.5)),
                name = "Temporal RR",
                hovertemplate = "<b>Year %{x}</b><br>Temporal RR: %{y:.4f}<extra></extra>") %>%
      add_segments(x    = min(bst_temporal$Year),
                   xend = max(bst_temporal$Year),
                   y = 1, yend = 1,
                   line = list(color = "#64748b", dash = "dot", width = 1.5),
                   name = "Average (RR=1)") %>%
      dark_plotly() %>%
      layout(xaxis = list(title = "", tickmode = "linear", dtick = 1),
             yaxis = list(title = "Temporal RR"),
             hovermode = "x unified")
  })
  
  # ── CHANGE-POINT ─────────────────────────────────────────
  output$cp_story_card <- renderUI({
    if (is.null(cp_results)) return(NULL)
    best_row <- cp_results %>% filter(CP_Year == best_cp_year)
    rr_val   <- round(best_row$RR_Change[1], 4)
    pct_val  <- round((rr_val - 1) * 100, 1)
    dir_txt  <- if (rr_val > 1) paste0("+", pct_val, "% increase") else
      paste0(abs(pct_val), "% decrease")
    dir_col  <- if (rr_val > 1) "#ef4444" else "#10b981"
    sig_txt  <- best_row$Significant[1]
    
    tags$div(class = "story-card",
             tags$h4("What the Analysis Found"),
             tags$p(
               "The Bayesian Change-Point Analysis identified ",
               tags$b(style = paste0("color:#3b82f6;"), best_cp_year),
               " as the year when crime patterns significantly shifted.
         After this year, crime changed by ",
               tags$b(style = paste0("color:", dir_col, ";"), dir_txt),
               " (Relative Risk = ", tags$b(rr_val), "). ",
               "This coincides with ",
               if (best_cp_year <= 2019)
                 "the Anti-Drug Campaign and Martial Law period."
               else if (best_cp_year <= 2021)
                 "the COVID-19 pandemic and related restrictions."
               else "the post-pandemic recovery period.",
               " The change is ",
               tags$b(style = paste0("color:", dir_col, ";"),
                      ifelse(sig_txt == "Yes",
                             "statistically significant",
                             "not statistically significant")),
               " at the 95% credible level."
             )
    )
  })
  
  output$cp_box_year <- renderInfoBox({
    infoBox("When Did It Change?",
            ifelse(is.null(best_cp_year), "N/A", best_cp_year),
            icon = icon("bolt"), color = "green", fill = TRUE)
  })
  output$cp_box_change <- renderInfoBox({
    val <- if (!is.null(cp_results)) {
      rr  <- cp_results$RR_Change[which.min(cp_results$WAIC)]
      pct <- round((rr - 1) * 100, 1)
      paste0(ifelse(pct > 0, "+", ""), pct, "%")
    } else "N/A"
    infoBox("Crime Change After Breakpoint", val,
            icon = icon("percent"), color = "orange", fill = TRUE)
  })
  output$cp_box_sig <- renderInfoBox({
    sig <- if (!is.null(cp_results))
      cp_results$Significant[which.min(cp_results$WAIC)] else "N/A"
    infoBox("Is This Change Significant?", sig,
            icon = icon("check-circle"), color = "blue", fill = TRUE)
  })
  
  output$cp_trend <- renderPlotly({
    if (is.null(cp_results)) {
      return(plotly_empty() %>% dark_plotly() %>%
               layout(title = list(text = "Run CPA model first",
                                   font = list(color = "#94a3b8"))))
    }
    d <- crime_clean %>% count(Year, name = "Count")
    p <- plot_ly(d, x = ~Year, y = ~Count, type = "scatter",
                 mode = "lines+markers", fill = "tozeroy",
                 fillcolor = "rgba(59,130,246,0.08)",
                 line   = list(color = "#3b82f6", width = 2),
                 marker = list(color = "#f97316", size = 7),
                 name   = "Observed",
                 hovertemplate = "<b>%{x}</b><br>%{y:,} crimes<extra></extra>")
    if (!is.null(best_cp_year)) {
      y_max <- max(d$Count, na.rm = TRUE)
      p <- p %>%
        add_segments(x = best_cp_year, xend = best_cp_year,
                     y = 0, yend = y_max * 1.05,
                     line = list(color = "#10b981", width = 2.5, dash = "dash"),
                     name = paste("Change-Point:", best_cp_year)) %>%
        add_annotations(x = best_cp_year, y = y_max,
                        text      = paste0("<b>", best_cp_year,
                                           " - Pattern Changed</b>"),
                        showarrow = FALSE,
                        font      = list(color = "#10b981", size = 11),
                        xanchor   = "left", yanchor = "top") %>%
        add_annotations(
          x = (min(d$Year) + best_cp_year) / 2, y = y_max * 0.5,
          text = "Before", showarrow = FALSE,
          font = list(color = "#64748b", size = 14), xanchor = "center") %>%
        add_annotations(
          x = (best_cp_year + max(d$Year)) / 2, y = y_max * 0.5,
          text = "After", showarrow = FALSE,
          font = list(color = "#64748b", size = 14), xanchor = "center")
    }
    p %>% dark_plotly() %>%
      layout(xaxis = list(title = "", tickmode = "linear", dtick = 1),
             yaxis = list(title = "Crime Count"),
             hovermode = "x unified")
  })
  
  output$cp_prov_bar <- renderPlotly({
    if (is.null(prov_cp)) {
      return(plotly_empty() %>% dark_plotly() %>%
               layout(title = list(text = "Run CPA model first",
                                   font = list(color = "#94a3b8"))))
    }
    plot_ly(prov_cp,
            x = ~Best_CP_Year,
            y = ~reorder(Province, Best_CP_Year),
            type = "bar", orientation = "h",
            marker = list(
              color = ifelse(prov_cp$Significant == "Yes",
                             "#10b981", "#334155"),
              line  = list(color = "#0f172a", width = 1)),
            text         = ~Best_CP_Year,
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>CP Year: %{x}<extra></extra>") %>%
      dark_plotly() %>%
      layout(xaxis  = list(title = "Change-Point Year", range = c(2014, 2026)),
             yaxis  = list(title = ""),
             margin = list(l = 130, t = 10, b = 30, r = 40))
  })
  
  output$cp_pct_change <- renderPlotly({
    if (is.null(prov_cp)) return(plotly_empty() %>% dark_plotly())
    d <- prov_cp %>% mutate(Pct = round((RR_Change - 1) * 100, 1))
    plot_ly(d, x = ~Pct, y = ~reorder(Province, Pct),
            type = "bar", orientation = "h",
            marker = list(
              color = ifelse(d$Pct > 0, "#ef4444", "#10b981"),
              line  = list(color = "#0f172a", width = 1)),
            text         = ~paste0(ifelse(Pct > 0, "+", ""), Pct, "%"),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Change: %{text}<extra></extra>") %>%
      add_segments(x = 0, xend = 0, y = 0, yend = nrow(d) + 0.5,
                   line = list(color = "#64748b", dash = "dot", width = 1.5),
                   name = "No Change") %>%
      dark_plotly() %>%
      layout(xaxis = list(title = "% Change in Crime"),
             yaxis = list(title = ""), showlegend = FALSE,
             margin = list(l = 130, t = 10, b = 30, r = 60))
  })
  
  # ── DATA TABLE ───────────────────────────────────────────
  output$data_tbl <- renderDT({
    tbl_data <- fdata() %>%
      count(Province, Year, Month, Category, name = "Count") %>%
      arrange(Province, Year, Month) %>%
      mutate(Month = month.name[Month])
    
    max_count <- max(tbl_data$Count, na.rm = TRUE)
    
    datatable(
      tbl_data,
      filter   = "top",
      rownames = FALSE,
      colnames = c("Province", "Year", "Month", "Category", "Crime Count"),
      options  = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        buttons    = c("copy", "csv", "excel"),
        columnDefs = list(
          list(className = "dt-center", targets = c(1, 2, 4))
        ),
        initComplete = JS(
          "function(settings, json){",
          "$(this.api().table().header())",
          ".css({'background-color':'#1e293b',",
          "'color':'#3b82f6','border-bottom':'1px solid #334155'});",
          "}"
        )
      ),
      extensions = "Buttons",
      class      = "stripe hover"
    ) %>%
      formatStyle(
        "Count",
        background         = styleColorBar(c(0, max_count), "#1d4ed8"),
        backgroundSize     = "100% 90%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle("Province", fontWeight = "bold") %>%
      formatCurrency("Count",
                     currency = "", interval = 3, mark = ",", digits = 0)
  })
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui = ui, server = server)