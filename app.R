# R Shiny application for display of International Migrant Stock 2026

# change this to point to the directory holding the app.R script
ms_dir <<- "D:/msShiny"

# Load required libraries
library(shiny)
library(ggplot2)
#library(openxlsx)
#library(readxl)
library(data.table)
library(dplyr)
library(bslib)
library(plotly)
library(DT)
library(lubridate)
library(RJSONIO)
library(httr)
library(jsonlite)
library(RCurl)
library(tictoc)
library(Hmisc)
library(tidyverse)

# load R functions
lf <- list.files(file.path(ms_dir,"R","functions"))
for (i in 1:length(lf)) {
  source(file.path(ms_dir, "R", "functions", lf[i]))
}

output_dir <<- file.path(ms_dir, "output")

# Load datasets
source(file.path(ms_dir, "R", "source", "load_datasets.R"))

# set start and end years for estimates
yrs_out_start <<- 1990
yrs_out_end <<- 2026

# Prepare the list of locations (LocID and LocName)
locs <- unique(MS_Locations)


# UI part of the Shiny App
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera"),
  
  # Add CSS for loading screen
  tags$head(
    tags$style(HTML("
      .loading-spinner {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1000;
      }
      .loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background-color: rgba(255, 255, 255, 0.7);
        z-index: 999;
        display: none;
      }
      .loading-active .loading-overlay {
        display: block;
      }
      .tabbable > .nav > li > a[data-value = 'notes_for_analysts']{
        color: orange; 
      }
    "))
  ),
  
  # Loading overlay
  div(class = "loading-overlay",
      div(class = "loading-spinner",
          tags$div(class = "spinner-border text-primary"),
          tags$div(style = "margin-top: 10px", "Loading country data...")
      )
  ),
  
  # Title
  titlePanel(
    div(
      class = "container-fluid",
      style = "background-color: #f8f9fa; padding: 20px; margin-bottom: 20px;",
      h1("International Migrant Stock 2026", 
         style = "color: #2c3e50; font-weight: 500;")
    )
  ),
  # Sidebar for country selection
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,
      # Sidebar content
      div(
        class = "sidebar-content sidebar-equal-height",
        #style = "height: 100%; overflow-y: auto; margin-bottom: 10px; padding: 10px;",
        tags$style(type='text/css',
                   ".selectize-dropdown-content{
                 height: 1000px;
                 width: 90%;
                 background-color: #b0c4de;
                }"),
        selectInput("country_name", "Select Destination:",
                    choices = c("Select a location" = "", sort(locs$LocName)), 
                    selected = ""),
        checkboxInput("show_excluded", 
                      label = "Show excluded empirical data", 
                      value = FALSE),
        checkboxInput("ms2026", label = tagList("MS2026 (incl. refugees)",
                                                div(style = "display: inline-block; border-top: 4px solid black; width: 20px; margin-left: 5px;")
        ), value = TRUE),
        checkboxInput("ms2026excl", label = tagList("MS2026 (excl. refugees)",
                                                    div(style = "display: inline-block; border-top: 4px dashed black; width: 20px; margin-left: 5px;")
        ), value = TRUE),
        checkboxInput("ms2024", label = tagList("MS2024",
                                                div(style = "display: inline-block; background-color: red; width: 10px; height: 10px; border-radius: 50%; margin-left: 5px;")
        ), value = TRUE),
        checkboxInput("ms2020", label = tagList("MS2020",
                                                div(style = "display: inline-block; background-color: yellow; width: 10px; height: 10px; border-radius: 50%; margin-left: 5px;")
        ), value = TRUE),
        checkboxInput("unhcr", label = tagList("UNHCR",
                                               div(style = "display: inline-block; border-top: 4px solid #00FF00; width: 20px; margin-left: 5px;")
        ), value = TRUE),
        checkboxInput("show_foreign_born", label = "Show foreign-born", value = TRUE),
        checkboxInput("show_foreign_citizen", label = "Show foreign citizen", value = TRUE),
        uiOutput("lines_ui"),
      ),
      # button to run update to estimates
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px;",
        actionButton('update_estimates', 'Update MS estimates',
                     icon("paper-plane"), 
                     style = "width: 100%; margin-bottom: 0; background-color: green; color: white;")
      ),
      # button to run update to estimates
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px;",
        checkboxInput("static_plots", label = "Generate static plots", value = FALSE),
        uiOutput("lines_ui")
      ),
      # button to download DataLoader excel template
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px;",
        downloadButton("download_dt", "Download DataLoader") %>%
          tagAppendAttributes(
            style = "width: 100%; margin-bottom: 0; background-color: blue; color: white; "
          )
      ),
      # Upload new empirical data
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px;",
        h5(style = "margin-top: 0; margin-bottom: 10px;", "Upload:"),
        div(
          style = "display: grid; grid-template-columns: 1fr; gap: 8px;",
          fileInput("upload_dt", "Load new empirical data", accept = c(".xlsx"),
                    multiple = FALSE
          ) %>% tagAppendAttributes(style = "margin-bottom: 0; width: 100%;")
        )
      ),
      # button to refresh empirical data from DemoData
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px;",
        actionButton('refresh_empirical', 'Refresh empirical DemoData',
                     icon("spinner"), 
                     style = "width: 100%; margin-bottom: 0; background-color: lightblue; color: black;")
      ),
      
    ),
    mainPanel(
      class = "container-fluid",
      width=9,
      tabsetPanel(
        id = "tabset",
        
        ##############################################################
        ##############################################################
        # open tab panel for "Total migrant stock"
        tabPanel("Total migrant stock", 
                 div(
                   class = "plot-container",
                   style = "display: flex; flex-direction: column; height: 90%;",
                   # slider selections
                   div(
                     style = "padding: 10px 0; border-top: 1px solid #ddd; margin-top: 10px;",
                     div(
                       style = "display: flex; justify-content: center; margin-bottom: 10px;",
                       div(
                         style = "display: flex; gap: 20px; align-items: center;",
                         tags$style(type='text/css', ".selectize-dropdown-content{
                       height: 600px;
                       width: 90%;
                       background-color: #b0c4de;
                      }"),
                         sliderInput("YearRange_Total", "Year range", 
                                     min = 1950, max = 2030, sep = "",
                                     value = c(yrs_out_start-5, yrs_out_end) 
                         )
                         
                       )
                     )),
                   # Plot
                   div(
                     style = "flex-grow: 1;",
                     plotlyOutput("totals_plot", height = "100%")
                   )
                 ),
                 div(
                   style = "padding: 10px 0; border-top: 1px solid #ddd; margin-top: 10px;",
                   # Data process type filters
                   div(
                     style = "display: flex; justify-content: center; margin-bottom: 10px;",
                     div(
                       style = "display: flex; gap: 20px; align-items: center;",
                       checkboxInput("census", label = tagList("Census", tags$svg(width="16", height="16", viewBox="0 0 16 16", style="margin-left: 5px; vertical-align: middle;",
                                                                                  tags$polygon(points="0 8,8 16,16 8,8 0", fill="#A020F0", stroke="#A020F0", strokeWidth="1"))), value = TRUE),
                       checkboxInput("estimate", label = tagList("Estimate", tags$svg(width="16", height="16", viewBox="0 0 16 16", style="margin-left: 5px; vertical-align: middle;",
                                                                                      tags$polygon(points="8,0 0,16 15,16", fill="orange", stroke="orange", strokeWidth="1"))), value = TRUE),
                       checkboxInput("survey", label = tagList("Survey", div(style = "display: inline-block; background-color: cornflowerblue; width: 13px; height: 13px; margin-left: 5px;")), value = TRUE)
                     )
                   )),
                 div(
                   class = "plot-container",
                   style = "display: flex; flex-direction: column; height: 90%;",
                   # Plot
                   div(
                     style = "flex-grow: 1;",
                     plotlyOutput("MigShare_plot", height = "100%")
                   )
                 ),
        ), # close tab panel for "Total migrant stock"
        ##############################################################
        ##############################################################
        # open tab panel for "Proportion Female"
        tabPanel("Proportion Female", 
                 div(
                   class = "plot-container",
                   style = "display: flex; flex-direction: column; height: 90%;",
                   # slider selections
                   div(
                     style = "padding: 10px 0; border-top: 1px solid #ddd; margin-top: 10px;",
                     div(
                       style = "display: flex; justify-content: center; margin-bottom: 10px;",
                       div(
                         style = "display: flex; gap: 20px; align-items: center;",
                         tags$style(type='text/css', ".selectize-dropdown-content{
                     height: 600px;
                     width: 90%;
                     background-color: #b0c4de;
                    }"),
                         sliderInput("YearRange_propF", "Year range", 
                                     min = 1950, max = 2030, sep = "",
                                     value = c(yrs_out_start-5, yrs_out_end) 
                         )
                         
                       )
                     )),
                   # Plot
                   div(
                     style = "flex-grow: 1;",
                     plotlyOutput("propF_plot", height = "100%")
                   )
                 ),
                 div(
                   style = "padding: 10px 0; border-top: 1px solid #ddd; margin-top: 10px;",
                   # Data process type filters
                   div(
                     style = "display: flex; justify-content: center; margin-bottom: 10px;",
                     div(
                       style = "display: flex; gap: 20px; align-items: center;",
                       checkboxInput("census", label = tagList("Census", tags$svg(width="16", height="16", viewBox="0 0 16 16", style="margin-left: 5px; vertical-align: middle;",
                                                                                  tags$polygon(points="0 8,8 16,16 8,8 0", fill="#A020F0", stroke="#A020F0", strokeWidth="1"))), value = TRUE),
                       checkboxInput("estimate", label = tagList("Estimate", tags$svg(width="16", height="16", viewBox="0 0 16 16", style="margin-left: 5px; vertical-align: middle;",
                                                                                      tags$polygon(points="8,0 0,16 15,16", fill="orange", stroke="orange", strokeWidth="1"))), value = TRUE),
                       checkboxInput("survey", label = tagList("Survey", div(style = "display: inline-block; background-color: cornflowerblue; width: 13px; height: 13px; margin-left: 5px;")), value = TRUE)
                     )
                   ))
        ), # close tab panel for "Proportion female"
        ##############################################################
        ##############################################################
        # open tab panel for "Origins (all)"
        tabPanel("Origins (all)", 
                 div(
                   class = "plot-container",
                   style = "display: flex; flex-direction: column; height: 90%;",
                   div(
                     style = "padding: 10px 0; border-top: 1px solid #ddd; margin-top: 10px;",
                     # Data process type filters
                     div(
                       style = "display: flex; justify-content: center; margin-bottom: 10px;",
                       div(
                         style = "display: flex; gap: 20px; align-items: center;",
                         tags$style(type='text/css', ".selectize-dropdown-content{
                 height: 600px;
                 width: 90%;
                 background-color: #b0c4de;
                }"),
                         
                         selectInput("max_origins", "Select number of origins to plot:", choices = c(1:237), selected = 10),
                         sliderInput("YearRange_OriginsAll", "Year range", 
                                     min = yrs_out_start, max = 2030, sep = "",
                                     value = c(yrs_out_start, yrs_out_end))))),
                   
                   
                   div(
                     class = "plot-container",
                     style = "display: flex; flex-direction: column; height: 90%;",
                     # Plot
                     div(
                       style = "flex-grow: 1;",
                       plotlyOutput("origins_all_plot", height = "100%")
                     )
                   ))
        ), # close tab panel for "Origins (all)"
        
        ##############################################################
        ##############################################################
        # open tab panel for "Origins (one at a time)"
        tabPanel("Origins (one at a time)", 
                 div(
                   class = "plot-container",
                   style = "display: flex; flex-direction: column; height: 90%;",        
                   # Dropdown and slider selections
                   div(
                     style = "padding: 10px 0; border-top: 1px solid #ddd; margin-top: 10px;",
                     div(
                       style = "display: flex; justify-content: center; margin-bottom: 10px;",
                       div(
                         style = "display: flex; gap: 20px; align-items: center;",
                         tags$style(type='text/css', ".selectize-dropdown-content{
                 height: 600px;
                 width: 90%;
                 background-color: #b0c4de;
                }"),
                         selectInput("origin_name", "Select Origin:", 
                                     choices = c("Select a location"="")),
                         selectInput("SexName_OriginOne", "Show sex:",
                                     choices = c("Both sexes", "Female", "Male"),
                                     selected = "Both sexes"),
                         sliderInput("YearRange_OriginOne", "Year range", 
                                     min = 1950, max = 2030, sep = "",
                                     value = c(yrs_out_start-5, yrs_out_end) 
                         )
                         
                       )
                     )),
                   # Plot
                   div(
                     style = "flex-grow: 1;",
                     plotlyOutput("origin_plot", height = "100%")
                   ),
                   div(
                     style = "padding: 10px 0; border-top: 1px solid #ddd; margin-top: 10px;",
                     # Data process type filters
                     div(
                       style = "display: flex; justify-content: center; margin-bottom: 10px;",
                       div(
                         style = "display: flex; gap: 20px; align-items: center;",
                         checkboxInput("census", label = tagList("Census", tags$svg(width="16", height="16", viewBox="0 0 16 16", style="margin-left: 5px; vertical-align: middle;",
                                                                                    tags$polygon(points="0 8,8 16,16 8,8 0", fill="#A020F0", stroke="#A020F0", strokeWidth="1"))), value = TRUE),
                         checkboxInput("estimate", label = tagList("Estimate", tags$svg(width="16", height="16", viewBox="0 0 16 16", style="margin-left: 5px; vertical-align: middle;",
                                                                                        tags$polygon(points="8,0 0,16 15,16", fill="orange", stroke="orange", strokeWidth="1"))), value = TRUE),
                         checkboxInput("survey", label = tagList("Survey", div(style = "display: inline-block; background-color: cornflowerblue; width: 13px; height: 13px; margin-left: 5px;")), value = TRUE)
                       )
                     ))
                 ),
                 div(
                   class = "plot-container",
                   style = "display: flex; flex-direction: column; height: 90%;",
                   # Plot
                   div(
                     style = "flex-grow: 1;",
                     plotlyOutput("origin_propF_plot", height = "100%")
                   )
                 ),
        ), # close tab panel for "Origins (one at a time)"
        ##############################################################
        ##############################################################
        # open tab panel for "Age"
        tabPanel("Age", 
                 div(
                   class = "plot-container",
                   style = "display: flex; flex-direction: column; height: 90%;",
                   # Dropdown and slider selections
                   div(
                     style = "padding: 10px 0; border-top: 1px solid #ddd; margin-top: 10px;",
                     div(
                       style = "display: flex; justify-content: center; margin-bottom: 10px;",
                       div(
                         style = "display: flex; gap: 20px; align-items: center;",
                         tags$style(type='text/css', ".selectize-dropdown-content{
                 height: 600px;
                 width: 90%;
                 background-color: #b0c4de;
                }"),
                         
                         # Age group selector
                         selectInput("AgeGroup", "Age group:",
                                     choices = c(
                                       # Broad age groups 
                                       "Total",
                                       "0-14", "15-24", "25-49", "50+", "60+", "70+", "80+",
                                       # Standard 5-year age groups
                                       "0-4","5-9","10-14","15-19","20-24",
                                       "25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74",
                                       "75-79","80-84","85+"),
                                     selected = "Total"),
                         
                         # Year range slider shared across all three age plots
                         sliderInput("YearRange_Age", "Year range",
                                     min   = 1950, max = 2030, sep = "",
                                     value = c(yrs_out_start, yrs_out_end))
                       )
                     )),
                   
                   # Plot1: migrant stock
                   div(
                     style = "display: flex; align-items: flex-start; gap: 10px; margin-top: 15px;",
                     
                     div(
                       style = "flex: 1; min-width: 0;",
                       h5(style = "text-align:center; color:#2c3e50;", 
                          "Migrant stock (thousands)"),
                       plotlyOutput("age_plot_stock", height = "400px")
                     ),
                     
                     div(
                       style = "width: 140px; flex-shrink: 0; padding-top: 36px;",
                       tags$div(
                         style = "font-size: 15px;",
                         selectInput("SexName_Age", "Sex:",
                                     choices  = c("Both sexes", "Male", "Female"),
                                     selected = "Both sexes",
                                     width = "150px"),
                         selectInput("AgeScale", "Scale:",
                                     choices  = c("Counts", "Proportion"),
                                     selected = "Counts",
                                     width = "150px")
                       )
                     )
                   ),
                   
                   # Plot2: sex ratios of migrant stock
                   div(
                     style = "flex-grow: 1;",
                     h5(style = "text-align:center; color:#2c3e50; margin-top:15px;", 
                        "Sex ratio of migrant stock"),
                     plotlyOutput("age_plot_sexratio", height = "400px")
                   ),
                   
                   # Plot3: Non-refugee migrant stock estimates
                   div(
                     style = "flex-grow: 1;",
                     h5(style = "text-align:center; color:#2c3e50; margin-top:15px;", 
                        "Non-refugee migrant stock estimates"),
                     plotlyOutput("age_plot_nonrefugee", height = "400px")
                   )
                 )
        ), # close tab panel for "Age"
        ##############################################################
        ##############################################################
        # open tab panel for "Diaspora"
        tabPanel("Diaspora", 
                 div(
                   class = "plot-container",
                   style = "display: flex; flex-direction: column; height: 90%;",
                   # dropdown menu for number of destinations to plot
                   div(
                     style = "padding: 10px 0; border-top: 1px solid #ddd; margin-top: 10px;",
                     # Data process type filters
                     div(
                       style = "display: flex; justify-content: center; margin-bottom: 10px;",
                       div(
                         style = "display: flex; gap: 20px; align-items: center;",
                         tags$style(type='text/css', ".selectize-dropdown-content{
                 height: 600px;
                 width: 90%;
                 background-color: #b0c4de;
                }"),
                         
                         selectInput("max_destinations", "Select number of destinations to plot:", choices = c(1:237), selected = 10),
                         sliderInput("YearRange_Diaspora", "Year range", 
                                     min = yrs_out_start, max = 2030, sep = "",
                                     value = c(yrs_out_start, yrs_out_end))))),
                   
                   
                   div(
                     class = "plot-container",
                     style = "display: flex; flex-direction: column; height: 90%;",
                     # Plot
                     div(
                       style = "flex-grow: 1;",
                       plotlyOutput("Diaspora_plot", height = "100%")
                     )
                   ))
        ), # close tab panel for "Diaspora"
        ##############################################################
        ##############################################################
        # open tab panel for "Notes for analysts"
        tabPanel(value = "notes_for_analysts", title = "Notes for analysts",
                 div(
                   style = "padding: 20px;",
                   uiOutput("analyst_notes")
                 )
        ) # close tab panel for "Notes"
        ##############################################################
        ##############################################################
        
      ) # close tabset panel
    ) # close for main panel
  ) # close for sidebar layout
) # close for fluid page




##############################################################
##############################################################
##
## Server part of the Shiny App -- this is data processing, estimation and plotting happens
##
##############################################################
##############################################################
server <- function(input, output, session) {
  
  # Reactive value to store cached data for countries
  country_cache <- reactiveVal(list())
  
  # Reactive value to store the currently selected country's data
  current_data <- reactiveVal(NULL)
  
  ##############################################################
  ##############################################################
  # Observe and respond to changes to country name drop-down selection 
  observeEvent(input$country_name, {
    req(input$country_name != "")
    loc_id <- locs[locs$LocName == input$country_name, "LocID"]
    iso3 <- locs[locs$LocID == loc_id, "ISO3"]
    
    cache <- country_cache()
    
    # check that data can be loaded
    an.error.occurred <- FALSE
    tryCatch({
      
      results <- NULL
      load(file.path(ms_dir, "output", paste0(loc_id, "_results.rda"))) 
      
    }, error = function(e){ an.error.occurred <<- TRUE }) # close tryCatch
    eMessage <- ifelse(an.error.occurred, "No estimates available. Please run Update MS estimates", "")
    
    if (eMessage != "") {
      
      showNotification(paste("Error:", eMessage), type = "error")
      cache[[as.character(loc_id)]] <- NULL
      country_cache(cache)
      current_data(NULL)
      
    } else {
      #########################################
      # Fetch new data and update cache
      cache[[as.character(loc_id)]] <- results
      country_cache(cache)
      current_data(results)
      #########################################
      
      # populate number of origins dropdown based on available data for the selected country
      results <- results
      updateSelectInput(session, "max_origins",
                        choices = c(1:length(results$origins)), selected = min(length(results$origins), 10))
      
      # populate origin dropdown based on available data for the selected country
      updateSelectInput(session, "origin_name",
                        choices = c("Select a location"="", results$origins),
                        selected = ifelse(length(results$origins) > 0 , results$origins[1], "Select a location"))
      
    }
  })
  
  ##############################################################
  ##############################################################
  # Total migrant stock plot
  plot_total <- reactive({
    req(input$country_name != "")
    data <- current_data()
    req(data)  # Ensure data is available
    
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      iso3 <- selected_loc$ISO3
      
      p_total <- ms_plots_emp(loc_id = loc_id, 
                              LocName = loc_name,
                              input = input, 
                              census_refs = census_ref_dates %>% dplyr::filter(LocID == loc_id) %>% dplyr::select(reference_date), # data frame with one column "reference_date", 
                              chartid = "total", 
                              indata_emp = data$DT, 
                              indata_modelled = data$DT_modelled$df, 
                              xmin = min(input$YearRange_Total),
                              xmax = max(input$YearRange_Total),
                              ymax = (max(ms2024$value[ms2024$LocID == loc_id]/1000))*1.1, 
                              ms2024_plt = ms2024 %>% dplyr::filter(LocID == loc_id & sex == 0), 
                              ms2020_plt = ms2020 %>% dplyr::filter(LocID == loc_id & sex == 0))
      
      constraint_note <- data$DT_modelled$constraint[!is.na(data$DT_modelled$constraint)]
      if (length(constraint_note) > 0) {
        constraint_note <- paste(constraint_note, collapse = "\n")
      } else {
        constraint_note <- NULL
      }
      
      Warning <- data$Warnings$Warning_Total
      # Convert to plotly and add hover information for vertical lines
      ply <- ggplotly(p_total, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0("<span style='font-size: 12pt'>", loc_name, "</span> <span style='font-size: 6pt'>", " (LocID = ", loc_id, ", ISO = ", iso3, ")", "</span>",
                                     '<br>',
                                     '<sup>',
                                     'International migrant stock (all origins)','</sup>')),
          annotations = 
            list(x = 1, y = 1.1, text = paste0(constraint_note, "\n", Warning), 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10, color="blue"))
        )
      return(ply)
    } else {
      return(NULL)
    }
  })
  
  # render the plot for Shiny display
  output$totals_plot <- NULL
  output$totals_plot <- renderPlotly({
    plot_total() %>%
      layout(
        autosize = TRUE,
        height = session$clientData$output_country_plot_width * 0.6
      )
  })
  
  ##############################################################
  ##############################################################
  # Migrant share of population
  plot_MigShare <- reactive({
    #req(input$country_code != "")
    req(input$country_name != "")
    data <- current_data()
    req(data)  # Ensure data is available
    
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      
      p_MigShare <- ms_plot_share(loc_id = loc_id, 
                                  LocName = loc_name,
                                  input = input,
                                  MS_share = data$MS_share)
      
      # Convert to plotly and add hover information for vertical lines
      ply <- ggplotly(p_MigShare, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0(loc_name,
                                     '<br>',
                                     '<sup>',
                                     'International migrant stock as a share of the total population (MS 2020 and 2024 excl. refugees and asylum seekers) ','</sup>'))
        )
      return(ply)
    } else {
      return(NULL)
    }
  })
  
  # render the plot for Shiny display
  output$MigShare_plot <- NULL
  output$MigShare_plot <- renderPlotly({
    plot_MigShare() %>%
      layout(
        autosize = TRUE,
        height = session$clientData$output_country_plot_width * 0.6
      )
  })
  
  
  ##############################################################
  ##############################################################
  # Proportion female plot
  plot_propF <- reactive({
    req(input$country_name != "")
    data <- current_data()
    req(data)  # Ensure data is available
    
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      
      p_propF <- ms_plots_emp(loc_id = loc_id, 
                              LocName = loc_name,
                              input = input, 
                              census_refs = census_ref_dates %>% dplyr::filter(LocID == loc_id) %>% dplyr::select(reference_date), # data frame with one column "reference_date", 
                              chartid = "propF", 
                              indata_emp = data$DF, 
                              indata_modelled = data$DF_modelled$df, 
                              xmin = min(input$YearRange_propF),
                              xmax = max(input$YearRange_propF),
                              ymax = 100, 
                              ms2024_plt = ms2024 %>% dplyr::filter(LocID == loc_id & sex != 0) %>% group_by(year) %>% summarise(value = value[sex == 2]/sum(value)), 
                              ms2020_plt = ms2020 %>% dplyr::filter(LocID == loc_id & sex != 0) %>% group_by(year) %>% summarise(value = value[sex == 2]/sum(value)))
      
      constraint_note <- data$DF_modelled$constraint[!is.na(data$DF_modelled$constraint)]
      if (length(constraint_note) > 0) {
        constraint_note <- paste(constraint_note, collapse = "\n")
      } else {
        constraint_note <- NULL
      }
      constraint_note <- paste(c(constraint_note, data$Warnings$Warning_PropF), collapse = "\n")
      
      
      # Convert to plotly and add hover information for vertical lines
      ply <- ggplotly(p_propF, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0(loc_name,
                                     '<br>',
                                     '<sup>',
                                     'Female share of international migrant stock (all origins)','</sup>')),
          annotations = 
            list(x = 1, y = 1.1, text = paste0(constraint_note, "\n", " "), 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10, color="blue"))
        )
      return(ply)
    } else {
      return(NULL)
    }
  })
  # render the plot for Shiny display
  output$propF_plot <- NULL
  output$propF_plot <- renderPlotly({
    plot_propF() %>%
      layout(
        autosize = TRUE,
        height = session$clientData$output_country_plot_width * 0.6
      )
  })
  
  ##############################################################
  ##############################################################
  # Migrant stock for alluvial origins plot of multiple origins
  plot_origins_all <- reactive({
    req(input$country_name != "")
    data <- current_data()
    req(data)  # Ensure data is available
    
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      n_origins <- as.numeric(input$max_origins)
      minyr <- min(input$YearRange_OriginsAll)
      maxyr <- max(input$YearRange_OriginsAll)
      
      p_origins_all <- ms_plot_origins_alluvial(LocName = loc_name,
                                                YearRange = c(minyr,maxyr),
                                                DO_modelled_df = data$DO_modelled$df %>% dplyr::filter(sex == "both sexes"),
                                                DT_modelled = data$DT_modelled,
                                                n_origins = n_origins)
      
      Message <- ifelse(is.null(p_origins_all$plot_env$DO_modelled_df), "No data to plot. Check that migrant stock has been estimated for at least one origin.","")
      
      # Convert to plotly and add hover information for vertical lines
      ply <- ggplotly(p_origins_all, tooltip = "text") %>%
        style(textposition = "right") %>% 
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0(loc_name,
                                     '<br>',
                                     '<sup>',
                                     'International migrant stock by origin (estimates for the 2026 revision)','</sup>')),
          annotations = 
            list(x = 0.1, y = 0.9, text = Message, 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='left', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=14, color="blue"))
        )
      return(ply)
    } else {
      return(NULL)
    }
  })
  
  # render the plot for Shiny display
  output$origins_all_plot <- NULL
  output$origins_all_plot <- renderPlotly({
    plot_origins_all() %>%
      layout(
        autosize = TRUE,
        width = session$clientData$output_country_plot_width * 1,
        height = 1000
      )
  })
  
  ##############################################################
  ##############################################################
  # Migrant stock by origin plot
  plot_origin <- reactive({
    req(input$country_name != "")
    data <- current_data()
    req(data)  # Ensure data is available
    
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      myorigin <- input$origin_name
      loc_id_org <- locs$LocID[locs$LocName == myorigin]
      mysex <- tolower(input$SexName_OriginOne)
      mysexid <- 0
      mysexid <- ifelse(mysex == "female", 2, mysexid)
      mysexid <- ifelse(mysex == "male", 1, mysexid)
      
      if (length(loc_id_org) != 0) {
        
        # parse international migrant stock counts data for this origin
        extract <- list(df = data$DO_modelled$df %>% dplyr::filter(origin == myorigin, sex == mysex),
                        constraint = data$DO_modelled$constraint %>% dplyr::filter(origin == myorigin))
        
        if (!is.null(data$DO)) {
          indata_emp <- data$DO[data$DO$Origin == myorigin & data$DO$SexID == mysexid,] %>% mutate(VALUE = DataValue)
        } else {
          indata_emp <- NULL
        }
        if (!nrow(extract$df) > 0) {
          extract <- NULL
        }
        
        ms2024_plt = ms2024org %>% dplyr::filter(LocIDdest == loc_id & sex == mysexid & LocIDorg == loc_id_org)
        ms2020_plt = ms2020org %>% dplyr::filter(LocIDdest == loc_id & sex == mysexid & LocIDorg == loc_id_org)
        
      } else {
        extract <- list(df = NULL, constraint = NULL)
        indata_emp <- NULL
        ms2024_plt <- NULL
        ms2020_plt <- NULL
      }
      p_origin <- ms_plots_emp(loc_id = loc_id, 
                               LocName = loc_name,
                               input = input, 
                               census_refs = census_ref_dates %>% dplyr::filter(LocID == loc_id) %>% dplyr::select(reference_date), # data frame with one column "reference_date", 
                               chartid = "origin_one", 
                               indata_emp = indata_emp, 
                               indata_modelled = extract$df,  
                               xmin = min(input$YearRange_OriginOne),
                               xmax = max(input$YearRange_OriginOne),
                               ymax = (max(ms2024$value[ms2024$LocID == loc_id]/1000))*1.1, 
                               ms2024_plt = ms2024_plt,
                               ms2020_plt = ms2020_plt)
      
      constraint_note_origin <- extract$constraint$constraint[!is.na(extract$constraint$constraint)]
      if (length(constraint_note_origin) > 0) {
        constraint_note_origin <- paste(constraint_note_origin, collapse = "\n")
      } else {
        constraint_note_origin <- " "
      }
      constraint_note_origin <- paste(c(constraint_note_origin, data$Warnings$Warning_Origin), collapse = "\n")
      
      # parse proportion female for this origin
      if (length(loc_id_org) != 0) {
        if (!is.null(data$DO)){
          extract_empirical <- data$DO %>% 
            dplyr::filter(Origin == myorigin, SexID %in% c(1, 2))
        } else {
          extract_empirical <- NULL
        }
        if (!is.null(extract_empirical)) {
          if (nrow(extract_empirical) > 0) {
            extract_empirical <- extract_empirical %>% 
              pivot_wider(names_from = SexID, values_from = DataValue) %>% 
              mutate(DataValue = `2`/(`2`+`1`),
                     VALUE = DataValue)
          }
        } else {
          extract_empirical <- NULL
        }
        extract_modelled <- data$DO_modelled$df %>% 
          dplyr::filter(origin == myorigin, sex %in% c("female", "male")) %>%
          dplyr::select(year, sex, value) %>% 
          pivot_wider(names_from = sex, values_from = value) %>% 
          mutate(IMF = female,
                 IMM = male, 
                 value = female/(female+male)) %>%
          dplyr::select(year, IMF, IMM, value) %>% 
          left_join(data$DO_modelled$df %>% 
                      dplyr::filter(origin == myorigin, sex %in% c("female", "male")) %>%
                      dplyr::select(year, sex, refugees) %>% 
                      pivot_wider(names_from = sex, values_from = refugees) %>%
                      mutate(rfgF = female,
                             rfgM = male) %>% 
                      dplyr::select(year, rfgF, rfgM), by = "year") %>% 
          mutate(IM_noRfgF = IMF - rfgF,
                 IM_noRfgM = IMM - rfgM)
        if (!nrow(extract_modelled) > 0) {
          extract_modelled <- NULL
        }
        
        ms2024_plt = ms2024org %>% dplyr::filter(LocIDdest == loc_id & LocIDorg == loc_id_org & sex != 0) %>% group_by(year) %>% summarise(value = value[sex == 2]/sum(value))
        ms2020_plt = ms2020org %>% dplyr::filter(LocIDdest == loc_id & LocIDorg == loc_id_org & sex != 0) %>% group_by(year) %>% summarise(value = value[sex == 2]/sum(value))
        
      } else {
        extract_empirical <- NULL
        extract_modelled <- NULL
        ms2024_plt <- NULL
        ms2020_plt <- NULL
      }
      p_origin_propF <- ms_plots_emp(loc_id = loc_id, 
                                     LocName = loc_name,
                                     input = input, 
                                     census_refs = census_ref_dates %>% dplyr::filter(LocID == loc_id) %>% dplyr::select(reference_date), # data frame with one column "reference_date", 
                                     chartid = "propF", 
                                     indata_emp = extract_empirical, 
                                     indata_modelled = extract_modelled, 
                                     xmin = min(input$YearRange_OriginOne),
                                     xmax = max(input$YearRange_OriginOne),
                                     ymax = 100, 
                                     ms2024_plt = ms2024_plt, 
                                     ms2020_plt = ms2020_plt)
      
      # Convert to plotly and add hover information for vertical lines
      ply_origin <- ggplotly(p_origin, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0(loc_name,
                                     '<br>',
                                     '<sup>',
                                     paste0("International migrant stock (", mysex, ") originating from ",myorigin),'</sup>')),
          annotations = 
            list(x = 1, y = 1.1, text = constraint_note_origin, 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10, color="blue"))
        )
      ply_origin_propF <- ggplotly(p_origin_propF, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0(loc_name,
                                     '<br>',
                                     '<sup>',
                                     paste0("Proportion female among international migrant stock originating from ",myorigin),'</sup>'))
        )
      
      ply <- list(ply_origin = ply_origin,
                  ply_origin_propF = ply_origin_propF)
      return(ply)
    } else {
      return(NULL)
    }
  })
  # render the plot for Shiny display
  output$origin_plot <- NULL
  output$origin_plot <- renderPlotly({
    plot_origin()$ply_origin %>%
      layout(
        autosize = TRUE,
        height = session$clientData$output_country_plot_width * 0.6
      )
  })
  
  output$origin_propF_plot <- NULL
  output$origin_propF_plot <- renderPlotly({
    plot_origin()$ply_origin_propF %>%
      layout(
        autosize = TRUE,
        height = session$clientData$output_country_plot_width * 0.6
      )
  })
  
  
  
  ##############################################################
  ##############################################################
  # Plot1: migrant stock
  parse_age_start <- function(age_group_str) {
    if (age_group_str == "85+") return(85)
    as.numeric(strsplit(age_group_str, "-")[[1]][1])
  }
  
  plot_age_stock <- reactive({
    req(input$country_name != "")
    data <- current_data()
    req(data)  # Ensure data is available
    
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      
      p_age_stock <- ms_plot_age_stock(loc_id = loc_id, 
                                       LocName = loc_name,
                                       input = input, 
                                       MS_age = data$DA, # empirical data
                                       MS_modelled = data$DA_modelled)
      
      Message <- paste(c(data$Warnings$Warning_Age), collapse = "\n")
      
      Message <- ifelse(is.null(p_age_stock$plot_env$indata_emp), "No data to plot. Check input Excel file to verify that 'Estimate_age_distribution' is TRUE in the parameters sheet\nand that 'Include_Age' is TRUE for at least one series in the INPUTS sheet.",Message)
      
      # Convert to plotly and add hover information for vertical lines
      ply <- ggplotly(p_age_stock, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0(loc_name,
                                     '<br>',
                                     '<sup>',
                                     paste0("Sex: ",input$SexName_Age),'</sup>')),
          annotations = 
            list(x = 1, y = 1.1, text = paste0(Message, "\n", " "), 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10, color="blue"))
        )
      return(ply)
    } else {
      return(NULL)
    }
  }) # close p_age_stock
  
  
  # Plot2: sex ratios of migrant stock
  plot_age_sexratio <- reactive({
    req(input$country_name != "")
    data <- current_data()
    req(data)
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id   <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      
      p_age_sexratio <- ms_plot_age_sexratio(loc_id      = loc_id,
                                             LocName     = loc_name,
                                             input       = input,
                                             MS_age      = data$DA,
                                             MS_modelled = data$DA_modelled)
      
      ply <- ggplotly(p_age_sexratio, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor     = "white",
            font        = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0(loc_name,
                                     '<br>',
                                     '<sup>',
                                     paste0("Sex ratio by age group: ", input$AgeGroup),
                                     '</sup>'))
        )
      return(ply)
    } else {
      return(NULL)
    }
  }) # close plot_age_sexratio
  
  
  # Plot3: Non-refugee migrant stock estimates
  plot_age_nonrefugee <- reactive({
    req(input$country_name != "")
    data <- current_data()
    req(data)
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id   <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      
      p_age_nonrefugee <- ms_plot_age_nonrefugee(loc_id      = loc_id,
                                                 LocName     = loc_name,
                                                 input       = input,
                                                 MS_modelled = data$DA_modelled)
      
      ply <- ggplotly(p_age_nonrefugee, tooltip = "text") %>%
        layout(
          hoverlabel = list(
            bgcolor     = "white",
            font        = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0(loc_name,
                                     '<br>',
                                     '<sup>',
                                     paste0("Non-refugee migrant stock  |  Age group: ", input$AgeGroup),
                                     '</sup>'))
        )
      return(ply)
    } else {
      return(NULL)
    }
  }) # close plot_age_nonrefugee  
  
  
  # render the plot for Shiny display
  output$age_plot_stock <- renderPlotly({
    plot_age_stock() %>%
      layout(autosize = TRUE,
             height   = session$clientData$output_country_plot_width * 0.6)
  })
  
  output$age_plot_sexratio <- renderPlotly({
    plot_age_sexratio() %>%
      layout(autosize = TRUE,
             height   = session$clientData$output_country_plot_width * 0.6)
  })
  
  output$age_plot_nonrefugee <- renderPlotly({
    plot_age_nonrefugee() %>%
      layout(autosize = TRUE,
             height   = session$clientData$output_country_plot_width * 0.6)
  })
  
  ##############################################################
  ##############################################################
  # Diaspora
  plot_Diaspora <- reactive({
    req(input$country_name != "")
    data <- current_data()
    req(data)  # Ensure data is available
    
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      
      p_Diaspora <- ms_plot_diaspora_alluvial(LocName = loc_name,
                                              input,
                                              diaspora = data$diaspora)
      
      Message <- ifelse(is.null(data$diaspora), "No data to plot.","")
      
      # Convert to plotly and add hover information for vertical lines
      ply <- ggplotly(p_Diaspora, tooltip = "text") %>%
        style(textposition = "right") %>% 
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(size = 12),
            bordercolor = "gray80"
          ),
          hovermode = "closest",
          title = list(text = paste0(loc_name,
                                     '<br>',
                                     '<sup>',
                                     'Diaspora (by destination)','</sup>')),
          annotations = 
            list(x = 0.1, y = 0.9, text = Message, 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='left', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=14, color="blue"))
        )
      return(ply)
    } else {
      return(NULL)
    }
  })
  
  # render the plot for Shiny display
  output$Diaspora_plot <- NULL
  output$Diaspora_plot <- renderPlotly({
    plot_Diaspora() %>%
      layout(
        autosize = TRUE,
        width = session$clientData$output_country_plot_width * 1,
        height = 1000
      )
  })
  
  ##############################################################
  ##############################################################
  # Notes for analysts: automated data quality checks
  output$analyst_notes <- renderUI({
    
    # If no country is selected, show a prompt
    if (is.null(input$country_name) || input$country_name == "") {
      return(div(
        style = "color: var(--bs-secondary); font-style: italic; margin-top: 40px; text-align: center;",
        icon("circle-info"), " Select a destination country to run analyst checks."
      ))
    }
    
    data <- current_data()
    
    if (is.null(data)) {
      return(div(
        class = "alert alert-warning",
        icon("triangle-exclamation"), " No estimates available for this country. Please run 'Update MS estimates' first."
      ))
    }
    
    selected_loc <- locs[locs$LocName == input$country_name, ]
    loc_id   <- selected_loc$LocID
    loc_name <- selected_loc$LocName
    
    flags <- list()  # collect all flagged issues
    
    
    # CHECK 1: Total migrant stock > 30% of total population
    tryCatch({
      if (!is.null(data$MS_share)) {
        share_data <- data$MS_share
        
        # Calculate migrant share(incl. refugees): y = val_new / pop * 100
        if (all(c("val_new", "pop") %in% names(share_data))) {
          share_data$share <- ifelse(
            share_data$pop > 0,
            share_data$val_new / share_data$pop * 100,
            NA
          )
          
          recent_shares <- share_data$share
          recent_shares <- recent_shares[!is.na(recent_shares)]
          max_share <- max(recent_shares, na.rm = TRUE)
          
          if (max_share > 30) {
            # find the year(s) where share exceeds 30%
            yr_col <- intersect(c("year","Year"), names(share_data))[1]
            if (length(yr_col) > 0) {
              flagged_yrs <- share_data[[yr_col]][share_data$share > 30 & !is.na(share_data$share)]
              yr_str <- paste(sort(unique(flagged_yrs)), collapse = ", ")
            } else {
              yr_str <- "unknown year(s)"
            }
            flags[["check1"]] <- list(
              severity = "warning",
              icon     = "triangle-exclamation",
              title    = "High migrant share of population",
              detail   = paste0(
                "The estimated total migrant stock exceeds 30% of the total population ",
                "in the following year(s): <strong>", yr_str, "</strong>. ",
                "Maximum observed share: <strong>", round(max_share, 1), "%</strong>. ",
                "Please verify population denominators and total stock estimates."
              )
            )
          }
        }
      }
    }, error = function(e) NULL)
    
    
    # CHECK 2: MS2026 estimates differ from MS2024 by more than 20%
    tryCatch({
      if (!is.null(data$DT_modelled) && !is.null(data$DT_modelled$df)) {
        ms26 <- data$DT_modelled$df
        ms24_loc <- ms2024 %>% dplyr::filter(LocID == loc_id, sex == 0)
        
        if (nrow(ms24_loc) > 0 && nrow(ms26) > 0) {
          # Align on common years
          yr_col26 <- intersect(c("year","Year"), names(ms26))[1]
          val_col26 <- intersect(c("value","Value"), names(ms26))[1]
          
          if (!is.na(yr_col26) && !is.na(val_col26)) {
            ms26_sub <- ms26 %>%
              dplyr::select(year = !!yr_col26, value26 = !!val_col26) %>%
              dplyr::filter(!is.na(value26))
            
            compare <- ms24_loc %>%
              dplyr::rename(year = year, value24 = value) %>%
              dplyr::inner_join(ms26_sub, by = "year") %>%
              dplyr::mutate(
                pct_diff = abs(value26 - value24) / value24 * 100
              ) %>%
              dplyr::filter(pct_diff > 20)
            
            if (nrow(compare) > 0) {
              worst <- compare %>% dplyr::slice_max(pct_diff, n = 1)
              flagged_yrs <- paste(sort(unique(compare$year)), collapse = ", ")
              flags[["check2"]] <- list(
                severity = "danger",
                icon     = "circle-exclamation",
                title    = "Large deviation from MS2024 estimates",
                detail   = paste0(
                  "The MS2026 total stock estimates differ from the MS2024 revision by more than 20% ",
                  "in year(s): <strong>", flagged_yrs, "</strong>. ",
                  "Largest discrepancy: <strong>", round(worst$pct_diff, 1), "%</strong> in ",
                  worst$year, " (MS2026: ", round(worst$value26/1000, 1), "k vs MS2024: ",
                  round(worst$value24/1000, 1), "k). ",
                  "Please review input data and model constraints."
                )
              )
            }
          }
        }
      }
    }, error = function(e) NULL)
    
    
    # CHECK 3: Proportion female changes by more than 10 pp during estimation period
    tryCatch({
      if (!is.null(data$DF_modelled) && !is.null(data$DF_modelled$df)) {
        df_mod <- data$DF_modelled$df
        yr_col  <- intersect(c("year","Year"), names(df_mod))[1]
        val_col <- intersect(c("value","Value"), names(df_mod))[1]
        
        if (!is.na(yr_col) && !is.na(val_col)) {
          pf_series <- df_mod %>%
            dplyr::select(year = !!yr_col, value = !!val_col) %>%
            dplyr::filter(year >= yrs_out_start, year <= yrs_out_end, !is.na(value)) %>%
            dplyr::arrange(year)
          
          if (nrow(pf_series) >= 2) {
            # scale to percentage if stored as proportion (0–1)
            if (max(pf_series$value, na.rm = TRUE) <= 1) {
              pf_series$value <- pf_series$value * 100
            }
            range_pp <- max(pf_series$value, na.rm = TRUE) - min(pf_series$value, na.rm = TRUE)
            
            if (range_pp > 10) {
              yr_min <- pf_series$year[which.min(pf_series$value)]
              yr_max <- pf_series$year[which.max(pf_series$value)]
              flags[["check3"]] <- list(
                severity = "warning",
                icon     = "triangle-exclamation",
                title    = "Large swing in proportion female",
                detail   = paste0(
                  "The estimated proportion female changes by <strong>",
                  round(range_pp, 1), " percentage points</strong> over the estimation period ",
                  "(min: ", round(min(pf_series$value, na.rm=TRUE),1), "% in ", yr_min, 
                  "; max: ", round(max(pf_series$value, na.rm=TRUE),1), "% in ", yr_max, "). ",
                  "A swing greater than 10 pp may indicate data inconsistencies or an unusual demographic shift."
                )
              )
            }
          }
        }
      }
    }, error = function(e) NULL)
    
    
    # CHECK 4: Origins in MS2024/MS2020 missing from new DO_modelled
    tryCatch({
      if (!is.null(data$DO_modelled) && !is.null(data$DO_modelled$df)) {
        new_origins <- unique(data$DO_modelled$df$origin)
        
        prev_origins_24 <- unique(ms2024org$LocIDorg[ms2024org$LocIDdest == loc_id])
        prev_origins_20 <- unique(ms2020org$LocIDorg[ms2020org$LocIDdest == loc_id])
        prev_origins_all <- union(prev_origins_24, prev_origins_20)
        
        # map LocIDs to names for readability
        prev_names <- locs$LocName[locs$LocID %in% prev_origins_all]
        missing_origins <- setdiff(prev_names, new_origins)
        
        if (length(missing_origins) > 0) {
          # cap display at 15 to avoid wall of text
          display_n    <- min(length(missing_origins), 15)
          display_list <- paste(missing_origins[1:display_n], collapse = ", ")
          more_str     <- if (length(missing_origins) > 15) paste0(" … and ", length(missing_origins)-15, " more") else ""
          
          flags[["check4"]] <- list(
            severity = "info",
            icon     = "circle-info",
            title    = "Origins from previous revisions not in new estimates",
            detail   = paste0(
              "<strong>", length(missing_origins), "</strong> origin(s) that had estimates in MS2020 or MS2024 ",
              "are absent from the current MS2026 estimates: ",
              "<em>", display_list, more_str, "</em>. ",
              "Verify whether these were intentionally excluded."
            )
          )
        }
      }
    }, error = function(e) NULL)
    
    
    # CHECK 5: Top-5 origins differ between MS2026 and MS2024
    tryCatch({
      if (!is.null(data$DO_modelled) && !is.null(data$DO_modelled$df)) {
        do_mod <- data$DO_modelled$df
        
        # latest year in new estimates
        yr_col  <- intersect(c("year","Year"), names(do_mod))[1]
        val_col <- intersect(c("value","Value"), names(do_mod))[1]
        sex_col <- intersect(c("sex","Sex"), names(do_mod))[1]
        
        if (!is.na(yr_col) && !is.na(val_col)) {
          do_filt <- do_mod
          if (!is.na(sex_col)) {
            do_filt <- do_filt %>% dplyr::filter(!!sym(sex_col) %in% c("both sexes", 0))
          }
          latest_yr26 <- max(do_filt[[yr_col]], na.rm = TRUE)
          
          top5_26 <- do_filt %>%
            dplyr::filter(!!sym(yr_col) == latest_yr26) %>%
            dplyr::arrange(desc(!!sym(val_col))) %>%
            dplyr::slice_head(n = 5) %>%
            dplyr::pull(origin)
          
          # MS2024 top-5 for most recent available year
          ms24_org_loc <- ms2024org %>% dplyr::filter(LocIDdest == loc_id, 
                                                      sex == 0,
                                                      LocIDdest < 900,
                                                      LocIDorg < 900)
          if (nrow(ms24_org_loc) > 0) {
            latest_yr24 <- max(ms24_org_loc$year, na.rm = TRUE)
            top5_24_ids <- ms24_org_loc %>%
              dplyr::filter(year == latest_yr24) %>%
              dplyr::arrange(desc(value)) %>%
              dplyr::slice_head(n = 5) %>%
              dplyr::pull(LocIDorg)
            
            top5_24 <- locs$LocName[locs$LocID %in% top5_24_ids]
            
            
            new_entries    <- setdiff(top5_26, top5_24)
            dropped_entries <- setdiff(top5_24, top5_26)
            
            if (length(new_entries) > 0 || length(dropped_entries) > 0) {
              
              flags[["check5"]] <- list(
                severity = "info",
                icon     = "circle-info",
                title    = "Top-5 origin countries have changed since MS2024",
                detail   = paste0(
                  "The five largest origin countries differ between MS2026 (", latest_yr26,
                  ") and MS2024 (", latest_yr24, "). ",
                  "<br><br>",
                  "<strong>MS2026 Top 5:</strong> ", paste(top5_26, collapse = ", "), "<br>",
                  "<strong>MS2024 Top 5:</strong> ", paste(top5_24, collapse = ", "), "<br><br>",
                  "Please confirm these changes are supported by the input data."
                )
              )
            }
          }
        }
      }
    }, error = function(e) NULL)
    
    # Render the notes panel
    # Header row
    header_ui <- div(
      style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px;",
      div(
        h4(style = "margin: 0; color: #2c3e50; font-weight: 500;",
           icon("clipboard-list"), " Analyst notes — ", loc_name),
        p(style = "margin: 4px 0 0; color: #6c757d; font-size: 0.85em;",
          paste0("Automated checks run: ", format(Sys.time(), "%Y-%m-%d %H:%M")))
      ),
      div(
        style = paste0(
          "padding: 6px 14px; border-radius: 20px; font-size: 0.85em; font-weight: 500; ",
          if (length(flags) == 0)
            "background: #d1e7dd; color: #0f5132;"
          else if (any(sapply(flags, `[[`, "severity") == "danger"))
            "background: #f8d7da; color: #842029;"
          else
            "background: #fff3cd; color: #664d03;"
        ),
        if (length(flags) == 0)
          tagList(icon("circle-check"), " All checks passed")
        else
          tagList(icon("triangle-exclamation"), paste0(" ", length(flags), " issue(s) found"))
      )
    )
    
    # All-clear message if no flags
    if (length(flags) == 0) {
      body_ui <- div(
        style = paste0(
          "background: #d1e7dd; border: 1px solid #a3cfbb; border-radius: 8px; ",
          "padding: 24px; text-align: center; color: #0f5132;"
        ),
        icon("circle-check", style = "font-size: 2em; margin-bottom: 10px; display: block;"),
        h5(style = "margin: 0 0 6px; font-weight: 500;", "No issues detected"),
        p(style = "margin: 0; font-size: 0.9em;",
          "All automated quality checks passed for ", strong(loc_name), ".")
      )
    } else {
      # Severity colour map
      sev_colours <- list(
        danger  = list(bg = "#f8d7da", border = "#f1aeb5", text = "#842029", label_bg = "#dc3545", label_col = "#fff"),
        warning = list(bg = "#fff3cd", border = "#ffe69c", text = "#664d03", label_bg = "#ffc107", label_col = "#000"),
        info    = list(bg = "#cff4fc", border = "#9eeaf9", text = "#055160", label_bg = "#0dcaf0", label_col = "#000")
      )
      sev_labels <- list(danger = "Action required", warning = "Review needed", info = "Note")
      
      flag_cards <- lapply(flags, function(f) {
        cols <- sev_colours[[f$severity]]
        div(
          style = paste0(
            "background: ", cols$bg, "; border: 1px solid ", cols$border, "; ",
            "border-left: 5px solid ", cols$label_bg, "; ",
            "border-radius: 6px; padding: 14px 18px; margin-bottom: 12px;"
          ),
          div(
            style = "display: flex; align-items: flex-start; gap: 12px;",
            # Icon column
            div(
              style = paste0("color: ", cols$label_bg, "; font-size: 1.2em; margin-top: 2px; flex-shrink: 0;"),
              icon(f$icon)
            ),
            # Text column
            div(
              style = "flex: 1;",
              div(
                style = "display: flex; align-items: center; gap: 8px; margin-bottom: 4px;",
                strong(style = paste0("color: ", cols$text, ";"), f$title),
                span(
                  style = paste0(
                    "font-size: 0.72em; padding: 2px 8px; border-radius: 10px; ",
                    "background: ", cols$label_bg, "; color: ", cols$label_col, "; font-weight: 500;"
                  ),
                  sev_labels[[f$severity]]
                )
              ),
              p(style = paste0("margin: 0; font-size: 0.88em; color: ", cols$text, "; line-height: 1.5;"),
                HTML(f$detail))
            )
          )
        )
      })
      
      body_ui <- div(tagList(flag_cards))
    }
    
    # Summary table of checks run
    check_names <- c(
      "check1" = "Migrant share (incl. refugees) > 30% of population",
      "check2" = "Deviation from MS2024 > 20%",
      "check3" = "Proportion female swing > 10 pp",
      "check4" = "Origins missing from new estimates",
      "check5" = "Top-5 origins changed since MS2024"
    )
    
    summary_rows <- lapply(names(check_names), function(k) {
      passed <- !(k %in% names(flags))
      tags$tr(
        tags$td(style = "padding: 5px 12px; font-size: 0.85em;", check_names[[k]]),
        tags$td(
          style = "padding: 5px 12px; text-align: center;",
          if (passed)
            span(style = "color: #0f5132; font-weight: 500;", icon("circle-check"), " Pass")
          else {
            sev <- flags[[k]]$severity
            col <- list(danger="#842029", warning="#664d03", info="#055160")[[sev]]
            span(style = paste0("color:", col, "; font-weight: 500;"), icon("triangle-exclamation"), " Flagged")
          }
        )
      )
    })
    
    summary_table <- div(
      style = "margin-top: 24px;",
      h6(style = "color: #6c757d; font-size: 1.1em; text-transform: uppercase; letter-spacing: .05em; margin-bottom: 8px;",
         "Check summary"),
      tags$table(
        style = paste0(
          "width: 100%; border-collapse: collapse; font-size: 1em; ",
          "border: 1px solid #dee2e6; border-radius: 6px; overflow: hidden;"
        ),
        tags$thead(
          tags$tr(
            style = "background: #f8f9fa;",
            tags$th(style = "padding: 7px 12px; text-align: left; font-weight: 500; border-bottom: 1px solid #dee2e6;", "Check"),
            tags$th(style = "padding: 7px 12px; text-align: center; font-weight: 500; border-bottom: 1px solid #dee2e6;", "Result")
          )
        ),
        tags$tbody(summary_rows)
      )
    )
    
    tagList(header_ui, body_ui, summary_table)
  })
  
}  ### Close server function

# Run the Shiny App
shinyApp(ui = ui, server = server)



