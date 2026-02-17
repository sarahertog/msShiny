# R Shiny application for display of International Migrant Stock 2026

# change this to point to the directory holding the app.R script
ms_dir <- "C:/Users/SHERTOG/OneDrive - United Nations/R packages/msShiny"

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
                       selectInput("SexName_Age", "Show sex:",
                                   choices = c("Both sexes", "Female", "Male"),
                                   selected = "Both sexes"),
                       selectInput("AgeScale", "Scale:",
                                   choices = c("Counts", "Proportion"),
                                   selected = "Counts"),
                       sliderInput("YearRange_Age", "Year range", 
                         min = 1950, max = 2030, sep = "",
                         value = c(yrs_out_start, yrs_out_end) 
                       )
                       
                     )
                   )),
                 # Plot
                 div(
                   style = "flex-grow: 1;",
                   plotlyOutput("age_plot", height = "100%")
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
            #DTOutput("notes")
            #DT::dataTableOutput("optim") 
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
  # Migrant stock by age plot
  plot_age <- reactive({
    req(input$country_name != "")
    data <- current_data()
    req(data)  # Ensure data is available
    
    selected_loc <- locs[locs$LocName == input$country_name, ]
    if (nrow(selected_loc) > 0) {
      loc_id <- selected_loc$LocID
      loc_name <- selected_loc$LocName
      
      p_age <- ms_plot_age(loc_id = loc_id, 
                           LocName = loc_name,
                           input = input, 
                           MS_age = data$DA, # empirical data
                           MS_modelled = data$DA_modelled)
      
      Message <- paste(c(data$Warnings$Warning_Age), collapse = "\n")
      
      Message <- ifelse(is.null(p_age$plot_env$indata_emp), "No data to plot. Check input Excel file to verify that 'Estimate_age_distribution' is TRUE in the parameters sheet\nand that 'Include_Age' is TRUE for at least one series in the INPUTS sheet.",Message)
      
      # Convert to plotly and add hover information for vertical lines
      ply <- ggplotly(p_age, tooltip = "text") %>%
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
  }) # close migrant stock by age plot
  
  # render the plot for Shiny display
  output$age_plot <- NULL
  output$age_plot <- renderPlotly({
    plot_age() %>%
      layout(
        autosize = TRUE,
        height = session$clientData$output_country_plot_width * 0.6
      )
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

}  ### Close server function


# Run the Shiny App
shinyApp(ui = ui, server = server)



