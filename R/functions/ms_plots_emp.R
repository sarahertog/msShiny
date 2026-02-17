# generic code that controls the plots for the International Migrant Stock 2025 RShiny application
# that display the new estimates against empirical observations
# used for totals, propf, and single origin plots

ms_plots_emp <- function(loc_id, LocName, input, census_refs, # data frame with one column "reference_date", 
                         main_title = NULL,
                         indata_emp, indata_modelled, chartid,
                         xmin, xmax = year(now()) + 1, ymax, 
                         ms2024_plt, ms2020_plt) {
  
  # chart ids c("total", "propf", "origin_one", "origin_all", "ms_share")
  chartid <- tolower(chartid)
  if (chartid %in% c("total", "origin_one")) {
    scale_value <- 1/1000
    y_lab <- "thousands"
    dlab1 <- "round(value,0)"
    dlab2 <- "round(DataValue,0)"
    dlab3 <- "round(VALUE, 0)"
    dlab4 <- "round(wo_refugees, 0)"
    dlab5 <- "round(refugees, 0)"
    dfmt <- "%.0f"
    if (!is.null(indata_modelled)) {
      indata_modelled <- indata_modelled %>% mutate(wo_refugees = value - refugees) # compute result excluding refugees
    }
  }
  
  if (chartid %in% c("propf")) {
    scale_value <- 100
    y_lab <- "percentage"
    dlab1 <- "round(value*100,1)"
    dlab2 <- "round(DataValue*100,1)"
    dlab3 <- "round(VALUE*100, 1)"
    dlab4 <- "round(wo_refugees * 100, 1)"
    dlab5 <- "round((rfgF/(rfgF+rfgM)) * 100, 1)"
    dfmt <- "%.1f"
    if (!is.null(indata_modelled)) {
      indata_modelled <- indata_modelled %>% mutate(wo_refugees = IM_noRfgF/(IM_noRfgF+IM_noRfgM)) # compute result excluding refugees
    }
  }
  

  # Set minimum/maximum year for plot
  min_year <- xmin
  max_year <- xmax
  
  if (!is.null(indata_emp)) {
    # remove MS2024 and MS2020 from empirical data to be plotted
    indata_emp <- indata_emp %>% 
      dplyr::filter(!(DataSourceShortName %in% c("MS2024","MS2020")))
    
    # filter on Include
    if (!input$show_excluded) {
      indata_emp <- indata_emp[indata_emp$Include == TRUE, ]
    } 
    if (!input$show_foreign_born) {
      indata_emp <- indata_emp[indata_emp$Definition != "Foreign-born",]
    }
    if (!input$show_foreign_citizen) {
      indata_emp <- indata_emp[indata_emp$Definition != "Foreign citizen",]
    }
    if (!input$census) {
      indata_emp <- indata_emp[!(indata_emp$DataProcess %in% c("Census", "Population and Housing Census")),]
    }
    if (!input$estimate) {
      indata_emp <- indata_emp[indata_emp$DataProcess != "Estimate",]
    }
    if (!input$survey) {
      indata_emp <- indata_emp[indata_emp$DataProcess != "Survey",]
    }
    
    if (!("VALUE" %in% names(indata_emp))) {
      indata_emp$VALUE <- indata_emp$DataValue
    }
    
    # identify analyst-adjusted empirical records
    indata_emp <- indata_emp %>%
      mutate(Adjusted = ifelse(Include == TRUE & is.na(DataValue) & !is.na(VALUE), TRUE, FALSE),
             Adjusted = ifelse(Include == TRUE & !is.na(DataValue) & VALUE != DataValue, TRUE, Adjusted),
             Include = ifelse(Adjusted == TRUE, FALSE, Include))

  }
  
  # Start the plot
  p <- ggplot()
  
  # Create data frame for vertical lines using DataCatalog reference years
  vlines_df <- data.frame(
    year = ifelse(census_refs$reference_date == 0, NA, census_refs$reference_date),
    y = ymax
  )
  
  # Add vertical lines with hover text
  p <- p + geom_vline(data = vlines_df,
                      aes(xintercept = year,
                          text = paste("Census Year:", sprintf("%.3f",year))),
                      color = "salmon",
                      linetype = "dashed",
                      linewidth = 0.4,
                      alpha = 0.6)
  
  # plot new estimates first
  if (input$ms2026excl) {
    if (!is.null(indata_modelled)) {
      # without refugees
      p <- p + geom_line(data = indata_modelled,
                         aes(x = year, y = wo_refugees * scale_value, color = "MS2026"), linetype = 2)
      p <- p + geom_point(data = indata_modelled,
                          aes(x = year, y = wo_refugees * scale_value, color = "MS2026",
                              text = paste(
                                "\u200b",
                                "MS2026 - excl. refugees", "\n",
                                "Time:", sprintf("%.1f",round(year,1)), "\n",
                                "Value:", sprintf(dfmt, eval(parse(text = dlab4))), "\n")
                          ), size = 1, shape = 19)
    }
  }

  if (input$ms2026) {
    if (!is.null(indata_modelled)) {
      # with refugees included
      p <- p + geom_line(data = indata_modelled,
                         aes(x = year, y = value * scale_value, color = "MS2026"), linewidth = 1)
      p <- p + geom_point(data = indata_modelled,
                          aes(x = year, y = value * scale_value, color = "MS2026",
                              text = paste(
                                "\u200b",
                                "MS2026 - incl. refugees", "\n",
                                "Time:", sprintf("%.1f",round(year,1)), "\n",
                                "Value:", sprintf(dfmt, eval(parse(text = dlab1))), "\n")
                          ), size = 1, shape = 19)
      }
  }
  
  if (input$ms2024 & !is.null(ms2024_plt)) {
    p <- p + geom_point(data = ms2024_plt,
                       aes(x = year, y = value * scale_value, color = "MS2024",
                           text = paste(
                             "\u200b",
                             "MS2024", "\n",
                             "Time:", sprintf("%.1f",round(year,1)), "\n",
                             "Value:", sprintf(dfmt, eval(parse(text = dlab1))), "\n")
                           ), size = 3, shape = 19)
  }
  if (input$ms2020 & !is.null(ms2020_plt)) {
    p <- p + geom_point(data = ms2020_plt,
                       aes(x = year, y = value * scale_value, color = "MS2020",
                           text = paste(
                             "\u200b",
                             "MS2020", "\n",
                             "Time:", sprintf("%.1f",round(year,1)), "\n",
                             "Value:", sprintf(dfmt, eval(parse(text = dlab1))), "\n")
                           ), size = 2, shape = 19)
  }
  
  if (!is.null(indata_emp)) {
    # add layers 
    
    # estimates
    p <- p + geom_point(data = subset(indata_emp, DataProcess == "Estimate"),
                        aes(x = reference_date, y = DataValue * scale_value, 
                            text = paste(
                              "\u200b",
                              "DataProcess:", DataProcess, "\n",
                              "Time:", sprintf("%.1f",round(reference_date,1)), "\n",
                              "Value:", sprintf(dfmt, eval(parse(text = dlab2))), "\n",
                              "Definition:", Definition, "\n",
                              "Includes refugees:", Includes_refugees, "\n",
                              "DataCatalogShortName:", DataCatalogShortName, "\n",
                              "DataSourceShortName:", substr(DataSourceShortName, 1, pmin(15, nchar(DataSourceShortName))), "\n"),
                            color = "Estimate"), 
                        size = 3, 
                        shape = ifelse(subset(indata_emp, DataProcess == "Estimate")$Include == FALSE, 2, 17)
    )
    # surveys
   p <- p + geom_point(data = subset(indata_emp, DataProcess == "Survey" ),
                        aes(x = reference_date, y = DataValue * scale_value, 
                            text = paste(
                              "\u200b",
                              "DataProcess:", DataProcess, "\n",
                              "Time:", sprintf("%.1f",round(reference_date,1)), "\n",
                              "Value:", sprintf(dfmt, eval(parse(text = dlab2))), "\n",
                              "Definition:", Definition, "\n",
                              "Includes refugees:", Includes_refugees, "\n",
                              "DataCatalogShortName:", DataCatalogShortName, "\n",
                              "DataSourceShortName:", substr(DataSourceShortName, 1, pmin(15, nchar(DataSourceShortName))), "\n"),
                            color = "Survey"), 
                       size = 3, 
                       shape = ifelse(subset(indata_emp, DataProcess == "Survey")$Include == FALSE, 0, 15)
    )
    # censuses
    p <- p + geom_point(data = subset(indata_emp, DataProcess %in% c("Census", "Register", "Population and Housing Census")),
                        aes(x = reference_date, y = DataValue * scale_value, 
                            text = paste(
                              "\u200b",
                              "DataProcess:", DataProcess, "\n",
                              "Time:", sprintf("%.1f",round(reference_date,1)), "\n",
                              "Value:", sprintf(dfmt, eval(parse(text = dlab2))), "\n",
                              "Definition:", Definition, "\n",
                              "Includes refugees:", Includes_refugees, "\n",
                              "DataCatalogShortName:", DataCatalogShortName, "\n",
                              "DataSourceShortName:", substr(DataSourceShortName, 1, pmin(15, nchar(DataSourceShortName))), "\n"),
                            color = "Census"), 
                        size = 3, 
                        shape = ifelse(subset(indata_emp, DataProcess %in% c("Census", "Register", "Population and Housing Census"))$Include == FALSE, 5, 18)
                        
    )
    # analyst-adjusted
    p <- p + geom_point(data = subset(indata_emp, Adjusted == TRUE),
                        aes(x = reference_date, y = VALUE * scale_value,
                            text = paste(
                              "\u200b",
                              "DataProcess:", DataProcess, "\n",
                              "Time:", sprintf("%.1f",round(reference_date,1)), "\n",
                              "Value:", sprintf(dfmt, eval(parse(text = dlab3))), "\n",
                              "Definition:", Definition, "\n",
                              "Includes refugees:", Includes_refugees, "\n",
                              "DataCatalogShortName:", paste(DataCatalogShortName, "analyst-adjusted", sep = "~"), "\n",
                              "DataSourceShortName:", substr(DataSourceShortName, 1, pmin(15, nchar(DataSourceShortName))), "\n"),
                            color = "Adjusted"),
                        size = 2,
                        shape = 19

    )
    if (nrow(subset(indata_emp, Adjusted == TRUE)) > 0) {
      p <- p + annotate("text", label = "Analyst-adjusted input values", x = xmin+5, y=ymax, hjust = 0, size = 3, color = "violet")
    }
  }
  

  if (input$unhcr) {
    if (!is.null(indata_modelled) & chartid %in% c("total", "origin_one")) {
      p <- p + geom_line(data = indata_modelled,
                         aes(x = year, y = refugees * scale_value, color = "UNHCR"), linewidth = 1)
      p <- p + geom_point(data = indata_modelled,
                          aes(x = year, y = refugees * scale_value, color = "UNHCR",
                              text = paste(
                                "\u200b",
                                "UNHCR", "\n",
                                "Time:", sprintf("%.1f",round(year,1)), "\n",
                                "Value:", sprintf(dfmt, eval(parse(text = dlab5))), "\n")
                          ), size = 0.5, shape = 19)
    }
    if (!is.null(indata_modelled) & chartid %in% c("propf")) {
      p <- p + geom_line(data = indata_modelled,
                         aes(x = year, y = (rfgF/(rfgF+rfgM)) * scale_value, color = "UNHCR"), linewidth = 1)
      p <- p + geom_point(data = indata_modelled,
                          aes(x = year, y = (rfgF/(rfgF+rfgM)) * scale_value, color = "UNHCR",
                              text = paste(
                                "\u200b",
                                "UNHCR", "\n",
                                "Time:", sprintf("%.1f",round(year,1)), "\n",
                                "Value:", sprintf(dfmt, eval(parse(text = dlab5))), "\n")
                          ), size = 0.5, shape = 19)
    }
  }
  
  main_title <- ifelse(is.null(main_title), LocName, paste0(LocName, " - ", main_title))
  
  # Final plot customization
  p <- p + scale_color_manual(
    values = c(
      "Census" = "purple",
      "Estimate" = "orange",
      "Survey" = "cornflowerblue",
      "MS2026" = "black",
      "MS2024" = "red",
      "MS2020" = "yellow" ,
      "UNHCR" = "green",
      "Adjusted" = "violet")
  ) +
    scale_x_continuous(
      breaks = seq(min_year, max_year, by = 5),
      limits = c(min_year, max_year)
    )  +
    labs(x = "Year", y = y_lab, color = "black") +
    theme_light() +
    ggtitle(main_title) +
    theme(
      plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Add margin around the plot
      aspect.ratio = 0.6  # Maintain aspect ratio
      ) +
    guides(color = "none")  # Remove the default legend from the plot
  
  return(p)
  
}