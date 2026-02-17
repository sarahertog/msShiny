# plot international migrants (non refugee) as a proportion of total population

ms_plot_age<- function(loc_id, LocName, input, MS_age, MS_modelled) {

  if (!is.null(MS_age)) {
    
    # Subset the data series according to the year range specified in the slidebar of the Age tab in the Shiny app
    indata_emp <- MS_age %>% dplyr::filter(TimeMid >= min(input$YearRange_Age),
                                           TimeMid <= max(input$YearRange_Age))
    
    # Subset foreign born or foreign citizen according to the check box selections on the left sidebar of the Shiny app
    if (!input$show_foreign_born) {
      indata_emp <- indata_emp %>% dplyr::filter(Definition != "Foreign-born")
    }
    if (!input$show_foreign_citizen) {
      indata_emp <- indata_emp %>% dplyr::filter(Definition != "Foreign citizen")
    }
  } else {
    indata_emp <- NULL
  }

  
  # Initialize the plot
  p <- ggplot()
  
  # Plot empirical data
    if (!is.null(indata_emp)) {
      # If the drop down menu on the Age tab indicates "Counts" then plot the numbers of international migrants by age
      if (input$AgeScale == "Counts") {
        
        # if indicated in Shiny check box selection "Show excluded"
        if (input$show_excluded) {
          
          # subset excluded series
          indata_excl <- indata_emp %>% dplyr::filter(Include == FALSE)
          
          # if excluded series exist, then plot them
          if (nrow(indata_excl) > 0) {
            
            # plot excluded series as a dashed line
            p <- p + geom_line(data = indata_excl %>% dplyr::filter(SexName == input$SexName_Age & !(AgeLabel %in% c("Total", "Unknown"))),
                               aes(x = AgeStart + 2.5, y = DataValue/1000, color = MS_SeriesID), linewidth = 1, linetype = "dashed") 
            
            # plot the data points over the line with text information for the tool tip
            p <- p + geom_point(data = indata_excl %>% dplyr::filter(SexName == input$SexName_Age & !(AgeLabel %in% c("Total", "Unknown"))),
                                aes(x = AgeStart + 2.5, y = DataValue/1000, color = MS_SeriesID,
                                    text = paste(
                                      "\u200b",
                                      "MS_SeriesID:", MS_SeriesID, "\n",
                                      "DataSourceShortName:", DataSourceShortName, "\n",
                                      "Definition:", Definition, "\n",
                                      "Time:", sprintf("%.1f",round(TimeMid,1)), "\n",
                                      "Age:", AgeLabel, "\n",
                                      "MS:", sprintf("%.0f", round(DataValue,0)), "\n")
                                ), size = 1, shape = 19)
            }
        }
        
        if (any(indata_emp$Include == TRUE)) {
          # plot included series as a solid line
          p <- p + geom_line(data = indata_emp %>% dplyr::filter(SexName == input$SexName_Age & !(AgeLabel %in% c("Total", "Unknown")) & Include == TRUE),
                               aes(x = AgeStart + 2.5, y = DataValue/1000, color = MS_SeriesID), linewidth = 1, linetype = "solid") 
          
          # plot the data points over the line with text information for the tool tip
          p <- p + geom_point(data = indata_emp %>% dplyr::filter(SexName == input$SexName_Age & !(AgeLabel %in% c("Total", "Unknown")) & Include == TRUE),
                              aes(x = AgeStart + 2.5, y = DataValue/1000, color = MS_SeriesID,
                                  text = paste(
                                    "\u200b",
                                    "MS_SeriesID:", MS_SeriesID, "\n",
                                    "DataSourceShortName:", DataSourceShortName, "\n",
                                    "Definition:", Definition, "\n",
                                    "Time:", sprintf("%.1f",round(TimeMid,1)), "\n",
                                    "Age:", AgeLabel, "\n",
                                    "MS:", sprintf("%.0f", round(DataValue,0)), "\n")
                              ), size = 1, shape = 19)
      
        }
        # Otherwise, if the drop down menu on the Age tab indicates "Proportions" then plot the percentage distribution of international migrants by age
      } else {
        
        # compute the percentage distribution of international migrants by age
        df_prop <- indata_emp %>% 
          dplyr::filter(SexName == input$SexName_Age & !(AgeLabel %in% c("Total", "Unknown"))) %>% 
          dplyr::select(MS_SeriesID, DataSourceShortName, Definition, TimeMid, AgeStart, AgeLabel, DataValue, Include) %>% 
          group_by(MS_SeriesID) %>% 
          mutate(proportion = DataValue/sum(DataValue)*100)
        
        # if indicated in Shiny check box selection "Show excluded"
        if (input$show_excluded) {
          
          # subset excluded series
          df_prop_excl <- df_prop %>% dplyr::filter(Include == FALSE)
          
          # if excluded series exist, then plot them
          if (nrow(df_prop_excl) > 0) {
          
            # plot excluded series as a dashed line
            p <- p + geom_line(data = df_prop_excl,
                               aes(x = AgeStart + 2.5, y = proportion, color = MS_SeriesID), linewidth = 1, linetype = "dashed") 
            
            # plot the data points over the line with text information for the tool tip
            p <- p + geom_point(data = df_prop_excl,
                                aes(x = AgeStart + 2.5, y = proportion, color = MS_SeriesID,
                                    text = paste(
                                      "\u200b",
                                      "MS_SeriesID:", MS_SeriesID, "\n",
                                      "DataSourceShortName:", DataSourceShortName, "\n",
                                      "Definition:", Definition, "\n",
                                      "Time:", sprintf("%.1f",round(TimeMid,1)), "\n",
                                      "Age:", AgeLabel, "\n",
                                      "Pct:", sprintf("%.1f", round(proportion,1)), "\n")
                                ), size = 1, shape = 19)
          }
        }
        
        if (any(indata_emp$Include == TRUE)) {
          # plot included series as a solid line
          p <- p + geom_line(data = df_prop %>% dplyr::filter(Include == TRUE),
                             aes(x = AgeStart + 2.5, y = proportion, color = MS_SeriesID), linewidth = 1, linetype = "solid") 
          
          # plot the data points over the line with text information for the tool tip
          p <- p + geom_point(data = df_prop %>% dplyr::filter(Include == TRUE),
                              aes(x = AgeStart + 2.5, y = proportion, color = MS_SeriesID,
                                  text = paste(
                                    "\u200b",
                                    "MS_SeriesID:", MS_SeriesID, "\n",
                                    "DataSourceShortName:", DataSourceShortName, "\n",
                                    "Definition:", Definition, "\n",
                                    "Time:", sprintf("%.1f",round(TimeMid,1)), "\n",
                                    "Age:", AgeLabel, "\n",
                                    "Pct:", sprintf("%.1f", round(proportion,1)), "\n")
                              ), size = 1, shape = 19)
        }
        
      }
    }

  # plot new estimates first
  if (input$ms2026) {
    if (!is.null(MS_modelled$df)) {
      
      if (input$AgeScale == "Counts") {
        df_age <- MS_modelled$df %>% dplyr::filter(age_span == 5, sex == tolower(input$SexName_Age))
        # plot only every five years
        yrs5 <- unique(c(df_age$year[substr(as.character(df_age$year), 4,4) %in% c("0","5")], max(df_age$year)))
        yrs5 <- yrs5[yrs5 >= min(input$YearRange_Age) & yrs5 <= max(input$YearRange_Age)]
        if (length(yrs5) > 0) {
          for (i in 1:length(yrs5)) {
            p <- p + geom_line(data = df_age %>% dplyr::filter(year == yrs5[i]),
                               aes(x = age_start + 2.5, y = value/1000, color = "MS2026"), linewidth = 1)
            # plot the data points over the line with text information for the tool tip
            p <- p + geom_point(data = df_age %>% dplyr::filter(year == yrs5[i]),
                                aes(x = age_start + 2.5, y = value/1000, color = "MS2026",
                                    text = paste(
                                      "\u200b",
                                      "MS2026", "\n",
                                      "Time:", sprintf("%.1f",round(as.numeric(year), 1)), "\n",
                                      "Age:", paste(age_start, age_start + 5, sep = "-"), "\n",
                                      "MS:", sprintf("%.0f", round(value,0)), "\n")
                                ), size = 1, shape = 19)
          }
        }
      } else {
        df_age <- MS_modelled$df %>% dplyr::filter(age_span == 5, sex == tolower(input$SexName_Age)) %>% 
          group_by(year) %>% 
          mutate(proportion = value/sum(value)*100)
        # plot only every five years
        yrs5 <- unique(c(df_age$year[substr(as.character(df_age$year), 4,4) %in% c("0","5")], max(df_age$year)))
        yrs5 <- yrs5[yrs5 >= min(input$YearRange_Age) & yrs5 <= max(input$YearRange_Age)]
        if (length(yrs5) > 0) {
          for (i in 1:length(yrs5)) {
            p <- p + geom_line(data = df_age %>% dplyr::filter(year == yrs5[i]),
                               aes(x = age_start + 2.5, y = proportion, color = "MS2026"), linewidth = 1)
            # plot the data points over the line with text information for the tool tip
            p <- p + geom_point(data = df_age %>% dplyr::filter(year == yrs5[i]),
                                aes(x = age_start + 2.5, y = proportion, color = "MS2026",
                                    text = paste(
                                      "\u200b",
                                      "MS2026", "\n",
                                      "Time:", sprintf("%.0f",as.numeric(year)), "\n",
                                      "Age:", paste(age_start, age_start + 5, sep = "-"), "\n",
                                      "Prop:", sprintf("%.1f", round(proportion,1)), "\n")
                                ), size = 1, shape = 19)
          }
        }
    }
    }
  }
  
  # if (input$ms2020 & !is.null(MS_share)) {
  #   p <- p + geom_point(data = MS_share,
  #                      aes(x = year, y = propMig_20, color = "MS2020",
  #                          text = paste(
  #                            "\u200b",
  #                            "MS2020", "\n",
  #                            "Time:", sprintf("%.1f",round(year,1)), "\n",
  #                            "Percentage:", sprintf("%.1f", round(propMig_20,1)), "\n")
  #                          ), size = 2, shape = 19)
  # }
  
  if (is.null(indata_emp)) {
    p <- p +
      scale_y_continuous(
        breaks = seq(0, 5, by = 5),
        limits = c(0, 5)
      ) +
      theme_light()
    
    
    color_values <- NULL
    
  } else {
    indata_emp <- indata_emp %>% arrange(TimeMid)
    id_vector <- unique(indata_emp$MS_SeriesID)
    color_values  <- rainbow(length(id_vector)+1)
    color_values <- color_values[color_values != "#0010FF"]
    color_values <- color_values[1:length(id_vector)]
    names(color_values) <- id_vector
      
    }

  # Final plot customization
  ylabel <- ifelse(input$AgeScale == "Counts", "Migrant stock (thousands)", "Age distribution of migrant stock (percentage)")
  p <- p + 
    scale_color_manual(
      values = c(color_values,
        "MS2026" = "black",
        "MS2024" = "red",
        "MS2020" = "yellow" ,
        "UNHCR" = "green")
    ) +
    scale_x_continuous(
      breaks = seq(0, 110, by = 5),
      limits = c(0, 110)
    )  +
    labs(x = "Age", y = ylabel, color = "black") +
    theme_light() +
    ggtitle(LocName) +
    theme(
      plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Add margin around the plot
      aspect.ratio = 0.6  # Maintain aspect ratio
      ) +
    guides(color = "none")  # Remove the default legend from the plot
  
  return(p)
  
}