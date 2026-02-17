

# plot model results of IM by origin, all origins or the top x origins as defined by n_origins parameter

ms_plot_diaspora_alluvial <- function(LocName,
                                     diaspora,
                                     input) {

  # Set minimum/maximum year for plot
  min_year <- min(input$YearRange_Diaspora)
  max_year <- max(input$YearRange_Diaspora)
  
  # initialize the plot
  p <- ggplot()
  
  if (!is.null(diaspora)) {
    
    if(!is.null(diaspora$diaspora_ms2026)) {

    # Subset the data series according to the year range specified in the slidebar of the Age tab in the Shiny app
    df <- diaspora$diaspora_ms2026 %>% 
      dplyr::filter(sex == "both sexes",
                    year >= min_year,
                    year <= max_year,
                    !is.na(value)) %>% 
      dplyr::select(destination, year, value) %>% 
      group_by(destination) %>% 
      mutate(latest_value = value[year == max(year)]) %>% 
      arrange(-latest_value, origin, year)
    
     max_destinations <- length(unique(df$destination))
     destinations <- unique(df$destination)[1:min(as.numeric(input$max_destinations), max_destinations)]
     
     # other named destinations grouped into "other"
     df_other <- df %>% 
       dplyr::filter(!(destination %in% destinations)) %>% 
       group_by(year) %>% 
       summarise(other = sum(value)) 
     
     # if no other group, then create a dummy dataset
     if (nrow(df_other) == 0) {
       df_other <- data.frame(year = unique(df$year)) %>% mutate(other = 0)
     }

     # subset of named destinations to plot
     df_plot <- df %>% 
       dplyr::filter(destination %in% destinations) %>% 
       arrange(year, value) %>% 
       group_by(year) %>% 
       mutate(cumsum = cumsum(value)) %>% 
       ungroup() %>% 
       arrange(year, -value) %>% 
       left_join(df_other, by = "year") %>% 
       mutate(cumsum = cumsum + other)
              
    
     # Always plot other in light gray at the bottom of the chart
     p <- p +
       geom_ribbon(data = df_other,
                   aes(x = year, ymin = 0, ymax = other/1000), fill = "lightgray", alpha = 0.6) +
       geom_text(data = df_other %>% dplyr::filter(year == max(year)),
                 aes(x = year, y = other/1000/2), label = "Other destinations", color = "black", hjust = "left", size = 2.5)
     
    # Then plot named destinations
    colors <- rainbow(length(destinations))
    for (i in 1:(length(destinations))) {
      p <- p +
        geom_ribbon(data = df_plot %>% dplyr::filter(destination == destinations[i]),
                  aes(x = year, ymin = (cumsum-value)/1000, ymax = cumsum/1000), fill = colors[i], alpha = 0.6) +
        geom_point(data = df_plot %>% dplyr::filter(destination == destinations[i]),
                   aes(x = year, y = (cumsum - (value/2))/1000,
                       text = paste(
                         "\u200b",
                         destination, "\n",
                         "Time:", sprintf("%.1f",round(year,1)), "\n",
                         "MS:", sprintf("%.0f", round(value,0)), "\n")), color = colors[i], alpha = 0.1) +
        geom_text(data = df_plot %>% dplyr::filter(destination == destinations[i] & year == max(year)),
                  aes(x = year, y = (cumsum - (value/2))/1000), label = destinations[i], color = "black", hjust = "left", size = 2.5)
      
    }
    
    } # close for diaspora_ms2026
    
    if (input$ms2024 & !is.null(diaspora$diaspora_ms2024)) {
      df2024 <- diaspora$diaspora_ms2024 %>% 
        group_by(year) %>% 
        summarise(value = sum(value))
      p <- p + geom_point(data = df2024,
                          aes(x = year, y = value/1000, color = "MS2024",
                              text = paste(
                                "\u200b",
                                "MS2024", "\n",
                                "Time:", sprintf("%.1f",round(year,1)), "\n",
                                "MS:", sprintf("%.0f", round(value,0)), "\n")
                          ), size = 3, shape = 19)
    }
    if (input$ms2020 & !is.null(diaspora$diaspora_ms2020)) {
      df2020 <- diaspora$diaspora_ms2020 %>% 
        group_by(year) %>% 
        summarise(value = sum(value))
      p <- p + geom_point(data = df2020,
                          aes(x = year, y = value/1000, color = "MS2020",
                              text = paste(
                                "\u200b",
                                "MS2020", "\n",
                                "Time:", sprintf("%.1f",round(year,1)), "\n",
                                "MS:", sprintf("%.0f", round(value,0)), "\n")
                          ), size = 2, shape = 19)
    }
    
  }
  
  p <- p +
    scale_color_manual(
      values = c(
        "MS2024" = "red",
        "MS2020" = "yellow")
    ) +
    scale_x_continuous(
      breaks = seq(min_year, max_year, by = 5),
      limits = c(min_year, max_year + 5)) +
    labs(x = "Year", y = "thousands", color = "black") +
    theme_light() +
    ggtitle(LocName) +
    theme(
      plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Add margin around the plot
      aspect.ratio = 0.6  # Maintain aspect ratio
      
    ) +
    guides(color = "none") 

return(p)

}

