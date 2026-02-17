

# plot model results of IM by origin, all origins or the top x origins as defined by n_origins parameter

ms_plot_origins_alluvial <- function(LocName,
                                     YearRange,
                                     DO_modelled_df, 
                                     DT_modelled, # can eliminate this input once origin results are calibrated to totals
                                     n_origins = 20) {

  # Set minimum/maximum year for plot
  min_year <- min(YearRange)
  max_year <- max(YearRange)
  
  # initialize the plot
  p <- ggplot()
  
  if (!is.null(DO_modelled_df)) {

    # Subset the data series according to the year range specified in the slidebar of the Age tab in the Shiny app
    df <- DO_modelled_df %>% 
      dplyr::filter(year >= min_year,
                    year <= max_year,
                    origin != "Total") %>% 
      dplyr::select(origin, year, value) %>% 
      group_by(origin) %>% 
      mutate(latest_value = value[year == max(year)]) %>% 
      arrange(-latest_value, origin, year)
    
     max_origins <- length(unique(df$origin))
     origins <- unique(df$origin)[1:min(n_origins, max_origins)]
     
     # unknown origin is total MS minus sum of all named origins
     df_unknown <- df %>% 
       group_by(year) %>% 
       summarise(value = sum(value)) %>% 
       left_join(DT_modelled$df %>% rename(total = value) %>% dplyr::select(year, total), by = "year") %>% 
       mutate(unknown = total-value) %>% 
       dplyr::select(year, unknown)
     
     # other named origins grouped into "other"
     df_other <- df %>% 
       dplyr::filter(!(origin %in% origins)) %>% 
       group_by(year) %>% 
       summarise(other = sum(value)) 
     
     # if no other group, then create a dummy dataset
     if (nrow(df_other) == 0) {
       df_other <- df_unknown %>% rename(other = unknown) %>% mutate(other = 0)
     }
     
     df_other <- df_other %>% 
       dplyr::select(year, other) %>% 
       left_join(df_unknown, by = "year")
     
     # subset of named origins to plot
     df_plot <- df %>% 
       dplyr::filter(origin %in% origins) %>% 
       arrange(year, value) %>% 
       group_by(year) %>% 
       mutate(cumsum = cumsum(value)) %>% 
       ungroup() %>% 
       arrange(year, -value) %>% 
       left_join(df_other, by = "year") %>% 
       mutate(cumsum = cumsum + other + unknown)
              
    
     # Always plot unknown in dark gray at the bottom of the chart
     p <- p +
       geom_ribbon(data = df_unknown,
                   aes(x = year, ymin = 0, ymax = unknown/1000), fill = "darkgray", alpha = 0.6) +
       geom_text(data = df_unknown %>% dplyr::filter(year == max(year)),
                 aes(x = year, y = unknown/1000/2), label = "Unspecified origin", color = "black", hjust = "left", size = 2.5)
     
    # Plot other light gray above unknown
    if (nrow(df_other[df_other$other != 0,]) >0) {
      p <- p +
        geom_ribbon(data = df_other,
                  aes(x = year, ymin = 0, ymax = (other + unknown)/1000), fill = "lightgray", alpha = 0.6) +
        geom_text(data = df_other %>% dplyr::filter(year == max(year)),
                  aes(x = year, y = (unknown/1000) + (other/1000/2)), label = "Other known origins", color = "black", hjust = "left", size = 2.5)
    
    }
    # Then plot named origins
    colors <- rainbow(length(origins))
    for (i in 1:(length(origins))) {
      p <- p +
        geom_ribbon(data = df_plot %>% dplyr::filter(origin == origins[i]),
                  aes(x = year, ymin = (cumsum-value)/1000, ymax = cumsum/1000), fill = colors[i], alpha = 0.6) +
        geom_point(data = df_plot %>% dplyr::filter(origin == origins[i]),
                   aes(x = year, y = (cumsum - (value/2))/1000,
                       text = paste(
                         "\u200b",
                         origin, "\n",
                         "Time:", sprintf("%.1f",round(year,1)), "\n",
                         "MS:", sprintf("%.0f", round(value,0)), "\n")), color = colors[i], alpha = 0.1) +
        geom_text(data = df_plot %>% dplyr::filter(origin == origins[i] & year == max(year)),
                  aes(x = year, y = (cumsum - (value/2))/1000), label = origins[i], color = "black", hjust = "left", size = 2.5)
      
    }
    
  }
  
  p <- p +
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

