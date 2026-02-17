# plot international migrants (non refugee) as a proportion of total population

ms_plot_diaspora<- function(loc_id, LocName, input, diaspora, y_lab = "thousands") {

  # Set minimum/maximum year for plot
  min_year <- 1985
  max_year <- 2025
 
  # Start the plot
  p <- ggplot()
  
  # plot new estimates first
  if (input$ms2025 & !is.null(diaspora)) {
    if (!is.null(diaspora$diaspora_ms2025)) {
      d2025 <- diaspora$diaspora_ms2025 %>% 
        group_by(year) %>% 
        summarise(value = sum(value))
      p <- p + geom_line(data = d2025,
                         aes(x = year, y = value/1000, color = "MS2025"), linewidth = 1)
    }
  }
  
  if (input$ms2024 & !is.null(diaspora)) {
    if (!is.null(diaspora$diaspora_ms2024)) {
      
      d2024 <- diaspora$diaspora_ms2024 %>% 
        group_by(year) %>% 
        summarise(value = sum(value))
      
    p <- p + geom_point(data = d2024,
                       aes(x = year, y = value/1000, color = "MS2024",
                           text = paste(
                             "\u200b",
                             "MS2024", "\n",
                             "Time:", sprintf("%.1f",round(year,1)), "\n",
                             "Diaspora:", sprintf("%.0f", round(value,0)), "\n")
                           ), size = 3, shape = 19)
    }
  }
  if (input$ms2020 & !is.null(diaspora)) {
    if (!is.null(diaspora$diaspora_ms2020)) {
      d2020 <- diaspora$diaspora_ms2020 %>% 
        group_by(year) %>% 
        summarise(value = sum(value))
      
    p <- p + geom_point(data = d2020,
                        aes(x = year, y = value/1000, color = "MS2020",
                            text = paste(
                              "\u200b",
                              "MS2020", "\n",
                              "Time:", sprintf("%.1f",round(year,1)), "\n",
                              "Diaspora:", sprintf("%.0f", round(value,0)), "\n")
                        ), size = 3, shape = 19)
    }
  }
  
  
  # Final plot customization
  p <- p + scale_color_manual(
    values = c(
      "MS2025" = "blue",
      "MS2024" = "red",
      "MS2020" = "yellow")
  ) +
    scale_x_continuous(
      breaks = seq(min_year, max_year, by = 5),
      limits = c(min_year, max_year)
    )  +
    labs(x = "Year", y = y_lab, color = "black") +
    theme_light() +
    ggtitle(LocName) +
    theme(
      plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Add margin around the plot
      aspect.ratio = 0.6  # Maintain aspect ratio
      ) +
    guides(color = "none")  # Remove the default legend from the plot
  
  return(p)
  
}