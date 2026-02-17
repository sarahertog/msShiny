# plot international migrants (non refugee) as a proportion of total population

ms_plot_share<- function(loc_id, LocName, input, MS_share, y_lab = "percentage", main_title = NULL) {

  # Set minimum/maximum year for plot
  min_year <- min(input$YearRange_Total)
  max_year <- max(input$YearRange_Total)
 
  # Start the plot
  p <- ggplot()
  
  # plot new estimates first
  if (input$ms2026excl) {
    if (!is.null(MS_share)) {
      p <- p + geom_line(data = MS_share,
                         aes(x = year, y = propMig_new, color = "MS2026"), linetype = 2)
      p <- p + geom_point(data = MS_share,
                          aes(x = year, y = propMig_new, color = "MS2026",
                              text = paste(
                                "\u200b",
                                "MS2026 - excl. refugees", "\n",
                                "Time:", sprintf("%.1f",round(year,1)), "\n",
                                "MS pct of pop:", sprintf("%.1f", round(propMig_new,1)), "\n")
                          ), size = 1, shape = 19)
    }
  }
  
  # plot new estimates first
  if (input$ms2026) {
    if (!is.null(MS_share)) {
      p <- p + geom_line(data = MS_share,
                         aes(x = year, y = val_new/pop*100, color = "MS2026"), linewidth = 1)
      p <- p + geom_point(data = MS_share,
                          aes(x = year, y = val_new/pop*100, color = "MS2026",
                              text = paste(
                                "\u200b",
                                "MS2026 - incl. refugees", "\n",
                                "Time:", sprintf("%.1f",round(year,1)), "\n",
                                "MS pct of pop:", sprintf("%.1f", round(val_new/pop*100,1)), "\n")
                          ), size = 1, shape = 19)
    }
  }
  
  if (input$ms2024 & !is.null(MS_share)) {
    p <- p + geom_point(data = MS_share,
                       aes(x = year, y = propMig_24, color = "MS2024",
                           text = paste(
                             "\u200b",
                             "MS2024", "\n",
                             "Time:", sprintf("%.1f",round(year,1)), "\n",
                             "MS pct of pop:", sprintf("%.1f", round(propMig_24,1)), "\n")
                           ), size = 3, shape = 19)
  }
  if (input$ms2020 & !is.null(MS_share)) {
    p <- p + geom_point(data = MS_share,
                       aes(x = year, y = propMig_20, color = "MS2020",
                           text = paste(
                             "\u200b",
                             "MS2020", "\n",
                             "Time:", sprintf("%.1f",round(year,1)), "\n",
                             "MS pct of pop:", sprintf("%.1f", round(propMig_20,1)), "\n")
                           ), size = 2, shape = 19)
  }
  
  main_title <- ifelse(is.null(main_title), LocName, paste0(LocName, " - ", main_title))
  
  # Final plot customization
  p <- p + scale_color_manual(
    values = c(
      "MS2026" = "black",
      "MS2024" = "red",
      "MS2020" = "yellow")
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