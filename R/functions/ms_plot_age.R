# Contains three plotting functions for the Age panel:
#   ms_plot_age_stock()      - Plot 1: migrant stock over time for a selected age group
#   ms_plot_age_sexratio()   - Plot 2: sex ratio of migrant stock over time
#   ms_plot_age_nonrefugee() - Plot 3: non-refugee migrant stock estimates over time
#
# Helper function:
#   parse_age_start()        - converts age group label (e.g. "5-9", "85+" -> 85) to numeric start age
#   age_group_to_starts()    - returns vector of 5-year age_start values for any age group label
#   is_broad_group()         - returns TRUE if selected group is a broad group (e.g. "Total", "0-14")
#   aggregate_emp_to_broad() - aggregates empirical DA data to a broad age group
#   aggregate_mod_to_broad() - aggregates modelled DA_modelled$df to a broad age group


parse_age_start <- function(age_group_str) {
  if (age_group_str == "85+") return(85)
  # broad groups do not map to a single age_start; return NA
  if (age_group_str %in% c("Total","0-14","15-24","25-49",
                           "50+","60+","70+","80+")) return(NA)
  as.numeric(strsplit(age_group_str, "-")[[1]][1])
}


# return vector of 5-year age_start values for a given age group label
# Works for both broad groups (e.g. "0-14") and standard 5-year groups (e.g. "20-24")
age_group_to_starts <- function(age_group_str) {
  switch(age_group_str,
         "Total"  = seq(0,  85, by = 5),
         "0-14"   = c(0, 5, 10),
         "15-24"  = c(15, 20),
         "25-49"  = seq(25, 45, by = 5),
         "50+"    = seq(50, 85, by = 5),
         "60+"    = seq(60, 85, by = 5),
         "70+"    = seq(70, 85, by = 5),
         "80+"    = c(80, 85),
         # Standard 5-year group: single start value
         {
           if (age_group_str == "85+") return(85)
           as.numeric(strsplit(age_group_str, "-")[[1]][1])
         }
  )
}


# return TRUE if the selected age group is a broad group
is_broad_group <- function(age_group_str) {
  age_group_str %in% c("Total","0-14","15-24","25-49",
                       "50+","60+","70+","80+")
}


# aggregate empirical DA data to a broad age group
# Sums DataValue across all constituent 5-year age groups,
aggregate_emp_to_broad <- function(df, age_starts, age_label) {
  df %>%
    dplyr::filter(AgeStart %in% age_starts) %>%
    group_by(MS_SeriesID, TimeMid, SexName, SexID, Definition,
             DataSourceShortName, Include, non_standard) %>%
    summarise(
      DataValue = sum(DataValue, na.rm = TRUE),
      AgeLabel  = age_label,       # use the selected label e.g. "0-14"
      AgeStart  = min(AgeStart),
      AgeEnd    = max(AgeStart) + 5,
      .groups   = "drop"
    )
}


# aggregate modelled DA_modelled$df to a broad age group
# Sums value and refugees across all constituent 5-year age groups,
# grouped by year and sex
aggregate_mod_to_broad <- function(df, age_starts) {
  df %>%
    dplyr::filter(age_start %in% age_starts) %>%
    group_by(year, sex) %>%
    summarise(
      value     = sum(value,    na.rm = TRUE),
      refugees  = sum(refugees, na.rm = TRUE),
      age_start = min(age_start),
      age_span  = length(unique(age_start)) * 5,
      .groups   = "drop"
    )
}


# Plot 1: Migrant stock over time for a selected age group
ms_plot_age_stock <- function(loc_id, LocName, input, MS_age, MS_modelled) {
  
  # Parse selected age group and year range from Shiny inputs
  age_start_sel <- parse_age_start(input$AgeGroup)  # NA for broad groups
  yr_min        <- min(input$YearRange_Age)
  yr_max        <- max(input$YearRange_Age)
  
  # Extract input$ values as local variables so they can be safely
  # referenced inside aes(text = paste(...)) without being misinterpreted
  # as data column names by ggplot/ggplotly
  sex_label     <- input$SexName_Age
  age_label     <- input$AgeGroup
  sex_filter    <- tolower(input$SexName_Age)
  
  # Determine the set of age_start values and whether this is a broad group
  age_starts <- age_group_to_starts(age_label)
  is_broad   <- is_broad_group(age_label)
  
  # Prepare empirical data
  if (!is.null(MS_age)) {
    
    # Broad group: aggregate across constituent 5-year groups;
    # Standard 5-year group: filter to single age_start value
    if (is_broad) {
      indata_emp <- MS_age %>%
        dplyr::filter(TimeMid >= yr_min, TimeMid <= yr_max) %>%
        aggregate_emp_to_broad(age_starts = age_starts, age_label = age_label)
    } else {
      indata_emp <- MS_age %>%
        dplyr::filter(AgeStart == age_starts,
                      TimeMid  >= yr_min,
                      TimeMid  <= yr_max)
    }
    
    # foreign-born / foreign-citizen filters from sidebar checkboxes
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
  if (!is.null(indata_emp) && nrow(indata_emp) > 0) {
    
    # Filter by selected sex from SexName_Age dropdown
    indata_emp <- indata_emp %>%
      dplyr::filter(SexName == sex_label)
    
    # Branch: Counts vs Proportion
    if (input$AgeScale == "Counts") {
      
      # Show excluded series as dashed lines if checkbox is selected
      if (input$show_excluded) {
        indata_excl <- indata_emp %>% dplyr::filter(Include == FALSE)
        if (nrow(indata_excl) > 0) {
          p <- p +
            geom_line(data = indata_excl,
                      aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID),
                      linewidth = 1, linetype = "dashed") +
            geom_point(data = indata_excl,
                       aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID,
                           text = paste("\u200b",
                                        "MS_SeriesID:", MS_SeriesID, "\n",
                                        "DataSourceShortName:", DataSourceShortName, "\n",
                                        "Definition:", Definition, "\n",
                                        "SexName:", SexName, "\n",
                                        "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                        "Age:", AgeLabel, "\n",
                                        "MS:", sprintf("%.0f", round(DataValue, 0)), "\n")),
                       size = 2, shape = 19)
        }
      }
      
      # Plot included series as solid lines
      indata_incl <- indata_emp %>% dplyr::filter(Include == TRUE)
      if (nrow(indata_incl) > 0) {
        p <- p +
          geom_line(data = indata_incl,
                    aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID),
                    linewidth = 1, linetype = "solid") +
          geom_point(data = indata_incl,
                     aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID,
                         text = paste("\u200b",
                                      "MS_SeriesID:", MS_SeriesID, "\n",
                                      "DataSourceShortName:", DataSourceShortName, "\n",
                                      "Definition:", Definition, "\n",
                                      "SexName:", SexName, "\n",
                                      "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                      "Age:", AgeLabel, "\n",
                                      "MS:", sprintf("%.0f", round(DataValue, 0)), "\n")),
                     size = 2, shape = 19)
      }
      
    } else {
      
      # Proportion mode: compute selected age group as % of all age groups
      # First compute denominator (total across all age groups per series and time point)
      if (!is.null(MS_age)) {
        denom_data <- MS_age %>%
          dplyr::filter(TimeMid  >= yr_min,
                        TimeMid  <= yr_max,
                        SexName  == sex_label,
                        !(AgeLabel %in% c("Total", "Unknown"))) %>%
          group_by(MS_SeriesID, TimeMid) %>%
          summarise(total = sum(DataValue, na.rm = TRUE), .groups = "drop")
        
        # compute proportion
        indata_emp_prop <- indata_emp %>%
          left_join(denom_data, by = c("MS_SeriesID", "TimeMid")) %>%
          mutate(proportion = DataValue / total * 100)
        
        # Show excluded series as dashed lines if checkbox is selected
        if (input$show_excluded) {
          excl_prop <- indata_emp_prop %>% dplyr::filter(Include == FALSE)
          if (nrow(excl_prop) > 0) {
            p <- p +
              geom_line(data = excl_prop,
                        aes(x = TimeMid, y = proportion, color = MS_SeriesID),
                        linewidth = 1, linetype = "dashed") +
              geom_point(data = excl_prop,
                         aes(x = TimeMid, y = proportion, color = MS_SeriesID,
                             text = paste("\u200b",
                                          "MS_SeriesID:", MS_SeriesID, "\n",
                                          "DataSourceShortName:", DataSourceShortName, "\n",
                                          "Definition:", Definition, "\n",
                                          "SexName:", SexName, "\n",
                                          "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                          "Age:", AgeLabel, "\n",
                                          "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                         size = 2, shape = 19)
          }
        }
        
        # Plot included series as solid lines
        incl_prop <- indata_emp_prop %>% dplyr::filter(Include == TRUE)
        if (nrow(incl_prop) > 0) {
          p <- p +
            geom_line(data = incl_prop,
                      aes(x = TimeMid, y = proportion, color = MS_SeriesID),
                      linewidth = 1, linetype = "solid") +
            geom_point(data = incl_prop,
                       aes(x = TimeMid, y = proportion, color = MS_SeriesID,
                           text = paste("\u200b",
                                        "MS_SeriesID:", MS_SeriesID, "\n",
                                        "DataSourceShortName:", DataSourceShortName, "\n",
                                        "Definition:", Definition, "\n",
                                        "SexName:", SexName, "\n",
                                        "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                        "Age:", AgeLabel, "\n",
                                        "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                       size = 2, shape = 19)
        }
      }
    }
  }
  
  # Plot MS2026 model estimates
  if (input$ms2026 && !is.null(MS_modelled$df)) {
    
    # Broad group: aggregate modelled data across constituent 5-year groups;
    # Standard 5-year group: filter directly
    if (is_broad) {
      df_mod <- MS_modelled$df %>%
        dplyr::filter(sex  == sex_filter,
                      year >= yr_min,
                      year <= yr_max) %>%
        aggregate_mod_to_broad(age_starts = age_starts)
    } else {
      df_mod <- MS_modelled$df %>%
        dplyr::filter(age_start == age_starts,
                      sex       == sex_filter,
                      year      >= yr_min,
                      year      <= yr_max)
    }
    
    if (nrow(df_mod) > 0) {
      
      if (input$AgeScale == "Counts") {
        # Plot counts as solid black line
        p <- p +
          geom_line(data = df_mod,
                    aes(x = year, y = value/1000, color = "MS2026"),
                    linewidth = 1, linetype = "solid") +
          geom_point(data = df_mod,
                     aes(x    = year, y = value/1000, color = "MS2026",
                         text = paste("\u200b",
                                      "MS2026", "\n",
                                      "Sex:", sex_label, "\n",
                                      "Age:", age_label, "\n",
                                      "Time:", sprintf("%.2f", year), "\n",
                                      "MS:", sprintf("%.0f", round(value, 0)), "\n")),
                     size = 1, shape = 19)
        
      } else {
        
        # Proportion mode: compute selected age group as % of all age groups
        denom_mod <- MS_modelled$df %>%
          dplyr::filter(sex      == sex_filter,
                        year     >= yr_min,
                        year     <= yr_max,
                        age_span == 5) %>%
          group_by(year) %>%
          summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
        
        df_mod_prop <- df_mod %>%
          left_join(denom_mod, by = "year") %>%
          mutate(proportion = value / total * 100)
        
        p <- p +
          geom_line(data = df_mod_prop,
                    aes(x = year, y = proportion, color = "MS2026"),
                    linewidth = 1, linetype = "solid") +
          geom_point(data = df_mod_prop,
                     aes(x = year, y = proportion, color = "MS2026",
                         text = paste("\u200b",
                                      "MS2026", "\n",
                                      "Sex:", sex_label, "\n",
                                      "Age:", age_label, "\n",
                                      "Time:", sprintf("%.2f", year), "\n",
                                      "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                     size = 1, shape = 19)
      }
    }
  }
  
  # Set colours for empirical series
  if (!is.null(indata_emp) && nrow(indata_emp) > 0) {
    id_vector    <- unique(indata_emp$MS_SeriesID)
    color_values <- rainbow(length(id_vector) + 1)
    color_values <- color_values[color_values != "#0010FF"]
    color_values <- color_values[1:length(id_vector)]
    names(color_values) <- id_vector
  } else {
    color_values <- character(0)
  }
  
  # Set y-axis label based on scale selection
  ylabel <- ifelse(input$AgeScale == "Counts",
                   "Migrant stock (thousands)",
                   "Age distribution of migrant stock (percentage)")
  
  # Combine empirical colours with fixed colours for modelled series
  all_colors <- c(color_values,
                  "MS2026" = "black",
                  "MS2024" = "red",
                  "MS2020" = "yellow",
                  "UNHCR"  = "green")
  
  # Final plot formatting
  p <- p +
    scale_color_manual(values = all_colors) +
    scale_x_continuous(breaks = seq(1950, 2030, by = 5),
                       limits = c(yr_min, yr_max)) +
    labs(x = "Year",
         y = ylabel,
         title = paste0(LocName, " \u2014 Age group: ", age_label,
                        "  |  Sex: ", sex_label),
         color = "") +
    theme_light() +
    theme(plot.margin  = unit(c(1, 1, 1, 1), "cm"),
          aspect.ratio = 0.6) +
    guides(color = "none")
  
  return(p)
}


# Plot 2: Sex ratio of migrant stock over time for a selected age group
# Sex ratio defined as males per 100 females
ms_plot_age_sexratio <- function(loc_id, LocName, input, MS_age, MS_modelled) {
  
  # Parse selected age group and year range from Shiny inputs
  age_start_sel <- parse_age_start(input$AgeGroup)  # NA for broad groups
  yr_min        <- min(input$YearRange_Age)
  yr_max        <- max(input$YearRange_Age)
  
  age_label <- input$AgeGroup
  
  # Determine the set of age_start values and whether this is a broad group
  age_starts <- age_group_to_starts(age_label)
  is_broad   <- is_broad_group(age_label)
  
  # Initialize the plot
  p <- ggplot()
  
  # Plot MS2026 modelled sex ratio
  if (!is.null(MS_modelled$df)) {
    
    # Broad group: aggregate male and female separately then compute ratio;
    # Standard 5-year group: filter directly
    if (is_broad) {
      df_sr <- MS_modelled$df %>%
        dplyr::filter(sex  %in% c("male", "female"),
                      year >= yr_min,
                      year <= yr_max) %>%
        aggregate_mod_to_broad(age_starts = age_starts) %>%
        dplyr::select(year, sex, value) %>%
        pivot_wider(names_from = sex, values_from = value) %>%
        drop_na(male, female)%>%
        mutate(sex_ratio = male / female * 100)
    } else {
      df_sr <- MS_modelled$df %>%
        dplyr::filter(age_start == age_starts,
                      sex       %in% c("male", "female"),
                      year      >= yr_min,
                      year      <= yr_max) %>%
        dplyr::select(year, sex, value) %>%
        # Pivot to wide format to compute ratio
        pivot_wider(names_from = sex, values_from = value) %>%
        drop_na(male, female) %>%
        mutate(sex_ratio = male / female * 100)   # males per 100 females
    }
    
    if (nrow(df_sr) > 0) {
      p <- p +
        geom_line(data = df_sr,
                  aes(x = year, y = sex_ratio, color = "MS2026"),
                  linewidth = 1) +
        geom_point(data = df_sr,
                   aes(x = year, y = sex_ratio, color = "MS2026",
                       text = paste("\u200b",
                                    "MS2026", "\n",
                                    "Age:", age_label, "\n",
                                    "Time:", sprintf("%.2f", year), "\n",
                                    "Sex ratio (M per 100F):",
                                    sprintf("%.1f", round(sex_ratio, 1)), "\n")),
                   size = 1, shape = 19)
    }
  }
  
  # Plot empirical sex ratio
  if (!is.null(MS_age)) {
    
    # Broad group: aggregate across constituent 5-year groups;
    # Standard 5-year group: filter directly
    if (is_broad) {
      indata_emp_sr <- MS_age %>%
        dplyr::filter(SexID   %in% c(1, 2),
                      TimeMid >= yr_min,
                      TimeMid <= yr_max) %>%
        aggregate_emp_to_broad(age_starts = age_starts, age_label = age_label)
    } else {
      indata_emp_sr <- MS_age %>%
        dplyr::filter(AgeStart == age_starts,
                      SexID    %in% c(1, 2),
                      TimeMid  >= yr_min,
                      TimeMid  <= yr_max)
    }
    
    # foreign-born / foreign-citizen filters from sidebar checkboxes
    if (!input$show_foreign_born) {
      indata_emp_sr <- indata_emp_sr %>% dplyr::filter(Definition != "Foreign-born")
    }
    if (!input$show_foreign_citizen) {
      indata_emp_sr <- indata_emp_sr %>% dplyr::filter(Definition != "Foreign citizen")
    }
    
    if (nrow(indata_emp_sr) > 0) {
      
      # Pivot to wide format and compute sex ratio
      df_emp_sr <- indata_emp_sr %>%
        dplyr::select(MS_SeriesID, TimeMid, SexID, DataValue,
                      Include, Definition, DataSourceShortName) %>%
        group_by(MS_SeriesID, TimeMid, SexID, Include, Definition, DataSourceShortName) %>%
        summarise(DataValue = mean(DataValue, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from   = SexID,
                    values_from  = DataValue,
                    names_prefix = "sex_") %>%
        mutate(sex_ratio = sex_1 / sex_2 * 100) %>%
        dplyr::filter(!is.na(sex_ratio))
      
      # Plot included empirical series
      df_emp_sr_incl <- df_emp_sr %>% dplyr::filter(Include == TRUE)
      if (nrow(df_emp_sr_incl) > 0) {
        p <- p +
          geom_point(data = df_emp_sr_incl,
                     aes(x    = TimeMid, y = sex_ratio, color = MS_SeriesID,
                         text = paste("\u200b",
                                      "Series:", MS_SeriesID, "\n",
                                      "Definition:", Definition, "\n",
                                      "Age:", age_label, "\n",
                                      "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                      "Sex ratio (M per 100F):",
                                      sprintf("%.1f", round(sex_ratio, 1)), "\n")),
                     size = 3, shape = 17)
      }
      
      # Show excluded empirical series if checkbox is selected
      if (input$show_excluded) {
        df_emp_sr_excl <- df_emp_sr %>% dplyr::filter(Include == FALSE)
        if (nrow(df_emp_sr_excl) > 0) {
          p <- p +
            geom_point(data = df_emp_sr_excl,
                       aes(x    = TimeMid, y = sex_ratio, color = MS_SeriesID,
                           text = paste("\u200b",
                                        "Series [EXCLUDED]:", MS_SeriesID, "\n",
                                        "Age:", age_label, "\n",
                                        "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                        "Sex ratio:",
                                        sprintf("%.1f", round(sex_ratio, 1)), "\n")),
                       size = 3, shape = 1, alpha = 0.5)
        }
      }
    }
  }
  
  # Add reference line at 100 (equal numbers of males and females)
  p <- p +
    geom_hline(yintercept = 100,
               linetype   = "dashed",
               color      = "gray50",
               linewidth  = 0.8)
  
  # Set colours: use age_starts (vector) instead of age_start_sel (scalar)
  # to correctly cover broad groups when checking available empirical series
  if (!is.null(MS_age)) {
    indata_check <- MS_age %>%
      dplyr::filter(AgeStart %in% age_starts,
                    SexID    %in% c(1, 2),
                    TimeMid  >= yr_min,
                    TimeMid  <= yr_max)
    if (nrow(indata_check) > 0) {
      id_vector_sr    <- unique(indata_check$MS_SeriesID)
      color_values_sr <- rainbow(length(id_vector_sr) + 1)
      color_values_sr <- color_values_sr[color_values_sr != "#0010FF"]
      color_values_sr <- color_values_sr[1:length(id_vector_sr)]
      names(color_values_sr) <- id_vector_sr
    } else {
      color_values_sr <- character(0)
    }
  } else {
    color_values_sr <- character(0)
  }
  
  # Combine empirical colours with fixed colour for MS2026 modelled series
  all_colors_sr <- c(color_values_sr, "MS2026" = "black")
  
  # Final plot formatting
  p <- p +
    scale_color_manual(values = all_colors_sr) +
    scale_x_continuous(breaks = seq(1950, 2030, by = 5),
                       limits = c(yr_min, yr_max)) +
    labs(x = "Year",
         y = "Sex ratio (males per 100 females)",
         title = paste0(LocName, " \u2014 Age group: ", age_label),
         color = "") +
    theme_light() +
    theme(plot.margin  = unit(c(1, 1, 1, 1), "cm"),
          aspect.ratio = 0.6) +
    guides(color = "none")
  
  return(p)
}


# Plot 3: Non-refugee migrant stock estimates over time for a selected age group
# Non-refugee stock = total value minus refugees
# Shown separately for both sexes, male, and female
ms_plot_age_nonrefugee <- function(loc_id, LocName, input, MS_modelled) {
  
  # Parse selected age group and year range from Shiny inputs
  age_start_sel <- parse_age_start(input$AgeGroup)  # NA for broad groups
  yr_min        <- min(input$YearRange_Age)
  yr_max        <- max(input$YearRange_Age)
  
  age_label <- input$AgeGroup
  
  # Determine the set of age_start values and whether this is a broad group
  age_starts <- age_group_to_starts(age_label)
  is_broad   <- is_broad_group(age_label)
  
  # Initialize the plot
  p <- ggplot()
  
  if (!is.null(MS_modelled$df)) {
    
    # Broad group: aggregate across constituent 5-year groups then subtract refugees;
    # Standard 5-year group: filter directly
    if (is_broad) {
      df_nr <- MS_modelled$df %>%
        dplyr::filter(year >= yr_min, year <= yr_max) %>%
        aggregate_mod_to_broad(age_starts = age_starts) %>%
        mutate(value_non_refugee = value - refugees)
    } else {
      df_nr <- MS_modelled$df %>%
        dplyr::filter(age_start == age_starts,
                      year      >= yr_min,
                      year      <= yr_max) %>%
        mutate(value_non_refugee = value - refugees)
    }
    
    # Plot both sexes as solid black line
    df_nr_both <- df_nr %>% dplyr::filter(sex == "both sexes")
    if (nrow(df_nr_both) > 0) {
      p <- p +
        geom_line(data = df_nr_both,
                  aes(x = year, y = value_non_refugee/1000, color = "Both sexes"),
                  linewidth = 1, linetype = "solid") +
        geom_point(data = df_nr_both,
                   aes(x = year, y = value_non_refugee/1000, color = "Both sexes",
                       text = paste("\u200b",
                                    "Non-refugee MS2026 (both sexes)", "\n",
                                    "Age:", age_label, "\n",
                                    "Time:", sprintf("%.2f", year), "\n",
                                    "Non-refugee MS:",
                                    sprintf("%.0f", round(value_non_refugee, 0)), "\n")),
                   size = 1, shape = 19)
    }
    
    # Plot male estimates as dashed blue line
    df_nr_male <- df_nr %>% dplyr::filter(sex == "male")
    if (nrow(df_nr_male) > 0) {
      p <- p +
        geom_line(data = df_nr_male,
                  aes(x = year, y = value_non_refugee/1000, color = "Male"),
                  linewidth = 1, linetype = "dashed") +
        geom_point(data = df_nr_male,
                   aes(x    = year, y = value_non_refugee/1000, color = "Male",
                       text = paste("\u200b",
                                    "Non-refugee MS2026 (male)", "\n",
                                    "Age:", age_label, "\n",
                                    "Time:", sprintf("%.2f", year), "\n",
                                    "Non-refugee MS:",
                                    sprintf("%.0f", round(value_non_refugee, 0)), "\n")),
                   size = 1, shape = 19)
    }
    
    # Plot female estimates as dotted red line
    df_nr_female <- df_nr %>% dplyr::filter(sex == "female")
    if (nrow(df_nr_female) > 0) {
      p <- p +
        geom_line(data = df_nr_female,
                  aes(x = year, y = value_non_refugee/1000, color = "Female"),
                  linewidth = 1, linetype = "dotted") +
        geom_point(data = df_nr_female,
                   aes(x    = year, y = value_non_refugee/1000, color = "Female",
                       text = paste("\u200b",
                                    "Non-refugee MS2026 (female)", "\n",
                                    "Age:", age_label, "\n",
                                    "Time:", sprintf("%.2f", year), "\n",
                                    "Non-refugee MS:",
                                    sprintf("%.0f", round(value_non_refugee, 0)), "\n")),
                   size = 1, shape = 19)
    }
  }
  
  # Final plot formatting
  p <- p +
    scale_color_manual(values = c("Both sexes" = "black",
                                  "Male"       = "steelblue",
                                  "Female"     = "tomato")) +
    scale_x_continuous(breaks = seq(1950, 2030, by = 5),
                       limits = c(yr_min, yr_max)) +
    labs(x     = "Year",
         y     = "Non-refugee migrant stock (thousands)",
         title = paste0(LocName, " \u2014 Age group: ", age_label),
         color = "Sex") +
    theme_light() +
    theme(plot.margin  = unit(c(1, 1, 1, 1), "cm"),
          aspect.ratio = 0.6)
  
  return(p)
}