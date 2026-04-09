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
  if (age_group_str %in% c("75+", "85+")) return(as.numeric(substr(age_group_str, 1, 2)))
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
           if (age_group_str %in% c("75+", "85+")) {
             return(as.numeric(substr(age_group_str, 1, 2)))
           }
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
  # Keep only rows whose AgeLabel corresponds to a standard 5-year group.
  # This prevents "Total" or other broad-group rows (which share the same
  # AgeStart values) from being double-counted in the sum.
  standard_5yr_labels <- c(
    "0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
    "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",
    "80-84","85+"
  )
  df %>%
    dplyr::filter(
      AgeStart %in% age_starts,
      AgeLabel %in% standard_5yr_labels
    ) %>%
    group_by(MS_SeriesID, TimeMid, SexName, SexID, Definition,
             DataSourceShortName, Include, non_standard, DataProcess) %>%  
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
# Modified to handle missing 'refugees' column (for ms2020age data)
aggregate_mod_to_broad <- function(df, age_starts) {
  df %>%
    dplyr::filter(age_start %in% age_starts) %>%
    group_by(year, sex) %>%
    summarise(
      value     = sum(value,    na.rm = TRUE),
      refugees  = if("refugees" %in% names(.)) sum(refugees, na.rm = TRUE) else 0,
      age_start = min(age_start),
      age_span  = length(unique(age_start)) * 5,
      .groups   = "drop"
    )
}


# Plot 1: Migrant stock over time for a selected age group
ms_plot_age_stock <- function(loc_id, LocName, input, MS_age, MS_modelled,
                              ms2020_age = NULL) {
  
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
        dplyr::filter(
          AgeStart == age_starts,
          AgeLabel == age_label,
          TimeMid  >= yr_min,
          TimeMid  <= yr_max
        )
    }
    
    
    if (!("DataProcess" %in% names(indata_emp))) {
      indata_emp <- indata_emp %>% mutate(DataProcess = "Census")
    }
    
    if (!input$census) {
      indata_emp <- indata_emp[!(indata_emp$DataProcess %in% c("Census", "Register", "Population and Housing Census")), ]
    }
    if (!input$estimate) {
      indata_emp <- indata_emp[indata_emp$DataProcess != "Estimate", ]
    }
    if (!input$survey) {
      indata_emp <- indata_emp[indata_emp$DataProcess != "Survey", ]
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
    indata_emp_sex <- indata_emp %>%
      dplyr::filter(SexName == sex_label)
    
    if (input$AgeScale == "Counts") {
      
      # Helper: determine DataProcess category for colour/shape mapping
      # Add a DataProcess column if not present (broad aggregation drops it)
      if (!("DataProcess" %in% names(indata_emp_sex))) {
        # After broad aggregation DataProcess is lost; fall back to "Census" as default
        # (in practice MS_age always has DataProcess; this is a safety net)
        indata_emp_sex <- indata_emp_sex %>% mutate(DataProcess = "Census")
      }
      
      # Split into included and excluded subsets
      incl_emp <- indata_emp_sex %>% dplyr::filter(Include == TRUE)
      excl_emp <- if (input$show_excluded) {
        indata_emp_sex %>% dplyr::filter(Include == FALSE)
      } else {
        indata_emp_sex %>% dplyr::filter(FALSE)   # empty
      }
      
      census_procs <- c("Census", "Register", "Population and Housing Census")
      
      # Included Census
      d <- incl_emp %>% dplyr::filter(DataProcess %in% census_procs)
      if (nrow(d) > 0)
        p <- p + geom_point(data = d,
                            aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID,
                                text = paste("\u200b",
                                             "MS_SeriesID:", MS_SeriesID, "\n",
                                             "DataSourceShortName:", DataSourceShortName, "\n",
                                             "Definition:", Definition, "\n",
                                             "SexName:", SexName, "\n",
                                             "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                             "Age:", AgeLabel, "\n",
                                             "MS:", sprintf("%.0f", round(DataValue, 0)), "\n")),
                            size = 3, shape = 18, color = "purple")   # filled diamond
      
      # Excluded Census
      d <- excl_emp %>% dplyr::filter(DataProcess %in% census_procs)
      if (nrow(d) > 0)
        p <- p + geom_point(data = d,
                            aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID,
                                text = paste("\u200b",
                                             "MS_SeriesID [EXCLUDED]:", MS_SeriesID, "\n",
                                             "DataSourceShortName:", DataSourceShortName, "\n",
                                             "Definition:", Definition, "\n",
                                             "SexName:", SexName, "\n",
                                             "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                             "Age:", AgeLabel, "\n",
                                             "MS:", sprintf("%.0f", round(DataValue, 0)), "\n")),
                            size = 3, shape = 5, color = "purple")    # open diamond
      
      # Included Estimate
      d <- incl_emp %>% dplyr::filter(DataProcess == "Estimate")
      if (nrow(d) > 0)
        p <- p + geom_point(data = d,
                            aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID,
                                text = paste("\u200b",
                                             "MS_SeriesID:", MS_SeriesID, "\n",
                                             "DataSourceShortName:", DataSourceShortName, "\n",
                                             "Definition:", Definition, "\n",
                                             "SexName:", SexName, "\n",
                                             "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                             "Age:", AgeLabel, "\n",
                                             "MS:", sprintf("%.0f", round(DataValue, 0)), "\n")),
                            size = 3, shape = 17, color = "orange")   # filled triangle
      
      # Excluded Estimate
      d <- excl_emp %>% dplyr::filter(DataProcess == "Estimate")
      if (nrow(d) > 0)
        p <- p + geom_point(data = d,
                            aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID,
                                text = paste("\u200b",
                                             "MS_SeriesID [EXCLUDED]:", MS_SeriesID, "\n",
                                             "DataSourceShortName:", DataSourceShortName, "\n",
                                             "Definition:", Definition, "\n",
                                             "SexName:", SexName, "\n",
                                             "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                             "Age:", AgeLabel, "\n",
                                             "MS:", sprintf("%.0f", round(DataValue, 0)), "\n")),
                            size = 3, shape = 2, color = "orange")    # open triangle
      
      # Included Survey
      d <- incl_emp %>% dplyr::filter(DataProcess == "Survey")
      if (nrow(d) > 0)
        p <- p + geom_point(data = d,
                            aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID,
                                text = paste("\u200b",
                                             "MS_SeriesID:", MS_SeriesID, "\n",
                                             "DataSourceShortName:", DataSourceShortName, "\n",
                                             "Definition:", Definition, "\n",
                                             "SexName:", SexName, "\n",
                                             "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                             "Age:", AgeLabel, "\n",
                                             "MS:", sprintf("%.0f", round(DataValue, 0)), "\n")),
                            size = 3, shape = 15, color = "cornflowerblue")  # filled square
      
      # Excluded Survey
      d <- excl_emp %>% dplyr::filter(DataProcess == "Survey")
      if (nrow(d) > 0)
        p <- p + geom_point(data = d,
                            aes(x = TimeMid, y = DataValue/1000, color = MS_SeriesID,
                                text = paste("\u200b",
                                             "MS_SeriesID [EXCLUDED]:", MS_SeriesID, "\n",
                                             "DataSourceShortName:", DataSourceShortName, "\n",
                                             "Definition:", Definition, "\n",
                                             "SexName:", SexName, "\n",
                                             "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                             "Age:", AgeLabel, "\n",
                                             "MS:", sprintf("%.0f", round(DataValue, 0)), "\n")),
                            size = 3, shape = 0, color = "cornflowerblue")   # open square
      
    } else {
      
      # Proportion mode: compute selected age group as % of all age groups
      if (!is.null(MS_age)) {
        denom_data <- MS_age %>%
          dplyr::filter(TimeMid  >= yr_min,
                        TimeMid  <= yr_max,
                        SexName  == sex_label,
                        !(AgeLabel %in% c("Total", "Unknown"))) %>%
          group_by(MS_SeriesID, TimeMid) %>%
          summarise(total = sum(DataValue, na.rm = TRUE), .groups = "drop")
        
        indata_emp_prop <- indata_emp_sex %>%
          left_join(denom_data, by = c("MS_SeriesID", "TimeMid")) %>%
          mutate(proportion = DataValue / total * 100)
        
        if (!("DataProcess" %in% names(indata_emp_prop))) {
          indata_emp_prop <- indata_emp_prop %>% mutate(DataProcess = "Census")
        }
        
        census_procs <- c("Census", "Register", "Population and Housing Census")
        
        incl_prop <- indata_emp_prop %>% dplyr::filter(Include == TRUE)
        excl_prop <- if (input$show_excluded) {
          indata_emp_prop %>% dplyr::filter(Include == FALSE)
        } else {
          indata_emp_prop %>% dplyr::filter(FALSE)
        }
        
        
        d <- incl_prop %>% dplyr::filter(DataProcess %in% census_procs)
        if (nrow(d) > 0)
          p <- p + geom_point(data = d,
                              aes(x = TimeMid, y = proportion, color = MS_SeriesID,
                                  text = paste("\u200b",
                                               "MS_SeriesID:", MS_SeriesID, "\n",
                                               "DataSourceShortName:", DataSourceShortName, "\n",
                                               "Definition:", Definition, "\n",
                                               "SexName:", SexName, "\n",
                                               "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                               "Age:", AgeLabel, "\n",
                                               "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                              size = 3, shape = 18, color = "purple")
        
        d <- excl_prop %>% dplyr::filter(DataProcess %in% census_procs)
        if (nrow(d) > 0)
          p <- p + geom_point(data = d,
                              aes(x = TimeMid, y = proportion, color = MS_SeriesID,
                                  text = paste("\u200b",
                                               "MS_SeriesID [EXCLUDED]:", MS_SeriesID, "\n",
                                               "DataSourceShortName:", DataSourceShortName, "\n",
                                               "Definition:", Definition, "\n",
                                               "SexName:", SexName, "\n",
                                               "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                               "Age:", AgeLabel, "\n",
                                               "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                              size = 3, shape = 5, color = "purple")
        
        d <- incl_prop %>% dplyr::filter(DataProcess == "Estimate")
        if (nrow(d) > 0)
          p <- p + geom_point(data = d,
                              aes(x = TimeMid, y = proportion, color = MS_SeriesID,
                                  text = paste("\u200b",
                                               "MS_SeriesID:", MS_SeriesID, "\n",
                                               "DataSourceShortName:", DataSourceShortName, "\n",
                                               "Definition:", Definition, "\n",
                                               "SexName:", SexName, "\n",
                                               "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                               "Age:", AgeLabel, "\n",
                                               "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                              size = 3, shape = 17, color = "orange")
        
        d <- excl_prop %>% dplyr::filter(DataProcess == "Estimate")
        if (nrow(d) > 0)
          p <- p + geom_point(data = d,
                              aes(x = TimeMid, y = proportion, color = MS_SeriesID,
                                  text = paste("\u200b",
                                               "MS_SeriesID [EXCLUDED]:", MS_SeriesID, "\n",
                                               "DataSourceShortName:", DataSourceShortName, "\n",
                                               "Definition:", Definition, "\n",
                                               "SexName:", SexName, "\n",
                                               "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                               "Age:", AgeLabel, "\n",
                                               "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                              size = 3, shape = 2, color = "orange")
        
        d <- incl_prop %>% dplyr::filter(DataProcess == "Survey")
        if (nrow(d) > 0)
          p <- p + geom_point(data = d,
                              aes(x = TimeMid, y = proportion, color = MS_SeriesID,
                                  text = paste("\u200b",
                                               "MS_SeriesID:", MS_SeriesID, "\n",
                                               "DataSourceShortName:", DataSourceShortName, "\n",
                                               "Definition:", Definition, "\n",
                                               "SexName:", SexName, "\n",
                                               "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                               "Age:", AgeLabel, "\n",
                                               "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                              size = 3, shape = 15, color = "cornflowerblue")
        
        d <- excl_prop %>% dplyr::filter(DataProcess == "Survey")
        if (nrow(d) > 0)
          p <- p + geom_point(data = d,
                              aes(x = TimeMid, y = proportion, color = MS_SeriesID,
                                  text = paste("\u200b",
                                               "MS_SeriesID [EXCLUDED]:", MS_SeriesID, "\n",
                                               "DataSourceShortName:", DataSourceShortName, "\n",
                                               "Definition:", Definition, "\n",
                                               "SexName:", SexName, "\n",
                                               "Time:", sprintf("%.1f", round(TimeMid, 1)), "\n",
                                               "Age:", AgeLabel, "\n",
                                               "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                              size = 3, shape = 0, color = "cornflowerblue")
      }
    }
  }
  
  
  
  if (!is.null(MS_modelled$df)) {
    
    # Prepare modelled data subset (shared base for both lines)
    if (is_broad) {
      df_mod_base <- MS_modelled$df %>%
        dplyr::filter(sex  == sex_filter,
                      year >= yr_min,
                      year <= yr_max) %>%
        aggregate_mod_to_broad(age_starts = age_starts)
    } else {
      df_mod_base <- MS_modelled$df %>%
        dplyr::filter(age_start == age_starts,
                      sex       == sex_filter,
                      year      >= yr_min,
                      year      <= yr_max)
    }
    
    if (nrow(df_mod_base) > 0) {
      
      
      if (isTRUE(input$ms2026)) {
        
        if (input$AgeScale == "Counts") {
          p <- p +
            geom_line(data = df_mod_base,
                      aes(x = year, y = value/1000, color = "MS2026"),
                      linewidth = 1, linetype = "solid") +
            geom_point(data = df_mod_base,
                       aes(x    = year, y = value/1000, color = "MS2026",
                           text = paste("\u200b",
                                        "MS2026 (incl. refugees)", "\n",
                                        "Sex:", sex_label, "\n",
                                        "Age:", age_label, "\n",
                                        "Time:", sprintf("%.2f", year), "\n",
                                        "MS:", sprintf("%.0f", round(value, 0)), "\n")),
                       size = 1, shape = 19)
          
        } else {
          
          denom_mod <- MS_modelled$df %>%
            dplyr::filter(sex      == sex_filter,
                          year     >= yr_min,
                          year     <= yr_max,
                          age_span == 5) %>%
            group_by(year) %>%
            summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
          
          df_mod_prop <- df_mod_base %>%
            left_join(denom_mod, by = "year") %>%
            mutate(proportion = value / total * 100)
          
          p <- p +
            geom_line(data = df_mod_prop,
                      aes(x = year, y = proportion, color = "MS2026"),
                      linewidth = 1, linetype = "solid") +
            geom_point(data = df_mod_prop,
                       aes(x = year, y = proportion, color = "MS2026",
                           text = paste("\u200b",
                                        "MS2026 (incl. refugees)", "\n",
                                        "Sex:", sex_label, "\n",
                                        "Age:", age_label, "\n",
                                        "Time:", sprintf("%.2f", year), "\n",
                                        "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                       size = 1, shape = 19)
        }
      }
      
      
      if (isTRUE(input$ms2026excl)) {
        
        df_mod_excl <- df_mod_base %>%
          mutate(value_excl = value - refugees)
        
        if (input$AgeScale == "Counts") {
          p <- p +
            geom_line(data = df_mod_excl,
                      aes(x = year, y = value_excl/1000, color = "MS2026excl"),
                      linewidth = 1, linetype = "dashed") +
            geom_point(data = df_mod_excl,
                       aes(x    = year, y = value_excl/1000, color = "MS2026excl",
                           text = paste("\u200b",
                                        "MS2026 (excl. refugees)", "\n",
                                        "Sex:", sex_label, "\n",
                                        "Age:", age_label, "\n",
                                        "Time:", sprintf("%.2f", year), "\n",
                                        "MS (excl. refugees):", sprintf("%.0f", round(value_excl, 0)), "\n")),
                       size = 1, shape = 19)
          
        } else {
          
          denom_mod <- MS_modelled$df %>%
            dplyr::filter(sex      == sex_filter,
                          year     >= yr_min,
                          year     <= yr_max,
                          age_span == 5) %>%
            group_by(year) %>%
            summarise(total = sum(value - refugees, na.rm = TRUE), .groups = "drop")
          
          df_mod_excl_prop <- df_mod_excl %>%
            left_join(denom_mod, by = "year") %>%
            mutate(proportion = value_excl / total * 100)
          
          p <- p +
            geom_line(data = df_mod_excl_prop,
                      aes(x = year, y = proportion, color = "MS2026excl"),
                      linewidth = 1, linetype = "dashed") +
            geom_point(data = df_mod_excl_prop,
                       aes(x = year, y = proportion, color = "MS2026excl",
                           text = paste("\u200b",
                                        "MS2026 (excl. refugees)", "\n",
                                        "Sex:", sex_label, "\n",
                                        "Age:", age_label, "\n",
                                        "Time:", sprintf("%.2f", year), "\n",
                                        "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                       size = 1, shape = 19)
        }
      }
    }
  }
  
  
  # MS2020 data processing with standardized column names and sex codes
  if (!is.null(ms2020_age) && isTRUE(input$ms2020) && !(age_label %in% c("75-79", "80-84", "85+"))) {
    
    # Standardize ms2020age column names to match function conventions
    # Convert numeric sex codes (0/1/2) to character strings
    # Add refugees column (0) for compatibility with aggregate_mod_to_broad
    # Remove Total rows to avoid double-counting during aggregation
    ms2020_std <- ms2020_age %>%
      rename(age_start = AgeStart, age_span = AgeSpan) %>%
      mutate(
        sex = case_when(
          sex == 0 ~ "both sexes",
          sex == 1 ~ "male",
          sex == 2 ~ "female",
          TRUE ~ as.character(sex)
        ),
        refugees = 0  # Add dummy refugees column for compatibility
      ) %>%
      filter(AgeLabel != "Total")  # Exclude total rows to prevent double counting
    
    if (is_broad) {
      df_ms2020 <- ms2020_std %>%
        dplyr::filter(sex  == sex_filter,
                      year >= yr_min,
                      year <= yr_max) %>%
        aggregate_mod_to_broad(age_starts = age_starts)
    } else {
      df_ms2020 <- ms2020_std %>%
        dplyr::filter(age_start == age_starts,
                      sex       == sex_filter,
                      year      >= yr_min,
                      year      <= yr_max)
    }
    
    if (nrow(df_ms2020) > 0) {
      
      if (input$AgeScale == "Counts") {
        p <- p +
          geom_point(data = df_ms2020,
                     aes(x = year, y = value/1000, color = "MS2020",
                         text = paste("\u200b",
                                      "MS2020", "\n",
                                      "Sex:", sex_label, "\n",
                                      "Age:", age_label, "\n",
                                      "Time:", sprintf("%.2f", year), "\n",
                                      "MS:", sprintf("%.0f", round(value, 0)), "\n")),
                     size = 2, shape = 19)   # yellow circle (colour set in scale_color_manual)
        
      } else {
        
        denom_ms2020 <- ms2020_std %>%
          dplyr::filter(sex  == sex_filter,
                        year >= yr_min,
                        year <= yr_max) %>%
          group_by(year) %>%
          summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
        
        df_ms2020_prop <- df_ms2020 %>%
          left_join(denom_ms2020, by = "year") %>%
          mutate(proportion = value / total * 100)
        
        p <- p +
          geom_point(data = df_ms2020_prop,
                     aes(x = year, y = proportion, color = "MS2020",
                         text = paste("\u200b",
                                      "MS2020", "\n",
                                      "Sex:", sex_label, "\n",
                                      "Age:", age_label, "\n",
                                      "Time:", sprintf("%.2f", year), "\n",
                                      "Pct:", sprintf("%.1f", round(proportion, 1)), "\n")),
                     size = 2, shape = 19)
      }
    }
  }
  
  # Final plot formatting
  p <- p +
    scale_color_manual(values = c(
      "MS2026"    = "black",
      "MS2026excl"= "black",   # same colour, distinguished by linetype (dashed)
      "MS2020"    = "yellow"
    )) +
    scale_x_continuous(breaks = seq(1950, 2030, by = 5),
                       limits = c(yr_min, yr_max)) +
    labs(x = "Year",
         y = ifelse(input$AgeScale == "Counts",
                    "Migrant stock (thousands)",
                    "Age distribution of migrant stock (percentage)"),
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
ms_plot_age_sexratio <- function(loc_id, LocName, input, MS_age, MS_modelled, 
                                 ms2020_age = NULL) {
  
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
  
  # Plot MS2026 modelled sex ratio (incl. refugees) — controlled by input$ms2026
  if (isTRUE(input$ms2026) && !is.null(MS_modelled$df)) {
    
    if (is_broad) {
      df_sr <- MS_modelled$df %>%
        dplyr::filter(sex  %in% c("male", "female"),
                      year >= yr_min,
                      year <= yr_max) %>%
        aggregate_mod_to_broad(age_starts = age_starts) %>%
        dplyr::select(year, sex, value) %>%
        pivot_wider(names_from = sex, values_from = value) %>%
        drop_na(male, female) %>%
        mutate(sex_ratio = male / female * 100)
    } else {
      df_sr <- MS_modelled$df %>%
        dplyr::filter(age_start == age_starts,
                      sex       %in% c("male", "female"),
                      year      >= yr_min,
                      year      <= yr_max) %>%
        dplyr::select(year, sex, value) %>%
        pivot_wider(names_from = sex, values_from = value) %>%
        drop_na(male, female) %>%
        mutate(sex_ratio = male / female * 100)
    }
    
    if (nrow(df_sr) > 0) {
      p <- p +
        geom_line(data = df_sr,
                  aes(x = year, y = sex_ratio, color = "MS2026"),
                  linewidth = 1, linetype = "solid") +
        geom_point(data = df_sr,
                   aes(x = year, y = sex_ratio, color = "MS2026",
                       text = paste("\u200b",
                                    "MS2026 (incl. refugees)", "\n",
                                    "Age:", age_label, "\n",
                                    "Time:", sprintf("%.2f", year), "\n",
                                    "Sex ratio (M per 100F):",
                                    sprintf("%.1f", round(sex_ratio, 1)), "\n")),
                   size = 1, shape = 19)
    }
  }
  
  # Plot MS2026 modelled sex ratio (excl. refugees) — controlled by input$ms2026excl
  if (isTRUE(input$ms2026excl) && !is.null(MS_modelled$df)) {
    
    if (is_broad) {
      df_sr_excl <- MS_modelled$df %>%
        dplyr::filter(sex  %in% c("male", "female"),
                      year >= yr_min,
                      year <= yr_max) %>%
        aggregate_mod_to_broad(age_starts = age_starts) %>%
        mutate(value_excl = value - refugees) %>%
        dplyr::select(year, sex, value_excl) %>%
        pivot_wider(names_from = sex, values_from = value_excl) %>%
        drop_na(male, female) %>%
        mutate(sex_ratio_excl = male / female * 100)
    } else {
      df_sr_excl <- MS_modelled$df %>%
        dplyr::filter(age_start == age_starts,
                      sex       %in% c("male", "female"),
                      year      >= yr_min,
                      year      <= yr_max) %>%
        mutate(value_excl = value - refugees) %>%
        dplyr::select(year, sex, value_excl) %>%
        pivot_wider(names_from = sex, values_from = value_excl) %>%
        drop_na(male, female) %>%
        mutate(sex_ratio_excl = male / female * 100)
    }
    
    if (nrow(df_sr_excl) > 0) {
      p <- p +
        geom_line(data = df_sr_excl,
                  aes(x = year, y = sex_ratio_excl, color = "MS2026excl"),
                  linewidth = 1, linetype = "dashed") +
        geom_point(data = df_sr_excl,
                   aes(x = year, y = sex_ratio_excl, color = "MS2026excl",
                       text = paste("\u200b",
                                    "MS2026 (excl. refugees)", "\n",
                                    "Age:", age_label, "\n",
                                    "Time:", sprintf("%.2f", year), "\n",
                                    "Sex ratio (M per 100F):",
                                    sprintf("%.1f", round(sex_ratio_excl, 1)), "\n")),
                   size = 1, shape = 19)
    }
  }
  
  # Plot MS2020 sex ratio
  if (!is.null(ms2020_age) && isTRUE(input$ms2020) && !(age_label %in% c("75-79", "80-84", "85+"))) {
    
    # Standardize MS2020 data column names and sex codes (consistent with Plot1)
    ms2020_std <- ms2020_age %>%
      rename(age_start = AgeStart, age_span = AgeSpan) %>%
      mutate(
        sex = case_when(
          sex == 0 ~ "both sexes",
          sex == 1 ~ "male",
          sex == 2 ~ "female",
          TRUE ~ as.character(sex)
        ),
        refugees = 0
      ) %>%
      filter(AgeLabel != "Total")  # Exclude total rows to prevent double counting
    
    # Extract male and female data separately for sex ratio calculation
    if (is_broad) {
      df_ms2020_male <- ms2020_std %>%
        dplyr::filter(sex == "male",
                      year >= yr_min,
                      year <= yr_max) %>%
        aggregate_mod_to_broad(age_starts = age_starts)
      
      df_ms2020_female <- ms2020_std %>%
        dplyr::filter(sex == "female",
                      year >= yr_min,
                      year <= yr_max) %>%
        aggregate_mod_to_broad(age_starts = age_starts)
    } else {
      df_ms2020_male <- ms2020_std %>%
        dplyr::filter(age_start == age_starts,
                      sex == "male",
                      year >= yr_min,
                      year <= yr_max)
      
      df_ms2020_female <- ms2020_std %>%
        dplyr::filter(age_start == age_starts,
                      sex == "female",
                      year >= yr_min,
                      year <= yr_max)
    }
    
    # Calculate sex ratio (males per 100 females) and plot
    if (nrow(df_ms2020_male) > 0 && nrow(df_ms2020_female) > 0) {
      df_ms2020_sr <- df_ms2020_male %>%
        select(year, male_value = value) %>%
        inner_join(
          df_ms2020_female %>% select(year, female_value = value),
          by = "year"
        ) %>%
        mutate(sex_ratio = male_value / female_value * 100) %>%
        filter(!is.na(sex_ratio))
      
      if (nrow(df_ms2020_sr) > 0) {
        p <- p +
          geom_point(data = df_ms2020_sr,
                     aes(x = year, y = sex_ratio, color = "MS2020",
                         text = paste("\u200b",
                                      "MS2020", "\n",
                                      "Age:", age_label, "\n",
                                      "Time:", sprintf("%.2f", year), "\n",
                                      "Sex ratio (M per 100F):",
                                      sprintf("%.1f", round(sex_ratio, 1)), "\n")),
                     size = 2, shape = 19)  
      }
    }
  }
  
  
  # Plot empirical sex ratio
  if (!is.null(MS_age)) {
    
    if (is_broad) {
      indata_emp_sr <- MS_age %>%
        dplyr::filter(SexID   %in% c(1, 2),
                      TimeMid >= yr_min,
                      TimeMid <= yr_max) %>%
        aggregate_emp_to_broad(age_starts = age_starts, age_label = age_label)
    } else {
      indata_emp_sr <- MS_age %>%
        dplyr::filter(AgeStart == age_starts,
                      AgeLabel == age_label,
                      SexID    %in% c(1, 2),
                      TimeMid  >= yr_min,
                      TimeMid  <= yr_max)
    }
    
    if (!("DataProcess" %in% names(indata_emp_sr))) {
      indata_emp_sr <- indata_emp_sr %>% mutate(DataProcess = "Census")
    }
    
    if (!input$census) {
      indata_emp_sr <- indata_emp_sr[!(indata_emp_sr$DataProcess %in% c("Census", "Register", "Population and Housing Census")), ]
    }
    if (!input$estimate) {
      indata_emp_sr <- indata_emp_sr[indata_emp_sr$DataProcess != "Estimate", ]
    }
    if (!input$survey) {
      indata_emp_sr <- indata_emp_sr[indata_emp_sr$DataProcess != "Survey", ]
    }
    
    # foreign-born / foreign-citizen filters from sidebar checkboxes
    if (!input$show_foreign_born) {
      indata_emp_sr <- indata_emp_sr %>% dplyr::filter(Definition != "Foreign-born")
    }
    if (!input$show_foreign_citizen) {
      indata_emp_sr <- indata_emp_sr %>% dplyr::filter(Definition != "Foreign citizen")
    }
    
    if (nrow(indata_emp_sr) > 0) {
      
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
                     size = 3, shape = 18, color = "purple")
      }
      
      
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
                       size = 3, shape = 5, color = "purple")
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
  
  all_colors_sr <- c("MS2026" = "black", "MS2026excl" = "black", "MS2020" = "yellow")
  
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