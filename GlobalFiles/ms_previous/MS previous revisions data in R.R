# compile estimates from previous Migrant Stock revisions into an R data frame
library(readxl)
library(tidyverse)

# ms_dir <- "C:/Users/SHERTOG/OneDrive - United Nations/MS 2025/Shiny"
ms_dir <- "D:/msShiny"

shp <- function(indata, cols, r24 = FALSE) {
  df <- indata[,cols] 
  names(df)[1:8] <- c("LocID","X1990","X1995","X2000","X2005","X2010","X2015","X2020")
  if (r24 == TRUE) {names(df)[9] <- "X2024"}
  df <- df %>% 
    pivot_longer(2:ncol(df), names_to = "year", values_to = "value") %>% 
    mutate(year = as.numeric(substr(year,2,5)) + 0.5)
  return(df)
}

# get totals by destination and sex from Table 1 of MS 2024 edition
Tbl1_2024 <- read_xlsx(path = file.path(ms_dir, "GlobalFiles", "ms_previous", "undesa_pd_2024_ims_stock_by_sex_and_destination.xlsx"), sheet = "Table 1", skip =10)

ms2024 <- shp(indata = Tbl1_2024, cols = c(5:13), r24 = TRUE) %>% 
  mutate(sex = 0) %>% 
  bind_rows(shp(indata = Tbl1_2024, cols = c(5, 14:21), r24 = TRUE) %>% 
              mutate(sex = 1)) %>% 
  bind_rows(shp(indata = Tbl1_2024, cols = c(5, 22:29), r24 = TRUE) %>% 
              mutate(sex = 2)) %>% 
  mutate(value = as.numeric(value))

save(ms2024, file = file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2024.rda"))

# get totals by destination and sex from Table 1 of MS 2020 edition
Tbl1_2020 <- read_xlsx(path = file.path(ms_dir, "GlobalFiles", "ms_previous", "undesa_pd_2020_ims_stock_by_sex_and_destination.xlsx"), sheet = "Table 1", skip =10)

ms2020 <- shp(indata = Tbl1_2020, cols = c(4, 6:12)) %>% 
  mutate(sex = 0) %>% 
  bind_rows(shp(indata = Tbl1_2020, cols = c(4, 13:19)) %>% 
              mutate(sex = 1)) %>% 
  bind_rows(shp(indata = Tbl1_2020, cols = c(4, 20:26)) %>% 
              mutate(sex = 2)) %>% 
  mutate(value = as.numeric(value))

save(ms2020, file = file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2020.rda"))

library(jsonlite)
eagle_locations <- fromJSON("https://popdiv.dfs.un.org/peps/eagle/api/file/ProcessedListCompact/2024", flatten=TRUE)
MS_Locations <- eagle_locations[,c("LocID","LocName","ISO3")]
save(MS_Locations, file = file.path(ms_dir, "GlobalFiles","MS_Locations.rda"))


shporg <- function(indata, cols, r24 = FALSE) {
  df <- indata[,cols] 
  names(df)[1:9] <- c("LocIDdest","LocIDorg","X1990","X1995","X2000","X2005","X2010","X2015","X2020")
  if (r24 == TRUE) {names(df)[10] <- "X2024"}
  df <- df %>% 
    pivot_longer(3:ncol(df), names_to = "year", values_to = "value") %>% 
    mutate(year = as.numeric(substr(year,2,5)) + 0.5)
  return(df)
}

# get totals by destination and origin from ms2024
Tbl1org_2024 <- read_xlsx(path = file.path(ms_dir, "GlobalFiles", "ms_previous", "undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx"), sheet = "Table 1", skip =10)

ms2024org <- shporg(indata = Tbl1org_2024, cols = c(5,7,8:15), r24 = TRUE) %>% 
  mutate(sex = 0) %>% 
  bind_rows(shporg(indata = Tbl1org_2024, cols = c(5,7,16:23), r24 = TRUE) %>% 
              mutate(sex = 1)) %>% 
  bind_rows(shporg(indata = Tbl1org_2024, cols = c(5,7, 24:31), r24 = TRUE) %>% 
              mutate(sex = 2)) %>% 
  mutate(value = as.numeric(value))
save(ms2024org, file = file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2024org.rda"))

# get totals by destination and origin from ms2024
Tbl1org_2020 <- read_xlsx(path = file.path(ms_dir, "GlobalFiles", "ms_previous", "undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx"), sheet = "Table 1", skip =10)

ms2020org <- shporg(indata = Tbl1org_2020, cols = c(4,7,8:14), r24 = FALSE) %>% 
  mutate(sex = 0) %>% 
  bind_rows(shporg(indata = Tbl1org_2020, cols = c(4,7,15:21), r24 = FALSE) %>% 
              mutate(sex = 1)) %>% 
  bind_rows(shporg(indata = Tbl1org_2020, cols = c(4,7, 22:28), r24 = FALSE) %>% 
              mutate(sex = 2)) %>% 
  mutate(value = as.numeric(value))
save(ms2020org, file = file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2020org.rda"))

# retrieve total refugee estimates used in 2020 revision (Table 6)
Tbl6_2020 <- read_xlsx(path = file.path(ms_dir, "GlobalFiles", "ms_previous", "undesa_pd_2020_ims_stock_by_sex_and_destination.xlsx"), sheet = "Table 6", skip =10)

ms2020rfg <- shp(indata = Tbl6_2020, cols = c(4, 6:12)) %>% 
  mutate(sex = 0,
         value = as.numeric(value),
         value = ifelse(is.na(value), 0, value))
save(ms2020rfg, file = file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2020rfg.rda"))

# retrieve migrant stock by age used in 2020 revision (Table 1)
Tbl1_2020_age <- read_xlsx(
  path  = file.path(ms_dir, "GlobalFiles", "ms_previous",
                    "undesa_pd_2020_ims_stock_by_age_sex_and_destination.xlsx"),
  sheet = "Table 1",
  skip  = 10
)

# define age group metadata (including Total)
age_labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+","Total")
age_starts <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 0)
age_ends   <- c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, -1, -1)
age_spans  <- c(5, 5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  -1, -1)
n_age      <- length(age_labels)   # 17 age groups (including Total)

# column indices: col 2 = Year, col 5 = LocID
# both sexes (sex=0): cols 7~23  (cols 7~22 = age groups, col 23 = Total)
# male       (sex=1): cols 24~40 (cols 24~39 = age groups, col 40 = Total)
# female     (sex=2): cols 41~57 (cols 41~56 = age groups, col 57 = Total)
col_year   <- 2
col_loc    <- 5
col_both   <- 7:(7  + n_age - 1)   # 7~23
col_male   <- 24:(24 + n_age - 1)  # 24~40
col_female <- 41:(41 + n_age - 1)  # 41~57

# helper function: extract values for one sex group and pivot to long format
extract_sex <- function(df, col_loc, col_year, col_vals, sex_code) {
  df[, c(col_loc, col_year, col_vals)] %>%
    setNames(c("LocID", "year", age_labels)) %>%
    pivot_longer(
      cols      = all_of(age_labels),
      names_to  = "AgeLabel",
      values_to = "value"
    ) %>%
    mutate(
      sex   = sex_code,
      year  = as.numeric(year) + 0.5,
      value = as.numeric(value)
    )
}

# combine all three sex groups and join age group metadata
ms2020age <- bind_rows(
  extract_sex(Tbl1_2020_age, col_loc, col_year, col_both,   sex_code = 0),
  extract_sex(Tbl1_2020_age, col_loc, col_year, col_male,   sex_code = 1),
  extract_sex(Tbl1_2020_age, col_loc, col_year, col_female, sex_code = 2)
) %>%
  left_join(
    data.frame(
      AgeLabel = age_labels,
      AgeStart = age_starts,
      AgeEnd   = age_ends,
      AgeSpan  = age_spans,
      stringsAsFactors = FALSE
    ),
    by = "AgeLabel"
  ) %>%
  dplyr::select(LocID, year, sex, AgeStart, AgeEnd, AgeSpan, AgeLabel, value) %>%
  arrange(LocID, year, sex, AgeStart)

save(ms2020age, file = file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2020age.rda"))
