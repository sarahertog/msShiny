

# Load the various datasets needed for International Migrant Stock estimation

# MS 2024 - migrant stock estimates (total, by sex, and by origin) from the 2024 revision
load(file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2024.rda")) # totals by sex
load(file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2024org.rda")) # by destination and origin

# MS 2020 - migrant stock estimates (total, by sex, by origin and by age) from the 2020 revision
load(file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2020.rda")) # totals by sex
load(file.path(ms_dir, "GlobalFiles", "ms_previous", "ms2020org.rda")) # by destination and origin

# # UNHCR refugees and asylum seekers
# load(file.path(ms_dir, "GlobalFiles", "unhcr", "rfg_origin_dest_1Jan.rda"))
# load(file.path(ms_dir, "GlobalFiles", "unhcr", "rfg_origin_dest_1Jul.rda"))
# load(file.path(ms_dir, "GlobalFiles", "unhcr", "rfg_propf_dest_1Jan.rda"))
# load(file.path(ms_dir, "GlobalFiles", "unhcr", "rfg_propf_dest_1Jul.rda"))
# load(file.path(ms_dir, "GlobalFiles", "unhcr", "rfg_propf_origin.rda"))
# 
# # load wpp total 1 january population by sex
# load(file.path(ms_dir, "GlobalFiles", "wpp", "wpp2024.rda"))

# Locations (LocID and LocName and ISO3)
load(file.path(ms_dir, "GlobalFiles","MS_Locations.rda"))
# census reference dates for all countries extracted from the DataCatalog
load(file.path(ms_dir, "GlobalFiles","census_ref_dates.rda"))

# # mapping of SubGroupIds to location ids
# subgroup_to_locid <- read.csv(file = file.path(ms_dir, "GlobalFiles", "SubGroupID_to_LocID_lookup.csv"), stringsAsFactors = FALSE)

