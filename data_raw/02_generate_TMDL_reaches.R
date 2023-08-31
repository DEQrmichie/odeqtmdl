# This script generates the TMDL reach and AU databases as an R data file.
# It should be opened within the Project Rstudio view so the working directory is set correctly.
# It pulls in the GIS features and tabular data and joins everything into one table.

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

# Read paths
paths <- readxl::read_excel(path = "data_raw/geoid_gis_path.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text', 'text', 'text', 'text'))

# Read TMDL actions table
tmdl_actions_tbl <- readxl::read_excel(path = "data_raw/TMDL_db_tabular.xlsx",
                                       sheet = "tmdl_actions_table" ,
                                       na = c("", "NA"),
                                       col_names = TRUE,
                                       col_types = c('text', 'text', 'numeric', 'text', 'logical', 'text',
                                                     'logical', 'text', 'date', 'date', 'text', 'text', 'text', 'text'))

tmdl_active_tbl <- readxl::read_excel(path = file.path(paths$tmdl_reaches_shp[1], "Mapping_List.xlsx"),
                                      sheet = "tmdl_extent_table",
                                      na = c("", "NA"),
                                      col_names = TRUE,
                                      col_types = c("text", "numeric", "text", "text", "text", "text",
                                                    "text", "text", "text", "text", "text",
                                                    "text", "text", "text", "text", "text", "text", "text", "text")) %>%
  select(action_id, TMDL_wq_limited_parameter = action_wq_limited_parameter,
         TMDL_pollutant = action_TMDL_pollutant, TMDL_active)

# Load the table that has the LU from old to new AUS
df.aufixes <- read_csv(file.path(paths$tmdl_reaches_shp[1], "R/AU_Fixes.csv"))

shp_dir <- paths$tmdl_reaches_shp[1]

huc6 <- sf::st_read(dsn = file.path(shp_dir,"Support_Features.gdb"), layer = "WBDHU6") %>%
  sf::st_drop_geometry() %>%
  select(HUC_6 = HUC6, HU_6_NAME = Name) %>%
  mutate(HUC6_full = paste0(HUC_6," ", HU_6_NAME))

huc8 <-  sf::st_read(dsn = file.path(shp_dir,"Support_Features.gdb"), layer = "WBDHU8") %>%
  sf::st_drop_geometry() %>%
  select(HUC_8 = HUC8, HU_8_NAME = Name) %>%
  mutate(HUC8_full = paste0(HUC_8," ", HU_8_NAME))

huc10 <-  sf::st_read(dsn = file.path(shp_dir,"Support_Features.gdb"), layer = "WBDHU10") %>%
  sf::st_drop_geometry() %>%
  select(HUC_10 = HUC10, HU_10_NAME = Name) %>%
  mutate(HUC10_full = paste0(HUC_10," ", HU_10_NAME))

files1 <- list.files(path = file.path(shp_dir, "01_Working_on"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files2 <- list.files(path = file.path(shp_dir, "02_DEQ_Under_Review"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files3 <- list.files(path = file.path(shp_dir, "03_DEQ_Final_Reviewed"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files4 <- list.files(path = file.path(shp_dir, "04_EPA_Under_Review"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files5 <- list.files(path = file.path(shp_dir, "05_EPA_Final_Reviewed"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files6 <- list.files(path = file.path(shp_dir, "06_Final_ATTAINS"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

tmdl.shps <- c(files1, files2, files3, files4, files5, files6)
rm(files1, files2, files3, files4, files5, files6)

# exclude files in the Supporting folder
tmdl.shps <- tmdl.shps[ !grepl("Supporting", tmdl.shps) ]

# Today's date
db_version <- paste0("v", gsub(pattern = "-", replacement = "", x = Sys.Date()))

# Load all the shps into a dataframe'
tmdl_reach_tbl <- data.frame()

for (i in 1:length(tmdl.shps)) {

  tmdl_dsn = dirname(tmdl.shps[i])
  tmdl_layer = sub("\\.shp$", "", basename(tmdl.shps[i]))

  tmdl_reach_tbl0 <- sf::st_read(dsn = tmdl_dsn,
                                 layer = tmdl_layer,
                                 stringsAsFactors = FALSE) %>%
    #sf::st_transform(crs = 2992) %>%
    #dplyr::mutate(length_feet = sf::st_length(.)) %>%
    sf::st_drop_geometry() %>%
    {
      if ("TMDL_scope" %in% names(.)) . else mutate(., TMDL_scope = NA_character_)
      } %>%
    {
      if ("period" %in% names(.)) . else mutate(., period = NA_character_)
      } %>%
    {
      if ("Source" %in% names(.)) . else mutate(., Source = NA_character_)
      } %>%
    select(action_id, action_wq_limited_parameter = TMDL_param,
           action_TMDL_pollutant = TMDL_pollu, TMDL_scope, Period = period, Source,
           Permanent_Identifier = Permanent_, AU_ID)

  tmdl_reach_tbl <- rbind(tmdl_reach_tbl, tmdl_reach_tbl0)

  rm(tmdl_reach_tbl0)
}

ornhd <- odeqmloctools::ornhd %>%
  dplyr::select(Permanent_Identifier, GNIS_Name, GNIS_ID, AU_ID, AU_Name, AU_Description, AU_GNIS_Name, AU_GNIS, LengthKM) %>%
  filter(!AU_ID == "99") %>%
  mutate(HUC_6 = substr(AU_ID, 7, 12),
         HUC_8 = substr(AU_ID, 7, 14),
         HUC_10 = substr(AU_ID, 7, 16),
         PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
  left_join(huc6) %>%
  left_join(huc8) %>%
  left_join(huc10)

oraus <- ornhd %>%
  dplyr::select(AU_ID, LengthKM) %>%
  dplyr::group_by(AU_ID) %>%
  dplyr::summarise(AU_length_km = sum(LengthKM, na.rm = TRUE)) %>%
  ungroup()

# Note, remove TMDL active mutate once all the GIS layers have been updated w/
# consistent parameter names. Better approach is to use a join w/ mapping list. e.g.
# select(-TMDL_active) %>%
# left_join(by=c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant"))
tmdl_reaches <- tmdl_reach_tbl %>%
  dplyr::mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
  select(PIDAUID, action_id, TMDL_wq_limited_parameter = action_wq_limited_parameter,
         TMDL_pollutant = action_TMDL_pollutant, TMDL_scope, Period) %>%
  dplyr::left_join(odeqtmdl::tmdl_actions, by = "action_id") %>%
  dplyr::mutate(TMDL_active = case_when(action_id %in% c("2043", "1230", "2021",
                                                         "10007", "42375",
                                                         "OR_TMDL_20171219",
                                                         "OR_TMDL_20191122") ~ FALSE,
                                        action_id == "1936" & TMDL_pollutant %in% c("Total Phosphorus",
                                                                                     "Ammonia Nitrogen (NH3-N)") ~ FALSE,
                                        action_id == "30674" & TMDL_pollutant %in% c("Mercury (total)",
                                                                                     "Methylmercury") ~ FALSE,
                                        TRUE ~ TRUE
                                        )) %>%
  left_join(ornhd, by = "PIDAUID") %>%
  filter(!AU_ID == "99") %>%
  dplyr::arrange(TMDL_issue_year, TMDL_name, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID) %>%
  dplyr::distinct() %>%
  dplyr::select(action_id, TMDL_name, TMDL_issue_year,
                TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_active,
                TMDL_scope, Period, in_attains, attains_status,
                citation_abbreviated, citation_full,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                Permanent_Identifier,
                GNIS_Name, GNIS_ID,
                AU_ID, AU_Name, AU_Description,
                AU_GNIS_Name, AU_GNIS,
                LengthKM)

tmdl_aus <- tmdl_reaches %>%
  dplyr::filter(TMDL_scope == "TMDL") %>%
  dplyr::select(action_id, TMDL_name, TMDL_issue_year,
                TMDL_wq_limited_parameter, TMDL_pollutant, Period, TMDL_active,
                TMDL_scope,
                citation_abbreviated, citation_full,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                AU_ID, AU_Name, AU_Description,
                LengthKM) %>%
  dplyr::group_by(action_id, TMDL_name, TMDL_issue_year,
                  TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_active, Period,
                  TMDL_scope,
                  citation_abbreviated, citation_full,
                  HUC_6, HU_6_NAME, HUC6_full,
                  HUC_8, HU_8_NAME, HUC8_full,
                  HUC_10, HU_10_NAME, HUC10_full,
                  AU_ID, AU_Name, AU_Description) %>%
  dplyr::summarise(TMDL_length_km = sum(LengthKM, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(oraus, by = "AU_ID") %>%
  dplyr::mutate(TMDL_AU_Percent = round(TMDL_length_km/AU_length_km * 100,0))

# Save a copy in data folder (replaces existing)
save(tmdl_reaches, file = file.path(paths$package_path[1], "data_raw", "tmdl_reaches.rda"))
save(tmdl_aus, file = file.path(paths$package_path[1], "data", "tmdl_aus.rda"))

# Save an archive w/ version #
save(tmdl_reaches, file = file.path(paths$tmdl_reaches_shp[1], paste0("R/tmdl_reaches_", db_version,".rda")))
save(tmdl_aus, file = file.path(paths$tmdl_reaches_shp[1], paste0("R/tmdl_aus_", db_version,".rda")))

