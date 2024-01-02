# This script makes targeted updates to tmdl_reaches when something changes
# in the base GIS features.
#
# tmdl_reaches
#
# It should be opened within the Project Rstudio view so the working directory is set correctly.
# It pulls in the GIS features and tabular data and joins everything into one table.

# DON'T FORGET TO UPDATE:
# tmdl_au
# tmdl_au_gnis
# tmdl_parameters
# tmdl_wqstd

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

update_action_id <- "30358"
update_pattern <- "action_30358"

# Read paths
paths <- readxl::read_excel(path = "data_raw/project_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text'))

#- Import tables ---------------------------------------------------------------

# tmdl_reaches
tmdl_reaches <- readRDS(file = file.path(paths$package_path[1], "data_raw", "tmdl_reaches.RDS"))

load(file = file.path(paths$package_path[1], "data", "LU_pollutant.rda"))

# Load the table that has the LU from old to new AUS
df.aufixes <- read_csv(file.path(paths$tmdl_reaches_shp[1], "R/AU_Fixes.csv"))

shp_dir <- paths$tmdl_reaches_shp[1]

ws_fix <- sf::st_read(dsn = file.path(shp_dir,"Support_Features_shp"), layer = "OR_WS_171003010505_02_106440_in_171003021101") %>%
  sf::st_drop_geometry() %>%
  pull(GLOBALID) %>%
  unique()

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

ornhd <- odeqmloctools::ornhd %>%
  dplyr::select(GLOBALID, Permanent_Identifier, ReachCode,
                GNIS_Name, GNIS_ID,
                AU_ID, AU_Name, AU_Description, AU_GNIS_Name, AU_GNIS, LengthKM) %>%
  filter(!AU_ID == "99") %>%
  mutate(HUC_6 = substr(AU_ID, 7, 12),
         HUC_8 = substr(AU_ID, 7, 14),
         HUC_10 = substr(AU_ID, 7, 16),
         AU_GNIS_Name = case_when(grepl("_WS", AU_ID, fixed = TRUE) & is.na(AU_GNIS_Name) ~ GNIS_Name,
                                  !grepl("_WS", AU_ID, fixed = TRUE) ~ NA_character_,
                                  TRUE ~ AU_GNIS_Name),
         AU_GNIS = case_when(grepl("_WS", AU_ID, fixed = TRUE) & is.na(AU_GNIS) ~ paste0(AU_ID,";"),
                             !grepl("_WS", AU_ID, fixed = TRUE) ~ NA_character_,
                             TRUE ~ AU_GNIS)) %>%
  mutate(HUC_8 = case_when(GLOBALID %in% ws_fix ~ "17100302",
                           TRUE ~ HUC_8),
         HUC_10 = case_when(GLOBALID %in% ws_fix ~ "1710030211",
                            TRUE ~ HUC_10)) %>%
  left_join(huc6) %>%
  left_join(huc8) %>%
  left_join(huc10) %>%
  distinct()

oraus <- ornhd %>%
  dplyr::select(AU_ID, LengthKM) %>%
  dplyr::group_by(AU_ID) %>%
  dplyr::summarise(AU_length_km = sum(LengthKM, na.rm = TRUE)) %>%
  ungroup()

#- tmdl_reaches ----------------------------------------------------------------

files1 <- list.files(path = file.path(shp_dir, "01_Working_on"),
                     pattern = paste0("^",update_pattern, ".*\\.shp$"),
                     recursive = TRUE, full.names = TRUE)
files2 <- list.files(path = file.path(shp_dir, "02_DEQ_Under_Review"),
                     pattern = paste0("^",update_pattern, ".*\\.shp$"),
                     recursive = TRUE, full.names = TRUE)
files3 <- list.files(path = file.path(shp_dir, "03_DEQ_Final_Reviewed"),
                     pattern = paste0("^",update_pattern, ".*\\.shp$"),
                     recursive = TRUE, full.names = TRUE)
files4 <- list.files(path = file.path(shp_dir, "04_EPA_Under_Review"),
                     pattern = paste0("^",update_pattern, ".*\\.shp$"),
                     recursive = TRUE, full.names = TRUE)
files5 <- list.files(path = file.path(shp_dir, "05_EPA_Final_Reviewed"),
                     pattern = paste0("^",update_pattern, ".*\\.shp$"),
                     recursive = TRUE, full.names = TRUE)
files6 <- list.files(path = file.path(shp_dir, "06_Final_ATTAINS"),
                     pattern = paste0("^",update_pattern, ".*\\.shp$"),
                     recursive = TRUE, full.names = TRUE)

tmdl.shps <- c(files1, files2, files3, files4, files5, files6)

# exclude files in the Supporting folder
tmdl.shps <- tmdl.shps[ !grepl("Supporting", tmdl.shps) ]
rm(files1, files2, files3, files4, files5, files6)

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
    }  %>%
    {
      if ("geo_id" %in% names(.)) . else mutate(., geo_id = NA_character_)
    } %>%
    {
      if ("GLOBALID" %in% names(.)) . else mutate(., GLOBALID = NA_character_)
    } %>%
    select(action_id, TMDL_wq_limited_parameter = TMDL_param,
           TMDL_pollutant = TMDL_pollu, TMDL_scope, Period = period, Source,
           geo_id, GLOBALID)

  tmdl_reach_tbl <- rbind(tmdl_reach_tbl, tmdl_reach_tbl0)

  rm(tmdl_reach_tbl0)
}

# Note, remove TMDL active mutate once all the GIS layers have been updated w/
# consistent parameter names. Better approach is to use a join w/ mapping list. e.g.
# select(-TMDL_active) %>%
# left_join(by=c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant"))
tmdl_reaches_update <- tmdl_reach_tbl %>%
  select(GLOBALID, action_id, TMDL_wq_limited_parameter,
         TMDL_pollutant, TMDL_scope, Period, Source, geo_id) %>%
  #dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_active")],
  #                 by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::mutate(Source = case_when(grepl("Nonpoint", Source, ignore.case = TRUE) ~ "Nonpoint source",
                                   grepl("Point", Source, ignore.case = TRUE) ~ "Point source",
                                   grepl("Both", Source, ignore.case = TRUE) ~ "Both",
                                   TRUE ~ NA_character_)) %>%
  left_join(ornhd, by = "GLOBALID") %>%
  filter(!AU_ID == "99") %>%
  dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, ReachCode) %>%
  dplyr::distinct() %>%
  left_join(LU_pollutant[,c("Pollu_ID", "Pollutant_DEQ")],
            by = c("TMDL_wq_limited_parameter" = "Pollutant_DEQ")) %>%
  dplyr::select(action_id,
                TMDL_wq_limited_parameter,
                TMDL_pollutant,
                TMDL_scope,
                Period,
                Source,
                Pollu_ID,
                geo_id,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                GLOBALID,
                Permanent_Identifier,
                ReachCode,
                GNIS_Name, GNIS_ID,
                AU_ID, AU_Name, AU_Description,
                AU_GNIS_Name, AU_GNIS,
                LengthKM) %>%
  as.data.frame()

# Remove the old rows and update with new ones
# CAREFUL HERE, overwrites tmdl_reaches
tmdl_reaches <- tmdl_reaches %>%
  dplyr::filter(!(action_id %in% update_action_id)) %>%
  rbind(tmdl_reaches_update) %>%
  dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, ReachCode) %>%
  dplyr::distinct() %>%
  as.data.frame()

num_df <- 4

tmdl_reaches0 <- tmdl_reaches %>%
  group_by((row_number() - 1 ) %/% (n() / num_df)) %>%
  nest %>%
  pull(data)

tmdl_reaches1 <- tmdl_reaches0[[1]] %>% as.data.frame()
tmdl_reaches2 <- tmdl_reaches0[[2]] %>% as.data.frame()
tmdl_reaches3 <- tmdl_reaches0[[3]] %>% as.data.frame()
tmdl_reaches4 <- tmdl_reaches0[[4]] %>% as.data.frame()

# Save as a RDS file in inst/extdata folder (replaces existing)
# File is too large to save in data and as single file
saveRDS(tmdl_reaches, compress = TRUE, file = file.path(paths$package_path[1], "data_raw", "tmdl_reaches.RDS"))
saveRDS(tmdl_reaches1, compress = TRUE, file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches1.RDS"))
saveRDS(tmdl_reaches2, compress = TRUE, file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches2.RDS"))
saveRDS(tmdl_reaches3, compress = TRUE, file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches3.RDS"))
saveRDS(tmdl_reaches4, compress = TRUE, file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches4.RDS"))

