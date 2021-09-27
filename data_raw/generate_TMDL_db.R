# This script generates the "TMDL database" as an R data file.
# It pulls in the GIS features and tabular data and joins everything into one table.
# The script also reads and saves the TMDL actions table.

library(readxl)
library(dplyr)
library(sf)
library(writexl)

#devtools::install_github("donco/odeqtmdl", host = "https://api.github.com", upgrade = FALSE)

# This shapefile is located on the TMDL server
tmdl_db_shp <- "tmdl_db_reaches"

reaches_tbl <- sf::st_read(dsn ="T:/Planning statewide/TMDL_DB/GIS",
                           layer=tmdl_db_shp,
                           stringsAsFactors=FALSE) %>%
  sf::st_drop_geometry() %>%
  select(geo_id, AU_ID, ReachCode, edit_date)

tmdl_actions_tbl <- readxl::read_excel(path="data_raw/TMDL_db_tabular.xlsx",
                                       sheet = "tmdl_actions_table" , col_names = TRUE,
                                       col_types = c('text', 'text', 'numeric', 'text', 'logical', 'text',
                                                     'logical', 'text', 'date', 'date', 'text', 'text'))

geoid_tbl <- readxl::read_excel(path="data_raw/TMDL_db_tabular.xlsx",
                                sheet = "geo_id_table" ,col_names = TRUE,
                                col_types = c('text', 'text', 'logical', 'text', 'numeric',
                                              'text')) %>%
  dplyr::select(-TMDL_issue_year, -TMDL_name)

pollutant_tbl <- readxl::read_excel(path="data_raw/TMDL_db_tabular.xlsx",
                                    sheet = "pollutant_table" ,col_names = TRUE,
                                    col_types = c('text', 'text', 'numeric', 'text', 'text',
                                                  'text', 'text', 'text', 'numeric', 'text',
                                                  'text', 'date', 'date', 'text', 'text',
                                                  'text')) %>%
  dplyr::mutate(season_start=format(season_start, "%d-%b"),
                season_end=format(season_end, "%d-%b")) %>%
  dplyr::select(-TMDL_issue_year, -TMDL_name)

db_version <- read_excel(path="data_raw/TMDL_db_tabular.xlsx",
                         sheet = "db_version" ,col_names = TRUE,
                         col_types = c('text', 'date', 'text')) %>%
  dplyr::filter(db_edit_date==max(db_edit_date)) %>%
  dplyr::pull(db_version)

tmdl_db <- geoid_tbl %>%
  dplyr::full_join(pollutant_tbl, by=c("geo_id", "action_id")) %>%
  dplyr::full_join(reaches_tbl, by=c("geo_id")) %>%
  dplyr::left_join(tmdl_actions_tbl, by="action_id") %>%
  dplyr::mutate(db_version=db_version) %>%
  dplyr::select(geo_id, geo_description, mapped,
                pollutant_name_TMDL, pollutant_name_AWQMS, wq_limited_parameters, target_type,
                target_value, target_units, target_stat_base, season_start,
                season_end, target_conditionals_references, TMDL_element, notes,
                action_id, TMDL_name, TMDL_issue_year,
                TMDL_active, issue_agency, in_attains, attains_status, TMDL_issue_date,
                EPA_action_date, AU_ID, ReachCode, edit_date, db_version)

tmdl_actions <- tmdl_actions_tbl

# Save a copy in data folder (replaces existing)
setwd("data/")
save(tmdl_db, file="tmdl_db.rda")
save(tmdl_actions, file="tmdl_actions.rda")

# Save a copy on the server w/ version #
setwd("T:/Planning statewide/TMDL_DB/R")
save(tmdl_db, file=paste0("tmdl_db_", db_version,".rda"))
save(tmdl_actions, file=paste0("tmdl_actions_", db_version,".rda"))

