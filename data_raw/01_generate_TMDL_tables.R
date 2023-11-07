# This script generates the following data tables

# LU_wqst
# LU_wqst_code
# LU_pollutant
# tmdl_actions
# tmdl_targets
# geo_id_table
#
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
                                       col_types = c("text", "text", 'numeric', "text", "text",
                                                     "logical", "text", "date", "date", "text",
                                                     "text", "text", "text", "text"))

# This will be used when all mapping is complete.
# tmdl_active_tbl <- readxl::read_excel(path = file.path(paths$tmdl_reaches_shp[1], "Mapping_List.xlsx"),
#                                       sheet = "tmdl_mapping_list",
#                                       na = c("", "NA"),
#                                       col_names = TRUE,
#                                       col_types = c("text", "numeric", "text", "text", "text", "text",
#                                                     "text", "text", "text", "text", "text", "text", "text",
#                                                     "text", "text", "text", "text", "text", "text", "text", "text")) %>%
#   select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_status) %>%
#   distinct()

#- LU_wqst ----------------------------------------------------------------

LU_wqstd <- readxl::read_excel(path = file.path(paths$package_path[1], "data_raw", "LU_wqstd_info.xlsx"),
                               sheet = "Final", col_names = TRUE,
                               col_types = c("text", "numeric", "numeric",
                                             "text")) %>%
  select(Pollu_ID, wqstd_code) %>%
  distinct() %>%
  arrange(Pollu_ID, wqstd_code) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(LU_wqstd, file = file.path(paths$package_path[1], "data", "LU_wqstd.rda"))

#- LU_wqst_code ----------------------------------------------------------------

LU_wqstd_code <- readxl::read_excel(path = file.path(paths$package_path[1], "data_raw", "LU_wqstd_code.xlsx"),
                                    sheet = "sheet1", col_names = TRUE,
                                    col_types = c("numeric", "text")) %>%
  select(wqstd_code, wqstd) %>%
  distinct() %>%
  arrange(wqstd_code, wqstd) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(LU_wqstd_code, file = file.path(paths$package_path[1], "data", "LU_wqstd_code.rda"))

#- LU_pollutant ----------------------------------------------------------------

LU_pollutant <- readxl::read_excel(path = file.path(paths$tmdl_reaches_shp[1], "Pollutant_Names", "LU_Pollu_ID.xlsx"),
                                   sheet = "Final", col_names = TRUE,
                                   col_types = c('numeric', 'text', 'text',
                                                 'text', 'numeric', 'text',
                                                 'text', "logical")) %>%
  select(-TMDL_program) %>%
  arrange(Pollutant_DEQ) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(LU_pollutant, file = file.path(paths$package_path[1], "data", "LU_pollutant.rda"))

#- tmdl_actions ----------------------------------------------------------------

tmdl_actions <- tmdl_actions_tbl %>%
  dplyr::select(action_id,
                TMDL_name,
                TMDL_issue_year,
                issue_agency,
                in_attains,
                attains_status,
                TMDL_issue_date,
                EPA_action_date,
                citation_abbreviated,
                citation_full,
                TMDL_comment,
                URL) %>%
  dplyr::distinct() %>%
  dplyr::mutate(TMDL_issue_date = as.Date(TMDL_issue_date),
                EPA_action_date = as.Date(EPA_action_date)) %>%
  dplyr::arrange(TMDL_issue_year,
                 TMDL_name) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_actions, file = file.path(paths$package_path[1], "data", "tmdl_actions.rda"))

#- tmdl_geo_ids ----------------------------------------------------------------

tmdl_geo_ids <- readxl::read_excel(path = "data_raw/TMDL_db_tabular.xlsx",
                                   sheet = "geo_id_table" ,col_names = TRUE,
                                   col_types = c('text', 'text', 'logical', 'text', 'numeric',
                                                 'text')) %>%
  dplyr::select(action_id, geo_id, geo_description, geo_id_mapped) %>%
  arrange(action_id, geo_id) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_geo_ids, file = file.path(paths$package_path[1], "data", "tmdl_geo_ids.rda"))

#- tmdl_targets ----------------------------------------------------------------

tmdl_targets <- readxl::read_excel(path = "data_raw/TMDL_db_tabular.xlsx",
                                   sheet = "target_table", col_names = TRUE,
                                   col_types = c("text", "text", "numeric", "text", "text",
                                                 "text", "text", "text", "numeric", "text",
                                                 "numeric", "text", "text", "date", "date",
                                                 "text", "text", "text", "text")) %>%
  dplyr::mutate(season_start = format(season_start, "%b %d"),
                season_end = format(season_end, "%b %d")) %>%
  dplyr::select(action_id,
                geo_id,
                TMDL_pollutant,
                field_parameter,
                target_type,
                target_value,
                target_units,
                Unit_UID,
                target_time_base,
                target_stat_base,
                season_start,
                season_end,
                target_conditionals,
                TMDL_element,
                target_reference,
                target_comments) %>%
  arrange(geo_id) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_targets, file = file.path(paths$package_path[1], "data", "tmdl_targets.rda"))
