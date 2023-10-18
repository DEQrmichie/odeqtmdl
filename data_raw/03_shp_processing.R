# This script builds the GIS features to be used in the TMDL mapping tools.

library(sf)
library(dplyr)
library(readxl)

# Read paths
paths <- readxl::read_excel(path = "data_raw/geoid_gis_path.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text', 'text', 'text', 'text'))


# Import NHD flowline feature ----------------------------------------------------------

# nhd_dsn <- file.path(paths$tmdl_reaches_shp[1],"Support_Features.gdb")
# nhd_layer <- "OR_NHD_Flowlines"
# nhd_fc <- sf::st_read(dsn = nhd_dsn, layer = nhd_layer, stringsAsFactors =  FALSE) %>%
#   sf::st_zm() %>%
#   dplyr::select(-action_id, -TMDL_param, -TMDL_pollu, -Source, -period, -TMDL_scope)
#
# save(nhd_fc, file = "data_raw/nhd_fc.rda")

# nhd_fc
load(file.path(paths$package_path[1], "data_raw", "nhd_fc.rda"))

# Import Statewide dissolved AU feature ----------------------------------------------------------

# au_dsn <- file.path(paths$tmdl_reaches_shp[1],"Support_Features.gdb")
# au_layer <- "AU_OR_Dissolve"
# au_fc <- sf::st_read(dsn = au_dsn, layer = au_layer, stringsAsFactors =  FALSE) %>%
#   sf::st_zm() %>%
#   dplyr::select(-AU_Type) %>%
#   dplyr::filter(!AU_ID == "99")
#
# save(au_fc, file = "data_raw/au_fc.rda")

#load(file.path(paths$package_path[1], "data_raw", "au_fc.rda"))

# - Import TMDL mapping data at reach scale ------------------------------------

# tmdl_reaches
load(file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches.rda"))

# tmdl_aus
load(file = file.path(paths$package_path[1], "data", "tmdl_aus.rda"))

# unique list of NHD permanent Identifiers where TMDLs or allocation apply
tmdl_pids <- dplyr::filter(tmdl_reaches, TMDL_scope %in% c("TMDL",
                                                           "Allocation only",
                                                           "Advisory Allocation")) %>%
  mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
  dplyr::pull(PIDAUID) %>%
  unique() %>%
  sort()

# Only where TMDLs apply cat5 -> cat 4a
tmdl_scope_pids <- dplyr::filter(tmdl_reaches, TMDL_scope == "TMDL") %>%
  mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
                        dplyr::pull(PIDAUID) %>%
                        unique() %>%
                        sort()


tmdl_reach_fc <- nhd_fc %>%
  mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
  dplyr::filter(PIDAUID %in% tmdl_pids) %>%
  dplyr::filter(!AU_ID == "99") %>%
  dplyr::select(AU_ID, AU_Name, AU_Description, AU_WBType, GNIS_ID, GNIS_Name,
                HUC12,
                AU_GNIS, AU_GNIS_Name, Permanent_Identifier,
                WBArea_Permanent_Identifier, FType) %>%
  sf::st_transform(crs = 4326)

# Dissolve to AUs
tmdl_au_fc <- tmdl_reach_fc %>%
  mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
  dplyr::filter(PIDAUID %in% tmdl_scope_pids) %>%
  dplyr::group_by(AU_ID, AU_Name, AU_Description, AU_WBType) %>%
  dplyr::summarize() %>%
  ungroup()

# create unique AU fc
# tmdl_au_fc_full <- tmdl_au_fc %>%
#   dplyr::select(AU_ID, AU_WBType) %>%
#   dplyr::inner_join(y = tmdl_aus, by = "AU_ID") %>%
#   dplyr::left_join(odeqtmdl::tmdl_actions, by = "action_id") %>%
#   dplyr::select(action_id,
#                 TMDL_name,
#                 TMDL_issue_year,
#                 TMDL_wq_limited_parameter,
#                 TMDL_pollutant,
#                 TMDL_active,
#                 TMDL_scope,
#                 Period,
#                 citation_abbreviated,
#                 citation_full,
#                 HUC_6,
#                 HU_6_NAME,
#                 HUC6_full,
#                 HUC_8,
#                 HU_8_NAME,
#                 HUC8_full,
#                 HUC_10,
#                 HU_10_NAME,
#                 HUC10_full,
#                 AU_ID,
#                 AU_Name,
#                 AU_Description,
#                 AU_WBType,
#                 TMDL_length_km,
#                 AU_length_km,
#                 TMDL_AU_Percent)
#
# sf::st_write(tmdl_au_fc_full,
#              dsn = file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg"),
#              layer = "TMDLs_by_AU",
#              driver = "GPKG",
#              delete_layer = TRUE)

save(tmdl_reach_fc, file = file.path(paths$package_path[1], "data_raw", "tmdl_reach_fc.rda"))
save(tmdl_au_fc, file = file.path(paths$package_path[1], "data_raw", "tmdl_au_fc.rda"))
