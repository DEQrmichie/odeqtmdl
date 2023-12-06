# This script builds the GIS features to be used in the TMDL mapping tools.

library(sf)
library(dplyr)
library(readxl)

# Read paths
paths <- readxl::read_excel(path = "data_raw/project_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text'))


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
tmdl_reaches <- readRDS(file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches.RDS"))

# unique list of GLOBALIDs where TMDLs or allocation apply
tmdl_gids <- dplyr::filter(tmdl_reaches, TMDL_scope %in% c("TMDL",
                                                           "Allocation only",
                                                           "Advisory Allocation")) %>%
  dplyr::pull(GLOBALID) %>%
  unique() %>%
  sort()

# Only where TMDLs apply cat5 -> cat 4a
tmdl_scope_gids <- dplyr::filter(tmdl_reaches, TMDL_scope == "TMDL") %>%
  dplyr::pull(GLOBALID) %>%
  unique() %>%
  sort()


tmdl_reach_fc <- nhd_fc %>%
  dplyr::filter(GLOBALID %in% tmdl_gids) %>%
  dplyr::filter(!AU_ID == "99") %>%
  dplyr::select(AU_ID, AU_Name, AU_Description, AU_WBType, GNIS_ID, GNIS_Name,
                HUC12,
                AU_GNIS, AU_GNIS_Name, GLOBALID, Permanent_Identifier,
                WBArea_Permanent_Identifier, FType) %>%
  sf::st_transform(crs = 4326)

# Dissolve to AUs
tmdl_au_fc <- tmdl_reach_fc %>%
  dplyr::filter(GLOBALID %in% tmdl_scope_gids) %>%
  dplyr::group_by(AU_ID, AU_Name, AU_Description, AU_WBType) %>%
  dplyr::summarize() %>%
  ungroup()

# Dissolve to AUs GNIS
tmdl_au_gnis_fc <- tmdl_reach_fc %>%
  dplyr::filter(GLOBALID %in% tmdl_scope_gids) %>%
  dplyr::group_by(AU_ID, AU_Name, AU_GNIS_Name, AU_GNIS, AU_WBType) %>%
  dplyr::summarize() %>%
  ungroup()

save(tmdl_reach_fc, file = file.path(paths$package_path[1], "data_raw", "tmdl_reach_fc.rda"))
save(tmdl_au_fc, file = file.path(paths$package_path[1], "data_raw", "tmdl_au_fc.rda"))
save(tmdl_au_gnis_fc, file = file.path(paths$package_path[1], "data_raw", "tmdl_au_gnis_fc.rda"))
