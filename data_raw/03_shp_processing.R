# This script builds the GIS features to be used in the TMDL mapping tool.


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


load(file.path(paths$package_path[1], "data_raw", "nhd_fc.rda"))

# - Import TMDL mapping data at reach scale ------------------------------------

load(file = file.path(paths$package_path[1],"data_raw/tmdl_reaches.rda"))

# unique list of NHD permanent Identifiers where TMDL or allocation apply
tmdl_pids <- dplyr::filter(tmdl_reaches, TMDL_scope %in% c("TMDL",
                                                           "Allocation only",
                                                           "Advisory Allocation")) %>%
  dplyr::pull(Permanent_Identifier) %>%
  unique() %>%
  sort()

# Only where TMDLs apply cat5 -> cat 4a
tmdl_scope_pids <- dplyr::filter(tmdl_reaches, TMDL_scope == "TMDL") %>%
                        dplyr::pull(Permanent_Identifier) %>%
                        unique() %>%
                        sort()


tmdl_reach_shp <- nhd_fc %>%
  dplyr::select(AU_ID, AU_Name, AU_Description, AU_WBType, GNIS_ID, GNIS_Name,
                HUC12,
                AU_GNIS, AU_GNIS_Name, Permanent_Identifier,
                WBArea_Permanent_Identifier, FType) %>%
  dplyr::filter(Permanent_Identifier %in% tmdl_pids) %>%
  dplyr::filter(!AU_ID == "99") %>%
  sf::st_transform(crs = 4326)

# Dissolve to AUs
tmdl_au_shp <- tmdl_reach_shp %>%
  dplyr::filter(Permanent_Identifier %in% tmdl_scope_pids) %>%
  dplyr::group_by(AU_ID, AU_Name, AU_Description, AU_WBType) %>%
  dplyr::summarize() %>%
  ungroup()

save(tmdl_reach_shp, file = file.path(paths$package_path[1], "data_raw", "tmdl_reach_shp.rda"))
save(tmdl_au_shp, file = file.path(paths$package_path[1], "data_raw", "tmdl_au_shp.rda"))
