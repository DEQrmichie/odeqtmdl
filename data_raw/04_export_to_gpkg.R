library(readxl)
library(dplyr)
library(sf)

# Read paths
paths <- readxl::read_excel(path = "data_raw/geoid_gis_path.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text', 'text', 'text', 'text'))

out_TMDL_param <- c("E. coli", "Fecal coliform", "Fecal Coliform")
out_gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "bacteria_tmdls.gpkg")
out_gpkg_layer <- "bacteria_tmdls_2023_08"

load(file.path(paths$package_path[1], "data_raw", "tmdl_reaches.rda"))
load(file.path(paths$package_path[1], "data_raw", "tmdl_reach_shp.rda"))

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = out_gpkg_dsn, gpkg_layer = out_gpkg_layer,
                           nhd_fc = tmdl_reach_shp, tmdl_reachs = tmdl_reaches,
                           TMDL_param = out_TMDL_param)
