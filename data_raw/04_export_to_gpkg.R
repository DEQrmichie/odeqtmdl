library(readxl)
library(dplyr)
library(sf)

# Read paths
paths <- readxl::read_excel(path = "data_raw/geoid_gis_path.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text', 'text', 'text', 'text'))



gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")

# nhd_fc
load(file.path(paths$package_path[1], "data_raw", "nhd_fc.rda"))

# tmdl_reaches
load(file.path(paths$package_path[1], "data_raw", "tmdl_reaches.rda"))

# tmdl_aus
load(file.path(paths$package_path[1], "data", "tmdl_aus.rda"))

# tmdl_au_fc
load(file = file.path(paths$package_path[1], "data_raw", "tmdl_au_fc.rda"))

#- Everything ------------------------------------------------------------------
TMDL_param <- NULL
gpkg_layer <- "OR_TMDLs_all_2023_08"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           TMDL_param = TMDL_param)

# save AU as a table
sf::st_write(tmdl_aus,
             dsn = gpkg_dsn,
             layer = "TMDL_by_AU",
             driver = "GPKG",
             delete_layer = TRUE,
             layer_options = "ASPATIAL_VARIANT=GPKG_ATTRIBUTES")



#- Bacteria --------------------------------------------------------------------
TMDL_param <- c("E. coli", "Fecal Coliform", "Enterococci")
gpkg_layer <- "OR_bacteria_TMDLs_2023_08"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           TMDL_param = TMDL_param)

#- Temperature -----------------------------------------------------------------
TMDL_param <- c("Temperature")
gpkg_layer <- "OR_temperature_TMDLs_2023_08"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           TMDL_param = TMDL_param)
