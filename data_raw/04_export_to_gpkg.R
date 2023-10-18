library(readxl)
library(dplyr)
library(sf)

# Read paths
paths <- readxl::read_excel(path = "data_raw/geoid_gis_path.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text', 'text', 'text', 'text'))

# nhd_fc
load(file.path(paths$package_path[1], "data_raw", "nhd_fc.rda"))

# tmdl_au_fc
load(file = file.path(paths$package_path[1], "data_raw", "tmdl_au_fc.rda"))

# tmdl_reaches
load(file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches.rda"))

# tmdl_au
load(file.path(paths$package_path[1], "data", "tmdl_au.rda"))

#- Everything ------------------------------------------------------------------
TMDL_param <- NULL
TMDL_pollu <- NULL
gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
gpkg_layer <- "OR_TMDLs_all_2023_10"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           TMDL_param = TMDL_param, TMDL_pollu = TMDL_pollu)

#- TMDLs by reach --------------------------------------------------------------

gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")

df <- tmdl_reaches %>%
  dplyr::filter(TMDL_active & !AU_ID == "99") %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name", "citation_abbreviated")], by = "action_id") %>%
  dplyr::mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID),
                TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  group_by(PIDAUID, HUC_6, HU_6_NAME, HUC6_full, HUC_8, HU_8_NAME, HUC8_full) %>%
  summarize(action_ids = paste((unique(action_id)), collapse = "; "),
            TMDL_names = paste((unique(TMDL_name)), collapse = "; "),
            TMDL_wq_limited_parameters = paste(sort(unique(TMDL_wq_limited_parameter)), collapse = "; "),
            TMDL_pollutants = paste(sort(unique(TMDL_pollutant)), collapse = "; "))


tmdls_by_reach <- nhd_fc %>%
  dplyr::select(AU_ID, Permanent_Identifier,
                WBArea_Permanent_Identifier,
                FType,
                GNIS_Name,
                GNIS_ID,
                AU_ID,
                AU_Name,
                AU_Description,
                AU_WBType,
                AU_GNIS_Name,
                AU_GNIS,
                LengthKM) %>%
  dplyr::mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
  dplyr::inner_join(y = df, by = "PIDAUID") %>%
  dplyr::select(action_ids,
                TMDL_names,
                TMDL_wq_limited_parameters,
                TMDL_pollutants,
                HUC_6,
                HU_6_NAME,
                HUC6_full,
                HUC_8,
                HU_8_NAME,
                HUC8_full,
                Permanent_Identifier,
                WBArea_Permanent_Identifier,
                FType,
                GNIS_Name,
                GNIS_ID,
                AU_ID,
                AU_Name,
                AU_Description,
                AU_WBType,
                AU_GNIS_Name,
                AU_GNIS,
                LengthKM)

sf::st_write(tmdls_by_reach,
             dsn = gpkg_dsn,
             layer = "TMDLs_by_reach",
             driver = "GPKG",
             delete_layer = TRUE) %>%



#- TMDLs by AU -----------------------------------------------------------------

gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")

df2 <- tmdl_aus %>%
  dplyr::filter(TMDL_active & !AU_ID == "99") %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name", "citation_abbreviated")], by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  group_by(AU_ID, HUC_6, HU_6_NAME, HUC6_full, HUC_8, HU_8_NAME, HUC8_full) %>%
  summarize(action_ids = paste((unique(action_id)), collapse = "; "),
            TMDL_names = paste((unique(TMDL_name)), collapse = "; "),
            TMDL_wq_limited_parameters = paste(sort(unique(TMDL_wq_limited_parameter)), collapse = "; "),
            TMDL_pollutants = paste(sort(unique(TMDL_pollutant)), collapse = "; "))

tmdls_by_au <- tmdl_au_fc %>%
  dplyr::inner_join(y = df2, by = "AU_ID") %>%
  dplyr::select(action_ids,
                TMDL_names,
                TMDL_wq_limited_parameters,
                TMDL_pollutants,
                HUC_6,
                HU_6_NAME,
                HUC6_full,
                HUC_8,
                HU_8_NAME,
                HUC8_full,
                AU_ID,
                AU_Name,
                AU_Description,
                AU_WBType,
                Shape)

sf::st_write(tmdls_by_au,
             dsn = gpkg_dsn,
             layer = "TMDLs_by_AU",
             driver = "GPKG",
             delete_layer = TRUE)


#- Bacteria --------------------------------------------------------------------
TMDL_param <- c("E. coli", "Fecal Coliform", "Enterococci")
TMDL_pollu <- NULL
gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
gpkg_layer <- "OR_bacteria_TMDLs"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           TMDL_param = TMDL_param, TMDL_pollu = TMDL_pollu)


#- Temperature -----------------------------------------------------------------
TMDL_param <- c("Temperature")
TMDL_pollu <- NULL
gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
gpkg_layer <- "OR_temperature_TMDLs"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           TMDL_param = TMDL_param, TMDL_pollu = TMDL_pollu)

#- Sediment--- -----------------------------------------------------------------
TMDL_param <- c("Sedimentation", "Turbidity")
TMDL_pollu <- c("Sedimentation", "Total suspended solids", "Fine Sediment")

z <- filter(tmdl_reaches, action_id == "2022")

gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "ODF_Sediment_TMDLs","OR_Sediment_TMDLs", "OR_Sediment_TMDLs.gpkg")
gpkg_layer <- "OR_sediment_TMDLs"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           TMDL_param = TMDL_param, TMDL_pollu = TMDL_pollu)

#- Sediment Surrogates--- ------------------------------------------------------
TMDL_param <- c("DDD 4,4'", "DDE 4,4'", "DDT 4,4'", "Dieldrin", "Iron", "Mercury (total)")
TMDL_pollu <- NULL
action_ids <- c("OR_TMDL_20191122", "30674", "35888")


gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "ODF_Sediment_TMDLs","OR_Sediment_TMDLs", "OR_Sediment_TMDLs.gpkg")
gpkg_layer <- "OR_sediment_surrogate_TMDLs_2023_09"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           action_ids = action_ids, TMDL_param = TMDL_param, TMDL_pollu = TMDL_pollu)
