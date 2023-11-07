library(readxl)
library(dplyr)
library(sf)
library(tidyr)

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
tmdl_reaches <- readRDS(file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches.RDS"))

# tmdl_au
#load(file.path(paths$package_path[1], "data", "tmdl_au.rda"))

#- Parameter Name Lookup -------------------------------------------------------

sort(unique(tmdl_reaches$TMDL_wq_limited_parameter))

param_names <- c(
  "E. coli" = "Ecoli",
  "Fecal Coliform" = "Fecal_Coliform",
  "Enterococci" = "Enterococci",
  "BioCriteria" = "BioCriteria",
  "Dissolved Oxygen" = "Dissolved_Oxygen",
  "Chlorophyll-a" = "Chlorophyll_a",
  "pH" = "pH",
  "Temperature" = "Temperature",
  "Total Dissolved gas" = "TDG",
  "DDD 4,4'" = "DDD",
  "DDE 4,4'" = "DDE",
  "DDT 4,4'" = "DDT",
  "Dieldrin" = "Dieldrin",
  "Chlordane" = "Chlordane",
  "Ammonia" = "Ammonia",
  "Mercury (total)" = "Mercury",
  "Methylmercury" = "Methylmercury",
  "Dioxin (2,3,7,8-TCDD)" = "Dioxin",
  "Lead" = "Lead",
  "Polychlorinated Biphenyls (PCBs)" = "PCBs",
  "Iron (total)" = "Iron",
  "Nitrates" = "Nitrates",
  "Sedimentation" = "Sedimentation",
  "Turbidity" = "Turbidity",
  "Excess Algal Growth" = "Excess_Algal_Growth",
  "Aquatic Weeds" = "Aquatic_Weeds",
  "Harmful Algal Blooms" = "HABs",
  "Total Phosphorus" = "Total_Phosphorus"
)


#- Write Tables ----------------------------------------------------------------
gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
gpkg_layer <- "TMDLs_by_AU"

sf::st_write(odeqtmdl::tmdl_parameters,
             dsn = gpkg_dsn,
             layer = "tmdl_parameters",
             driver = "GPKG",
             delete_layer = TRUE)

sf::st_write(odeqtmdl::tmdl_actions,
             dsn = gpkg_dsn,
             layer = "tmdl_actions",
             driver = "GPKG",
             delete_layer = TRUE)

sf::st_write(odeqtmdl::tmdl_geo_ids,
             dsn = gpkg_dsn,
             layer = "tmdl_geo_ids",
             driver = "GPKG",
             delete_layer = TRUE)

sf::st_write(odeqtmdl::tmdl_targets,
             dsn = gpkg_dsn,
             layer = "tmdl_targets",
             driver = "GPKG",
             delete_layer = TRUE)

sf::st_write(tmdl_reaches,
             dsn = gpkg_dsn,
             layer = "tmdl_reaches",
             driver = "GPKG",
             delete_layer = TRUE)

sf::st_write(odeqtmdl::tmdl_au_gnis,
             dsn = gpkg_dsn,
             layer = "tmdl_au_gnis",
             driver = "GPKG",
             delete_layer = TRUE)

sf::st_write(odeqtmdl::tmdl_au,
             dsn = gpkg_dsn,
             layer = "tmdl_au",
             driver = "GPKG",
             delete_layer = TRUE)

#- Everything ------------------------------------------------------------------
TMDL_param <- NULL
TMDL_pollu <- NULL
gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
gpkg_layer <- "OR_TMDLs_all"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           TMDL_param = TMDL_param, TMDL_pollu = TMDL_pollu,
                           collapse = FALSE)

#- TMDLs by reach --------------------------------------------------------------

gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
gpkg_layer <- "TMDLs_by_reach_scope_TMDL"

# This version groups TMDL names and parameter/pollutant
df1 <- tmdl_reaches %>%
  dplyr::filter(!AU_ID == "99") %>%
  dplyr::filter(TMDL_scope == "TMDL") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::filter(TMDL_status == "Active") %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name", "citation_abbreviated")], by = "action_id") %>%
  dplyr::mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID),
                TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  group_by(PIDAUID, HUC_6, HU_6_NAME, HUC6_full, HUC_8, HU_8_NAME, HUC8_full) %>%
  summarize(action_ids = paste((unique(action_id)), collapse = "; "),
            TMDL_names = paste((unique(TMDL_name)), collapse = "; "),
            TMDL_wq_limited_parameters = paste(sort(unique(TMDL_wq_limited_parameter)), collapse = "; "),
            TMDL_pollutants = paste(sort(unique(TMDL_pollutant)), collapse = "; "))

tmdls_by_reach <- nhd_fc %>%
  dplyr::select(AU_ID, Permanent_Identifier, ReachCode,
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
  dplyr::inner_join(y = df1, by = "PIDAUID") %>%
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
                ReachCode,
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
             layer = gpkg_layer,
             driver = "GPKG",
             delete_layer = TRUE)


#- Allocation Only by reach --------------------------------------------------------------

gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
gpkg_layer <- "TMDL_by_reach_scope_allocations"

# This version groups TMDL names and parameter/pollutant
df2 <- tmdl_reaches %>%
  dplyr::filter(!AU_ID == "99") %>%
  dplyr::filter(!TMDL_scope == "TMDL") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::filter(TMDL_status == "Active") %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name", "citation_abbreviated")], by = "action_id") %>%
  dplyr::mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID),
                TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
 group_by(PIDAUID, HUC_6, HU_6_NAME, HUC6_full, HUC_8, HU_8_NAME, HUC8_full) %>%
 summarize(action_ids = paste((unique(action_id)), collapse = "; "),
           TMDL_names = paste((unique(TMDL_name)), collapse = "; "),
           TMDL_wq_limited_parameters = paste(sort(unique(TMDL_wq_limited_parameter)), collapse = "; "),
           TMDL_pollutants = paste(sort(unique(TMDL_pollutant)), collapse = "; "))

tmdl_alloc_by_reach <- nhd_fc %>%
  dplyr::select(AU_ID, Permanent_Identifier, ReachCode,
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
  dplyr::inner_join(y = df2, by = "PIDAUID") %>%
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
                ReachCode,
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

sf::st_write(tmdl_alloc_by_reach,
             dsn = gpkg_dsn,
             layer = gpkg_layer,
             driver = "GPKG",
             delete_layer = TRUE)


#- TMDLs by AU -----------------------------------------------------------------

gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
gpkg_layer <- "TMDLs_by_AU"

df3 <- odeqtmdl::tmdl_au %>%
  dplyr::filter(!AU_ID == "99") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::filter(TMDL_status == "Active") %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name", "citation_abbreviated")], by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  group_by(AU_ID, HUC_6, HU_6_NAME, HUC6_full, HUC_8, HU_8_NAME, HUC8_full) %>%
  summarize(action_ids = paste((unique(action_id)), collapse = "; "),
            TMDL_names = paste((unique(TMDL_name)), collapse = "; "),
            TMDL_wq_limited_parameters = paste(sort(unique(TMDL_wq_limited_parameter)), collapse = "; "),
            TMDL_pollutants = paste(sort(unique(TMDL_pollutant)), collapse = "; "))

tmdls_by_au <- tmdl_au_fc %>%
  dplyr::inner_join(y = df3, by = "AU_ID") %>%
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
             layer = gpkg_layer,
             driver = "GPKG",
             delete_layer = TRUE)

#- Outputs by TMDL parameter ---------------------------------------------------

gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
TMDL_params <- names(param_names)

for (param in TMDL_params) {

  print(param)

  gpkg_layer <- param_names[[param]]
  odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                             nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                             TMDL_param = param, TMDL_pollu = NULL)
}

#- Sediment Surrogates--- ------------------------------------------------------
# TMDL_param <- c("DDD 4,4'", "DDE 4,4'", "DDT 4,4'", "Dieldrin", "Iron", "Mercury (total)")
# TMDL_pollu <- NULL
# action_ids <- c("OR_TMDL_20191122", "30674", "35888")
#
# gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls.gpkg")
# gpkg_layer <- "OR_sediment_surrogate_TMDLs"
#
# odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
#                            nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
#                            action_ids = action_ids,
#                            TMDL_param = TMDL_param, TMDL_pollu = TMDL_pollu)

# HUC8 Map ---------------------------------------------------------------------

huc8 <-  sf::st_read(dsn = file.path(paths$tmdl_reaches_shp[1],"Support_Features.gdb"), layer = "WBDHU8") %>%
  select(HUC_8 = HUC8, HU_8_NAME = Name)

tmdl_count_by_huc8 <- tmdl_reaches %>%
  dplyr::filter(TMDL_active & !AU_ID == "99") %>%
  dplyr::filter(TMDL_scope == "TMDL") %>%
  select(HUC_8, HU_8_NAME, TMDL_wq_limited_parameter) %>%
  distinct() %>%
  group_by(HUC_8, HU_8_NAME) %>%
  summarise(TMDL_param = n()) %>%
  ungroup() %>%
  #mutate(HUC_8 =  as.character(HUC_8)) %>%
  as.data.frame()

TMDL_huc8 <- huc8 %>%
  left_join(tmdl_count_by_huc8)

sf::st_write(TMDL_huc8, dsn = file.path(paths$tmdl_reaches_shp[1],"Deliverables", "OR_tmdls_huc8_summary.shp"),
             delete_layer = TRUE)
