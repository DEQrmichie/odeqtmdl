# This script generates the GIS features and tables and saves to a geopackage

library(readxl)
library(dplyr)
library(sf)
library(tidyr)

# Read paths
paths <- readxl::read_excel(path = "data_raw/project_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text'))

# nhd_fc
load(file.path(paths$package_path[1], "data_raw", "nhd_fc.rda"))

# tmdl_reaches
tmdl_reaches <- tmdl_reaches()

#tmdl_ndh_fc
load(file = file.path(paths$package_path[1], "data_raw", "tmdl_reach_fc.rda"))

# tmdl_au_fc
load(file = file.path(paths$package_path[1], "data_raw", "tmdl_au_fc.rda"))

gpkg_dsn <- file.path(paths$tmdl_reaches_shp[1], "web_map", "OR_TMDLs.gpkg")

# tmdl_au
#load(file.path(paths$package_path[1], "data", "tmdl_au.rda"))


#- TMDLs by reach --------------------------------------------------------------


gpkg_layer <- "TMDLs_by_reach_scope_TMDL"

# This version separates pollutants by TMDL scope.

pastee <- function(x) {paste(sort(na.omit(unique(x))), collapse = "; ")}

df1 <- tmdl_reaches %>%
  dplyr::filter(!AU_ID == "99") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::filter(TMDL_status == "Active") %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name", "citation_abbreviated")], by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")"),
                Scope_TMDL = if_else(TMDL_scope == "TMDL", TMDL_pollutant, NA_character_),
                Scope_Allocation_only = if_else(TMDL_scope == "Allocation only", TMDL_pollutant, NA_character_),
                Scope_Advisory_allocation = if_else(TMDL_scope == "Advisory allocation", TMDL_pollutant, NA_character_)) %>%
  group_by(GLOBALID, HUC_6, HU_6_NAME, HUC6_full, HUC_8, HU_8_NAME, HUC8_full) %>%
  summarize(action_ids = pastee(action_id),
            TMDL_names = pastee(TMDL_name),
            TMDL_wq_limited_parameters = pastee(TMDL_wq_limited_parameter),
            TMDL_pollutants = pastee(TMDL_pollutant),
            Scope_TMDL = pastee(Scope_TMDL),
            Scope_Allocation_only = pastee(Scope_Allocation_only),
            Scope_Advisory_allocation  = pastee(Scope_Advisory_allocation),
            TMDL_status = pastee(TMDL_status),
            geo_id = pastee(geo_id)) %>%
  ungroup() %>%
  mutate(Scope_TMDL = na_if(Scope_TMDL, ""),
         Scope_Allocation_only = na_if(Scope_Allocation_only, ""),
         Scope_Advisory_allocation = na_if(Scope_Advisory_allocation, ""),
         geo_id = na_if(geo_id, ""))

# This version does not separate pollutants by TMDL scope. So it is not clear which pollutants are allocation only vs TMDL.
# df1 <- tmdl_reaches %>%
#   dplyr::filter(!AU_ID == "99") %>%
#   dplyr::filter(TMDL_scope == "TMDL") %>%
#   dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
#                    by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
#   dplyr::filter(TMDL_status == "Active") %>%
#   dplyr::left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name", "citation_abbreviated")], by = "action_id") %>%
#   dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
#   group_by(GLOBALID, HUC_6, HU_6_NAME, HUC6_full, HUC_8, HU_8_NAME, HUC8_full) %>%
#   summarize(action_ids = pastee(action_id),
#             TMDL_names = pastee(TMDL_name),
#             TMDL_wq_limited_parameters = pastee(TMDL_wq_limited_parameter),
#             TMDL_pollutants = pastee(TMDL_pollutant),
#             TMDL_scope = pastee(TMDL_scope),
#             TMDL_status = pastee(TMDL_status),
#             geo_id = pastee(geo_id))

tmdls_by_reach <- nhd_fc %>%
  dplyr::select(AU_ID, GLOBALID, Permanent_Identifier, ReachCode,
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
  dplyr::inner_join(y = df1, by = "GLOBALID") %>%
  dplyr::select(action_ids,
                TMDL_names,
                TMDL_wq_limited_parameters,
                any_of(c("TMDL_pollutants",
                         "TMDL_scope",
                         "Scope_TMDL",
                         "Scope_Allocation_only",
                         "Scope_Advisory_allocation")),
                TMDL_status,
                geo_id,
                HUC_6,
                HU_6_NAME,
                HUC6_full,
                HUC_8,
                HU_8_NAME,
                HUC8_full,
                GLOBALID,
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


#- TMDLs by AU -----------------------------------------------------------------

gpkg_layer <- "TMDLs_by_AU"

df3 <- odeqtmdl::tmdl_au %>%
  dplyr::filter(!AU_ID == "99") %>%
  dplyr::filter(TMDL_scope == "TMDL") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::filter(TMDL_status == "Active") %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name", "citation_abbreviated")], by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  group_by(AU_ID, HUC_6, HU_6_NAME, HUC6_full, HUC_8, HU_8_NAME, HUC8_full) %>%
  summarize(action_ids = pastee(action_id),
            TMDL_names = pastee(TMDL_name),
            TMDL_wq_limited_parameters = pastee(TMDL_wq_limited_parameter),
            TMDL_pollutants = pastee(TMDL_pollutant),
            TMDL_scope = pastee(TMDL_scope),
            TMDL_status = pastee(TMDL_status))

tmdls_by_au <- tmdl_au_fc %>%
  dplyr::inner_join(y = df3, by = "AU_ID") %>%
  dplyr::select(action_ids,
                TMDL_names,
                TMDL_wq_limited_parameters,
                TMDL_pollutants,
                TMDL_scope,
                TMDL_status,
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

#- Write Tables ----------------------------------------------------------------

sf::st_write(odeqtmdl::tmdl_parameters,
             dsn = gpkg_dsn,
             layer = "tmdl_parameters",
             driver = "GPKG",
             delete_layer = TRUE)

sf::st_write(odeqtmdl::tmdl_actions %>% mutate(TMDL_issue_date = as.POSIXct(TMDL_issue_date),
                                               EPA_action_date = as.POSIXct(EPA_action_date)),
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
gpkg_layer <- "OR_TMDLs_all"

odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                           nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                           TMDL_param = TMDL_param, TMDL_pollu = TMDL_pollu,
                           collapse = FALSE)

#- Outputs by TMDL parameter ---------------------------------------------------

param_names <- c(
  "Ammonia" = "Ammonia",
  "Aquatic Weeds" = "Aquatic_Weeds",
  "BioCriteria" = "BioCriteria",
  "Chlordane" = "Chlordane",
  "Chlorophyll-a" = "Chlorophyll_a",
  "DDD 4,4'" = "DDD",
  "DDE 4,4'" = "DDE",
  "DDT 4,4'" = "DDT",
  "Dieldrin" = "Dieldrin",
  "Dioxin (2,3,7,8-TCDD)" = "Dioxin",
  "Dissolved Oxygen" = "Dissolved_Oxygen",
  "E. coli" = "Ecoli",
  "Enterococci" = "Enterococci",
  "Excess Algal Growth" = "Excess_Algal_Growth",
  "Fecal Coliform" = "Fecal_Coliform",
  "Harmful Algal Blooms" = "HABs",
  "Iron (total)" = "Iron",
  "Lead" = "Lead",
  "Mercury (total)" = "Mercury",
  "Methylmercury" = "Methylmercury",
  "Nitrates" = "Nitrates",
  "pH" = "pH",
  "Polychlorinated Biphenyls (PCBs)" = "PCBs",
  "Sedimentation" = "Sedimentation",
  "Temperature" = "Temperature",
  "Total Dissolved gas" = "TDG",
  "Total Phosphorus" = "Total_Phosphorus",
  "Turbidity" = "Turbidity"
)

TMDL_params <- tmdl_reaches %>%
  filter(!AU_ID == "99") %>%
  left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
            by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  filter(TMDL_status == "Active") %>%
  select(TMDL_wq_limited_parameter) %>%
  distinct() %>%
  pull(TMDL_wq_limited_parameter) %>% sort()

for (param in TMDL_params) {

  print(param)

  gpkg_layer <- param_names[[param]]
  odeqtmdl::tmdl_export_gpkg(gpkg_dsn = gpkg_dsn, gpkg_layer = gpkg_layer,
                             nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                             TMDL_param = param, TMDL_pollu = NULL)
}
