# This script generates the GIS features and tables and saves to a geodatabase

library(arcgisbinding)
library(sf)
library(dplyr)
library(readxl)
library(odeqtmdl)

arc.check_product()

# Read paths
paths <- readxl::read_excel(path = "data_raw/project_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text'))

# nhd_fc
load(file.path(paths$package_path[1], "data_raw", "nhd_fc.rda"))

# tmdl_reaches
tmdl_reaches <- tmdl_reaches()

gdb_path <- file.path(paths$tmdl_reaches_shp[1], "Maps", "web_map", "OR_TMDLs.gdb")

pastee <- function(x) {paste(sort(na.omit(unique(x))), collapse = "; ")}

# - Create Base features -------------------------------------------------------

# unique list of GLOBALIDs where TMDLs apply
tmdl_gids <- dplyr::filter(tmdl_reaches, TMDL_scope %in% c("TMDL",
                                                           "Allocation only",
                                                           "Advisory Allocation")) %>%
  dplyr::pull(GLOBALID) %>%
  unique() %>%
  sort()

tmdl_scope_gids <- dplyr::filter(tmdl_reaches, TMDL_scope == "TMDL") %>%
  dplyr::pull(GLOBALID) %>%
  unique() %>%
  sort()

geo_ids_gids <- dplyr::filter(tmdl_reaches, !is.na(geo_id)) %>%
  dplyr::pull(GLOBALID) %>%
  unique() %>%
  sort()

# filter nhd to only where TMDLs apply
tmdl_reach_fc <- nhd_fc %>%
  dplyr::filter(GLOBALID %in% tmdl_gids) %>%
  dplyr::filter(!AU_ID == "99") %>%
  dplyr::select(AU_ID, AU_Name, AU_Description, AU_WBType, GNIS_ID, GNIS_Name,
                HUC12,
                AU_GNIS, AU_GNIS_Name, GLOBALID, Permanent_Identifier,
                WBArea_Permanent_Identifier, FType)

# Dissolve to geo_ids
tmdl_geo_id_fc <- nhd_fc %>%
  dplyr::filter(GLOBALID %in% geo_ids_gids) %>%
  dplyr::select(AU_ID, AU_Name, AU_Description, AU_WBType, GNIS_ID, GNIS_Name,
                AU_GNIS, AU_GNIS_Name, GLOBALID)

# Dissolve to AUs
tmdl_au_fc <- tmdl_reach_fc %>%
  dplyr::filter(GLOBALID %in% tmdl_scope_gids) %>%
  dplyr::group_by(AU_ID, AU_Name, AU_Description, AU_WBType) %>%
  dplyr::summarize() %>%
  ungroup()

# Dissolve to AU GNIS
tmdl_au_gnis_fc <- tmdl_reach_fc %>%
  dplyr::filter(GLOBALID %in% tmdl_scope_gids) %>%
  dplyr::filter(grepl("_WS", AU_ID, fixed = TRUE)) %>%
  dplyr::group_by(AU_ID, AU_Name, AU_GNIS_Name, AU_GNIS, AU_WBType) %>%
  dplyr::summarize() %>%
  ungroup()

save(tmdl_reach_fc, file = file.path(paths$package_path[1], "data_raw", "tmdl_reach_fc.rda"))
save(tmdl_geo_id_fc, file = file.path(paths$package_path[1], "data_raw", "tmdl_geo_id_fc.rda"))
save(tmdl_au_fc, file = file.path(paths$package_path[1], "data_raw", "tmdl_au_fc.rda"))
save(tmdl_au_gnis_fc, file = file.path(paths$package_path[1], "data_raw", "tmdl_au_gnis_fc.rda"))

rm(tmdl_au_gnis_fc, tmdl_reach_fc, tmdl_gids, tmdl_scope_gids, geo_ids_gids)

 # -load base features----------------------------------------------------------

#tmdl_ndh_fc
load(file = file.path(paths$package_path[1], "data_raw", "tmdl_reach_fc.rda"))

# tmdl_au_fc
load(file = file.path(paths$package_path[1], "data_raw", "tmdl_au_fc.rda"))

# tmdl_au
#load(file.path(paths$package_path[1], "data", "tmdl_au.rda"))

#- TMDLs by reach --------------------------------------------------------------
# ~30 min`

time_start1 <- Sys.time()

gdb_path <- file.path(paths$tmdl_reaches_shp[1], "Maps", "web_map", "OR_TMDLs.gdb")

fc_name <- "TMDLs_by_reach"

# This version separates pollutants by TMDL scope.

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
  group_by(GLOBALID, HUC6, HUC6_Name, HUC6_full, HUC8, HUC8_Name, HUC8_full) %>%
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
                HUC6,
                HUC6_Name,
                HUC6_full,
                HUC8,
                HUC8_Name,
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

arc.write(path = file.path(gdb_path, fc_name),
          data = tmdls_by_reach,
          validate = TRUE,
          overwrite = TRUE)

time_end1 <- Sys.time()
time_end1 - time_start1

rm(df1, tmdls_by_reach)

#- TMDLs by AU -----------------------------------------------------------------

time_start2 <- Sys.time()

fc_name <- "TMDLs_by_AU"

df3 <- odeqtmdl::tmdl_au %>%
  dplyr::filter(!AU_ID == "99") %>%
  dplyr::filter(TMDL_scope == "TMDL") %>%
  dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                   by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::filter(TMDL_status == "Active") %>%
  dplyr::left_join(odeqtmdl::tmdl_actions[,c("action_id", "TMDL_name", "citation_abbreviated")], by = "action_id") %>%
  dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
  group_by(AU_ID, HUC6, HUC6_Name, HUC6_full, HUC8, HUC8_Name, HUC8_full) %>%
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
                HUC6,
                HUC6_Name,
                HUC6_full,
                HUC8,
                HUC8_Name,
                HUC8_full,
                AU_ID,
                AU_Name,
                AU_Description,
                AU_WBType,
                Shape)

arc.write(path = file.path(gdb_path, fc_name),
          data = tmdls_by_au,
          validate = TRUE,
          overwrite = TRUE)

time_end2 <- Sys.time()
time_end2 - time_start2

rm(df3, tmdls_by_au)

#- geo IDs ---------------------------------------------------------------------

fc_name <- "geo_ids"

tmdl_reach_geo_ids_only <- dplyr::filter(tmdl_reaches, !is.na(geo_id)) %>%
  left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
            by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  filter(TMDL_status == "Active") %>%
  select(geo_id, HUC6, HUC6_Name, HUC6_full, HUC8, HUC8_Name, HUC8_full, GLOBALID) %>%
  distinct()

tmdl_geo_ids <- tmdl_geo_id_fc %>%
  inner_join(y = tmdl_reach_geo_ids_only, by = "GLOBALID") %>%
  group_by(geo_id, HUC6, HUC6_Name, HUC6_full, HUC8, HUC8_Name, HUC8_full,
           AU_ID, AU_Name, AU_Description, AU_WBType,
           AU_GNIS, AU_GNIS_Name) %>%
  summarize() %>%
  ungroup() %>%
  arrange(geo_id, AU_ID, AU_GNIS)

arc.write(path = file.path(gdb_path, fc_name),
          data = tmdl_geo_ids,
          validate = TRUE,
          overwrite = TRUE)

#- Write Tables ----------------------------------------------------------------
# ~1.2 hours

time_start3 <- Sys.time()

arc.write(path = file.path(gdb_path, "tmdl_actions"),
          data = odeqtmdl::tmdl_actions %>% mutate(TMDL_issue_date = as.POSIXct(TMDL_issue_date),
                                                   EPA_action_date = as.POSIXct(EPA_action_date)),
          overwrite = TRUE)

arc.write(path = file.path(gdb_path, "tmdl_au_gnis"),
          data = odeqtmdl::tmdl_au_gnis,
          overwrite = TRUE)

arc.write(path = file.path(gdb_path, "tmdl_au"),
          data = odeqtmdl::tmdl_au,
          overwrite = TRUE)

arc.write(path = file.path(gdb_path, "tmdl_geo_ids"),
          data = odeqtmdl::tmdl_geo_ids,
          overwrite = TRUE)

arc.write(path = file.path(gdb_path, "tmdl_parameters"),
          data = odeqtmdl::tmdl_parameters,
          overwrite = TRUE)

arc.write(path = file.path(gdb_path, "tmdl_targets"),
          data = odeqtmdl::tmdl_targets,
          overwrite = TRUE)

arc.write(path = file.path(gdb_path, "tmdl_reaches"),
          data = tmdl_reaches,
          overwrite = TRUE)

time_start <- Sys.time()

time_end3 <- Sys.time()
time_end3 - time_start3

#- Outputs by TMDL parameter ---------------------------------------------------
# ~ 2 hours

sort(unique(tmdl_reaches$TMDL_wq_limited_parameter))

time_start4 <- Sys.time()

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

gdb_path <- file.path(paths$tmdl_reaches_shp[1], "Maps", "web_map", "OR_TMDLs.gdb/TMDLs_by_wq_limited_parameter")

tmdl_params_df <- odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")] %>%
  filter(action_id %in% unique(tmdl_reaches$action_id))

TMDL_params <- tmdl_reaches %>%
  filter(!AU_ID == "99") %>%
  left_join(tmdl_params_df, by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  filter(TMDL_status == "Active") %>%
  select(TMDL_wq_limited_parameter) %>%
  distinct() %>%
  pull(TMDL_wq_limited_parameter) %>% sort()

for (param in TMDL_params) {

  print(param)

  gdb_fc <- param_names[[param]]
  odeqtmdl::tmdl_export_gdb(gdb_path = gdb_path, gdb_fc = gdb_fc,
                             nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                             TMDL_param = param, TMDL_pollu = NULL)
}

time_end4 <- Sys.time()
time_end4 - time_start4

# Time -------------------------------------------------------------------------

time_end4 - time_start1

#- Outputs by TMDL pollutant ---------------------------------------------------

sort(unique(tmdl_reaches$TMDL_pollutant))

pollu_names <- c(
  "Ammonia Nitrogen (NH3-N)" = "Ammonia_Nitrogen",
  "Ammonium (NH4-N)" = "Ammonium",
  "Biochemical Oxygen Demand" = "BOD",
  "Biochemical Oxygen Demand (5-day)" = "BOD5",
  "Carbonaceous Biochemical Oxygen Demand" =  "CBOD",
  "Carbonaceous Biochemical Oxygen Demand (5-day)" = "CBOD5",
  "Chlordane" = "Chlordane",
  "DDD 4,4'" = "DDD",
  "DDE 4,4'" = "DDE",
  "DDT 4,4'" = "DDT",
  "Dieldrin" = "Dieldrin",
  "Dioxin (2,3,7,8-TCDD)" = "Dioxin",
  "Dissolved Inorganic Nitrogen" = "DIN",
  "Dissolved Orthophosphate as Phosphorus" = "Dissolved_Orthophosphate",
  "Dissolved Oxygen" = "Dissolved_Oxygen",
  "E. coli" = "Ecoli",
  "Fecal Coliform" = "Fecal_Coliform",
  "Fine Sediment" = "Fine_Sediment",
  "Heat" = "Heat",
  "Inorganic Phosphorus" = "Inorganic_Phosphorus",
  "Iron (total)" = "Iron",
  "Lead" = "Lead",
  "Mercury (total)" = "Mercury",
  "Methylmercury" = "Methylmercury",
  "Nitrates" = "Nitrates",
  "Nitrite + Nitrate, as N (NO23-N)" = "Nitrite_Nitrate",
  "pH" = "pH",
  "Polychlorinated Biphenyls (PCBs)" = "PCBs",
  "Sediment Oxygen Demand" = "SOD",
  "Sedimentation" = "Sedimentation",
  "Solar Radiation" = "Solar_Radiation",
  "Total Dissolved gas" = "TDG",
  "Total Nitrogen" = "Total_Nitrogen",
  "Total Phosphorus" = "Total_Phosphorus",
  "Total suspended solids" = "TSS",
  "Turbidity" = "Turbidity",
  "Ultimate Biochemical Oxygen Demand" = "UBOD",
  "Volatile Solids" = "Volatile_Solids",
  "Volatile Suspended Solids" = "Volatile_Suspended_Solids"
)

TMDL_pollus <- names(pollu_names)

TMDL_pollus <- tmdl_reaches %>%
  filter(!AU_ID == "99") %>%
  left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
            by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  filter(TMDL_status == "Active") %>%
  select(TMDL_pollutant) %>%
  distinct() %>%
  pull(TMDL_pollutant) %>% sort()

gdb_path <- file.path(paths$tmdl_reaches_shp[1], "Maps", "web_map", "OR_TMDLs.gdb/TMDLs_by_pollutant")

for (pollu in TMDL_pollus) {

  print(pollu)

  gdb_fc <- paste0("pollutant_", pollu_names[[pollu]])
  odeqtmdl::tmdl_export_gdb(gdb_path = gdb_path, gdb_fc = gdb_fc,
                            nhd_fc = nhd_fc, tmdl_reaches = tmdl_reaches,
                            TMDL_param = NULL, TMDL_pollu = pollu)
}
