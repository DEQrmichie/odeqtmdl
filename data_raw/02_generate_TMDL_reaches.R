# This script generates the following data tables

# tmdl_reaches
# tmdl_au
# tmdl_au_gnis
# tmdl_parameters
# tmdl_wqstd

# It should be opened within the Project Rstudio view so the working directory is set correctly.
# It pulls in the GIS features and tabular data and joins everything into one table.

#- Setup -----------------------------------------------------------------------
library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(odeqmloctools)
library(odeqtmdl)


# Read paths
paths <- readxl::read_excel(path = "data_raw/project_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text'))

# Load the table that has the LU from old to new AUS
df.aufixes <- read_csv(file.path(paths$tmdl_reaches_shp[1], "R/AU_Fixes.csv"))

ws_fix <- sf::st_read(dsn = file.path(paths$tmdl_reaches_shp[1],"Support_Features_shp"), layer = "OR_WS_171003010505_02_106440_in_171003021101") %>%
  sf::st_drop_geometry() %>%
  pull(GLOBALID) %>%
  unique()

huc6 <- sf::st_read(dsn = file.path(paths$tmdl_reaches_shp[1],"Support_Features.gdb"), layer = "WBDHU6") %>%
  sf::st_drop_geometry() %>%
  select(HUC_6 = HUC6, HU_6_NAME = Name) %>%
  mutate(HUC6_full = paste0(HUC_6," ", HU_6_NAME))

huc8 <-  sf::st_read(dsn = file.path(paths$tmdl_reaches_shp[1],"Support_Features.gdb"), layer = "WBDHU8") %>%
  sf::st_drop_geometry() %>%
  select(HUC_8 = HUC8, HU_8_NAME = Name) %>%
  mutate(HUC8_full = paste0(HUC_8," ", HU_8_NAME))

huc10 <-  sf::st_read(dsn = file.path(paths$tmdl_reaches_shp[1],"Support_Features.gdb"), layer = "WBDHU10") %>%
  sf::st_drop_geometry() %>%
  select(HUC_10 = HUC10, HU_10_NAME = Name) %>%
  mutate(HUC10_full = paste0(HUC_10," ", HU_10_NAME))

ornhd <- odeqmloctools::ornhd %>%
  dplyr::select(GLOBALID, Permanent_Identifier, ReachCode,
                GNIS_Name, GNIS_ID,
                AU_ID, AU_Name, AU_Description, AU_GNIS_Name, AU_GNIS, LengthKM) %>%
  filter(!AU_ID == "99") %>%
  mutate(HUC_6 = substr(AU_ID, 7, 12),
         HUC_8 = substr(AU_ID, 7, 14),
         HUC_10 = substr(AU_ID, 7, 16),
         AU_GNIS_Name = case_when(grepl("_WS", AU_ID, fixed = TRUE) & is.na(AU_GNIS_Name) ~ GNIS_Name,
                                  !grepl("_WS", AU_ID, fixed = TRUE) ~ NA_character_,
                                  TRUE ~ AU_GNIS_Name),
         AU_GNIS = case_when(grepl("_WS", AU_ID, fixed = TRUE) & is.na(AU_GNIS) ~ paste0(AU_ID,";"),
                             !grepl("_WS", AU_ID, fixed = TRUE) ~ NA_character_,
                             TRUE ~ AU_GNIS)) %>%
  mutate(HUC_8 = case_when(GLOBALID %in% ws_fix ~ "17100302",
                           TRUE ~ HUC_8),
         HUC_10 = case_when(GLOBALID %in% ws_fix ~ "1710030211",
                            TRUE ~ HUC_10)) %>%
  left_join(huc6) %>%
  left_join(huc8) %>%
  left_join(huc10) %>%
  distinct()

#- tmdl_reaches ----------------------------------------------------------------

files1 <- list.files(path = file.path(paths$tmdl_reaches_shp[1], "01_Working_on"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files2 <- list.files(path = file.path(paths$tmdl_reaches_shp[1], "02_DEQ_Under_Review"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files3 <- list.files(path = file.path(paths$tmdl_reaches_shp[1], "03_DEQ_Final_Reviewed"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files4 <- list.files(path = file.path(paths$tmdl_reaches_shp[1], "04_EPA_Under_Review"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files5 <- list.files(path = file.path(paths$tmdl_reaches_shp[1], "05_EPA_Final_Reviewed"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files6 <- list.files(path = file.path(paths$tmdl_reaches_shp[1], "06_Final_ATTAINS"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

tmdl.shps <- c(files1, files2, files3, files4, files5, files6)
rm(files1, files2, files3, files4, files5, files6)

# exclude files in the Supporting folder
tmdl.shps <- tmdl.shps[ !grepl("Supporting", tmdl.shps) ]

# Today's date
db_version <- paste0("v", gsub(pattern = "-", replacement = "", x = Sys.Date()))

# Load all the shps into a dataframe'
tmdl_reach_tbl <- data.frame()

for (i in 1:length(tmdl.shps)) {

  tmdl_dsn = dirname(tmdl.shps[i])
  tmdl_layer = sub("\\.shp$", "", basename(tmdl.shps[i]))

  tmdl_reach_tbl0 <- sf::st_read(dsn = tmdl_dsn,
                                 layer = tmdl_layer,
                                 stringsAsFactors = FALSE) %>%
    #sf::st_transform(crs = 2992) %>%
    #dplyr::mutate(length_feet = sf::st_length(.)) %>%
    sf::st_drop_geometry() %>%
    {
      if ("TMDL_scope" %in% names(.)) . else mutate(., TMDL_scope = NA_character_)
    } %>%
    {
      if ("period" %in% names(.)) . else mutate(., period = NA_character_)
    } %>%
    {
      if ("Source" %in% names(.)) . else mutate(., Source = NA_character_)
    }  %>%
    {
      if ("geo_id" %in% names(.)) . else mutate(., geo_id = NA_character_)
    } %>%
    {
      if ("GLOBALID" %in% names(.)) . else mutate(., GLOBALID = NA_character_)
    } %>%
    select(action_id, TMDL_wq_limited_parameter = TMDL_param,
           TMDL_pollutant = TMDL_pollu, TMDL_scope, Period = period, Source,
           geo_id, GLOBALID)

  tmdl_reach_tbl <- rbind(tmdl_reach_tbl, tmdl_reach_tbl0)

  rm(tmdl_reach_tbl0)
}

# For updates
# tmdl_reach_tbl <- tmdl_reaches()

tmdl_reaches <- tmdl_reach_tbl %>%
  select(GLOBALID, action_id, TMDL_wq_limited_parameter,
         TMDL_pollutant, TMDL_scope, Period, Source, geo_id) %>%
  #dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_active")],
  #                 by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  dplyr::mutate(Source = case_when(grepl("Nonpoint", Source, ignore.case = TRUE) ~ "Nonpoint source",
                                   grepl("Point", Source, ignore.case = TRUE) ~ "Point source",
                                   grepl("Both", Source, ignore.case = TRUE) ~ "Both",
                                   TRUE ~ NA_character_)) %>%
  left_join(ornhd, by = "GLOBALID") %>%
  filter(!AU_ID == "99") %>%
  dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, ReachCode) %>%
  dplyr::distinct() %>%
  left_join(LU_pollutant[,c("Pollu_ID", "Pollutant_DEQ")],
            by = c("TMDL_wq_limited_parameter" = "Pollutant_DEQ")) %>%
  dplyr::select(action_id,
                TMDL_wq_limited_parameter,
                TMDL_pollutant,
                TMDL_scope,
                Period,
                Source,
                Pollu_ID,
                geo_id,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                GLOBALID,
                Permanent_Identifier,
                ReachCode,
                GNIS_Name, GNIS_ID,
                AU_ID, AU_Name, AU_Description,
                AU_GNIS_Name, AU_GNIS,
                LengthKM) %>%
  as.data.frame()

num_df <- 4

tmdl_reaches0 <- tmdl_reaches %>%
  group_by((row_number() - 1 ) %/% (n() / num_df)) %>%
  nest %>%
  pull(data)

tmdl_reaches1 <- tmdl_reaches0[[1]] %>% as.data.frame()
tmdl_reaches2 <- tmdl_reaches0[[2]] %>% as.data.frame()
tmdl_reaches3 <- tmdl_reaches0[[3]] %>% as.data.frame()
tmdl_reaches4 <- tmdl_reaches0[[4]] %>% as.data.frame()

# Save as a RDS file in inst/extdata folder (replaces existing)
# File is too large to save in data and as single file
saveRDS(tmdl_reaches, compress = TRUE, file = file.path(paths$package_path[1], "data_raw", "tmdl_reaches.RDS"))
saveRDS(tmdl_reaches1, compress = TRUE, file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches1.RDS"))
saveRDS(tmdl_reaches2, compress = TRUE, file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches2.RDS"))
saveRDS(tmdl_reaches3, compress = TRUE, file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches3.RDS"))
saveRDS(tmdl_reaches4, compress = TRUE, file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches4.RDS"))

#- tmdl_au_gnis --------------------------------------------------------------------

or_au_gnis <- ornhd %>%
  dplyr::select(AU_ID, AU_GNIS, LengthKM) %>%
  dplyr::filter(grepl("_WS", AU_ID, fixed = TRUE)) %>%
  dplyr::group_by(AU_ID, AU_GNIS) %>%
  dplyr::summarise(AU_GNIS_length_km = sum(LengthKM, na.rm = TRUE)) %>%
  ungroup()

tmdl_au_gnis <- tmdl_reaches %>%
  dplyr::filter(grepl("_WS", AU_ID, fixed = TRUE)) %>%
  dplyr::filter(!is.na(TMDL_scope)) %>%
  dplyr::group_by(action_id, AU_ID, AU_GNIS, TMDL_pollutant) %>%
  dplyr::mutate(Source = dplyr::case_when(any(grepl("Both", Source, ignore.case = TRUE)) ~ "Both",
                                          any(grepl("Point", Source, ignore.case = TRUE)) & any(grepl("Nonpoint", Source, ignore.case = TRUE)) ~ "Both",
                                          all(grepl("Point", Source, ignore.case = TRUE)) ~ "Point source",
                                          all(grepl("Nonpoint", Source, ignore.case = TRUE)) ~ "Nonpoint source",
                                          any(grepl("Point", Source, ignore.case = TRUE)) & any(is.na(Source)) ~ "Point source",
                                          any(grepl("Nonpoint", Source, ignore.case = TRUE)) & any(is.na(Source)) ~ "Nonpoint source",
                                          TRUE ~ NA_character_)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(action_id, AU_ID, AU_GNIS, TMDL_pollutant) %>%
  dplyr::mutate(Period = case_when(TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                     length(unique(na.omit(Period))) > 1 ~ paste0("Mixed (",paste0(sort(unique(na.omit(Period))), collapse = ", "),")"),
                                   TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                     length(unique(na.omit(Period))) == 1 ~ paste0(sort(unique(na.omit(Period))), collapse = ", "),
                                   TRUE ~ NA_character_)) %>%
  dplyr::ungroup() %>%
  dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                TMDL_scope, Period, Source, Pollu_ID,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                AU_ID, AU_Name, AU_GNIS_Name, AU_GNIS,
                LengthKM) %>%
  tidyr::pivot_wider(names_from = "TMDL_scope", values_from = "LengthKM",
                     values_fn = sum, values_fill = 0) %>%
  dplyr::rename(TMDL_length_km = TMDL,
                Allocation_only_km = "Allocation only",
                Advisory_allocation_km = "Advisory allocation") %>%
  dplyr::left_join(or_au_gnis, by = c("AU_ID", "AU_GNIS")) %>%
  dplyr::mutate(TMDL_scope = dplyr::case_when(TMDL_length_km > 0 ~ "TMDL",
                                              Allocation_only_km > 0 ~ "Allocation only",
                                              Advisory_allocation_km > 0 ~ "Advisory allocation",
                                              TRUE ~ NA_character_),
                TMDL_AU_GNIS_Percent = round(TMDL_length_km/AU_GNIS_length_km * 100,0),
                Allocation_AU_GNIS_Percent = round((Allocation_only_km + Advisory_allocation_km)/AU_GNIS_length_km * 100,0)) %>%
  dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                TMDL_scope, Period, Source, Pollu_ID,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                AU_ID, AU_Name, AU_GNIS_Name, AU_GNIS,
                TMDL_length_km, Allocation_only_km, Advisory_allocation_km,
                AU_GNIS_length_km,
                TMDL_AU_GNIS_Percent, Allocation_AU_GNIS_Percent) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_au_gnis, file = file.path(paths$package_path[1], "data", "tmdl_au_gnis.rda"))

#- tmdl_au --------------------------------------------------------------------

or_au <- ornhd %>%
  dplyr::select(AU_ID, LengthKM) %>%
  dplyr::group_by(AU_ID) %>%
  dplyr::summarise(AU_length_km = sum(LengthKM, na.rm = TRUE)) %>%
  ungroup()

tmdl_au <- tmdl_reaches %>%
  dplyr::filter(!is.na(TMDL_scope)) %>%
  dplyr::group_by(action_id, AU_ID, TMDL_pollutant) %>%
  dplyr::mutate(Source = dplyr::case_when(any(grepl("Both", Source, ignore.case = TRUE)) ~ "Both",
                                          any(grepl("Point", Source, ignore.case = TRUE)) & any(grepl("Nonpoint", Source, ignore.case = TRUE)) ~ "Both",
                                          all(grepl("Point", Source, ignore.case = TRUE)) ~ "Point source",
                                          all(grepl("Nonpoint", Source, ignore.case = TRUE)) ~ "Nonpoint source",
                                          any(grepl("Point", Source, ignore.case = TRUE)) & any(is.na(Source)) ~ "Point source",
                                          any(grepl("Nonpoint", Source, ignore.case = TRUE)) & any(is.na(Source)) ~ "Nonpoint source",
                                          TRUE ~ NA_character_)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(action_id, AU_ID, TMDL_wq_limited_parameter) %>%
  dplyr::mutate(Period = case_when(TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                     length(unique(na.omit(Period))) > 1 ~ paste0("Mixed (",paste0(sort(unique(na.omit(Period))), collapse = ", "),")"),
                                   TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                     length(unique(na.omit(Period))) == 1 ~ paste0(sort(unique(na.omit(Period))), collapse = ", "),
                                   TRUE ~ NA_character_)) %>%
  dplyr::ungroup() %>%
  dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                TMDL_scope, Period, Source, Pollu_ID,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                AU_ID, AU_Name, AU_Description,
                LengthKM) %>%
  tidyr::pivot_wider(names_from = "TMDL_scope", values_from = "LengthKM",
                     values_fn = sum, values_fill = 0) %>%
  dplyr::rename(TMDL_length_km = TMDL,
                Allocation_only_km = "Allocation only",
                Advisory_allocation_km = "Advisory allocation") %>%
  dplyr::left_join(or_au, by = "AU_ID") %>%
  dplyr::mutate(TMDL_scope = dplyr::case_when(TMDL_length_km > 0 ~ "TMDL",
                                              Allocation_only_km > 0 ~ "Allocation only",
                                              Advisory_allocation_km > 0 ~ "Advisory allocation",
                                              TRUE ~ NA_character_),
                TMDL_AU_Percent = round(TMDL_length_km/AU_length_km * 100,0),
                Allocation_AU_Percent = round((Allocation_only_km + Advisory_allocation_km)/AU_length_km * 100,0)) %>%
  dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                TMDL_scope, Period, Source, Pollu_ID,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                AU_ID, AU_Name, AU_Description,
                TMDL_length_km, Allocation_only_km, Advisory_allocation_km,
                AU_length_km,
                TMDL_AU_Percent, Allocation_AU_Percent) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_au, file = file.path(paths$package_path[1], "data", "tmdl_au.rda"))

#- tmdl_parameters -------------------------------------------------------------

# Logic to assign "Not Active" TMDL status. This is already attributed in spreadsheet but
# leaving here just in case.

# TMDL_status = case_when(action_id %in% c("2043", "1230", "2021",
#                                          "10007", "42375",
#                                          "OR_TMDL_20171219",
#                                          "OR_TMDL_20191122") ~ "Not Active",
#                         action_id == "1936" & TMDL_pollutant %in% c("Total Phosphorus",
#                                                                     "Ammonia Nitrogen (NH3-N)") ~ "Not Active",
#                         action_id == "30674" & TMDL_pollutant %in% c("Mercury (total)",
#                                                                      "Methylmercury") ~ "Not Active",
#                         TRUE ~ TMDL_status),


tmdl_parameters_tbl <- readxl::read_excel(path = file.path(paths$package_path[1], "data_raw", "TMDL_db_tabular.xlsx"),
                                          sheet = "tmdl_parameters",
                                       na = c("", "NA"),
                                       col_names = TRUE, skip = 1,
                                       col_types = c("text", "numeric", "text", "text", "text",
                                                     "text", "text", "text")) %>%
  select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
         TMDL_status, revision_action_id, TMDL_status_comment) %>%
  distinct()

# At a future date maybe we will add the scope narrative and/or HUCs to the parameters table.
# Right now that information is in the TMDL mapping list. Here's how to add it.

# Read TMDL mapping table
# tmdl_mapping_tbl <- readxl::read_excel(path = file.path(paths$tmdl_reaches_shp[1], "Mapping_List.xlsx"),
#                                        sheet = "mapping_list",
#                                        na = c("", "NA"),
#                                        col_names = TRUE, skip = 1,
#                                        col_types = c("text", "numeric", "text", "text", "text",
#                                                      "text", "text", "text", "text", "text",
#                                                      "text", "text", "text", "text", "text",
#                                                      "text", "text", "text", "text", "text",
#                                                      "text", "text")) %>%
#   mutate(scope_citation = replace_na(scope_citation, ""),
#          scope_narrative = case_when(!is.na(scope_narrative) ~ paste0(scope_narrative, " ", scope_citation),
#                                      TRUE ~ NA_character_)) %>%
#   select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, scope_narrative, scope_citation) %>%
#   distinct()


# This only includes mapped TMDLs.
# tmdl_parameters <- tmdl_reaches %>%
#   left_join(tmdl_mapping_tbl, by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
#   select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_status, TMDL_status_comment) %>%
#   mutate(TMDL_mapped = TRUE) %>%
#   distinct() %>%
#   arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant) %>%
#   as.data.frame()

# This relies on the info from the xlsx tmdl parameters table, which usually includes un-mapped TMDLs.
# Use the code above if only mapped TMDLs need to be included.
tmdl_parameters <- tmdl_reaches %>%
  select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant) %>%
  distinct() %>%
  mutate(TMDL_mapped = TRUE) %>%
  right_join(tmdl_parameters_tbl, by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_status, revision_action_id, TMDL_status_comment, TMDL_mapped) %>%
  mutate(TMDL_mapped = ifelse(is.na(TMDL_mapped), FALSE, TMDL_mapped)) %>%
  distinct() %>%
  arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_parameters, file = file.path(paths$package_path[1], "data", "tmdl_parameters.rda"))

