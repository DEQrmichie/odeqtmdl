# This script generates the following data tables

# tmdl_reaches
# tmdl_au
# tmdl_au_gnis
# tmdl_parameters
# tmdl_wqstd

# It should be opened within the Project Rstudio view so the working directory is set correctly.
# It pulls in the GIS features and tabular data and joins everything into one table.

#- tmdl_reaches ----------------------------------------------------------------

# Load the table that has the LU from old to new AUS
df.aufixes <- read_csv(file.path(paths$tmdl_reaches_shp[1], "R/AU_Fixes.csv"))

shp_dir <- paths$tmdl_reaches_shp[1]

huc6 <- sf::st_read(dsn = file.path(shp_dir,"Support_Features.gdb"), layer = "WBDHU6") %>%
  sf::st_drop_geometry() %>%
  select(HUC_6 = HUC6, HU_6_NAME = Name) %>%
  mutate(HUC6_full = paste0(HUC_6," ", HU_6_NAME))

huc8 <-  sf::st_read(dsn = file.path(shp_dir,"Support_Features.gdb"), layer = "WBDHU8") %>%
  sf::st_drop_geometry() %>%
  select(HUC_8 = HUC8, HU_8_NAME = Name) %>%
  mutate(HUC8_full = paste0(HUC_8," ", HU_8_NAME))

huc10 <-  sf::st_read(dsn = file.path(shp_dir,"Support_Features.gdb"), layer = "WBDHU10") %>%
  sf::st_drop_geometry() %>%
  select(HUC_10 = HUC10, HU_10_NAME = Name) %>%
  mutate(HUC10_full = paste0(HUC_10," ", HU_10_NAME))

files1 <- list.files(path = file.path(shp_dir, "01_Working_on"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files2 <- list.files(path = file.path(shp_dir, "02_DEQ_Under_Review"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files3 <- list.files(path = file.path(shp_dir, "03_DEQ_Final_Reviewed"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files4 <- list.files(path = file.path(shp_dir, "04_EPA_Under_Review"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files5 <- list.files(path = file.path(shp_dir, "05_EPA_Final_Reviewed"),
                     pattern = "^action.*\\.shp$", recursive = TRUE, full.names = TRUE)

files6 <- list.files(path = file.path(shp_dir, "06_Final_ATTAINS"),
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
    select(action_id, TMDL_wq_limited_parameter = TMDL_param,
           TMDL_pollutant = TMDL_pollu, TMDL_scope, Period = period, Source,
           geo_id, Permanent_Identifier = Permanent_, ReachCode, AU_ID)

  tmdl_reach_tbl <- rbind(tmdl_reach_tbl, tmdl_reach_tbl0)

  rm(tmdl_reach_tbl0)
}

ornhd <- odeqmloctools::ornhd %>%
  dplyr::select(Permanent_Identifier, ReachCode, GNIS_Name, GNIS_ID, AU_ID,
                AU_Name, AU_Description, AU_GNIS_Name, AU_GNIS, LengthKM) %>%
  filter(!AU_ID == "99") %>%
  mutate(HUC_6 = substr(AU_ID, 7, 12),
         HUC_8 = substr(AU_ID, 7, 14),
         HUC_10 = substr(AU_ID, 7, 16),
         PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
  left_join(huc6) %>%
  left_join(huc8) %>%
  left_join(huc10)

or_au <- ornhd %>%
  dplyr::select(AU_ID, LengthKM) %>%
  dplyr::group_by(AU_ID) %>%
  dplyr::summarise(AU_length_km = sum(LengthKM, na.rm = TRUE)) %>%
  ungroup()

or_au_gnis <- ornhd %>%
  dplyr::select(AU_ID, AU_GNIS, LengthKM) %>%
  dplyr::group_by(AU_ID, AU_GNIS) %>%
  dplyr::summarise(AU_GNIS_length_km = sum(LengthKM, na.rm = TRUE)) %>%
  ungroup()

# Note, remove TMDL active mutate once all the GIS layers have been updated w/
# consistent parameter names. Better approach is to use a join w/ mapping list. e.g.
# select(-TMDL_active) %>%
# left_join(by=c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant"))
tmdl_reaches <- tmdl_reach_tbl %>%
  dplyr::mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
  select(PIDAUID, action_id, TMDL_wq_limited_parameter,
         TMDL_pollutant, TMDL_scope, Period, Source, geo_id) %>%
  dplyr::left_join(tmdl_actions, by = "action_id") %>%
  dplyr::mutate(TMDL_active = case_when(action_id %in% c("2043", "1230", "2021",
                                                         "10007", "42375",
                                                         "OR_TMDL_20171219",
                                                         "OR_TMDL_20191122") ~ FALSE,
                                        action_id == "1936" & TMDL_pollutant %in% c("Total Phosphorus",
                                                                                    "Ammonia Nitrogen (NH3-N)") ~ FALSE,
                                        action_id == "30674" & TMDL_pollutant %in% c("Mercury (total)",
                                                                                     "Methylmercury") ~ FALSE,
                                        TRUE ~ TRUE),
                Source = case_when(grepl("Nonpoint", Source, ignore.case = TRUE) ~ "Nonpoint source",
                                   grepl("Point", Source, ignore.case = TRUE) ~ "Point source",
                                   grepl("Both", Source, ignore.case = TRUE) ~ "Both",
                                   TRUE ~ NA_character_)) %>%
  left_join(ornhd, by = "PIDAUID") %>%
  filter(!AU_ID == "99") %>%
  dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, ReachCode) %>%
  dplyr::distinct() %>%
  left_join(LU_pollutant[,c("Pollu_ID", "Pollutant_DEQ")],
            by = c("TMDL_wq_limited_parameter" = "Pollutant_DEQ")) %>%
  dplyr::select(action_id,
                TMDL_wq_limited_parameter,
                TMDL_pollutant,
                TMDL_active,
                TMDL_scope,
                Period,
                Source,
                Pollu_ID,
                geo_id,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                Permanent_Identifier,
                ReachCode,
                GNIS_Name, GNIS_ID,
                AU_ID, AU_Name, AU_Description,
                AU_GNIS_Name, AU_GNIS,
                LengthKM) %>%
  as.data.frame()

# Save as a RDS file in inst/extdata folder (replaces existing)
# File is too large to save in data.
saveRDS(tmdl_reaches, file = file.path(paths$package_path[1], "inst", "extdata", "tmdl_reaches.RDS"))

#- tmdl_au_gnis --------------------------------------------------------------------

tmdl_au_gnis <- tmdl_reaches %>%
  dplyr::filter(TMDL_scope == "TMDL") %>%
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
                                     length(unique(Period)) > 1 ~ paste0("Mixed (",paste0(sort(unique(Period)), collapse = ", "),")"),
                                   TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                     length(unique(Period)) == 1 ~ paste0(sort(unique(Period)), collapse = ", "),
                                   TRUE ~ NA_character_)) %>%
  dplyr::ungroup() %>%
  dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_active,
                TMDL_scope, Period, Source, Pollu_ID,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                AU_ID, AU_Name, AU_GNIS_Name, AU_GNIS,
                LengthKM) %>%
  dplyr::group_by(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_active,
                  TMDL_scope, Period, Source, Pollu_ID,
                  HUC_6, HU_6_NAME, HUC6_full,
                  HUC_8, HU_8_NAME, HUC8_full,
                  HUC_10, HU_10_NAME, HUC10_full,
                  AU_ID, AU_Name, AU_GNIS_Name, AU_GNIS) %>%
  dplyr::summarise(TMDL_length_km = sum(LengthKM, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(or_au_gnis, by = c("AU_ID", "AU_GNIS")) %>%
  dplyr::mutate(TMDL_AU_GNIS_Percent = round(TMDL_length_km/AU_GNIS_length_km * 100,0)) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_au_gnis, file = file.path(paths$package_path[1], "data", "tmdl_au_gnis.rda"))


#- tmdl_au --------------------------------------------------------------------

tmdl_au <- tmdl_reaches %>%
  dplyr::filter(TMDL_scope == "TMDL") %>%
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
                                     length(unique(Period)) > 1 ~ paste0("Mixed (",paste0(sort(unique(Period)), collapse = ", "),")"),
                                   TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                     length(unique(Period)) == 1 ~ paste0(sort(unique(Period)), collapse = ", "),
                                   TRUE ~ NA_character_)) %>%
  dplyr::ungroup() %>%
  dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_active,
                TMDL_scope, Period, Source, Pollu_ID,
                HUC_6, HU_6_NAME, HUC6_full,
                HUC_8, HU_8_NAME, HUC8_full,
                HUC_10, HU_10_NAME, HUC10_full,
                AU_ID, AU_Name, AU_Description,
                LengthKM) %>%
  dplyr::group_by(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_active,
                  TMDL_scope, Period, Source, Pollu_ID,
                  HUC_6, HU_6_NAME, HUC6_full,
                  HUC_8, HU_8_NAME, HUC8_full,
                  HUC_10, HU_10_NAME, HUC10_full,
                  AU_ID, AU_Name, AU_Description) %>%
  dplyr::summarise(TMDL_length_km = sum(LengthKM, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(TMDL_length_km = case_when(!TMDL_scope == "TMDL" ~ 0.0,
                                           TRUE ~ TMDL_length_km)) %>%
  dplyr::left_join(or_au, by = "AU_ID") %>%
  dplyr::mutate(TMDL_AU_Percent = round(TMDL_length_km/AU_length_km * 100,0)) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_au, file = file.path(paths$package_path[1], "data", "tmdl_au.rda"))

#- tmdl_parameters -------------------------------------------------------------

tmdl_mapping <- readxl::read_excel(path = file.path(paths$tmdl_reaches_shp[1], "Mapping_List.xlsx"),
                                   sheet = "tmdl_mapping_list",
                                   na = c("", "NA"),
                                   col_names = TRUE,
                                   col_types = c("text", "numeric", "text", "text", "text", "text",
                                                 "text", "text", "text", "text", "text", "text", "text",
                                                 "text", "text", "text", "text", "text", "text", "text")) %>%
  mutate(scope_citation = replace_na(scope_citation, ""),
         scope_narrative = case_when(!is.na(scope_narrative) ~ paste0(scope_narrative, " ", scope_citation),
                                     TRUE ~ NA_character_)) %>%
  select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
         scope_narrative)

tmdl_active_note_tbl <- tmdl_actions_tbl %>%
  select(action_id, TMDL_active_note)

tmdl_parameters <- tmdl_reaches %>%
  left_join(tmdl_active_note_tbl, by = "action_id") %>%
  select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, TMDL_active,
         TMDL_active_note) %>%
  mutate(TMDL_active_note = case_when(action_id == "1936" & !(TMDL_pollutant %in% c("Total Phosphorus",
                                                                                    "Ammonia Nitrogen (NH3-N)")) ~ NA_character_,
                                      action_id == "30674" & !(TMDL_pollutant %in% c("Mercury (total)",
                                                                                     "Methylmercury")) ~ NA_character_,
                                      TRUE ~ TMDL_active_note)) %>%
  left_join(tmdl_mapping, by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
  distinct() %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_parameters, file = file.path(paths$package_path[1], "data", "tmdl_parameters.rda"))

#- tmdl_wqstd ------------------------------------------------------------------

tmdl_wqstd <- tmdl_reaches %>%
  select(action_id, Pollu_ID) %>%
  distinct() %>%
  left_join(LU_wqstd, by = "Pollu_ID") %>%
  select(action_id, Pollu_ID, wqstd_code) %>%
  arrange(action_id, Pollu_ID) %>%
  as.data.frame()

# Save a copy in data folder (replaces existing)
save(tmdl_wqstd, file = file.path(paths$package_path[1], "data", "tmdl_wqstd.rda"))

#- tmdl_db ---------------------------------------------------------------------

# Not used but keeping just in case.

tmdl_db <- tmdl_reaches %>%
  left_join(tmdl_actions, by = "action_id") %>%
  left_join(tmdl_parameters, by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_active")) %>%
  left_join(geo_id_table, by = c("action_id", "geo_id")) %>%
  inner_join(tmdl_targets, by = c("action_id", "geo_id", "TMDL_pollutant")) %>%
  dplyr::mutate(db_version = db_version) %>%
  dplyr::select(geo_id, geo_description, mapped,
                TMDL_pollutant, TMDL_wq_limited_parameter,
                target_type, target_value, target_units, target_stat_base,
                season_start, season_end, target_conditionals_references,
                TMDL_element, notes, action_id, TMDL_name, TMDL_issue_year,
                TMDL_active, issue_agency, in_attains, attains_status, TMDL_issue_date,
                EPA_action_date, AU_ID, ReachCode,
                citation_abbreviated, citation_full, db_version)

save(tmdl_db, file = file.path("data_raw", "tmdl_db.rda"))
