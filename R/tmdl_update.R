#' Update TMDL tables with new or revised information
#'
#' Imports TMDL information from DEQ's TMDL database template and GIS shapefiles; and
#' add the new or revised the information to the odeqtmdl package tables. To
#' run this function the xlsx template and/or the parameter/pollutant pair GIS files
#' must be completed.
#'
#' Updates are made to the existing package tables by first removing all rows
#' with the same 'action_id' identified in \code{action_ids}. The new
#' information from the \code{xlsx_template} and any GIS files are then added to the package tables. There
#' is no filtering for specific parameters or pollutants. All
#' information attributed to the action_id is removed before being replaced, so
#' all information attributed to the action must be included in the \code{xlsx_template}.
#'
#' @param action_ids vector of TMDL action IDs to update. Required.
#' @param gis_path Path to the directory that holds the TMDL
#'    GIS shapefiles. The GIS shapefiles can be in a subdirctory. The function
#'    completes a recursive search using \code{\link[base]{list.files}}. The GIS files
#'    must be ESRI shapefiles and named in the following way:
#'        \itemize{
#'          \item NHD reaches: 'action_[action_id]_[parameter]_[pollutant]_NHD'
#'          \item AU Flowlines: 'action_[action_id]_[parameter]_[pollutant]_AU_Flowline'
#'          \item AU Waterbodies: 'action_[action_id]_[parameter]_[pollutant]_AU_WB'
#'          }
#'          where, action_id, parameter, and pollutant are user supplied.
#'
#' @param xlsx_template The path including file name of the TMDL excel template.
#' @param package_path Path to the top level directory of the odeqtmdl R package. The 'data', data_raw', and 'inst/extdata' folders must exist.
#' @param update_tables logical. If TRUE, updates the following package tables:
#'        \itemize{
#'          \item tmdl_actions
#'          \item tmdl_targets
#'          \item tmdl_geo_id
#'          \item tmdl_wqstd
#'          }
#' @param update_reaches logical. if TRUE, imports GIS files and updates the following package tables:
#'      \itemize{
#'        \item tmdl_reaches
#'        \item tmdl_au
#'        \item tmdl_au_gnis
#'        \item tmdl_parameters
#'        }
#' @export

tmdl_update <- function(action_ids = NULL, xlsx_template, gis_path, package_path,
                        update_tables = TRUE, update_reaches = TRUE) {

  # Check to make sure paths to path, data, data_raw, and inst/extdata exist
  if (!file.exists(gis_path)) {
    stop(paste0("Error. Path in 'gis_path' not found: ", gis_path) )
  }

  if (!file.access(gis_path, mode = 2)[[1]] == 0) {
    stop(paste0("Error. Write access permission not granted for 'gis_path': ", gis_path))
  }

  if (!file.exists(package_path)) {
    stop(paste0("Error. Path in 'package_path' not found: ", package_path) )
  }

  if (!file.access(package_path, mode = 2)[[1]] == 0)  {
    stop(paste0("Error. Write access permission not granted for 'package_path': ", package_path))
  }

  if (!file.exists(file.path(package_path, "data"))) {
    stop(paste0("Error. There must be a 'data' folder in 'package_path': ", file.path(package_path, "data")) )
  }

  if (update_reaches) {

    if (!file.exists(file.path(package_path, "data_raw"))) {
      stop(paste0("Error. There must be a 'data_raw' folder in 'package_path': ", file.path(package_path, "data_raw")) )
    }

    if (!file.exists(file.path(package_path, "inst", "extdata"))) {
      stop(paste0("Error. There must be 'inst/extdata' folders in 'package_path': ", file.path(package_path, "inst", "extdata")) )
    }
  }

  if (is.null(action_ids)) {
    stop("Error. Please identify the action_ids to update")
  }

  update_action_ids <- unique(action_ids)

  # UPDATE TABLES --------------------------------------------------------------

  if (update_tables) {

    cat("Updating tables","\n")

    #- tmdl_actions --------------------------------------------------------------
    cat("-- tmdl_actions\n")

    # Read TMDL actions table
    tmdl_actions_tbl <- readxl::read_excel(file.path(xlsx_template),
                                           sheet = "tmdl_actions" ,
                                           na = c("", "NA"), skip = 1,
                                           col_names = TRUE,
                                           col_types = c("text", "text", 'numeric', "text", "text",
                                                         "logical", "text", "date", "date", "text",
                                                         "text", "text"))

    tmdl_actions_update <- tmdl_actions_tbl %>%
      dplyr::filter(action_id %in% update_action_ids) %>%
      dplyr::select(action_id,
                    TMDL_name,
                    TMDL_issue_year,
                    issue_agency,
                    in_attains,
                    attains_status,
                    TMDL_issue_date,
                    EPA_action_date,
                    citation_abbreviated,
                    citation_full,
                    TMDL_comment,
                    URL) %>%
      dplyr::distinct() %>%
      dplyr::mutate(TMDL_issue_date = as.Date(TMDL_issue_date),
                    EPA_action_date = as.Date(EPA_action_date)) %>%
      dplyr::arrange(TMDL_issue_year,
                     TMDL_name) %>%
      as.data.frame()

    # This updates the whole dataframe
    tmdl_actions <- odeqtmdl::tmdl_actions %>%
      dplyr::filter(!action_id %in% update_action_ids) %>%
      rbind(tmdl_actions_update) %>%
      dplyr::distinct() %>%
      dplyr::arrange(TMDL_issue_date,
                     TMDL_name) %>%
      as.data.frame()

    # Save a copy in data folder (replaces existing)
    save(tmdl_actions, file = file.path(package_path, "data", "tmdl_actions.rda"))

    #- tmdl_geo_ids ------------------------------------------------------------

    cat("-- tmdl_geo_ids\n")
    tmdl_geo_ids_update <- readxl::read_excel(file.path(xlsx_template),
                                              sheet = "tmdl_geo_ids",
                                              col_names = TRUE, skip = 1,
                                              col_types = c("text", "text", "logical",
                                                            "text", "text", "numeric"
                                              )) %>%
      dplyr::select(action_id, geo_id, geo_description, geo_id_mapped) %>%
      dplyr::arrange(action_id, geo_id) %>%
      as.data.frame()

    # This updates the whole dataframe
    tmdl_geo_ids <- odeqtmdl::tmdl_geo_ids %>%
      dplyr::filter(!action_id %in% update_action_ids) %>%
      rbind(tmdl_geo_ids_update) %>%
      dplyr::distinct() %>%
      dplyr::arrange(action_id, geo_id) %>%
      as.data.frame()

    # Save a copy in data folder (replaces existing)
    save(tmdl_geo_ids, file = file.path(package_path, "data", "tmdl_geo_ids.rda"))

    #- tmdl_targets ------------------------------------------------------------

    cat("-- tmdl_targets\n")
    tmdl_targets_update <- readxl::read_excel(file.path(xlsx_template),
                                              sheet = "tmdl_targets",
                                              col_names = TRUE, skip = 1,
                                              col_types = c("text", "text", "text", "numeric", "text",
                                                            "text", "text", "text", "text", "text",
                                                            "numeric", "text", "numeric", "text", "numeric",
                                                            "date", "date", "text", "text", "text",
                                                            "text")) %>%
      dplyr::mutate(season_start = format(season_start, "%b %d"),
                    season_end = format(season_end, "%b %d"),
                    target_value = dplyr::case_when(grepl("^[[:digit:]]", target_value) ~ as.character(as.numeric(target_value)),
                                                    TRUE ~ target_value)) %>%
      dplyr::select(action_id,
                    geo_id,
                    TMDL_pollutant,
                    field_parameter,
                    target_type,
                    target_value,
                    target_units,
                    Unit_UID,
                    target_time_base,
                    time_base_UID,
                    target_stat_base,
                    stat_base_UID,
                    season_start,
                    season_end,
                    target_conditionals,
                    TMDL_element,
                    target_reference,
                    target_comments) %>%
      dplyr::arrange(geo_id) %>%
      as.data.frame()

    # This updates the whole dataframe
    tmdl_targets <- odeqtmdl::tmdl_targets %>%
      dplyr::filter(!action_id %in% update_action_ids) %>%
      rbind(tmdl_targets_update) %>%
      dplyr::distinct() %>%
      dplyr::arrange(geo_id) %>%
      as.data.frame()

    # Save a copy in data folder (replaces existing)
    save(tmdl_targets, file = file.path(package_path, "data", "tmdl_targets.rda"))

    #- tmdl_WLA-----------------------------------------------------------------

    cat("-- tmdl_wla\n")

    tmdl_wla_update <- readxl::read_excel(path = file.path(xlsx_template),
                                          sheet = "tmdl_wla",
                                          col_names = TRUE, skip = 1,
                                          col_types = c("text", "text", "numeric", "text", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "numeric", "date", "date")) %>%
      dplyr::mutate(WLA_season_start = format(WLA_season_start, "%b %d"),
                    WLA_season_end = format(WLA_season_end, "%b %d")) %>%
      dplyr::select(action_id, AU_ID, TMDL_pollutant, EPANum,
                    WQFileNum, facility_name,
                    -TMDL_name, -WLA, -WLA_units, -Unit_UID, -WLA_season_start, -WLA_season_end) %>%
      dplyr::arrange(action_id, facility_name)

    # This updates the whole dataframe
    tmdl_wla <- odeqtmdl::tmdl_wla %>%
      dplyr::filter(!action_id %in% update_action_ids) %>%
      rbind(tmdl_wla_update) %>%
      dplyr::distinct() %>%
      dplyr::arrange(action_id, facility_name) %>%
      as.data.frame()

    # Save a copy in data folder (replaces existing)
    save(tmdl_wla, file = file.path(package_path, "data", "tmdl_wla.rda"))

    #- tmdl_wqstd ----------------------------------------------------------------

    cat("-- tmdl_wqstd\n")

    tmdl_wqstd_update <- readxl::read_excel(path = file.path(xlsx_template),
                                            sheet = "tmdl_wqstd",
                                            col_names = TRUE, skip = 1,
                                            col_types = c("text", "text", "numeric")) %>%
      dplyr::left_join(odeqtmdl::LU_pollutant[,c("Pollu_ID", "Pollutant_DEQ")],
                       by = c("TMDL_wq_limited_parameter" = "Pollutant_DEQ")) %>%
      dplyr::select(action_id, Pollu_ID, wqstd_code) %>%
      dplyr::arrange(action_id, Pollu_ID, wqstd_code) %>%
      as.data.frame()

    # This updates the whole dataframe
    tmdl_wqstd <- odeqtmdl::tmdl_wqstd %>%
      dplyr::filter(!action_id %in% update_action_ids) %>%
      rbind(tmdl_wqstd_update) %>%
      dplyr::distinct() %>%
      dplyr::arrange(action_id, Pollu_ID, wqstd_code) %>%
      as.data.frame()

    # Save a copy in data folder (replaces existing)
    save(tmdl_wqstd, file = file.path(package_path, "data", "tmdl_wqstd.rda"))
  }

  # UPDATE REACHES -------------------------------------------------------------

  if (update_reaches) {

    cat("Updating reaches\n")

    #- ornhd -------------------------------------------------------------------

    huc6 <- odeqmloctools::orhuc6 %>%
      dplyr::mutate(HUC6_full = paste0(HUC6," ", HUC6_Name)) %>%
      dplyr::select(HUC6, HUC6_Name, HUC6_full)

    huc8 <- odeqmloctools::orhuc8 %>%
      dplyr::mutate(HUC8_full = paste0(HUC8," ", HUC8_Name)) %>%
      dplyr::select(HUC8, HUC8_Name, HUC8_full)

    huc10 <- odeqmloctools::orhuc10 %>%
      dplyr::mutate(HUC10_full = paste0(HUC10," ", HUC10_Name)) %>%
      dplyr::select(HUC10, HUC10_Name, HUC10_full)

    ornhd <- odeqmloctools::ornhd %>%
      dplyr::select(GLOBALID, Permanent_Identifier, ReachCode,
                    GNIS_Name, GNIS_ID,
                    AU_ID, AU_Name, AU_Description, AU_GNIS_Name, AU_GNIS, LengthKM) %>%
      dplyr::filter(!AU_ID == "99") %>%
      dplyr::mutate(HUC6 = substr(AU_ID, 7, 12),
                    HUC8 = substr(AU_ID, 7, 14),
                    HUC10 = substr(AU_ID, 7, 16),
                    AU_GNIS_Name = dplyr::case_when(grepl("_WS", AU_ID, fixed = TRUE) & is.na(AU_GNIS_Name) ~ GNIS_Name,
                                                    !grepl("_WS", AU_ID, fixed = TRUE) ~ NA_character_,
                                                    TRUE ~ AU_GNIS_Name),
                    AU_GNIS = dplyr::case_when(grepl("_WS", AU_ID, fixed = TRUE) & is.na(AU_GNIS) ~ paste0(AU_ID,";"),
                                               !grepl("_WS", AU_ID, fixed = TRUE) ~ NA_character_,
                                               TRUE ~ AU_GNIS)) %>%
      dplyr::left_join(huc6, by = "HUC6") %>%
      dplyr::left_join(huc8, by = "HUC8") %>%
      dplyr::left_join(huc10, by = "HUC10") %>%
      dplyr::distinct()

    # - orau -------------------------------------------------------------------

    or_au <- ornhd %>%
      dplyr::select(AU_ID, LengthKM) %>%
      dplyr::group_by(AU_ID) %>%
      dplyr::summarise(AU_length_km = sum(LengthKM, na.rm = TRUE)) %>%
      dplyr::ungroup()

    or_au_wb <- odeqmloctools::orau %>%
      dplyr::anti_join(or_au, by = "AU_ID") %>%
      dplyr::mutate(AU_length_km = 0.01) %>%
      dplyr::select(AU_ID, AU_length_km)

    or_au <- rbind(or_au, or_au_wb)

    #- import GIS: nhd_reaches -------------------------------------------------

    cat("-- import GIS: NHD reaches\n")

    nhd_update_pattern <- paste0(paste0("^action_",update_action_ids, ".*NHD\\.shp$"),
                                 collapse = "|")

    nhd.shps <- list.files(path = file.path(gis_path),
                           pattern = nhd_update_pattern,
                           recursive = TRUE, full.names = TRUE,
                           ignore.case = TRUE)

    # exclude files in Supporting folder
    nhd.shps <- nhd.shps[ !grepl("Supporting", nhd.shps, ignore.case = TRUE) ]

    # Load all the shps into a dataframe'
    tmdl_reach_tbl <- data.frame()

    if (!identical(nhd.shps, character(0))) {
      # Only continue if there are shapefiles

      for (i in 1:length(nhd.shps)) {

        nhd_dsn = dirname(nhd.shps[i])
        nhd_layer = sub("\\.shp$", "", basename(nhd.shps[i]))

        tmdl_reach_tbl0 <- sf::st_read(dsn = nhd_dsn,
                                       layer = nhd_layer,
                                       stringsAsFactors = FALSE) %>%
          sf::st_drop_geometry() %>%
          dplyr::rename(dplyr::any_of(c(period = "Period", Source = "source"))) %>%
          {
            if ("TMDL_scope" %in% names(.)) . else  dplyr::mutate(., TMDL_scope = NA_character_)
          } %>%
          {
            if ("period" %in% names(.)) . else  dplyr::mutate(., period = NA_character_)
          } %>%
          {
            if ("Source" %in% names(.)) . else  dplyr::mutate(., Source = NA_character_)
          }  %>%
          {
            if ("geo_id" %in% names(.)) . else  dplyr::mutate(., geo_id = NA_character_)
          } %>%
          {
            if ("GLOBALID" %in% names(.)) . else  dplyr::mutate(., GLOBALID = NA_character_)
          } %>%
          dplyr::select(action_id, TMDL_wq_limited_parameter = TMDL_param,
                        TMDL_pollutant = TMDL_pollu, TMDL_scope, Period = period, Source,
                        geo_id, GLOBALID)

        tmdl_reach_tbl <- rbind(tmdl_reach_tbl, tmdl_reach_tbl0)

        rm(tmdl_reach_tbl0)
      }
    }

    #- import GIS: AU Flowlines ------------------------------------------------

    cat("-- import GIS: AU Flowlines\n")

    AU_flow_update_pattern <- paste0(paste0("^action_",update_action_ids, ".*AU_Flowline\\.shp$"),
                                     collapse = "|")

    AU_flow.shps <- list.files(path = file.path(gis_path),
                               pattern = AU_flow_update_pattern,
                               recursive = TRUE, full.names = TRUE,
                               ignore.case = TRUE)

    # exclude files in Supporting folder
    AU_flow.shps <- AU_flow.shps[ !grepl("Supporting", AU_flow.shps, ignore.case = TRUE) ]

    # Load all the shps into a dataframe
    AU_flow_tbl <- data.frame()

    if (!identical(AU_flow.shps, character(0))) {
      # Only continue if there are shapefiles

      for (i in 1:length(AU_flow.shps)) {

        AU_flow_dsn = dirname(AU_flow.shps[i])
        AU_flow_layer = sub("\\.shp$", "", basename(AU_flow.shps[i]))

        AU_flow_tbl0 <- sf::st_read(dsn = AU_flow_dsn,
                                    layer = AU_flow_layer,
                                    stringsAsFactors = FALSE) %>%
          sf::st_drop_geometry() %>%
          dplyr::rename(dplyr::any_of(c(period = "Period", Source = "source"))) %>%
          {
            if ("TMDL_scope" %in% names(.)) . else  dplyr::mutate(., TMDL_scope = NA_character_)
          } %>%
          {
            if ("period" %in% names(.)) . else  dplyr::mutate(., period = NA_character_)
          } %>%
          {
            if ("Source" %in% names(.)) . else  dplyr::mutate(., Source = NA_character_)
          }  %>%
          {
            if ("geo_id" %in% names(.)) . else  dplyr::mutate(., geo_id = NA_character_)
          } %>%
          dplyr::select(AU_ID, action_id, TMDL_wq_limited_parameter = TMDL_param,
                        TMDL_pollutant = TMDL_pollu, TMDL_scope, Period = period, Source,
                        geo_id)

        AU_flow_tbl <- rbind(AU_flow_tbl, AU_flow_tbl0)

        rm(AU_flow_tbl0)
      }

      # extract NHD reaches based on AU_ID and add to tmdl_reach_tbl

      tmdl_reach_tbl0 <- ornhd %>%
        dplyr::filter(!AU_ID == "99") %>%
        dplyr::inner_join(AU_flow_tbl, by = "AU_ID", relationship = "many-to-many") %>%
        dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, ReachCode) %>%
        dplyr::distinct() %>%
        dplyr::left_join(odeqtmdl::LU_pollutant[,c("Pollu_ID", "Pollutant_DEQ")],
                         by = c("TMDL_wq_limited_parameter" = "Pollutant_DEQ")) %>%
        dplyr::select(action_id,
                      TMDL_wq_limited_parameter,
                      TMDL_pollutant,
                      TMDL_scope,
                      Period,
                      Source,
                      Pollu_ID,
                      geo_id,
                      HUC6, HUC6_Name, HUC6_full,
                      HUC8, HUC8_Name, HUC8_full,
                      HUC10, HUC10_Name, HUC10_full,
                      GLOBALID,
                      Permanent_Identifier,
                      ReachCode,
                      GNIS_Name, GNIS_ID,
                      AU_ID, AU_Name, AU_Description,
                      AU_GNIS_Name, AU_GNIS,
                      LengthKM) %>%
        as.data.frame()

      tmdl_reach_tbl <- rbind(tmdl_reach_tbl, tmdl_reach_tbl0)

    }

    #- import GIS: AU_Waterbodies ----------------------------------------------

    cat("-- import GIS: AU Waterbodies\n")

    AU_WB_update_pattern <- paste0(paste0("^action_",update_action_ids, ".*AU_WB\\.shp$"),
                                   collapse = "|")

    AU_WB.shps <- list.files(path = file.path(gis_path),
                             pattern = AU_WB_update_pattern,
                             recursive = TRUE, full.names = TRUE,
                             ignore.case = TRUE)

    # exclude files in Supporting folder
    AU_WB.shps <- AU_WB.shps[ !grepl("Supporting", AU_WB.shps, ignore.case = TRUE) ]

    # Load all the shps into a dataframe
    AU_WB_tbl <- data.frame()

    if (!identical(AU_WB.shps, character(0))) {

      for (i in 1:length(AU_WB.shps)) {
        # Only continue if there are shapefiles

        AU_WB_dsn = dirname(AU_WB.shps[i])
        AU_WB_layer = sub("\\.shp$", "", basename(AU_WB.shps[i]))

        AU_WB_tbl0 <- sf::st_read(dsn = AU_WB_dsn,
                                  layer = AU_WB_layer,
                                  stringsAsFactors = FALSE) %>%
          sf::st_drop_geometry() %>%
          dplyr::rename(dplyr::any_of(c(period = "Period", Source = "source"))) %>%
          {
            if ("TMDL_scope" %in% names(.)) . else  dplyr::mutate(., TMDL_scope = NA_character_)
          } %>%
          {
            if ("period" %in% names(.)) . else  dplyr::mutate(., period = NA_character_)
          } %>%
          {
            if ("Source" %in% names(.)) . else  dplyr::mutate(., Source = NA_character_)
          }  %>%
          {
            if ("geo_id" %in% names(.)) . else  dplyr::mutate(., geo_id = NA_character_)
          } %>%
          dplyr::select(AU_ID, action_id, TMDL_wq_limited_parameter = TMDL_param,
                        TMDL_pollutant = TMDL_pollu, TMDL_scope, Period = period, Source,
                        geo_id)

        AU_WB_tbl <- rbind(AU_WB_tbl, AU_WB_tbl0)

        rm(AU_WB_tbl0)
      }

      # Some processing for joins
      AU_WB_update <- AU_WB_tbl %>%
        dplyr::filter(!AU_ID == "99") %>%
        dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID) %>%
        dplyr::distinct() %>%
        dplyr::left_join(odeqtmdl::LU_pollutant[,c("Pollu_ID", "Pollutant_DEQ")],
                         by = c("TMDL_wq_limited_parameter" = "Pollutant_DEQ")) %>%
        dplyr::inner_join(odeqmloctools::orau, by = "AU_ID") %>%
        dplyr::mutate(HUC6_full = paste0(HUC6," ", HUC6_Name),
                      HUC8_full = paste0(HUC8," ", HUC8_Name),
                      HUC10_full = paste0(HUC10," ", HUC10_Name),
                      LengthKM = 0.01) %>%
        dplyr::distinct() %>%
        dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                      TMDL_scope, Period, Source, Pollu_ID,
                      HUC6, HUC6_Name, HUC6_full,
                      HUC8, HUC8_Name, HUC8_full,
                      HUC10, HUC10_Name, HUC10_full,
                      AU_ID, AU_Name, AU_Description,
                      LengthKM) %>%
        as.data.frame()

    } else {

      AU_WB_update <- data.frame()
    }

    #- tmdl_reaches ------------------------------------------------------------

    cat("-- tmdl_reaches\n")

    tmdl_reaches_update <- tmdl_reach_tbl %>%
      dplyr::select(GLOBALID, action_id, TMDL_wq_limited_parameter,
                    TMDL_pollutant, TMDL_scope, Period, Source, geo_id) %>%
      dplyr::mutate(Source = dplyr::case_when(grepl("Nonpoint", Source, ignore.case = TRUE) ~ "Nonpoint source",
                                              grepl("Point", Source, ignore.case = TRUE) ~ "Point source",
                                              grepl("Both", Source, ignore.case = TRUE) ~ "Both",
                                              TRUE ~ NA_character_)) %>%
      dplyr::left_join(ornhd, by = "GLOBALID") %>%
      dplyr::filter(!AU_ID == "99") %>%
      dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, ReachCode) %>%
      dplyr::distinct() %>%
      dplyr::left_join(odeqtmdl::LU_pollutant[,c("Pollu_ID", "Pollutant_DEQ")],
                       by = c("TMDL_wq_limited_parameter" = "Pollutant_DEQ")) %>%
      dplyr::select(action_id,
                    TMDL_wq_limited_parameter,
                    TMDL_pollutant,
                    TMDL_scope,
                    Period,
                    Source,
                    Pollu_ID,
                    geo_id,
                    HUC6, HUC6_Name, HUC6_full,
                    HUC8, HUC8_Name, HUC8_full,
                    HUC10, HUC10_Name, HUC10_full,
                    GLOBALID,
                    Permanent_Identifier,
                    ReachCode,
                    GNIS_Name, GNIS_ID,
                    AU_ID, AU_Name, AU_Description,
                    AU_GNIS_Name, AU_GNIS,
                    LengthKM) %>%
      as.data.frame()

    # Remove the old rows and update with new ones
    # CAREFUL HERE, overwrites tmdl_reaches
    tmdl_reaches <- odeqtmdl::tmdl_reaches() %>%
      dplyr::filter(!(action_id %in% update_action_ids)) %>%
      rbind(tmdl_reaches_update) %>%
      dplyr::distinct() %>%
      dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, ReachCode) %>%
      as.data.frame()

    # the number of RDS files that tmdl_reaches dataframe is separated into.
    # This number should be set so each file is < 50 MB to avoid GitHub commit warnings.
    # As of 2024, the maximum file size allowed on GitHub is 100 MB.
    num_df <- 6

    tmdl_reaches0 <- tmdl_reaches %>%
      dplyr::group_by((dplyr::row_number() - 1 ) %/% ( dplyr::n() / num_df)) %>%
      tidyr::nest() %>%
      dplyr::pull(data)

    tmdl_reaches1 <- tmdl_reaches0[[1]] %>% as.data.frame()
    tmdl_reaches2 <- tmdl_reaches0[[2]] %>% as.data.frame()
    tmdl_reaches3 <- tmdl_reaches0[[3]] %>% as.data.frame()
    tmdl_reaches4 <- tmdl_reaches0[[4]] %>% as.data.frame()
    tmdl_reaches5 <- tmdl_reaches0[[5]] %>% as.data.frame()
    tmdl_reaches6 <- tmdl_reaches0[[6]] %>% as.data.frame()

    cat("-- tmdl_reaches (saving)\n")

    # Save a dev copy to a duckdb for fast reading.
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(package_path, "data_raw", "tmdl_reaches.duckdb"))
    DBI::dbWriteTable(con, "tmdl_reaches", tmdl_reaches, overwrite = TRUE)
    duckdb::dbDisconnect(con, shutdown = TRUE)

    # Save as a RDS file in inst/extdata folder (replaces existing)
    # File is too large to save in data and as single file
    # Ideally each file should be < 50 MB to avoid GitHub warnings.
    saveRDS(tmdl_reaches1, compress = TRUE, file = file.path(package_path, "inst", "extdata", "tmdl_reaches1.RDS"))
    saveRDS(tmdl_reaches2, compress = TRUE, file = file.path(package_path, "inst", "extdata", "tmdl_reaches2.RDS"))
    saveRDS(tmdl_reaches3, compress = TRUE, file = file.path(package_path, "inst", "extdata", "tmdl_reaches3.RDS"))
    saveRDS(tmdl_reaches4, compress = TRUE, file = file.path(package_path, "inst", "extdata", "tmdl_reaches4.RDS"))
    saveRDS(tmdl_reaches5, compress = TRUE, file = file.path(package_path, "inst", "extdata", "tmdl_reaches5.RDS"))
    saveRDS(tmdl_reaches6, compress = TRUE, file = file.path(package_path, "inst", "extdata", "tmdl_reaches6.RDS"))

    #- tmdl_au_gnis ------------------------------------------------------------

    cat("-- tmdl_au_gnis\n")

    or_au_gnis <- ornhd %>%
      dplyr::select(AU_ID, AU_GNIS, LengthKM) %>%
      dplyr::filter(grepl("_WS", AU_ID, fixed = TRUE)) %>%
      dplyr::group_by(AU_ID, AU_GNIS) %>%
      dplyr::summarise(AU_GNIS_length_km = sum(LengthKM, na.rm = TRUE)) %>%
      dplyr::ungroup()

    tmdl_au_gnis_update <- tmdl_reaches_update %>%
      dplyr::filter(grepl("_WS", AU_ID, fixed = TRUE)) %>%
      dplyr::filter(!is.na(TMDL_scope)) %>%
      dplyr::group_by(action_id, AU_ID, AU_GNIS, TMDL_pollutant) %>%
      dplyr::mutate(Source = dplyr::case_when(all(c("Both") %in% Source) ~ "Both",
                                              all(c("Point source") %in% Source) ~ "Point source",
                                              all(c("Nonpoint source") %in% Source) ~ "Nonpoint source",
                                              all(c("Point source", "Nonpoint source") %in% Source) ~ "Point source",
                                              all(c("Nonpoint source", NA_character_) %in% Source) ~ "Nonpoint source",
                                              all(c("Point source", NA_character_) %in% Source) ~ "Point source",
                                              TRUE ~ NA_character_)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(action_id, AU_ID, AU_GNIS, TMDL_pollutant) %>%
      dplyr::mutate(Period = dplyr::case_when(TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                                length(unique(na.omit(Period))) > 1 ~ paste0("Mixed (",paste0(sort(unique(na.omit(Period))), collapse = ", "),")"),
                                              TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                                length(unique(na.omit(Period))) == 1 ~ paste0(sort(unique(na.omit(Period))), collapse = ", "),
                                              TRUE ~ NA_character_)) %>%
      dplyr::ungroup() %>%
      dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                    TMDL_scope, Period, Source, Pollu_ID,
                    HUC6, HUC6_Name, HUC6_full,
                    HUC8, HUC8_Name, HUC8_full,
                    HUC10, HUC10_Name, HUC10_full,
                    AU_ID, AU_Name, AU_GNIS_Name, AU_GNIS,
                    LengthKM) %>%
      tidyr::pivot_wider(names_from = "TMDL_scope", values_from = "LengthKM",
                         values_fn = sum, values_fill = 0) %>%
      dplyr::bind_rows(dplyr::tibble(TMDL = numeric(),
                                     "Allocation only" = numeric(),
                                     "Advisory allocation" = numeric())) %>%
      dplyr::rename(TMDL_length_km = TMDL,
                    Allocation_only_km = "Allocation only",
                    Advisory_allocation_km = "Advisory allocation") %>%
      dplyr::mutate(TMDL_length_km = tidyr::replace_na(TMDL_length_km, 0),
                    Allocation_only_km = tidyr::replace_na(Allocation_only_km, 0),
                    Advisory_allocation_km = tidyr::replace_na(Advisory_allocation_km, 0)) %>%
      dplyr::left_join(or_au_gnis, by = c("AU_ID", "AU_GNIS")) %>%
      dplyr::mutate(TMDL_scope = dplyr::case_when(TMDL_length_km > 0 ~ "TMDL",
                                                  Allocation_only_km > 0 ~ "Allocation only",
                                                  Advisory_allocation_km > 0 ~ "Advisory allocation",
                                                  TRUE ~ NA_character_),
                    TMDL_AU_GNIS_Percent = round(TMDL_length_km/AU_GNIS_length_km * 100,0),
                    Allocation_AU_GNIS_Percent = round((Allocation_only_km + Advisory_allocation_km)/AU_GNIS_length_km * 100,0)) %>%
      dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                    TMDL_scope, Period, Source, Pollu_ID,
                    HUC6, HUC6_Name, HUC6_full,
                    HUC8, HUC8_Name, HUC8_full,
                    HUC10, HUC10_Name, HUC10_full,
                    AU_ID, AU_Name, AU_GNIS_Name, AU_GNIS,
                    TMDL_length_km, Allocation_only_km, Advisory_allocation_km,
                    AU_GNIS_length_km,
                    TMDL_AU_GNIS_Percent, Allocation_AU_GNIS_Percent) %>%
      as.data.frame()

    # Remove the old rows and update with new ones
    # CAREFUL HERE, overwrites tmdl_au_gnis
    tmdl_au_gnis <- odeqtmdl::tmdl_au_gnis %>%
      dplyr::filter(!(action_id %in% update_action_ids)) %>%
      rbind(tmdl_au_gnis_update) %>%
      dplyr::distinct() %>%
      dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, AU_GNIS) %>%
      as.data.frame()

    # Save a copy in data folder (replaces existing)
    save(tmdl_au_gnis, file = file.path(package_path, "data", "tmdl_au_gnis.rda"))

    #- tmdl_au --------------------------------------------------------------------

    cat("-- tmdl_au\n")

    tmdl_au_update <- tmdl_reaches_update %>%
      dplyr::filter(!is.na(TMDL_scope)) %>%
      dplyr::group_by(action_id, AU_ID, TMDL_pollutant) %>%
      dplyr::mutate(Source = dplyr::case_when(all(c("Both") %in% Source) ~ "Both",
                                              all(c("Point source") %in% Source) ~ "Point source",
                                              all(c("Nonpoint source") %in% Source) ~ "Nonpoint source",
                                              all(c("Point source", "Nonpoint source") %in% Source) ~ "Point source",
                                              all(c("Nonpoint source", NA_character_) %in% Source) ~ "Nonpoint source",
                                              all(c("Point source", NA_character_) %in% Source) ~ "Point source",
                                              TRUE ~ NA_character_)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(action_id, AU_ID, TMDL_wq_limited_parameter) %>%
      dplyr::mutate(Period = dplyr::case_when(TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                                length(unique(na.omit(Period))) > 1 ~ paste0("Mixed (",paste0(sort(unique(na.omit(Period))), collapse = ", "),")"),
                                              TMDL_wq_limited_parameter %in% c("Temperature", "Dissolved Oxygen") &
                                                length(unique(na.omit(Period))) == 1 ~ paste0(sort(unique(na.omit(Period))), collapse = ", "),
                                              TRUE ~ NA_character_)) %>%
      dplyr::ungroup() %>%
      dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                    TMDL_scope, Period, Source, Pollu_ID,
                    HUC6, HUC6_Name, HUC6_full,
                    HUC8, HUC8_Name, HUC8_full,
                    HUC10, HUC10_Name, HUC10_full,
                    AU_ID, AU_Name, AU_Description,
                    LengthKM) %>%
      rbind(AU_WB_update) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = "TMDL_scope", values_from = "LengthKM",
                         values_fn = sum, values_fill = 0) %>%
      dplyr::bind_rows(dplyr::tibble(TMDL = numeric(),
                                     "Allocation only" = numeric(),
                                     "Advisory allocation" = numeric())) %>%
      dplyr::rename(TMDL_length_km = TMDL,
                    Allocation_only_km = "Allocation only",
                    Advisory_allocation_km = "Advisory allocation") %>%
      dplyr::mutate(TMDL_length_km = tidyr::replace_na(TMDL_length_km, 0),
                    Allocation_only_km = tidyr::replace_na(Allocation_only_km, 0),
                    Advisory_allocation_km = tidyr::replace_na(Advisory_allocation_km, 0)) %>%
      dplyr::left_join(or_au, by = "AU_ID") %>%
      dplyr::mutate(TMDL_scope = dplyr::case_when(TMDL_length_km > 0 ~ "TMDL",
                                                  Allocation_only_km > 0 ~ "Allocation only",
                                                  Advisory_allocation_km > 0 ~ "Advisory allocation",
                                                  TRUE ~ NA_character_),
                    TMDL_AU_Percent = round(TMDL_length_km/AU_length_km * 100,0),
                    Allocation_AU_Percent = round((Allocation_only_km + Advisory_allocation_km)/AU_length_km * 100,0)) %>%
      dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                    TMDL_scope, Period, Source, Pollu_ID,
                    HUC6, HUC6_Name, HUC6_full,
                    HUC8, HUC8_Name, HUC8_full,
                    HUC10, HUC10_Name, HUC10_full,
                    AU_ID, AU_Name, AU_Description,
                    TMDL_length_km, Allocation_only_km, Advisory_allocation_km,
                    AU_length_km,
                    TMDL_AU_Percent, Allocation_AU_Percent) %>%
      as.data.frame()

    # Remove the old rows and update with new ones
    # CAREFUL HERE, overwrites tmdl_au
    tmdl_au <- odeqtmdl::tmdl_au %>%
      dplyr::filter(!(action_id %in% update_action_ids)) %>%
      rbind(tmdl_au_update) %>%
      dplyr::distinct() %>%
      dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID) %>%
      as.data.frame()

    # Save a copy in data folder (replaces existing)
    save(tmdl_au, file = file.path(package_path, "data", "tmdl_au.rda"))

    #- tmdl_parameters ----------------------------------------------------------

    cat("-- tmdl_parmeters\n")

    # Logic to assign "Not Active" TMDL status. This is already attributed in
    # master spreadsheet but leaving here just in case.

    # TMDL_status = case_when(action_id %in% c("2043", "1230", "2021",
    #                                          "10007", "42375",
    #                                          "OR_TMDL_20171219",
    #                                          "OR_TMDL_20191122") ~ "Not Active",
    #                         action_id == "1936" & TMDL_pollutant %in% c("Total Phosphorus",
    #                                                                     "Ammonia Nitrogen (NH3-N)") ~ "Not Active",
    #                         action_id == "30674" & TMDL_pollutant %in% c("Mercury (total)",
    #                                                                      "Methylmercury") ~ "Not Active",
    #                         TRUE ~ TMDL_status),


    tmdl_parameters_tbl <- readxl::read_excel(path = file.path(xlsx_template),
                                              sheet = "tmdl_parameters",
                                              na = c("", "NA"),
                                              col_names = TRUE, skip = 1,
                                              col_types = c("text", "text", "numeric", "text", "text",
                                                            "text", "text", "text")) %>%
      dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                    TMDL_status, revision_action_id, TMDL_status_comment) %>%
      dplyr::distinct()

    tmdl_parameters_update <- tmdl_reaches_update %>%
      dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant) %>%
      dplyr::distinct() %>%
      dplyr::mutate(TMDL_mapped = TRUE) %>%
      dplyr::right_join(tmdl_parameters_tbl, by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
      dplyr::select(action_id, TMDL_wq_limited_parameter, TMDL_pollutant,
                    TMDL_status, revision_action_id, TMDL_status_comment, TMDL_mapped) %>%
      dplyr::mutate(TMDL_mapped = ifelse(is.na(TMDL_mapped), FALSE, TMDL_mapped)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant) %>%
      as.data.frame()

    # This updates the whole dataframe
    tmdl_parameters <- odeqtmdl::tmdl_parameters %>%
      dplyr::filter(!action_id %in% update_action_ids) %>%
      rbind(tmdl_parameters_update) %>%
      dplyr::distinct() %>%
      dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant) %>%
      as.data.frame()

    # Save a copy in data folder (replaces existing)
    save(tmdl_parameters, file = file.path(package_path, "data", "tmdl_parameters.rda"))

  }

  cat("Complete\n")

}
