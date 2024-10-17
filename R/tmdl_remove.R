#' Remove information from the TMDL package tables
#'
#' Removes TMDL information from the odeqtmdl package tables. Removes all rows
#' in existing package tables with the same 'action_id' identified
#' in \code{action_ids}. There is no filtering for specific parameters or pollutants. All
#' information attributed to the action_id is removed. Use \code{\link{tmdl_update}}
#' to update or add new information. Removes rows from the following package tables:
#' \itemize{
#'      \item tmdl_actions
#'      \item tmdl_targets
#'      \item tmdl_geo_id
#'      \item tmdl_wqstd
#'      \item tmdl_reaches
#'      \item tmdl_au
#'      \item tmdl_au_gnis
#'      \item tmdl_parameters
#'      }
#'
#' @param action_ids vector of TMDL action IDs to be removed. Required.
#' @param package_path Path to the top level directory of the odeqtmdl R package. The 'data', data_raw', and 'inst/extdata' folders must exist.
#'
#' @export

tmdl_remove <- function(action_ids = NULL, package_path) {

  # Check to make sure paths to path, data, data_raw, and inst/extdata exist
  if (!file.exists(package_path)) {
    stop(paste0("Error. Path in 'package_path' not found: ", package_path) )
  }

  if (!file.access(package_path, mode = 2)[[1]] == 0)  {
    stop(paste0("Error. Write access permission not granted for 'package_path': ", package_path))
  }

  if (!file.exists(file.path(package_path, "data"))) {
    stop(paste0("Error. There must be a 'data' folder in 'package_path': ", file.path(package_path, "data")) )
  }

  if (!file.exists(file.path(package_path, "data_raw"))) {
    stop(paste0("Error. There must be a 'data_raw' folder in 'package_path': ", file.path(package_path, "data_raw")) )
  }

  if (!file.exists(file.path(package_path, "inst", "extdata"))) {
    stop(paste0("Error. There must be 'inst/extdata' folders in 'package_path': ", file.path(package_path, "inst", "extdata")) )
  }

  if (is.null(action_ids)) {
    stop("Error. Please identify the action_ids to remove")
  }

  remove_action_ids <- unique(action_ids)

  #- tmdl_actions --------------------------------------------------------------
  cat("-- tmdl_actions\n")

  # This updates the whole dataframe
  tmdl_actions <- odeqtmdl::tmdl_actions %>%
    dplyr::filter(!action_id %in% remove_action_ids) %>%
    dplyr::distinct() %>%
    dplyr::arrange(TMDL_issue_date,
                   TMDL_name) %>%
    as.data.frame()

  # Save a copy in data folder (replaces existing)
  save(tmdl_actions, file = file.path(package_path, "data", "tmdl_actions.rda"))

  #- tmdl_geo_ids ------------------------------------------------------------

  cat("-- tmdl_geo_ids\n")

  tmdl_geo_ids <- odeqtmdl::tmdl_geo_ids %>%
    dplyr::filter(!action_id %in% remove_action_ids) %>%
    dplyr::distinct() %>%
    dplyr::arrange(action_id, geo_id) %>%
    as.data.frame()

  # Save a copy in data folder (replaces existing)
  save(tmdl_geo_ids, file = file.path(package_path, "data", "tmdl_geo_ids.rda"))

  #- tmdl_targets ------------------------------------------------------------

  tmdl_targets <- odeqtmdl::tmdl_targets %>%
    dplyr::filter(!action_id %in% remove_action_ids) %>%
    dplyr::distinct() %>%
    dplyr::arrange(geo_id) %>%
    as.data.frame()

  # Save a copy in data folder (replaces existing)
  save(tmdl_targets, file = file.path(package_path, "data", "tmdl_targets.rda"))

  #- tmdl_WLA-----------------------------------------------------------------

  tmdl_wla <- odeqtmdl::tmdl_wla %>%
    dplyr::filter(!action_id %in% remove_action_ids) %>%
    dplyr::distinct() %>%
    dplyr::arrange(action_id, facility_name) %>%
    as.data.frame()

  # Save a copy in data folder (replaces existing)
  save(tmdl_wla, file = file.path(package_path, "data", "tmdl_wla.rda"))

  #- tmdl_wqstd ----------------------------------------------------------------

  cat("-- tmdl_wqstd\n")

  tmdl_wqstd <- odeqtmdl::tmdl_wqstd %>%
    dplyr::filter(!action_id %in% remove_action_ids) %>%
    dplyr::distinct() %>%
    dplyr::arrange(action_id, Pollu_ID, wqstd_code) %>%
    as.data.frame()

  # Save a copy in data folder (replaces existing)
  save(tmdl_wqstd, file = file.path(package_path, "data", "tmdl_wqstd.rda"))

  #- tmdl_reaches ------------------------------------------------------------

  cat("-- tmdl_reaches\n")

  tmdl_reaches <- odeqtmdl::tmdl_reaches() %>%
    dplyr::filter(!(action_id %in% remove_action_ids)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, ReachCode) %>%
    as.data.frame()

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

  tmdl_au_gnis <- odeqtmdl::tmdl_au_gnis %>%
    dplyr::filter(!(action_id %in% remove_action_ids)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID, AU_GNIS) %>%
    as.data.frame()

  # Save a copy in data folder (replaces existing)
  save(tmdl_au_gnis, file = file.path(package_path, "data", "tmdl_au_gnis.rda"))

  #- tmdl_au --------------------------------------------------------------------

  cat("-- tmdl_au\n")

  tmdl_au <- odeqtmdl::tmdl_au %>%
    dplyr::filter(!(action_id %in% remove_action_ids)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant, AU_ID) %>%
    as.data.frame()

  # Save a copy in data folder (replaces existing)
  save(tmdl_au, file = file.path(package_path, "data", "tmdl_au.rda"))

  #- tmdl_parameters ----------------------------------------------------------

  cat("-- tmdl_parmeters\n")

  tmdl_parameters <- odeqtmdl::tmdl_parameters %>%
    dplyr::filter(!action_id %in% remove_action_ids) %>%
    dplyr::distinct() %>%
    dplyr::arrange(action_id, TMDL_wq_limited_parameter, TMDL_pollutant) %>%
    as.data.frame()

  # Save a copy in data folder (replaces existing)
  save(tmdl_parameters, file = file.path(package_path, "data", "tmdl_parameters.rda"))

  cat("Complete\n")
}
