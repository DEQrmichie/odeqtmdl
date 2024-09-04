#' Export TMDL info to ATTAINS upload files
#'
#' Exports all or a subset of the Oregon TMDL assessment unit data table (\code{\link{tmdl_au}})
#' as csv files formatted for batch upload into ATTAINS. The actions, parameters, and pollutants csv files are produced.
#' The format is based on EPA's TMDL action template version 1.4, released on 2022-07-18.
#'
#' @param out_dir: The directory to save the output csv files.
#' @param df_tmdl_actions: Data frame formatted the same as \code{\link{tmdl_actions}}, Default is NULL and the \code{\link{tmdl_actions}} data table will be used.
#' @param df_tmdl_au: Data frame formatted the same as \code{\link{tmdl_au}}. Default is NULL and the \code{\link{tmdl_au}} data table will be used.
#' @param df_tmdl_wla: Data frame formatted the same as \code{\link{tmdl_wla}}. Default is NULL and the \code{\link{tmdl_wla}} data table will be used.
#' @param AU_IDs: Vector of assessment units used to filter 'tmdl_au'. Default is NULL and all AU IDs are included.
#' @param status_attains: Vector of the attains status used to filter 'tmdl_au'. Default is NULL and all statuses are included.
#' @param action_ids: Vector of action IDs used to filter 'tmdl_au'. Default is NULL and all action IDs are included.
#' @param TMDL_param: Vector of water quality  parameter names used to filter 'tmdl_au'. The output will include TMDLs that addressed that water quality parameter. Default is NULL and all parameters are included.
#' @param TMDL_pollu: Vector of TMDL pollutant parameter names used to filter the TMDLs. The output will include TMDLs that addressed that pollutant parameter. Default is NULL and all pollutants are included.
#' @export
#' @keywords Oregon TMDL ATTAINS batch upload

tmdl_export_attains <- function(out_dir,
                                df_tmdl_actions = NULL,
                                df_tmdl_au = NULL,
                                df_tmdl_wla = NULL,
                                AU_IDs = NULL,
                                status_attains = NULL,
                                action_ids = NULL,
                                TMDL_param = NULL,
                                TMDL_pollu = NULL) {

  if (is.null(df_tmdl_actions)) {
    df_actions <- odeqtmdl::tmdl_actions
  } else {
    df_actions <- df_tmdl_actions
  }

  if (is.null(df_tmdl_au)) {
    df_au <- odeqtmdl::tmdl_au
  } else {
    df_au <- df_tmdl_au
  }

  if (is.null(df_tmdl_wla)) {
    df_wla <- odeqtmdl::tmdl_wla
  } else {
    df_wla <- df_tmdl_wla
  }

  # Filter to TMDL scope
  df_au <- dplyr::filter(df_au, TMDL_scope == "TMDL")

  df <- dplyr::left_join(df_au, df_actions, by = c("action_id"))

  # Filter to action IDs
  if (!is.null(action_ids)) {
    df <- df %>%
      dplyr::filter(action_id %in% action_ids)

    df_wla <- df_wla %>%
      dplyr::filter(action_id %in% action_ids)
  }

  # Filter to in attains status
  if (!is.null(status_attains)) {
    df <- df %>%
      dplyr::filter(attains_status %in% status_attains)
  }

  # Filter to AU_IDs
  if (!is.null(AU_IDs)) {
    df <- df %>%
      dplyr::filter(AU_ID %in% AU_IDs)

    df_wla <- df_wla %>%
      dplyr::filter(AU_ID %in% AU_IDs)
  }

  # Filter both TMDL param and TMDL pollu
  if ((!is.null(TMDL_param) & !is.null(TMDL_pollu))) {
    df <- df %>%
      dplyr::filter(TMDL_wq_limited_parameter %in% TMDL_param | TMDL_pollutant %in% TMDL_pollu)
  }

  # TMDL param only
  if ((!is.null(TMDL_param) & is.null(TMDL_pollu))) {
    df <- df %>%
      dplyr::filter(TMDL_wq_limited_parameter %in% TMDL_param)
  }

  # TMDL pollu only
  if ((is.null(TMDL_param) & !is.null(TMDL_pollu))) {
    df <- df %>%
      dplyr::filter(TMDL_pollutant %in% TMDL_pollu)
  }

  # TMDL pollu only for WLA
  if (!is.null(TMDL_pollu)) {
    df_wla <- df_wla %>%
      dplyr::filter(TMDL_pollutant %in% TMDL_pollu)
  }

  df_pollu <- odeqtmdl::LU_pollutant

  # - Actions --------------------------------------------------------------------

  actions_csv <- df %>%
    dplyr::mutate(AGENCY_CODE = "S",
                  ACTION_TYPE = "TMDL",
                  ACTION_STATUS = "Draft",
                  ACTION_COMMENT = TMDL_comment,
                  TMDL_OTHER_IDENTIFIER = NA_character_,
                  INDIAN_COUNTRY_INDICATOR = NA_character_) %>%
    dplyr::rename(ACTION_ID = action_id,
                  ACTION_NAME = TMDL_name,
                  COMPLETION_DATE = TMDL_issue_date) %>%
    dplyr::select(ACTION_ID,
                  ACTION_NAME,
                  AGENCY_CODE,
                  ACTION_TYPE,
                  ACTION_STATUS,
                  COMPLETION_DATE,
                  ACTION_COMMENT,
                  TMDL_OTHER_IDENTIFIER,
                  INDIAN_COUNTRY_INDICATOR) %>%
    dplyr::distinct() %>%
    dplyr::arrange(ACTION_ID)

  write.csv(x = actions_csv, file = file.path(out_dir, "Actions.csv"),
            row.names = FALSE, na = "", fileEncoding = "UTF-8")

  # - Pollutants -----------------------------------------------------------------

  pollu_csv <- df %>%
    dplyr::left_join(df_pollu, by = c("TMDL_pollutant" = "Pollutant_DEQ")) %>%
    dplyr::mutate(EXPLICIT_MARGIN_OF_SAFETY = NA_character_,
                  IMPLICIT_MARGIN_OF_SAFETY = NA_character_,
                  TMDL_END_POINT = NA_character_) %>%
    dplyr::rename(ACTION_ID = action_id,
                  ASSESSMENT_UNIT_ID = AU_ID,
                  POLLUTANT_NAME = Attains_Pollutant,
                  POLLUTANT_SOURCE_TYPE = Source) %>%
    dplyr::select(ACTION_ID,
                  ASSESSMENT_UNIT_ID,
                  POLLUTANT_NAME,
                  POLLUTANT_SOURCE_TYPE,
                  EXPLICIT_MARGIN_OF_SAFETY,
                  IMPLICIT_MARGIN_OF_SAFETY,
                  TMDL_END_POINT) %>%
    dplyr::distinct() %>%
    dplyr::arrange(ACTION_ID,
                   ASSESSMENT_UNIT_ID,
                   POLLUTANT_NAME)

  write.csv(x = pollu_csv, file = file.path(out_dir, "Pollutants.csv"),
            row.names = FALSE, na = "", fileEncoding = "UTF-8")

  # - Parameter ------------------------------------------------------------

  param_csv <- df %>%
    dplyr::left_join(df_pollu, by = c("TMDL_pollutant" = "Pollutant_DEQ")) %>%
    dplyr::rename(ACTION_ID = action_id,
                  ASSESSMENT_UNIT_ID = AU_ID,
                  ASSOCIATED_POLLUTANT = Attains_Pollutant) %>%
    dplyr::left_join(df_pollu, by = c("TMDL_wq_limited_parameter" = "Pollutant_DEQ")) %>%
    dplyr::rename(PARAMETER_NAME = Attains_Pollutant) %>%
    dplyr::select(ACTION_ID,
                  ASSESSMENT_UNIT_ID,
                  ASSOCIATED_POLLUTANT,
                  PARAMETER_NAME) %>%
    dplyr::distinct() %>%
    dplyr::arrange(ACTION_ID,
                   PARAMETER_NAME,
                   ASSOCIATED_POLLUTANT,
                   ASSESSMENT_UNIT_ID)

  write.csv(x = param_csv, file = file.path(out_dir, "Parameters.csv"),
            row.names = FALSE, na = "", fileEncoding = "UTF-8")

  # - Permit ---------------------------------------------------------------

  permit_csv <- df_wla %>%
    dplyr::left_join(df_pollu, by = c("TMDL_pollutant" = "Pollutant_DEQ")) %>%
    dplyr::mutate(WASTE_LOAD_ALLOCATION = NA_character_,
                  WASTE_LOAD_ALLOCATION_UNIT = NA_character_,
                  SEASON_START = NA_character_,
                  SEASON_END = NA_character_) %>%
    dplyr::rename(ACTION_ID = action_id,
                  ASSESSMENT_UNIT_ID = AU_ID,
                  POLLUTANT_NAME = Attains_Pollutant,
                  NPDES_IDENTIFIER = EPANum,
                  OTHER_IDENTIFIER = WQFileNum) %>%
    dplyr::select(ACTION_ID,
                  ASSESSMENT_UNIT_ID,
                  POLLUTANT_NAME,
                  NPDES_IDENTIFIER,
                  OTHER_IDENTIFIER,
                  WASTE_LOAD_ALLOCATION,
                  WASTE_LOAD_ALLOCATION_UNIT,
                  SEASON_START,
                  SEASON_END)

  write.csv(x = permit_csv, file = file.path(out_dir, "Permits.csv"),
            row.names = FALSE, na = "", fileEncoding = "UTF-8")

  # - README ---------------------------------------------------------------

  readme_txt <- c(paste0("ATTAINS batch upload csv files were generated using the odeqtmdl R package version ", packageVersion('odeqtmdl'),"."),
                  "",
                  paste0("Export on ", Sys.time())
                  )

  file.create(file.path(out_dir,"README.txt"))

  writeLines(text = readme_txt, con = file.path(out_dir,"README.txt"))


}


