#' Export TMDL info to ATTAINS upload files
#'
#' Exports all or a subset of the Oregon TMDL assessment unit database (\code{\link{tmdl_aus}}) as csv files formatted for batch upload into ATTAINS. The actions, parameters, and pollutants csv files are produced. The format is based on EPA's tmdl_action_batchupload_template_2022-07-18.
#'
#' @param out_dir: The directory to save the output csv files.
#' @param tmdl_aus: Data frame formatted the same as the Oregon TMDL assessment unit database. Default is NULL and the \code{\link{tmdl_aus}} database will be used.
#' @param AU_IDs: Vector of assessment units used to filter 'tmdl_aus'. Default is NULL and all AU IDs are included.
#' @param status_attains: Vector of the attains status used to filter 'tmdl_aus'. Default is NULL and all statuses are included.
#' @param action_ids: Vector of action IDs used to filter 'tmdl_aus'. Default is NULL and all action IDs are included.
#' @param TMDL_param: Vector of water quality  parameter names used to filter 'tmdl_aus'. The output will include TMDLs that addressed that water quality parameter. Default is NULL and all parameters are included.
#' @param TMDL_pollu: Vector of TMDL pollutant parameter names used to filter the TMDLs. The output will include TMDLs that addressed that pollutant parameter. Default is NULL and all pollutants are included.
#' @export
#' @keywords Oregon TMDL ATTAINS batch upload

tmdl_export_attains <- function(out_dir, tmdl_aus = NULL, AU_IDs = NULL,
                                status_attains = NULL,
                                action_ids = NULL, TMDL_param = NULL, TMDL_pollu = NULL) {

  if (is.null(tmdl_aus)) {
    df <- odeqtmdl::tmdl_aus
  } else {
    df <- tmdl_aus
  }

  # Filter to action IDs
  if (!is.null(action_ids)) {
    df <- df %>%
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

  LU_pollu <- odeqtmdl::LU_pollutant

# - Actions --------------------------------------------------------------------

  actions_csv <- df %>%
    dplyr::mutate(AGENCY_CODE = "S",
                  ACTION_TYPE = "TMDL",
                  ACTION_STATUS = "Draft",
                  ACTION_COMMENT = NA_character_,
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
              row.names = FALSE, na = "")

# - Pollutants -----------------------------------------------------------------

    pollu_csv <- df %>%
      dplyr::left_join(LU_pollu, by = c("TMDL_pollutant" = "Pollutant_DEQ")) %>%
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
                row.names = FALSE, na = "")

      # - Parameter ------------------------------------------------------------------

      param_csv <- df %>%
        dplyr::left_join(LU_pollu, by = c("TMDL_pollutant" = "Pollutant_DEQ")) %>%
        dplyr::rename(ACTION_ID = action_id,
                      ASSESSMENT_UNIT_ID = AU_ID,
                      ASSOCIATED_POLLUTANT = Attains_Pollutant) %>%
      dplyr::left_join(LU_pollu, by = c("TMDL_wq_limited_parameter" = "Pollutant_DEQ")) %>%
        dplyr::rename(PARAMETER_NAME = Attains_Pollutant) %>%
      dplyr::select(ACTION_ID,
                    ASSESSMENT_UNIT_ID,
                    ASSOCIATED_POLLUTANT,
                    PARAMETER_NAME) %>%
        distinct() %>%
        dplyr::arrange(ACTION_ID,
                       PARAMETER_NAME,
                       ASSOCIATED_POLLUTANT,
                       ASSESSMENT_UNIT_ID)

      write.csv(x = param_csv, file = file.path(out_dir, "Parameters.csv"),
                row.names = FALSE, na = "")

}


