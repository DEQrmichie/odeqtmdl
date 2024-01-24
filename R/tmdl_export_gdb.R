#' Export TMDLs to a ESRI geodatabase feature
#'
#' Export active TMDLs from the TMDL reach database to a geodatabase (.gdb). This allows the data to
#' be viewed spatially in a GIS. The TMDL database is an inventory where non-tribal
#' Oregon TMDLs have been developed. The output does not includes TMDLs that have been rescinded or replaced with newer TMDLs.
#' Note the inventory is still being developed and some information may not be accurate.
#' See each relevant TMDL document for more information and applicability.
#'
#' The TMDL database is georeferenced to the National Hydrography Dataset (NHD).
#' The NHD field attributes corresponds to NHDH_OR_931v220, which is the current
#' version used for DEQ business data. Oregon's Assessment Unit fields are current as of 03-30-2022.
#'
#' Output Geodatabase feature fields include:
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item TMDL_name:	Name of TMDL document and abbreviated citation of TMDL.
#'   \item TMDL_wq_limited_parameters:	Name of the water quality limited 303(d) parameter that the TMDL addresses.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item TMDL_status: Status of TMDL for the parameter and pollutant.
#'   \itemize{
#'        \item Active: TMDL has been approved by EPA and is active.
#'        \item Not Active: TMDL has been withdrawn, disapproved by EPA, and/or replaced with a newer TMDL.
#'        \item In Development: TMDL is in development.
#'   }
#'   \item TMDL_scope: Provides information about how the TMDL applies.
#'      \itemize{
#'      \item TMDL:	Identifies segments that a TMDL was developed for.
#'      \item Allocation only: Identifies segments where a TMDL allocation applies
#'                            but the TMDL does not address a 303(d) listing in that segment.
#'                            Typically this situation is applicable for tributaries or canals
#'                            that are upstream of the segment where the "TMDL" applies.
#'                            The pollutant reduction in the upstream segment is needed to achieve the
#'                            TMDL loading capacity of the downstream segment.
#'                            This is common for TMDLs that address narrative water quality standards
#'                            or cases when the pollutant is not the same as the listed parameter (e.g. nutrients).
#'      \item Advisory allocation: Identifies segments that have suggested non regulatory allocations;
#'                                or segments that may be used to assess progress or status of allocation attainment
#'                                but the segment is not the regulatory compliance point as defined in the TMDL.
#'                                }
#'   \item Period: Identifies the fish use period TMDLs for temperature or dissolved oxygen address.
#'      \itemize{
#'      \item year_round: TMDL developed to address only non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      \item spawning: TMDL developed to address only spawning uses for the temperature or dissolved oxygen water quality standards.
#'      \item Both: TMDL developed to address both spawning and non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      }
#'   \item geo_id: Unique ID assigned to the NHD reaches where a TMDL target applies. ID is structured as YearTMDLissued_ShortTMDLdocName_TargetGeoArea.
#'   \item citation_abbreviated: Abbreviated citation of TMDL document using DEQ style guidelines (Chicago Manual of Style).
#'   \item citation_full: Full citation of TMDL document using DEQ style guidelines (Chicago Manual of Style).
#'   \item HUC6: Basin six digit USGS hydrological unit code.
#'   \item HUC6_Name: USGS Basin name.
#'   \item HUC6_full: Concatenation of the HUC_6 and HU_6_NAME fields.
#'   \item HUC8: Subbasin six digit USGS hydrological unit code.
#'   \item HUC8_Name: USGS Subbasin name.
#'   \item HUC8_full: Concatenation of the HUC_8 and HU_8_NAME fields.
#'   \item GLOBALID: Unique ID for every NHD reach record. Used for joins with TMDL GIS features.
#'   \item Permanent_Identifier: NHD Permanent Identifier.
#'   \item ReachCode: NHD Reach code.
#'   \item WBArea_Permanent_Identifier: NHD Waterbody feature Permanent Identifier
#'   \item FType: Three-digit integer value; unique identifier of a feature type.
#'   \item GNIS_Name: Proper name, specific term, or expression by which a particular geographic entity is known
#'   \item GNIS_ID: Unique identifier assigned by GNIS, length 10.
#'   \item AU_ID:	Assessment Unit ID.
#'   \item AU_Name: Name of the assessment unit.
#'   \item AU_Description: Assessment unit descriptions.
#'   \item AU_WBType: Assessment unit waterbody type code
#'   \item AU_GNIS_Name: Assessment unit and GNIS name concatenation.
#'   \item AU_GNIS: Same as GNIS name but with a few additional names not in NHD.
#'   \item LengthKM: Length of linear feature based on Albers Equal Area.
#' }
#'
#' @param gdb_path data source name. The path and name of output GeoPackage database. Passed to 'dsn' argument in \code{\link[sf]{st_write}}.
#' @param gdb_layer layer name. The name of the output layer in the GeoPackage database. Passed to 'layer' argument in \code{\link[sf]{st_write}}.
#' @param tmdl_reaches Oregon TMDL reach table. The database is currently only available to Oregon DEQ employees.
#' @param tmdl_actions Oregon TMDL action table. Default is NULL and \code{\link{tmdl_actions}} will be used.
#' @param nhd_fc The NHD feature class that the TMDL database will be attributed to. The "Permanent_Identifier" field is used for joining.
#' @param action_ids vector of TMDL action IDs used to filter the TMDLs. Default is NULL and all TMDLs are included.
#' @param TMDL_param vector of water quality  parameter names used to filter the TMDLs. The output will include TMDLs that addressed that water quality parameter. Default is NULL and all parameters are included.
#' @param TMDL_pollu vector of TMDL pollutant parameter names used to filter the TMDLs. The output will include TMDLs that addressed that pollutant parameter. Default is NULL and all pollutants are included.
#' @param collapse logical. If TRUE (the default), the values in action id, TMDL_name, TMDL_wq_limited_parameter, and TMDL_pollutant are collapsed (pasted together) into a string separated by a semi-colon if they share a common GLOBALID, HUC_6, HUC_8, and TMDL_status value. Each field is collapsed separately.
#'
#' @export
#' @keywords Oregon TMDL reach database

tmdl_export_gdb <- function(gdb_path, gdb_fc, tmdl_reaches, tmdl_actions = NULL, nhd_fc,
                             action_ids = NULL, TMDL_param = NULL, TMDL_pollu = NULL,
                             collapse = TRUE) {

  tmdl_status <- "Active"

  df <- tmdl_reaches %>%
    dplyr::left_join(odeqtmdl::tmdl_parameters[,c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant", "TMDL_status")],
                     by = c("action_id", "TMDL_wq_limited_parameter", "TMDL_pollutant")) %>%
    dplyr::filter(TMDL_status == tmdl_status)

  if (is.null(tmdl_actions)) {
    tmdl_actions_tbl <- odeqtmdl::tmdl_actions
  }

  # Filter to action IDs
  if (!is.null(action_ids)) {
    df <- df %>%
      dplyr::filter(action_id %in% action_ids)
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

  if (collapse) {

    df <- df %>%
      dplyr::left_join(tmdl_actions_tbl, by = "action_id") %>%
      dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
      group_by(GLOBALID, HUC6, HUC6_Name, HUC6_full, HUC8, HUC8_Name, HUC8_full, TMDL_status) %>%
      summarize(action_id = paste((unique(action_id)), collapse = "; "),
                TMDL_name = paste((unique(TMDL_name)), collapse = "; "),
                TMDL_wq_limited_parameter = paste(sort(unique(TMDL_wq_limited_parameter)), collapse = "; "),
                TMDL_pollutant = paste(sort(unique(TMDL_pollutant)), collapse = "; "),
                TMDL_scope = paste(sort(unique(TMDL_scope)), collapse = "; "),
                Period = paste(sort(unique(Period)), collapse = "; "),
                geo_id = paste(sort(unique(geo_id)), collapse = "; "))

    tmdl_reach_fc_param <- nhd_fc %>%
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
      dplyr::inner_join(y = df, by = "GLOBALID") %>%
      dplyr::select(action_id,
                    TMDL_name,
                    TMDL_wq_limited_parameter,
                    TMDL_pollutant,
                    TMDL_status,
                    TMDL_scope,
                    Period,
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

  } else {

    df <- df %>%
      dplyr::left_join(tmdl_actions_tbl, by = "action_id")

    tmdl_reach_fc_param <- nhd_fc %>%
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
      dplyr::mutate(TMDL_name = paste0(TMDL_name," (",citation_abbreviated,")")) %>%
      dplyr::inner_join(y = df, by = "GLOBALID") %>%
      dplyr::select(action_id,
                    TMDL_name,
                    TMDL_wq_limited_parameter,
                    TMDL_pollutant,
                    TMDL_status,
                    TMDL_scope,
                    Period,
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

  }

  arcgisbinding::arc.check_product()
  arcgisbinding::arc.write(path = file.path(gdb_path, gdb_fc),
            data = tmdl_reach_fc_param, validate = TRUE,
            overwrite = TRUE)

}
