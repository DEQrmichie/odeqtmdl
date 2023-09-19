#' Export TMDLs to a GeoPackage
#'
#' Export active TMDLs from the TMDL reach database to a GeoPackage (.gpkg). This allows the data to
#' be viewed spatially in a GIS. The TMDL database is an inventory where non-tribal
#' Oregon TMDLs have been developed. The output does not includes TMDLs that have been rescinded or replaced with newer TMDLs.
#' Note the inventory is still being developed and some information may not be accurate.
#' See each relevant TMDL document for more information and applicability.
#' The full TMDL reach database is currently only available to Oregon DEQ employee.
#'
#' The TMDL database is georeferenced to the National Hydrography Dataset (NHD).
#' The NHD field attributes corresponds to NHDH_OR_931v220, which is the current
#' version used for DEQ business data. Oregon's Assessment Unit fields are current as of 03-30-2022.
#'
#' Output GeoPackage fields include:
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item TMDL_name:	Name of TMDL document.
#'   \item TMDL_issue_year:	Year the TMDL was issued by the issue agency.
#'   \item TMDL_wq_limited_parameters:	Name of the water quality limited 303(d) parameter that the TMDL addresses.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item TMDL_active: Boolean to indicate if the TMDL and TMDL allocations are effective and being implemented.
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
#'   \item in_attains:	Boolean to indicate if the TMDL action has been entered into USEPA's ATTAINS database.
#'   \item attains_status: Status of TMDL action in ATTAINS. NA indicates the TMDL action is not included in ATTAINS.
#'   \item citation_abbreviated: Abbreviated citation of TMDL document using DEQ style guidelines (Chicago Manual of Style).
#'   \item citation_full: Full citation of TMDL document using DEQ style guidelines (Chicago Manual of Style).
#'   \item HUC_6: Basin six digit USGS hydrological unit code.
#'   \item HU_6_NAME: USGS Basin name.
#'   \item HUC6_full: Concatenation of the HUC_6 and HU_6_NAME fields.
#'   \item HUC_8: Subbasin six digit USGS hydrological unit code.
#'   \item HU_8_NAME: USGS Subbasin name.
#'   \item HUC8_full: Concatenation of the HUC_8 and HU_8_NAME fields.
#'   \item Permanent_Identifier: NHD Permanent Identifier.
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
#' @param gpkg_dsn data source name. The path and name of output GeoPackage database. Passed to 'dsn' argument in \code{\link[sf]{st_write}}.
#' @param gpkg_layer layer name. The name of the output layer in the GeoPackage database. Passed to 'layer' argument in \code{\link[sf]{st_write}}.
#' @param tmdl_reaches Oregon TMDL reach database. The database is currently only available to Oregon DEQ employees.
#' @param nhd_fc The NHD feature class that the TMDL database will be attributed to. The "Permanent_Identifier" field is used for joining.
#' @param action_ids vector of TMDL action IDs used to filter the TMDLs. Default is NULL and all TMDLs are included.
#' @param TMDL_param vector of water quality  parameter names used to filter the TMDLs. The output will include TMDLs that addressed that water quality parameter. Default is NULL and all parameters are included.
#' @param TMDL_pollu vector of TMDL pollutant parameter names used to filter the TMDLs. The output will include TMDLs that addressed that pollutant parameter. Default is NULL and all pollutants are included.
#'
#' @export
#' @keywords Oregon TMDL reach database

tmdl_export_gpkg <- function(gpkg_dsn, gpkg_layer, tmdl_reaches, nhd_fc,
                             action_ids = NULL, TMDL_param = NULL, TMDL_pollu = NULL) {

  df <- tmdl_reaches %>%
    dplyr::filter(TMDL_active)

  # Filter to action IDs
  if ((!is.null(action_ids)) {
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

  df <- df %>%
    dplyr::mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
    dplyr::select(-Permanent_Identifier, -AU_ID)

  tmdl_reach_fc_param <- nhd_fc %>%
    dplyr::select(AU_ID, Permanent_Identifier, WBArea_Permanent_Identifier, FType, AU_WBType) %>%
    dplyr::mutate(PIDAUID = paste0(Permanent_Identifier, ";", AU_ID)) %>%
    dplyr::inner_join(y = df, by = "PIDAUID") %>%
    dplyr::select(action_id,
                  TMDL_name,
                  TMDL_issue_year,
                  TMDL_wq_limited_parameter,
                  TMDL_pollutant,
                  TMDL_active,
                  TMDL_scope,
                  Period,
                  in_attains,
                  attains_status,
                  citation_abbreviated,
                  citation_full,
                  HUC_6,
                  HU_6_NAME,
                  HUC6_full,
                  HUC_8,
                  HU_8_NAME,
                  HUC8_full,
                  Permanent_Identifier,
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

  sf::st_write(tmdl_reach_fc_param,
               dsn = gpkg_dsn,
               layer = gpkg_layer,
               driver = "GPKG",
               delete_layer = TRUE)

}
