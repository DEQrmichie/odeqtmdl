#' Oregon TMDL database by NHD reach
#'
#' Inventory of where non-tribal Oregon TMDLs have been developed. Note the inventory
#' is still being developed and some information may not be accurate.
#' See each relevant TMDL document for more information and applicability. A full listing of all
#' non-tribal TMDL actions in Oregon can be viewed using \code{\link{tmdl_actions}}.
#'
#' Note: tmdl_reaches is large so it may take a minute to load.
#'
#' NHD values are derived from NHDH_OR_931v220, which is the current version used for DEQ business data. Database fields include:
#'
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
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
#'   \item Period: Identifies the fish use period that the TMDL addresses. Only used for TMDLs that address temperature or dissolved oxygen.
#'      \itemize{
#'      \item year_round: TMDL addresses only non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      \item spawning: TMDL addresses only spawning uses for the temperature or dissolved oxygen water quality standards.
#'      \item Both: TMDL addresses both spawning and non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      }
#'   \item Source:
#'      \itemize{
#'      \item Point source: Identifies assessment units where pollutant loading is from point sources only.
#'      \item Nonpoint source: Identifies assessment units where pollutant loading is from nonpoint sources only.
#'      \item Both: Identifies assessment units where pollutant loading is from point sources and nonpoint sources.
#'      }
#'   \item Pollu_ID: DEQ water quality parameter ID. ID is for the parameter in 'TMDL_wq_limited_parameter'.
#'   \item geo_id:	Unique ID assigned to the NHD reaches codes where a TMDL target applies. ID is structured as YearTMDLissued_ShortTMDLdocName_TargetGeoArea.
#'   \item HUC_6: Basin six digit USGS hydrological unit code.
#'   \item HU_6_NAME: USGS Basin name.
#'   \item HUC6_full: Concatenation of the HUC_6 and HU_6_NAME fields.
#'   \item HUC_8: Subbasin six digit USGS hydrological unit code.
#'   \item HU_8_NAME: USGS Subbasin name.
#'   \item HUC8_full: Concatenation of the HUC_8 and HU_8_NAME fields.
#'   \item Permanent_Identifier: NHD Permanent Identifier.
#'   \item ReachCode: NHD reach code.
#'   \item GNIS_Name: Proper name, specific term, or expression by which a particular geographic entity is known
#'   \item GNIS_ID: Unique identifier assigned by GNIS, length 10.
#'   \item AU_ID:	Assessment Unit ID.
#'   \item AU_Name: Name of the assessment unit.
#'   \item AU_Description: Assessment unit descriptions.
#'   \item AU_GNIS_Name: Assessment unit and GNIS name concatenation.
#'   \item AU_GNIS: Same as GNIS name but with a few additional names not in NHD.
#'   \item LengthKM: Length of linear feature based on Albers Equal Area.
#' }
#'
#' @keywords Oregon TMDL reach data table
#' @export
#' @return loads Rdata into environment

tmdl_reaches <- function(){

  warning(immediate. = TRUE, "Loading tmdl_reaches may take a minute")

  file_path <- system.file("extdata", "tmdl_reaches.RDS", package = "odeqtmdl",
                           mustWork = TRUE)

  df <- readRDS(file = file_path)

  return(df)

}







