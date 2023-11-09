#' Oregon TMDL database by assessment unit
#'
#' Inventory of Oregon Assessment Units (AUs) where non-tribal Oregon TMDLs have been developed. Note the inventory
#' is still being developed and some information may not be accurate.
#' See each relevant TMDL document for more information and applicability. A full listing of all
#' non-tribal TMDL actions in Oregon can be viewed using \code{\link{tmdl_actions}}.
#'
#' NHD values are derived from NHDH_OR_931v220, which is the current version used for DEQ business data. Database fields include:
#'
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item TMDL_wq_limited_parameters:	Name of the water quality limited 303(d) parameter that the TMDL addresses.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item TMDL_scope: Provides information about how the TMDL applies.
#'      \itemize{
#'      \item TMDL:	The TMDL was developed to address a 303(d) listing or future listing in this assessment unit.
#'      \item Allocation only: A TMDL allocation applies in this assessment unit
#'                            but the TMDL does not address a 303(d) listing or future listing.
#'                            Typically this situation is applicable for tributaries or canals
#'                            that are upstream of the reach where the "TMDL" applies.
#'                            The pollutant reduction in the upstream reach is needed to achieve the
#'                            TMDL loading capacity of the downstream reach.
#'      \item Advisory allocation: A TMDL allocation may be applied at the discretion of DEQ, per TMDL language,
#'                            based on assessment of source loads and if pollutant
#'                            reduction is needed to achieve a TMDL allocation or
#'                            loading capacity downstream. See TMDL document for details.
#'                            The TMDL does not address a 303(d) listing or future listing in this assessment unit.
#'                                }
#'   \item Period: Identifies the fish use period that the TMDL addresses. Only used for TMDLs that address temperature or dissolved oxygen.
#'      \itemize{
#'      \item year_round: TMDL addresses only non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      \item spawning: TMDL addresses only spawning uses for the temperature or dissolved oxygen water quality standards.
#'      \item Both: TMDL addresses both spawning and non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      \item Mixed: TMDL addresses different fish use periods in different sections of the assessment unit.
#'      }
#'   \item Source:
#'      \itemize{
#'      \item Point source: Identifies assessment units where pollutant loading is from point sources only.
#'      \item Nonpoint source: Identifies assessment units where pollutant loading is from nonpoint sources only.
#'      \item Both: Identifies assessment units where pollutant loading is from point sources and nonpoint sources.
#'      }
#'   \item Pollu_ID: DEQ water quality parameter ID. ID is for the parameter in 'TMDL_wq_limited_parameter'.
#'   \item HUC_6: Basin six digit USGS hydrological unit code
#'   \item HU_6_NAME: USGS Basin name.
#'   \item HUC6_full: Concatenation of the HUC_6 and HU_6_NAME fields.
#'   \item HUC_8: Subbasin six digit USGS hydrological unit code.
#'   \item HU_8_NAME: USGS Subbasin name.
#'   \item HUC8_full: Concatenation of the HUC_8 and HU_8_NAME fields.
#'   \item AU_ID:	Assessment Unit ID.
#'   \item AU_Name: Name of the assessment unit
#'   \item AU_Description: Assessment unit descriptions
#'   \item TMDL_length_km: Length of the assessment unit in kilometers where TMDL_scope = 'TMDL'. Length is calculated using the linear flowline feature in Albers Equal Area projection.
#'   \item Allocation_only_km: Length of the assessment unit in kilometers where TMDL_scope = 'Allocation only'.
#'   \item Advisory_allocation_km:Length of the assessment unit in kilometers where TMDL_scope = 'Advisory allocation'.
#'   \item AU_length_km: Length of the entire assessment unit in kilometers. Length is calculated using the linear flowline feature in Albers Equal Area projection.
#'   \item TMDL_AU_Percent: Percent of the assessment unit where a TMDL has been developed to address a 303(d) listing or future listing.
#'   \item Allocation_AU_Percent: Percent of the assessment unit where a TMDL allocation applies.
#' }
#'
#' @docType data
#' @usage data(tmdl_au)
#' @keywords Oregon TMDL assessment unit database
#' @keywords datasets
#' @examples
#' db1 <- data(tmdl_au)
#' db2 <-odeqtmdl::tmd_au
#'

"tmdl_au"







