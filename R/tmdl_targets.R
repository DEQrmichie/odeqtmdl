#' Oregon TMDL targets
#'
#' Inventory of unique TMDL targets from non-tribal Oregon TMDLs. Note the inventory is incomplete. See each relevant TMDL document
#' for more information. A full listing of all non-tribal TMDL actions in Oregon can be viewed using \code{\link{tmdl_actions}}.
#'
#'
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item geo_id:	Unique ID assigned to the NHD reaches where a TMDL target applies. ID is structured as YearTMDLissued_ShortTMDLdocName_TargetGeoArea.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item field_parameter: Name of the parameter to be sampled in the field.
#'   \item target_type:	Type of target, e.g. concentration, load, clarity, percent, temperature.
#'   \item target_value:	Value of TMDL target.
#'   \item target_units:	Parameter units of the TMDL target.
#'   \item Unit_UID: Parameter unit ID.
#'   \item target_time_base:	The time period to use with the statistical base to assess results against the TMDL target.
#'   \item target_stat_base:	The statistical method used to assess results against the TMDL target.
#'   \item season_start:	The beginning of the period when the target applies. Date in format '%b %d' (e.g Aug 31)
#'   \item season_end:	The end of the period when the target applies. Date in format '%b %d' (e.g Aug 31)
#'   \item target_conditionals:	Specific conditions or information describing how or when to apply the target.
#'   \item TMDL_element: The TMDL element under which the target is identified.e.g. Loading Capacity, Load Allocation, Endpoint
#'   \item target_reference: TMDL page or table reference where target is discussed.
#'   \item target_comments:	Comments about the TMDL target.
#' }
#'
#' @docType data
#' @usage data(tmdl_targets)
#' @keywords Oregon TMDL database
#' @keywords datasets
#' @examples
#' db1 <- data(tmdl_targets)
#' db2 <-odeqtmdl::tmdl_targets
#'

"tmdl_targets"
