#' Oregon TMDL parameters
#'
#'Summary of all unique water quality limited parameter and pollutant pair
#'combinations for each TMDL action.
#'
#'Database fields include:
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item TMDL_wq_limited_parameters:	Name of the water quality limited 303(d) parameter that the TMDL addresses.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item TMDL_active: Boolean to indicate if the TMDL and TMDL allocations are effective and being implemented.
#'   \item TMDL_active_note: Note summarizing information about the TMDL and if it was replaced or modified.
#'   \item scope_narrative:	Narrative summary of where the TMDL allocations apply.
#'
#' }
#'
#' @docType data
#' @usage data(tmdl_parameters)
#' @keywords Oregon TMDL database
#' @keywords datasets
#' @examples
#' actions1 <- data(tmdl_parameters)
#' actions2 <- odeqtmdl::tmdl_parameters
#'

"tmdl_parameters"







