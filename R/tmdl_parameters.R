#' Oregon TMDL parameters
#'
#'Summary and status of all unique water quality limited parameter and pollutant pair
#'combinations for each TMDL action.
#'
#'Database fields include:
#' \itemize{
#'   \item action_id:	EPA ATTAINS action ID assigned to each TMDL document.
#'   \item TMDL_wq_limited_parameters:	Name of the water quality limited 303(d) parameter that the TMDL addresses.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item TMDL_status: Status of TMDL for the parameter and pollutant.
#'   \itemize{
#'        \item Active: TMDL has been approved by EPA and is active.
#'        \item Not Active: TMDL has been withdrawn, disapproved by EPA, and/or replaced with a newer TMDL.
#'        \item In Development: TMDL is in development.
#'   }
#'   \item TMDL_status_comment: Note summarizing information about the TMDL and if it was replaced or modified.
#'   \item revision_action_id: The EPA ATTAINS action ID assigned to the TMDL revision.
#' }
#'
#' @docType data
#' @usage data(tmdl_parameters)
#' @keywords Oregon TMDL database
#' @keywords datasets
#' @examples
#' p1 <- data(tmdl_parameters)
#' p2 <- odeqtmdl::tmdl_parameters
#'

"tmdl_parameters"







