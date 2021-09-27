#' Oregon TMDL Actions
#'
#'Summary of all non tribal TMDL actions applicable in Oregon
#'
#' \itemize{
#'   \item action_id:	USEPA ATTAINS Action ID assigned to each TMDL document.
#'   \item TMDL_name:	Name of TMDL document.
#'   \item TMDL_issue_year:	Year the TMDL was issued by the issue agency.
#'   \item issue_agency:	The agency that developed and issued the TMDL (OR or EPA).
#'   \item TMDL_active:	Boolean to indicate if the TMDL and TMDL allocations are effective and being implemented.
#'   \item TMDL_active_note: Note summarizing information about the TMDL and if it was replaced or modified.
#'   \item in_attains:	Boolean to indicate if the TMDL action has been entered into EPA's ATTAINS database.
#'   \item attains_status: Status of TMDL action in ATTAINS. NA indicates the TMDL action is not included in ATTAINS.
#'   \item TMDL_issue_date:	The date the TMDL was issued by the issue agency.
#'   \item EPA_action_date: The date EPA took action (approval or disapproval) on the TMDL.
#'   \item action_wq_limited_parameters: All 303(d) water quality limited parameters being addressed by the TMDL action.
#'   \item action_TMDL_pollutants: All pollutants causing the water quality listings associated with the TMDL action.
#'
#' }
#'
#' @docType data
#' @usage data(tmdl_actions)
#' @keywords Oregon TMDL database
#' @keywords datasets
#' @examples
#' actions1 <- data(tmdl_actions)
#' actions2 <-odeqtmdl::tmdl_actions
#'

"tmdl_actions"







