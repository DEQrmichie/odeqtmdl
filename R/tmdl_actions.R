#' Oregon TMDL Actions
#'
#'Summary of all non tribal TMDL actions applicable in Oregon
#'
#' \itemize{
#'   \item action_id:	USEPA ATTAINS Action ID assigned to each TMDL document.
#'   \item TMDL_name:	Name of TMDL document.
#'   \item TMDL_issue_year:	Year the TMDL was issued by Oregon DEQ.
#'   \item TMDL_active:	Boolean to indicate if the TMDL and TMDL allocations are effective and being implemented.
#'   \item issue_agency:	The agency that developed and issued the TMDL (OR or EPA).
#'   \item in_attains:	Boolean to indicate if the TMDL action has been entered into EPA's ATTAINS database.
#'   \item TMDL_issue_date:	The date the TMDL was issued by the issue agency.
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







