#' NPDES permittees assigned a TMDL WLA
#'
#' Table of NPDES permittees that were assigned a wasteload allocation (WLA) in
#' a TMDL. The inventory is incomplete. See each relevant TMDL document
#' for more information.
#'
#'
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item AU_ID:	Assessment Unit ID.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item EPANum: EPA NPDES facility number.
#'   \item WQFileNum:	DEQ NPDES facility water quality file number.
#'   \item facility_name:	Common Name or name in the TMDL document of the NPDES permitted facility.
#' }
#'
#' @docType data
#' @usage data(tmdl_wla)
#' @keywords Oregon TMDL database
#' @keywords datasets
#' @examples
#' db1 <- data(tmdl_wla)
#' db2 <-odeqtmdl::tmdl_wla
#'

"tmdl_wla"
