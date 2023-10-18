#' TMDL pollutant parameter and Water quality standard table
#'
#' Look up water quality standard codes by pollutant parameter ID.
#'
#' Data fields include:
#' \itemize{
#'   \item action_id:	USEPA ATTAINS Action ID assigned to each TMDL document.
#'   \item Pollu_ID: DEQ water quality parameter ID. ID is for the parameter 'TMDL_wq_limited_parameter' in the TMDL tables.
#'   \item wqstd_code: Code assigned to each four-digit Oregon water quality standards rule number in OAR 340-41.
#' }
#'
#' @docType data
#' @usage data(tmdl_wqstd)
#' @keywords Oregon water quality standards
#' @keywords datasets
#' @examples
#' tmdl_wqstd_LU1 <- data(tmdl_wqstd)
#' tmdl_wqstd_LU2 <- odeqtmdl::tmdl_wqstd
#'

"tmdl_wqstd"
