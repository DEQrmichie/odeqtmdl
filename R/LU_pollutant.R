#' Pollutant parameter look up
#'
#' Look up table of DEQ and EPA ATTAINS water quality parameter names and IDs.
#'
#' Data fields include:
#' \itemize{
#'   \item Pollu_ID: DEQ water quality parameter ID.
#'   \item Pollutant_DEQ: DEQ water quality parameter name.
#'   \item CAS: Chemical Abstracts Service registry number.
#'   \item WQS_Unit: Unit used for the parameter in the water quality standards.
#'   \item Unit_UID: Unit ID.
#'   \item Attains_Pollutant: EPA ATTAINS water quality parameter name.
#'   \item Attains_Group: EPA ATTAINS water quality parameter group.
#' }
#'
#' @docType data
#' @usage data(LU_pollutant)
#' @keywords Oregon TMDL database
#' @keywords datasets
#' @examples
#' polluLU1 <- data(LU_pollutant)
#' polluLU2 <- odeqtmdl::LU_pollutant
#'

"LU_pollutant"
