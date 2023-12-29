#' Oregon TMDL database by NHD reach
#'
#' Inventory of where Oregon TMDLs have been developed based on the high resolution
#' National Hydrography Dataset (NHD) version NHDH_OR_931v220.
#' The \code{tmdl_reaches} data table is very large so it may take a minute to load.
#'
#' Note the TMDL information is still being assembled and is undergoing review.
#' Some information may not be accurate. See each relevant TMDL document for the
#' official record and more information. TMDLs developed by tribal governments are not included at this time. The inventory
#' is still being developed and some information may not be accurate.
#' A full listing of all non-tribal TMDL actions in Oregon can be viewed using \code{\link{tmdl_actions}}.
#'
#' Database fields include:
#'
#' \itemize{
#'   \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
#'   \item TMDL_wq_limited_parameters:	Name of the water quality limited 303(d) parameter that the TMDL addresses.
#'   \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
#'   \item TMDL_scope: Provides information about how the TMDL applies.
#'      \itemize{
#'      \item TMDL:	The TMDL was developed to address a 303(d) listing or future listing in this reach.
#'      \item Allocation only: A TMDL allocation applies in this reach
#'                            but the TMDL does not address a 303(d) listing or future listing.
#'                            Typically this situation is applicable for tributaries or canals
#'                            that are upstream of the reach where the "TMDL" applies.
#'                            The pollutant reduction in the upstream reach is needed to achieve the
#'                            TMDL loading capacity of the downstream reach.
#'      \item Advisory allocation: A TMDL allocation may apply
#'                            based on assessment of source loads and if pollutant
#'                            reduction is needed to achieve a TMDL allocation or
#'                            loading capacity downstream. See TMDL document for details and requirements.
#'                            The TMDL does not address a 303(d) listing or future listing in this reach.
#'                                }
#'   \item Period: Identifies the fish use period that the TMDL addresses. Only used for TMDLs that address temperature or dissolved oxygen.
#'      \itemize{
#'      \item year_round: TMDL addresses only non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      \item spawning: TMDL addresses only spawning uses for the temperature or dissolved oxygen water quality standards.
#'      \item Both: TMDL addresses both spawning and non spawning (year round) uses for temperature or dissolved oxygen water quality standards.
#'      }
#'   \item Source:
#'      \itemize{
#'      \item Point source: Identifies reaches where pollutant loading is from point sources only.
#'      \item Nonpoint source: Identifies reaches where pollutant loading is from nonpoint sources only.
#'      \item Both: Identifies reaches where pollutant loading is from point sources and nonpoint sources.
#'      }
#'   \item Pollu_ID: DEQ water quality parameter ID. ID is for the parameter in 'TMDL_wq_limited_parameter'.
#'   \item geo_id:	Unique ID assigned to the NHD reaches codes where a TMDL target applies. ID is structured as YearTMDLissued_ShortTMDLdocName_TargetGeoArea.
#'   \item HUC_6: Basin six digit USGS hydrological unit code.
#'   \item HU_6_NAME: USGS Basin name.
#'   \item HUC6_full: Concatenation of the HUC_6 and HU_6_NAME fields.
#'   \item HUC_8: Subbasin eight digit USGS hydrological unit code.
#'   \item HU_8_NAME: USGS Subbasin name.
#'   \item HUC8_full: Concatenation of the HUC_8 and HU_8_NAME fields.
#'   \item HUC_10: Watershed ten digit USGS hydrological unit code.
#'   \item HU_10_NAME: USGS Watershed name.
#'   \item HUC10_full: Concatenation of the HUC_10 and HU_10_NAME fields.
#'   \item GLOBALID: Unique ID for every NHD reach record. Used for joins with TMDL GIS features.
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
#' @return loads dataframe

tmdl_reaches <- function(){

  warning(immediate. = TRUE, "Loading tmdl_reaches may take a minute")

  file_path1 <- system.file("extdata", "tmdl_reaches1.RDS", package = "odeqtmdl",
                           mustWork = TRUE)
  file_path2 <- system.file("extdata", "tmdl_reaches2.RDS", package = "odeqtmdl",
                            mustWork = TRUE)
  file_path3 <- system.file("extdata", "tmdl_reaches3.RDS", package = "odeqtmdl",
                            mustWork = TRUE)
  file_path4 <- system.file("extdata", "tmdl_reaches4.RDS", package = "odeqtmdl",
                            mustWork = TRUE)

  df1 <- readRDS(file = file_path1)
  df2 <- readRDS(file = file_path2)
  df3 <- readRDS(file = file_path3)
  df4 <- readRDS(file = file_path4)

  df <- rbind(df1, df2, df3, df4)

  return(df)

}






