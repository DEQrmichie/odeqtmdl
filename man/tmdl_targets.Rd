% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmdl_targets.R
\docType{data}
\name{tmdl_targets}
\alias{tmdl_targets}
\title{Oregon TMDL targets}
\format{
An object of class \code{data.frame} with 476 rows and 18 columns.
}
\usage{
data(tmdl_targets)
}
\description{
Inventory of unique TMDL targets from Oregon TMDLs. Targets may include the
TMDL Loading capacity, Load Allocation, Surrogate Measure, or Endpoint.
}
\details{
Note the TMDL information is still being assembled and is undergoing review.
Some information is not included or may be accurate. See each relevant TMDL document for the
official record and more information. TMDLs developed by tribal governments are not included at this time.


\itemize{
  \item action_id:	EPA ATTAINS Action ID assigned to each TMDL document.
  \item geo_id:	Unique ID assigned to the NHD reaches where a TMDL target applies. ID is structured as YearTMDLissued_ShortTMDLdocName_TargetGeoArea.
  \item TMDL_pollutant:	Name of TMDL pollutant causing the water quality impairment.
  \item field_parameter: Name of the parameter to be sampled in the field.
  \item target_type:	Type of target, e.g. concentration, load, clarity, percent, temperature.
  \item target_value:	Value of TMDL target.
  \item target_units:	Parameter units of the TMDL target.
  \item Unit_UID: Parameter unit Unique ID. UIDs are the same as Oregon DEQ's AWQMS database.
  \item target_time_base:	The time period to use with the statistical base to assess results against the TMDL target.
  \item time_base_UID:  The target time period unique ID. UIDs are the same as Oregon DEQ's AWQMS database. NA indicates a UID has not been assigned.
  \item target_stat_base:	The statistical method used to assess results against the TMDL target.
  \item stat_base_UID: The statistical method unique ID. UIDs are the same as Oregon DEQ's AWQMS database. NA indicates a UID has not been assigned.
  \item season_start:	The beginning of the period when the target applies. Date in format '%b %d' (e.g Aug 31)
  \item season_end:	The end of the period when the target applies. Date in format '%b %d' (e.g Aug 31)
  \item target_conditionals:	Specific conditions or information describing how or when to apply the target.
  \item TMDL_element: The TMDL element under which the target is identified.e.g. Loading Capacity, Load Allocation, Surrogate Measure, Endpoint
  \item target_reference: TMDL page or table reference where target is discussed.
  \item target_comments:	Comments about the TMDL target.
}
}
\examples{
db1 <- data(tmdl_targets)
db2 <-odeqtmdl::tmdl_targets

}
\keyword{Oregon}
\keyword{TMDL}
\keyword{database}
\keyword{datasets}
