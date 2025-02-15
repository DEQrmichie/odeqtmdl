% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmdl_remove.R
\name{tmdl_remove}
\alias{tmdl_remove}
\title{Remove information from the TMDL package tables}
\usage{
tmdl_remove(action_ids = NULL, package_path)
}
\arguments{
\item{action_ids}{vector of TMDL action IDs to be removed. Required.}

\item{package_path}{Path to the top level directory of the odeqtmdl R package. The 'data', data_raw', and 'inst/extdata' folders must exist.}
}
\description{
Removes TMDL information from the odeqtmdl package tables. Removes all rows
in existing package tables with the same 'action_id' identified
in \code{action_ids}. There is no filtering for specific parameters or pollutants. All
information attributed to the action_id is removed. Use \code{\link{tmdl_update}}
to update or add new information. Removes rows from the following package tables:
\itemize{
     \item tmdl_actions
     \item tmdl_targets
     \item tmdl_geo_id
     \item tmdl_wqstd
     \item tmdl_reaches
     \item tmdl_au
     \item tmdl_au_gnis
     \item tmdl_parameters
     }
}
