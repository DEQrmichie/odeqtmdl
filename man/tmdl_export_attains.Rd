% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/protection_export_attains.R,
%   R/tmdl_export_attains.R
\name{tmdl_export_attains}
\alias{tmdl_export_attains}
\title{Export protection plan info to ATTAINS upload files}
\usage{
tmdl_export_attains(
  out_dir,
  df_tmdl_actions = NULL,
  df_tmdl_au = NULL,
  df_tmdl_wla = NULL,
  AU_IDs = NULL,
  status_attains = NULL,
  action_ids = NULL,
  TMDL_param = NULL,
  TMDL_pollu = NULL
)

tmdl_export_attains(
  out_dir,
  df_tmdl_actions = NULL,
  df_tmdl_au = NULL,
  df_tmdl_wla = NULL,
  AU_IDs = NULL,
  status_attains = NULL,
  action_ids = NULL,
  TMDL_param = NULL,
  TMDL_pollu = NULL
)
}
\arguments{
\item{out_dir:}{The directory to save the output csv files.}

\item{df_tmdl_actions:}{Data frame formatted the same as \code{\link{tmdl_actions}}, Default is NULL and the \code{\link{tmdl_actions}} data table will be used.}

\item{df_tmdl_au:}{Data frame formatted the same as \code{\link{tmdl_au}}. Default is NULL and the \code{\link{tmdl_au}} data table will be used.}

\item{df_tmdl_wla:}{Data frame formatted the same as \code{\link{tmdl_wla}}. Default is NULL and the \code{\link{tmdl_wla}} data table will be used.}

\item{AU_IDs:}{Vector of assessment units used to filter 'tmdl_au'. Default is NULL and all AU IDs are included.}

\item{status_attains:}{Vector of the attains status used to filter 'tmdl_au'. Default is NULL and all statuses are included.}

\item{action_ids:}{Vector of action IDs used to filter 'tmdl_au'. Default is NULL and all action IDs are included.}

\item{TMDL_param:}{Vector of water quality  parameter names used to filter 'tmdl_au'. The output will include TMDLs that addressed that water quality parameter. Default is NULL and all parameters are included.}

\item{TMDL_pollu:}{Vector of TMDL pollutant parameter names used to filter the TMDLs. The output will include TMDLs that addressed that pollutant parameter. Default is NULL and all pollutants are included.}
}
\description{
Exports all or a subset of the Oregon TMDL assessment unit data table (\code{\link{tmdl_au}})
as csv files formatted for batch upload into ATTAINS. The actions and parameters csv files are produced.
The format is based on EPA's Non-TMDL action template version 1.4, released on 7/18/2022.

Exports all or a subset of the Oregon TMDL database
action table (\code{\link{tmdl_actions}}), assessment unit table (\code{\link{tmdl_au}}),
and NPDES permittee WLA table (\code{\link{tmdl_wla}}) as csv files formatted
for batch upload into ATTAINS. The actions, parameters, permits, and pollutants csv files are produced.
The format is based on EPA's TMDL action template version 1.4, released on 2022-07-18.
}
\keyword{ATTAINS}
\keyword{Oregon}
\keyword{TMDL}
\keyword{batch}
\keyword{upload}
