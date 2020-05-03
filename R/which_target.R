#' Which TMDL target ID applies
#'
#' Takes a sample reachcode, parameter, and date and returns the TMDL target ID(s) that apply
#' @param reachcode The GeoID for the sample location
#' @param parameter The pollutant or characteristic name value for the sample
#' @param date The sample date in yyyy-mm-dd format
#' @return The applicable TMDL target
#' @export
#' @examples
#' which_target(GeoID = "2006_Willamette_JohnsonCreek", parameter = "Total phosphorus, mixed forms", date = "2015-07-01")

which_target <- function(reachcode, parameter, date){

  target <- tmdl_db
  target <- target %>% dplyr::filter(ReachCode == reachcode, pollutant_name_AWQMS == parameter)

  if(nrow(target) == 0){
    return(NA)
  }

  target <- target %>% dplyr::mutate(
    # Append start and end dates with year
    start_datetime = ifelse(!is.na(season_start), paste0(season_start,"-", lubridate::year(date)), NA ) ,
    end_datetime = ifelse(!is.na(season_end), paste0(season_end,"-", lubridate::year(date)), NA ),
    # Make dates POSIXct format
    start_datetime = as.POSIXct(start_datetime, format = "%d-%b-%Y"),
    end_datetime = as.POSIXct(end_datetime, format = "%d-%b-%Y"),
    # If dates span a calendar year, account for year change in end date
    end_datetime = if_else(end_datetime < start_datetime & date >= end_datetime, end_datetime + lubridate::years(1), # add a year if inperiod carrying to next year
                           end_datetime), # otherwise, keep End_spawn as current year
    start_datetime = if_else(end_datetime < start_datetime & date <= end_datetime, start_datetime - lubridate::years(1), # subtract a year if in period carrying from previous year
                             start_datetime))

  target_id <- target %>% dplyr::filter(season_start <= date, season_end >= date)

  return(unique(target_id[,-ReachCode]))

}
