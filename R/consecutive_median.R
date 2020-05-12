#' Create seasonal median for targets requiring consecutive years
#'
#' Takes a dataframe from which_target_df(), calculates the consecutive year seasonal median results, and returns the dataframe with only
#' seasonal median results
#' @param df A dataframe with values provided by which_target_df()
#' @param n_years The number of consecutive years over which to calculate the seasonal median.
#' @return The dataframe consisting of calculated seasonal medians
#' @export
#' @examples
#' consecutive_year_seasonal_median(df, n_years = 2)

consecutive_median <- function(df, n_years = 2){

  tmp <- df %>%
    dplyr::filter(tmdl_season) %>%
    dplyr::mutate(year = lubridate::year(sample_datetime)) %>%
    dplyr::group_by_at(vars(-Project1, -Result, -Result_Numeric, -Result_Operator, -Result_Unit, -Result_cen,
                            -Statistical_Base, -QualifierAbbr, -Method_Code, -Activity_Type, -act_id, -MRLValue,
                            -Result_status, -sample_datetime, -sample_id, -Spawn_type)) %>%
    dplyr::summarise(seasonal_median = median(Result_cen, na.rm = TRUE)) %>%
    dplyr::mutate(exceed = if_else(seasonal_median > target_value, 1, 0)) %>%
    dplyr::ungroup()

  consecutive_check <- function(stn, yr){
    data <- tmp %>% dplyr::filter(year == yr - 1, MLocID == stn)
    if(nrow(data) > 0){
      return(data$exceed)
    } else {
      return(NA)
    }
  }

  tmp$prev_year <- mapply(consecutive_check, tmp$MLocID, tmp$year, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  tmp$excursion <- if_else((tmp$exceed + tmp$prev_year) == 2, 1, 0)

  return(df)

}
