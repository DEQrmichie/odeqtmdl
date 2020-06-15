#' Create monthly median values for a dataset
#'
#' Takes a dataframe from which_target_df(), calculates the monthly median results, and returns the dataframe with only
#' monthly median results
#' @param df A dataframe with values provided by which_target_df()
#' @return The dataframe consisting of calculated monthly medians
#' @export
#' @examples
#' monthly_median(df)

monthly_median <- function(df){

  df <- df %>%
    dplyr::filter(tmdl_season) %>%
    dplyr::mutate(mon = lubridate::month(sample_datetime),
                  year = lubridate::year(sample_datetime)) %>%
    dplyr::group_by_at(vars(-Project1, -Result, -Result_Numeric, -Result_Operator, -Result_Unit, -Result_cen,
                            -Statistical_Base, -QualifierAbbr, -Method_Code, -Activity_Type, -act_id, -MRLValue,
                            -Result_status, -sample_datetime, -sample_id, -Spawn_type)) %>%
    dplyr::summarise(monthly_median = median(Result_cen, na.rm = TRUE),
                     sample_datetime = as.POSIXct(paste0(first(year), "-", first(mon), "-01"))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Result_cen = monthly_median)

  return(df)

}
