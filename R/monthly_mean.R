#' Create monthly mean values for a dataset
#'
#' Takes a dataframe from which_target_df(), calculates the monthly mean results, and returns the dataframe with only
#' monthly mean results
#' @param df A dataframe with values provided by which_target_df()
#' @return The dataframe consisting of calculated monthly means
#' @export
#' @examples
#' monthly_mean(df)

monthly_mean <- function(df){

  df <- df %>%
    dplyr::filter(tmdl_season) %>%
    dplyr::mutate(mon = lubridate::month(sample_datetime)) %>%
    dplyr::group_by_at(vars(-Project1, -Result, -Result_Numeric, -Result_Operator, -Result_Unit, -Result_cen,
                            -Statistical_Base, -QualifierAbbr, -Method_Code, -Activity_Type, -act_id, -MRLValue,
                            -Result_status, -sample_datetime, -sample_id, -Spawn_type)) %>%
    dplyr::summarise(monthly_mean = mean(Result_cen, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(df)

}
