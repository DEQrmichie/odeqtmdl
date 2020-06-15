#' Create seasonal mean values for a dataset
#'
#' Takes a dataframe from which_target_df(), calculates the seasonal mean results, and returns the dataframe with only
#' seasonal mean results
#' @param df A dataframe with values provided by which_target_df()
#' @return The dataframe consisting of calculated seasonal means
#' @export
#' @examples
#' seasonal_mean(df)

seasonal_mean <- function(df){

  df <- df %>%
    dplyr::filter(tmdl_season) %>%
    dplyr::group_by_at(vars(-Project1, -Result, -Result_Numeric, -Result_Operator, -Result_Unit, -Result_cen,
                            -Statistical_Base, -QualifierAbbr, -Method_Code, -Activity_Type, -act_id, -MRLValue,
                            -Result_status, -sample_datetime, -sample_id, -Spawn_type)) %>%
    dplyr::summarise(seasonal_mean = mean(Result_cen, na.rm = TRUE),
                     sample_datetime = first(start_datetime)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Result_cen = seasonal_mean)

  return(df)

}
