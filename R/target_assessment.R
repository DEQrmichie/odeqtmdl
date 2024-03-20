#' Create target statistical base values for a dataset
#'
#' Takes a dataframe from which_target_df(), calculates the target statistical base results, and returns the dataframe with only
#' target statistical base results
#' @param df A dataframe with values provided by which_target_df()
#' @return The dataframe consisting of calculated target statistical bases
#' @export
#' @examples
#' target_assessment(df)

target_assessment <- function(df){

  df_assessed <- df %>%
    dplyr::filter(is.na(target_value)) %>%
    dplyr::mutate(excursion_cen = NA)

  df <- df %>%
    dplyr::filter(!is.na(target_value))

  df_seasonal_median <- df %>%
    dplyr::filter(target_time_base %in% c("seasonal", "annual"),
                  target_stat_base %in% c("median"))
  if(nrow(df_seasonal_median) > 0){
    df_seasonal_median <- seasonal_median(df_seasonal_median)
    df_seasonal_median$excursion_cen <- if_else(df_seasonal_median$seasonal_median > df_seasonal_median$target_value, 1, 0)
    df_assessed <- bind_rows(df_assessed, df_seasonal_median)
  }

  df_seasonal_mean <- df %>%
    dplyr::filter(target_time_base %in% c("seasonal", "annual"),
                  target_stat_base %in% c("mean"))
  if(nrow(df_seasonal_mean) > 0){
    df_seasonal_mean <- seasonal_mean(df_seasonal_mean)
    df_seasonal_mean$excursion_cen <- if_else(df_seasonal_mean$seasonal_mean > df_seasonal_mean$target_value, 1, 0)
    df_assessed <- bind_rows(df_assessed, df_seasonal_mean)
  }

  df_monthly_median <- df %>%
    dplyr::filter(target_time_base %in% c("monthly"),
                  target_stat_base %in% c("median"))
  if(nrow(df_monthly_median) > 0){
    df_monthly_median <- monthly_median(df_monthly_median)
    df_monthly_median$excursion_cen <- if_else(df_monthly_median$monthly_median > df_monthly_median$target_value, 1, 0)
    df_assessed <- bind_rows(df_assessed, df_monthly_median)
  }

  df_monthly_mean <- df %>%
    dplyr::filter(target_time_base %in% c("monthly"),
                  target_stat_base %in% c("mean"))
  if(nrow(df_monthly_mean) > 0){
    df_monthly_mean <- monthly_mean(df_monthly_mean)
    df_monthly_mean$excursion_cen <- if_else(df_monthly_mean$monthly_mean > df_monthly_mean$target_value, 1, 0)
    df_assessed <- bind_rows(df_assessed, df_monthly_mean)
  }

  df_consecutive_median <- df %>%
    dplyr::filter(target_time_base %in% c("two consecutive years"),
                  target_stat_base %in% c("median"))
  if(nrow(df_consecutive_median) > 0){
    df_consecutive_median <- consecutive_median(df_consecutive_median)
    df_assessed <- bind_rows(df_assessed, df_consecutive_median)
  }

  df_daily_max <- df %>%
    dplyr::filter(target_time_base %in% c("daily"),
                  target_stat_base %in% c("maximum"))
  if(nrow(df_daily_max) > 0){
    df_daily_max$excursion_cen <- if_else(df_daily_max$tmdl_season & df_daily_max$Result_cen > df_daily_max$target_value, 1, 0)
    df_assessed <- bind_rows(df_assessed, df_daily_max)
  }

  df_ss <- df %>%
    dplyr::filter(target_time_base %in% c("sample", NA),
                  target_stat_base %in% c("maximum", NA))
  if(nrow(df_ss) > 0){
    df_ss$excursion_cen <- if_else(df_ss$tmdl_season & df_ss$Result_cen > df_ss$target_value, 1, 0)
    df_assessed <- bind_rows(df_assessed, df_ss)
  }

  return(df_assessed)
}
