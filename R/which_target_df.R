#' Which TMDL target ID applies
#'
#' Takes a dataframe with sample reachcode, parameter, and date and returns the TMDL target info
#' @param df Data frame in which to add target information
#' @return Data frame with tmdl target columns
#' @export
#' @examples
#' which_target(df)

which_target_df <- function(df, all_obs = TRUE){

  tmdl_db_mins <- odeqtmdl::tmdl_db[, c("ReachCode", "pollutant_name_AWQMS", "geo_id", "TMDL_name", "TMDL_issue_year",
                                        "target_value", "target_units", "target_stat_base", "target_type", "season_start",
                                        "season_end", "target_conditionals_references")] %>%
    dplyr::filter(is.na(target_conditionals_references), target_type %in% c("temperature", "concentration")) %>%
    dplyr::group_by(ReachCode, pollutant_name_AWQMS, target_units, target_stat_base, TMDL_name, TMDL_issue_year,
                    season_start, season_end) %>%
    dplyr::summarise(target_value = min(target_value, na.rm = TRUE)) %>%
    dplyr::mutate(tmdl_period = paste(season_start, "-", season_end),
                  tmdl = paste0(TMDL_name, " (DEQ ", TMDL_issue_year, ")")) %>%
    dplyr::select(-TMDL_issue_year, -TMDL_name)

  df <- merge(df, tmdl_db_mins,
              by.x = c("Reachcode", "Char_Name"), by.y = c("ReachCode", "pollutant_name_AWQMS"), all.x = all_obs, all.y = FALSE)

  if(nrow(df) > 0){
    df <- df %>% dplyr::mutate(
      # Append start and end dates with year
      start_datetime = ifelse(!is.na(season_start), paste0(season_start, "-", lubridate::year(sample_datetime)), NA ) ,
      end_datetime = ifelse(!is.na(season_end), paste0(season_end, "-", lubridate::year(sample_datetime)), NA ),
      # Make dates POSIXct format
      start_datetime = as.POSIXct(start_datetime, format = "%d-%b-%Y"),
      end_datetime = as.POSIXct(end_datetime, format = "%d-%b-%Y"),
      # If dates span a calendar year, account for year change in end date
      end_datetime = if_else(!is.na(end_datetime),
                             if_else(end_datetime < start_datetime & sample_datetime >= end_datetime,
                                     end_datetime + lubridate::years(1), # add a year if inperiod carrying to next year
                                     end_datetime), # otherwise, keep end_datetime as current year
                             end_datetime),
      start_datetime = if_else(!is.na(start_datetime),
                               if_else(end_datetime < start_datetime & sample_datetime <= end_datetime,
                                       start_datetime - lubridate::years(1), # subtract a year if in period carrying from previous year
                                       start_datetime),
                               start_datetime),
      tmdl_season = if_else(!is.na(start_datetime),
                            if_else(sample_datetime >= start_datetime & sample_datetime <= end_datetime,
                                    TRUE,
                                    FALSE),
                            FALSE),
      criteria = if_else(tmdl_season, "TMDL", NA_character_),
      # Append spawn start and end dates with year
      Start_spawn = ifelse(!is.na(spawn_start), paste0(spawn_start,"/",lubridate::year(sample_datetime)), NA ) ,
      End_spawn = ifelse(!is.na(spawn_end), paste0(spawn_end,"/",lubridate::year(sample_datetime)), NA ),
      # Make spwnmn start and end date date format
      Start_spawn = lubridate::mdy(Start_spawn),
      End_spawn = lubridate::mdy(End_spawn),
      # If Spawn dates span a calendar year, account for year change in spawn end date
      End_spawn = dplyr::if_else(End_spawn < Start_spawn & sample_datetime >= End_spawn, End_spawn + lubridate::years(1), # add a year if in spawn period carrying to next year
                                 End_spawn), # otherwise, keep End_spawn as current year
      Start_spawn = dplyr::if_else(End_spawn < Start_spawn & sample_datetime <= End_spawn, Start_spawn - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                                   Start_spawn), # otherwise, keep Start_spawn as current year
      # Flag for results in critical period
      In_crit_period = ifelse(sample_datetime >= Crit_period_start & sample_datetime <= Crit_period_end, 1, 0 ),
      # Print if result is in spawn or out of spawn
      Spawn_type = ifelse((sample_datetime >= Start_spawn & sample_datetime <= End_spawn & !is.na(Start_spawn)),  "Spawn", "Not_Spawn")
    ) %>% dplyr::select(-season_start, -season_end)
  }

  return(df)

}
