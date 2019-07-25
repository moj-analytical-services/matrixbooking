library(dplyr)
library(lubridate)

convert_bookings_to_sensors <- function(bookings) {
  
  bookings %>% 
    rowwise() %>% 
    mutate(obs_booked_datetime = list(seq(time_from, time_to, by = "10 mins"))) %>% 
    unnest() %>%
    filter(obs_booked_datetime != time_to)
  
}

dt_to_numeric <- function(dt) {
  3600 * hour(dt) + 60 * minute(dt) + second(dt)
}

hours_minutes_string_to_numeric <- function(hm_string) {
  # 2010-01-01 is an arbitary date because we're just interested in the time
  dt <- as.POSIXct(paste("2010-01-01", hm_string), tz = "UTC")
  dt_to_numeric(dt)
}

in_time_range <- function(datetime_column, start_time, end_time) {
  
  between(dt_to_numeric(datetime_column),
          hours_minutes_string_to_numeric(start_time),
          hours_minutes_string_to_numeric(end_time) - 600) # -600 because seq() is inclusive of the endtime
  
}

filter_time_range <- function(df, start_time, end_time) {
  df %>%
    filter(in_time_range(obs_datetime, start_time, end_time))
}

fix_bad_sensor_observations <- function(df) {
  # Set any observation which is not 1 or 0 to null
  bad_rows <- !(df$sensor_value %in% c(1, 0))
  df[bad_rows, "sensor_value"] <- NA
  df
}

remove_non_business_days <- function(df) {
  bank_holidays <- jsonlite::fromJSON("https://www.gov.uk/bank-holidays.json")
  bank_holidays <- bank_holidays$`england-and-wales`$events
  
  df %>%
    filter(!(date(obs_datetime) %in% as.Date(bank_holidays$date))) %>% 
    filter(!(weekdays(date(obs_datetime)) %in% c("Saturday", "Sunday")))
}

get_full_occupeye_df <- function(occupeye_df, sensors) {
  inner_join(occupeye_df, sensors, by = c("survey_device_id" = "surveydeviceid")) %>% 
    rename(surveydeviceid = survey_device_id) %>%
    fix_bad_sensor_observations %>%
    remove_non_business_days
}


get_joined_df <- function(full_occupeye_df, sensorised_bookings) {
  full_occupeye_df %>%
    left_join(sensorised_bookings, 
              by=c("location"= "location_id",
                   "obs_datetime" = "obs_booked_datetime")) %>%
    mutate(is_booked = recode(id, .default = 1, .missing = 0),
           date = date(obs_datetime))
}


get_util_cat <- function(joined_observations) {
  # This actually mutates a summarised datset, rather than joined observations. To fix at some point  
  level_order <- c("Unused", "Under utilised", "Effective utilisation")
  
  joined_observations %>%
    mutate(util_cat = case_when(utilisation <= 0.15 ~ "Unused",
                                utilisation < 0.5 ~ "Under utilised",
                                utilisation >= 0.5 ~ "Effective utilisation")) %>%
    mutate(util_cat = factor(util_cat, levels = level_order))
  
}

add_created_to_meeting <- function(bookings) {
  
  bookings %>%
    mutate(created_to_meeting = difftime(time_from, created, units = "days"))
}

add_created_to_cancelled <- function(bookings) {
  bookings %>%
    mutate(created_to_cancelled = difftime(cancelled_datetime, created, units = "days"))
}