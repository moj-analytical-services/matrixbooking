library(tidyverse)
library(feather)
library(dbtools)
library(reticulate)


source("data_cleaning_functions.R")
source("charting_functions.R")

use_condaenv(condaenv = 'rstudio', required = TRUE)

bookings <- dbtools::read_sql("select * from matrix_db.bookings")
locations <- dbtools::read_sql("select * from matrix_db.locations")

sensors <- dbtools::read_sql("select * from occupeye_db_live.sensors")

leeds_sensors <- sensors %>% filter(survey_id == 342, location %in% locations$id)

leeds_observations <- s3tools::read_using(feather::read_feather,
                                          "alpha-app-occupeye-automation/surveys/342/All.feather") %>% 
  get_full_occupeye_df(leeds_sensors) %>% 
  filter(date(obs_datetime) >= "2019-01-01")

sensorised_bookings <- convert_bookings_to_sensors(bookings)



joined_observations <- get_joined_df(leeds_observations,
                                     sensorised_bookings %>% filter(status != "CANCELLED")) %>% 
  filter_time_range("09:00","17:00")

write_feather(joined_observations, "leeds.feather")

s3tools::write_file_to_s3("leeds.feather","alpha-app-matrixbooking/leeds.feather", overwrite = T)