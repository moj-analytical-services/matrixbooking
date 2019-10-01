library(dbtools)
library(glue)


get_surveys <- function() {
  
  dbtools::read_sql("select * from matrixbooking_app_db.surveys")
}

get_sensor_observations <- function(survey_id, start_date, end_date) {
  sql <- glue("select * from matrixbooking_app_db.sensor_observations
                         where survey_id = {survey_id}
                         and obs_datetime >= timestamp '{start_date} 00:00'
                         and obs_datetime <= timestamp '{end_date} 23:50'")
  dbtools::read_sql(sql)
  
}

get_bookings <- function(survey_id, start_date, end_date) {
  sql <- glue("select distinct b.* from matrixbooking_app_db.bookings as b
                         inner join matrixbooking_app_db.locations as l
                         on b.location_id = l.id
                         where l.survey_id = {survey_id}
                         and shareid = 'nan'
                         and time_from >= timestamp '{start_date} 00:00'
                         and time_to <= timestamp '{end_date} 23:50'")
  print(sql)
  dbtools::read_sql(sql)
  
}

get_locations <- function(survey_id) {
  sql <- glue("select * from matrixbooking_app_db.locations
                         where survey_id = {survey_id}")
  print(sql)
  
  dbtools::read_sql(sql)
}