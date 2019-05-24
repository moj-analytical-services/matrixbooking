library(dplyr)
library(ggplot2)
library(scales)


get_utlisation_by_date <- function(joined_observations) {
  joined_observations %>% filter(status != "CANCELLED") %>%
    group_by(date, roomname, is_booked) %>% 
    summarise(utilisation = mean(sensor_value, na.rm = T)) %>% 
    filter(!is.na(id)) %>%
    get_util_cat()
}

get_booked_permutation <- function(joined_observations) {
  joined_observations %>% 
    mutate(booked_permutation = case_when(
      sensor_value == 1 & is_booked == 1 ~ "4. Booked and occupied",
      sensor_value == 1 & is_booked == 0 ~ "3. Occupied but not booked",
      sensor_value != 1 & is_booked == 1 ~ "2. Booked but not occupied",
      sensor_value != 1 & is_booked == 0 ~ "1. Neither booked nor occupied")
    )
}

room_utilisation_permutation <- function(joined_observations) {
  utilisation_by_permutation <- get_booked_permutation(joined_observations)
  
  ggplot(data = utilisation_by_permutation,
         mapping = aes(x = date,
                       fill = booked_permutation)) +
    geom_bar(position = "fill") +
    scale_fill_brewer(palette = "Spectral")
  
}

room_utilisation_by_date <- function(joined_observations, bar_position = 'fill') {
  # Plots the number of bookings that Bar position can either be stack or fill
  
  utilisation_by_date <- get_utilisation_by_date(joined_observations)
  
  count_by_cat <- utilisation_by_date %>% group_by(date, util_cat) %>% summarise(count = n())
  
  ggplot(data = count_by_cat, 
         mapping = aes(x = date, 
                       y = count, 
                       fill = util_cat)) +
    geom_bar(position = bar_position, 
             stat = "identity") + 
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    ggtitle("Rooms by utilisation")
}


get_utilisation_by_date_and_booking <- function(joined_observations) {
  # counts the number of non-cancelled bookings by util_cat and day
  
  joined_observations %>% filter(status != "CANCELLED") %>%
    group_by(id, date, roomname, is_booked) %>% 
    summarise(utilisation = mean(sensor_value, na.rm = T)) %>% 
    filter(!is.na(id)) %>%
    get_util_cat()
}

booking_utilisation_by_date <- function(joined_observations, bar_position = 'fill') {
  # Plots the number of bookings that Bar position can either be stack or fill
  
  utilisation_by_date <- get_utilisation_by_date_and_booking(joined_observations)
  
  count_by_cat <- utilisation_by_date %>% group_by(date, util_cat) %>% summarise(count = n())
  
  ggplot(data = count_by_cat, 
         mapping = aes(x = date, 
                       y = count, 
                       fill = util_cat)) +
    geom_bar(position = bar_position, 
             stat = "identity") + 
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    ggtitle("Bookings by utilisation")
}



bookings_during_occupied_time <- function(joined_observations) {
  data <- joined_observations %>%
    filter(sensor_value == 1)
  
  ggplot(data = data, aes(x = date)) +
    stat_summary(aes(y = is_booked), 
                 fun.y = "mean",
                 geom = "bar") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Proportion of occupied time that contains a booking")
  
}

occupancy_during_booked_time <- function(joined_observations) {
  data <- joined_observations %>%
    filter(is_booked == 1)
  
  ggplot(data = data, aes(x = date)) +
    stat_summary(aes(y = sensor_value), 
                 fun.y = "mean",
                 geom = "bar") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Proportion of booked time when the room is occupied")
}

room_booking_length_histogram <- function(joined_observations) {
  booking_lengths <- joined_observations %>%
    filter(is_booked == 1) %>%
    group_by(id) %>%
    mutate(booking_length = sum(is_booked)) %>%
    group_by(booking_length) %>%
    summarise(freq = n(),
              utilisation = sum(sensor_value/booking_length)) %>%
    mutate(freq = freq/booking_length, 
           booking_length = booking_length * 10)
  
  utilisation <- scales::percent(booking_lengths$utilisation / booking_lengths$freq)
  

  plot_ly(booking_lengths) %>%
    add_trace(x = ~booking_length, 
              y = ~freq,
              type = "bar",
              yaxis = "y1",
              name = "Number of bookings",
              text = paste0("Booking length: ",
                            booking_lengths$booking_length,
                            "\n",
                            "Number of bookings: ",
                            booking_lengths$freq),
              hoverinfo = "text") %>%
    add_trace(x = ~booking_length,
              y = ~utilisation,
              type = "bar",
              text = paste0("Utilisation: ",utilisation),
              name = "Occupied time",
              hoverinfo = "text") %>%
    layout(xaxis = list(title = "booking length (minutes)"),
           yaxis = list(side = "left", 
                        title = "Number of bookings"),
           barmode = "overlay")
  
  
}