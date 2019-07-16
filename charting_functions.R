library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)


get_booked_permutation <- function(joined_observations) {
  joined_observations %>% 
    mutate(booked_permutation = case_when(
      sensor_value == 1 & is_booked == 1 ~ "3. Booked and occupied",
      sensor_value == 1 & is_booked == 0 ~ "2. Occupied but not booked",
      sensor_value == 0 & is_booked == 1 ~ "1. Booked but not occupied",
      sensor_value == 0 & is_booked == 0 ~ "0. Neither booked nor occupied",
      is.na(sensor_value) ~ "invalid occupeye sensor reading")
    )
}

room_utilisation_permutation <- function(joined_observations, varname) {
  
  expr <- sym(varname)
  
  
  utilisation_by_permutation <- get_booked_permutation(joined_observations) %>%
    count(booked_permutation, !!expr) %>%
    group_by(!!expr) %>%
    mutate(prop = prop.table(n)) %>% 
    filter(booked_permutation != "0. Neither booked nor occupied")
  
  
  
  ggplot(data = utilisation_by_permutation,
         mapping = aes(x = !!expr,
                       y = prop,
                       fill = booked_permutation)) +
    geom_bar(position = "stack",
             stat = "identity") +
    scale_fill_brewer(palette = "Spectral") +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    ggtitle("Room booking and occupancy")
  
}

get_utilisation_by_date <- function(joined_observations) {
  joined_observations %>%
    group_by(date, roomname) %>% 
    summarise(utilisation = mean(sensor_value, na.rm = T)) %>%
    get_util_cat() %>%
    ungroup()
}


room_utilisation_by_date <- function(joined_observations, bar_position = 'fill') {
  # Plots the number of bookings that Bar position can either be stack or fill
  
  utilisation_by_date <- get_utilisation_by_date(joined_observations)
  
  count_by_cat <- utilisation_by_date %>%
    group_by(date,
             util_cat) %>%
    summarise(count = n())
  
  ggplot(data = count_by_cat, 
         mapping = aes(x = date, 
                       y = count, 
                       fill = util_cat)) +
    geom_bar(position = bar_position, 
             stat = "identity") + 
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Rooms by utilisation")
}




room_utilisation_by_weekday <- function(joined_observations, bar_position = 'fill') {
  # Plots the number of bookings that Bar position can either be stack or fill
  
  utilisation_by_weekday <- get_utilisation_by_date(joined_observations) %>% 
    mutate(day = weekdays(date))
  
  count_by_cat <- utilisation_by_weekday %>%
    group_by(day,
             util_cat) %>%
    summarise(count = n())
  
  weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  
  ggplot(data = count_by_cat, 
         mapping = aes(x = day, 
                       y = count, 
                       fill = util_cat)) +
    geom_bar(position = bar_position, 
             stat = "identity") + 
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = weekday)  +
    ggtitle("Rooms by utilisation by weekday")
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
    scale_y_continuous(labels = scales::percent) +
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
  
  # Switched to using plot_ly, because ggplotly produced strange results.
  # Should I port it?
  
  booking_lengths <- joined_observations %>%
    filter(is_booked == 1) %>%
    group_by(id) %>%
    mutate(booking_length = sum(is_booked)) %>%
    group_by(booking_length) %>%
    summarise(freq = n(),
              utilisation = sum(sensor_value/booking_length)) %>%
    mutate(freq = freq/booking_length, 
           booking_length = booking_length / 6)
  
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
    layout(xaxis = list(title = "booking length (hours)"),
           yaxis = list(side = "left", 
                        title = "Number of bookings"),
           barmode = "overlay",
           title = "Bookings by length (hours)")
  
  
}


occupancy_through_day <- function(joined_observations) {
  my_data <- joined_observations %>%
    get_booked_permutation() %>%
    mutate(time = strftime(obs_datetime, "%H:%M:%S"))
  
  plot_ly(my_data) %>%
    add_trace(x = ~date,
              y = ~time,
              type = "scatter",
              color = ~booked_permutation,
              colors = c("grey",
                        "sandybrown",
                         "lemonchiffon",
                         "darkseagreen3"),
              mode = "markers",
              symbol = ~booked_permutation,
              symbols = c('square',
                          'square',
                          'square',
                          'square')) %>%
    layout(yaxis = list(autorange = "reversed"))
  
  
}


get_smoothing_table <- function(joined_observations, smoothing_factor) {
  
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  
  cat_order <- c("Full Smoothing", "Current Utilisation", "Partial Smoothing")
  
  get_utilisation_by_date(joined_observations) %>%
    mutate(day = weekdays(date)) %>%
    count(util_cat, day) %>%
    group_by(day) %>%
    mutate(prop = prop.table(n)) %>%
    filter(util_cat == "Unused") %>%
    ungroup() %>%
    mutate(current_utilisation = 1 - prop,
           full_smoothing = mean(current_utilisation),
           partial_smoothing = (full_smoothing * smoothing_factor) +
             (current_utilisation * (1 - smoothing_factor))) %>%
    select(day,
           "Full Smoothing" = full_smoothing, 
           "Current Utilisation" = current_utilisation,
           "Partial Smoothing" = partial_smoothing) %>%
    gather(variable, value, -day) %>%
    mutate(variable = factor(variable, cat_order))
}


smoothing_chart <- function(joined_observations, smoothing_factor) {
  
  
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  
  cat_order <- c("Full Smoothing", "Current Utilisation", "Partial Smoothing")
  
  smoothing <- get_smoothing_table(joined_observations, smoothing_factor)
  
  
  ggplot(smoothing,
         aes(x = day, y = value, fill = variable)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    ggtitle("Required Desk Allocation - Smoothing Assumptions") + 
    scale_x_discrete(limits = weekdays) + 
    theme(legend.position = "top") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    expand_limits(y = 0) + 
    scale_y_continuous(expand = c(0, 0),labels = percent) +
    coord_cartesian(ylim = c(0, 1)) + 
    labs(y="Desk Utilisation",fill="") + 
    scale_fill_brewer(palette = "Accent")
  
}

occupancy_by_sensor <- function(joined_observations) {
  joined_observations %>%
    group_by(surveydeviceid) %>%
    summarise(occupied_hours = sum(sensor_value)/6,
              booked_hours = sum(is_booked)/6,
              sample_hours = n()/6)
}


closest_colour <- function(r,g,b) {
  
  # Ranks the built-in R colours by distance to a given RGB.
  
  colour_rgbs <- col2rgb(colours()) %>%
    t() %>%
    as_tibble() %>%
    mutate(colour = colours())
  
  colour_rgbs %>% 
    mutate(d_red = (red - r) ^ 2,
           d_green = (green - g) ^ 2,
           d_blue = (blue - b) ^2,
           d_total = d_red + d_green + d_blue) %>%
    arrange(d_total)
  
  
}

closest_colour_plot <- function(r,g,b) {
  
  chart_data <- closest_colour(r,g,b) %>% head(20)
  
  ggplot(chart_data, aes(x = fct_reorder(colour,d_total),
                         y = max(d_total) - d_total,
                         fill = colour)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none") +
    scale_fill_manual(values = chart_data$colour)
  
  
}
