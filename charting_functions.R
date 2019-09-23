library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(janitor)
library(forcats)

get_booked_permutation <- function(joined_observations) {
  joined_observations %>% 
    mutate(booked_permutation = case_when(
      sensor_value == 1 & is_booked == 1 ~ "3. Booked and occupied",
      sensor_value == 1 & is_booked == 0 ~ "2. Occupied but not booked",
      sensor_value == 0 & is_booked == 1 ~ "1. Booked but not occupied",
      sensor_value == 0 & is_booked == 0 ~ "0. Neither booked nor occupied",
      is.na(sensor_value) ~ "invalid occupeye sensor reading") %>% factor()
    )
}

room_utilisation_permutation <- function(joined_observations, varname) {
  
  expr <- sym(varname)
  
  
  utilisation_by_permutation <- get_booked_permutation(joined_observations) %>%
    count(booked_permutation, !!expr) %>%
    group_by(!!expr) %>%
    mutate(prop = prop.table(n)) %>% 
    dplyr::filter(booked_permutation != "0. Neither booked nor occupied")
  
  
  
  ggplot(data = utilisation_by_permutation,
         mapping = aes(x = !!expr,
                       y = prop,
                       fill = booked_permutation)) +
    geom_bar(position = "stack",
             stat = "identity") +
    scale_fill_brewer(palette = "Spectral") +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    theme(axis.text.x = element_text(angle = 45)) +
    ggtitle("Room booking and occupancy")
  
}

get_utilisation_by_date <- function(joined_observations) {
  joined_observations %>%
    group_by(date, roomname, devicetype) %>% 
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

room_utilisation_by_type <- function(joined_observations, bar_position = 'fill') {
  utilisation_by_type <- get_utilisation_by_date(joined_observations)
  
  count_by_cat <- utilisation_by_type %>%
    group_by(devicetype,
             util_cat) %>%
    summarise(count = n()) %>%
    mutate(prop = prop.table(count)) %>%
    ungroup()
  
  device_order <- count_by_cat %>%
    dplyr::filter(util_cat == "Effective utilisation") %>%
    mutate(devicetype = fct_reorder(devicetype, desc(prop))) %>%
    pull(devicetype) %>%
    levels()
  
  
  ggplot(data = count_by_cat, 
         mapping = aes(x = factor(devicetype, levels = device_order), 
                       y = count, 
                       fill = util_cat)) +
    geom_bar(position = bar_position, 
             stat = "identity") + 
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Room types by utilisation") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
}

get_utilisation_by_date_and_booking <- function(joined_observations) {
  # counts the number of non-cancelled bookings by util_cat and day
  
  joined_observations %>% dplyr::filter(status != "CANCELLED") %>%
    group_by(id, date, roomname, is_booked) %>% 
    summarise(utilisation = mean(sensor_value, na.rm = T)) %>% 
    dplyr::filter(!is.na(id)) %>%
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
    dplyr::filter(sensor_value == 1)
  
  ggplot(data = data, aes(x = date)) +
    stat_summary(aes(y = is_booked), 
                 fun.y = "mean",
                 geom = "bar") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Proportion of occupied time that contains a booking")
  
}

occupancy_during_booked_time <- function(joined_observations) {
  data <- joined_observations %>%
    dplyr::filter(is_booked == 1)
  
  ggplot(data = data, aes(x = date)) +
    stat_summary(aes(y = sensor_value), 
                 fun.y = "mean",
                 geom = "bar") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle("Proportion of booked time when the room is occupied")
}

room_booking_length_histogram <- function(joined_observations) {
  
  # Switched to using plot_ly, because ggplotly produced strange results.
  # Should I port everything else?
  
  booking_lengths <- joined_observations %>%
    dplyr::filter(is_booked == 1) %>%
    group_by(id) %>%
    mutate(booked_hours = is_booked/6,
           booking_length = sum(booked_hours)) %>%
    group_by(booking_length) %>%
    summarise(booking_hours = sum(booked_hours),
              utilisation = sum(sensor_value)/6) %>%
    mutate(freq = booking_hours/booking_length)
  
  utilisation <- scales::percent(booking_lengths$utilisation / booking_lengths$booking_hours)
  
  
  plot_ly(booking_lengths) %>%
    add_trace(x = ~booking_length, 
              y = ~booking_hours,
              type = "bar",
              yaxis = "y1",
              name = "booking hours",
              text = paste0("</br> Booking length (hours): ",
                            booking_lengths$booking_length,
                            "</br> Total hours booked: ",
                            booking_lengths$booking_hours,
                            "</br> Of which was occupied: ",
                            booking_lengths$utilisation,
                            "</br> Number of bookings: ",
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
                        title = "Total hours"),
           barmode = "overlay",
           title = "Bookings by length (hours)",
           hovermode = "compare")
  
  
}

permutation_summary <- function(joined_observations) {
  
  joined_observations %>%
    get_booked_permutation() %>%
    group_by(booked_permutation) %>%
    summarise(working_hours = n()/6) %>%
    mutate(proportion = prop.table(working_hours),
           working_hours = round(working_hours, 2)) %>%
    adorn_totals() %>%
    mutate(proportion = scales::percent(proportion))
  
}


permutation_summary_pie <- function(joined_observations) {
  permutation_summary_table <- permutation_summary(joined_observations) %>% 
    dplyr::filter(booked_permutation != "Total")
  
  category_colours <- c("lightgrey",
                        "sandybrown",
                        "lemonchiffon",
                        "forestgreen")
  
  plot_ly(permutation_summary_table,
          labels = ~(booked_permutation),
          values = ~working_hours,
          type = "pie",
          textinfo = "label+percent",
          showlegend = FALSE,
          marker = list(colors = category_colours),
          sort = F)
  
}

occupancy_through_day <- function(joined_observations) {
  my_data <- joined_observations %>%
    get_booked_permutation() %>%
    mutate(time = strftime(obs_datetime, "%H:%M"))
  
  every_nth = function(n) {
    return(function(x) {x[c(rep(FALSE, n - 1), TRUE)]})
  }
  
  category_colours <- c("lightgrey",
                        "sandybrown",
                        "lemonchiffon",
                        "darkseagreen3",
                        "red")
  
  ggplot(my_data,
         aes(x = date,
             y = fct_rev(time),
             fill = booked_permutation)) +
    geom_tile() +
    ylab("Time") +
    scale_y_discrete(breaks = every_nth(n = 6)) +
    scale_fill_manual(values = category_colours) +
    theme(legend.title = element_blank())
    
    
    
}


get_smoothing_table <- function(joined_observations, smoothing_factor) {
  
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  
  cat_order <- c("Full Smoothing", "Current Utilisation", "Partial Smoothing")
  
  get_utilisation_by_date(joined_observations) %>%
    mutate(day = weekdays(date)) %>%
    count(util_cat, day) %>%
    group_by(day) %>%
    mutate(prop = prop.table(n)) %>%
    dplyr::filter(util_cat == "Unused") %>%
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

bookings_created_to_meeting_histogram <- function(bookings) {
  bookings %>%
    add_created_to_meeting() %>%
    dplyr::filter(created_to_meeting <= 90) %>%
    ggplot(aes(x = created_to_meeting, fill = status)) +
    geom_histogram(alpha=0.5, position="identity", binwidth = 1) +
    labs(x = "Days between creating the booking and start of meeting")
}

cancelled_bookings_histogram <- function(cancelled_bookings) {
  
  cancelled_bookings %>%
    add_created_to_meeting() %>%
    dplyr::filter(created_to_meeting <= 90) %>%
    ggplot(aes(x = created_to_meeting, fill = status_reason)) +
    geom_histogram(alpha=0.5, position="identity", binwidth = 1)
  
}

start_to_cancelled_bookings_histogram <- function(cancelled_bookings) {
  
  cancelled_bookings %>%
    add_created_to_cancelled() %>%
    dplyr::filter(created_to_cancelled <= 90) %>%
    ggplot(aes(x = created_to_cancelled, fill = status_reason)) +
    geom_histogram(alpha=0.5, position="identity", binwidth = 1)
  
}

cancelled_to_meeting_histogram <- function(cancelled_bookings) {
  cancelled_bookings %>%
    add_cancelled_to_meeting() %>%
    dplyr::filter(cancelled_to_meeting <= 90) %>%
    ggplot(aes(x = cancelled_to_meeting, fill = status_reason)) +
    geom_histogram(alpha=0.5, position="identity", binwidth = 1)
  
}

cancelled_bookings_by_day <- function(cancelled_bookings) {
  
  bookings %>%
    mutate(start_date = as_date(time_from),
           created_date = as_date(cancelled_time)) %>%
    group_by(created_date) %>%
    count() %>%
    plot_ly(x = ~created_date,
            y = ~n,
            type = 'bar')
  
  
}

time_of_day_heatmap <- function(joined_observations, varname) {
  
  expr <- sym(varname)
  
  weekdays <- c("Monday",
                "Tuesday",
                "Wednesday",
                "Thursday",
                "Friday")
  
  data <- joined_observations %>%
    mutate(time_of_day = strftime(obs_datetime, format="%H:%M"),
           weekday = weekdays(obs_datetime)) %>%
    group_by(weekday, time_of_day) %>%
    summarise(count = sum(!!expr, na.rm = T))
  
  plot_ly(data,
          x = ~factor(weekday, levels = weekdays),
          y = ~fct_rev(time_of_day),
          z = ~count,
          type = "heatmap",
          colors = "Reds") %>%
    layout(title = case_when(varname == "is_booked" ~"most popular booking times",
                             TRUE ~ "Occupancy heatmap"),
           xaxis = list(title = ""),
           yaxis = list(title = "Time of day"))
  
}

time_of_day_bar <- function(joined_observations) {
  
  
  
  weekdays <- c("Monday",
                "Tuesday",
                "Wednesday",
                "Thursday",
                "Friday")
  
  data <- joined_observations %>%
    mutate(time_of_day = strftime(obs_datetime, format="%H:%M"),
           weekday = weekdays(obs_datetime)) %>%
    group_by(weekday, time_of_day) %>%
    summarise(booked = mean(is_booked, na.rm = T),
              occupied = mean(sensor_value, na.rm = T)) %>%
    gather(key = "variable", value = "value", booked, occupied)
  
  chart <- ggplot(data, aes(x = hm(time_of_day), y = value, fill = fct_rev(variable))) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
    facet_wrap(~factor(weekday, levels = weekdays), nrow = 5) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    scale_x_time() +
    labs(title = "Occupancy and bookings by weekday and time",
         x = "Time of Day") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank())
  
  ggplotly(chart)
}

out_of_hours_table <- function(bookings) {
  bookings %>%
    dplyr::filter(!in_time_range(created, "09:00", "17:00")) %>%
    group_by(booked_by_id) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  
}

top_booked_hours_by_user <- function(bookings) {
  bookings %>%
    mutate(booked_hours = difftime(time_to, time_from, units = "hours")) %>%
    group_by(booked_by_id, status) %>%
    summarise(booked_hours = sum(booked_hours)) %>%
    spread(status, booked_hours) %>%
    arrange(desc(CONFIRMED))
  
}

top_no_showers <- function(bookings) {
  
  bookings %>%
    dplyr::filter(status_reason == "CANCELLED_NO_SHOW") %>%
    group_by(booked_by_id) %>%
    summarise(no_shows = n()) %>%
    arrange(desc(no_shows))
  
}