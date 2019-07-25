library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(lubridate)
library(dbtools)
library(DT)

source("data_cleaning_functions.R")
source("charting_functions.R")



my_data <- s3tools::read_using(feather::read_feather, "alpha-app-matrixbooking/leeds.feather")
bookings <- s3tools::read_using(feather::read_feather, "alpha-app-matrixbooking/bookings.feather")
locations <- s3tools::read_using(feather::read_feather, "alpha-app-matrixbooking/locations.feather")

date_list <- lubridate::date(my_data$obs_datetime)


ui <- dashboardPage(
  dashboardHeader(title = "Matrixbooking app v0.0.5", titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput(inputId = "date_filter",
                     label = "pick a date range",
                     start = max(date_list) %m-% months(1),
                     end = max(date_list),
                     min = min(date_list),
                     max = max(date_list)),
      #menuItem("Data Download", tabName = "data_download"),
      menuItem("Report by room", tabName = "by_room"),
      menuItem("Report by building", tabName = "by_building")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_download",
              fluidRow(
                selectInput(inputId = "download_building", 
                            label = "Select building",
                            choices = unique(my_data$building)),
                
                dateRangeInput(inputId = "download_date_range", 
                               label = "Select time period to download"),
                
                actionButton(inputId = "download_data", label = "download data")
              )
      ),
      
      tabItem(tabName = "by_room",
              fluidRow(
                selectInput(inputId = "room",
                            label = "Select room",
                            choices = sort(unique(my_data$roomname))
                ),
                tabBox(id = "by_room_tabBox",
                       tabPanel("bookings by length",
                                plotlyOutput(outputId = "booking_length_by_room")),
                       tabPanel("throughout the day",
                                plotlyOutput(outputId = "booked_permutation_room"),
                                plotlyOutput(outputId = "permutation_throughout_day")),
                       tabPanel("bookedness and occupancy",
                                plotlyOutput(outputId = "booked_by_occupancy"),
                                plotlyOutput(outputId = "occupancy_by_bookedness")),
                       tabPanel("Time to booking",
                                plotlyOutput(outputId = "room_booking_histogram")),
                       tabPanel("Time to booking (cancellations)",
                                plotlyOutput(outputId = "room_cancellations_histogram")),
                       tabPanel("Time from booking to cancellation",
                                plotlyOutput(outputId = "room_time_to_cancellation_histogram"))
                ),
                tabBox(id = "room_narrative_tabBox",
                       tabPanel("booking permutation summary",
                                dataTableOutput(outputId = "permutation_table_room"))
                ),
                fluidRow(
                  tabBox(id = "room_data_tabBox",
                         tabPanel("bookings data",
                                  dataTableOutput(outputId = "bookings_data")),
                         tabPanel("locations data",
                                  dataTableOutput(outputId = "locations_data"))
                  )
                )
              )
      ),
      
      tabItem(tabName = "by_building",
              fluidRow(
                pickerInput(inputId = "room_type",
                            label = "Select Room Type(s)",
                            choices = sort(unique(my_data$devicetype)),
                            selected = unique(my_data$devicetype),
                            options = list(`actions-box` = TRUE, `selected-text-format` = "count > 4"),
                            multiple = TRUE),
                tabBox(id = "by_building_tabBox",
                       tabPanel("room usage by date",
                                plotlyOutput(outputId = "daily_rooms_by_occupancy")),
                       tabPanel("room usage by weekday",
                                plotlyOutput(outputId = "weekday_rooms_by_occupancy")),
                       tabPanel("smoothing",
                                plotlyOutput(outputId = "smoothing_chart")),
                       tabPanel("Bookings by utilisation",
                                plotlyOutput(outputId = "daily_bookings_by_occupancy_fill"),
                                plotlyOutput(outputId = "daily_bookings_by_occupancy_stack")),
                       tabPanel("Bookings by permutation",
                                selectInput(inputId = "selected_column",
                                            label = "Select grouping column",
                                            choices = c("date",
                                                        "roomname",
                                                        "devicetype"),
                                            selected = "date"),
                                plotlyOutput(outputId = "booked_permutation_building")),
                       tabPanel("Time to booking",
                                plotlyOutput(outputId = "room_booking_histogram")),
                       tabPanel("Time to booking (cancellations)",
                                plotlyOutput(outputId = "room_cancellations_histogram")),
                       tabPanel("Time from booking to cancellation",
                                plotlyOutput(outputId = "building_time_to_cancellation_histogram"))
                ),
                
                tabBox(id = "building_data_tabBox",
                       tabPanel("booking permutation summary",
                                dataTableOutput(outputId = "permutation_table_building"))
                       
                )
              )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  
  # create reactive data object -----------------------------------------------------------
  joined_observations <- reactive({
    my_data %>%
      dplyr::filter(obs_datetime >= input$date_filter[1],
                    obs_datetime <= paste0(input$date_filter[2], " 23:50"))
  })
  
  room_observations <- reactive({
    joined_observations() %>%
      dplyr::filter(roomname == input$room)
  })
  
  building_observations <- reactive({
    joined_observations() %>%
      dplyr::filter(devicetype %in% input$room_type)
  })
  
  # by room charts ----------------------------------------------------------
  
  
  output$booked_by_occupancy <- renderPlotly({
    bookings_during_occupied_time(room_observations())
  })
  
  output$occupancy_by_bookedness <- renderPlotly({
    occupancy_during_booked_time(room_observations())
  })
  
  output$booked_permutation_room <- renderPlotly({
    room_utilisation_permutation(room_observations(), "date")
    
  })
  
  output$booking_length_by_room <- renderPlotly({
    
    room_booking_length_histogram(room_observations())
  })
  
  output$room_booking_histogram <- renderPlotly({
    bookings_created_to_meeting_histogram(bookings %>%
                                            filter(location_id %in% unique(room_observations()$location)
                                            )
    )
  })
  
  output$room_cancellations_histogram <- renderPlotly({
    cancelled_bookings_histogram(bookings %>%
                                   filter(status == "CANCELLED",
                                          location_id %in% unique(room_observations()$location)
                                   )
    )
  })
  
  output$room_time_to_cancellation_histogram <- renderPlotly({
    start_to_cancelled_bookings_histogram(bookings %>%
                                            filter(status == "CANCELLED",
                                                   location_id %in% unique(room_observations()$location)
                                            )
    )
  })
  
  
  output$permutation_table_room <- renderDataTable({
    permutation_summary(room_observations())
  })
  
  output$permutation_throughout_day <- renderPlotly({
    occupancy_through_day(room_observations())
  })
  
  output$bookings_data <- renderDataTable({
    DT::datatable(bookings, filter = list(position = 'top', clear = FALSE))
  })
  
  output$locations_data <- renderDataTable({
    DT::datatable(locations, filter = list(position = 'top', clear = FALSE))
  })
  
  # by building charts ------------------------------------------------------
  
  
  
  
  output$daily_bookings_by_occupancy_fill <- renderPlotly({
    booking_utilisation_by_date(building_observations())
    
  })
  
  output$daily_bookings_by_occupancy_stack <- renderPlotly({
    booking_utilisation_by_date(building_observations(),
                                "stack")
    
  })
  
  output$daily_rooms_by_occupancy <- renderPlotly({
    room_utilisation_by_date(building_observations())
  })
  output$weekday_rooms_by_occupancy <- renderPlotly({
    room_utilisation_by_weekday(building_observations())
  })
  
  output$smoothing_chart <- renderPlotly({
    smoothing_chart(building_observations(), 0.5)
  })
  
  output$booked_permutation_building <- renderPlotly({
    room_utilisation_permutation(building_observations(),
                                 input$selected_column)
  })
  
  output$building_booking_histogram <- renderPlotly({
    bookings_created_to_meeting_histogram(bookings %>%
                                            filter(location_id %in% unique(building_observations()$location)
                                            )
    )
  })
  
  output$building_cancellations_histogram <- renderPlotly({
    cancelled_bookings_histogram(bookings %>%
                                   filter(status == "CANCELLED",
                                          location_id %in% unique(building_observations()$location)
                                   )
    )
  })
  
  output$building_time_to_cancellation_histogram <- renderPlotly({
    start_to_cancelled_bookings_histogram(bookings %>%
                                            filter(status == "CANCELLED",
                                                   location_id %in% unique(building_observations()$location)
                                            )
    )
  })
  
  output$permutation_table_building <- renderDataTable({
    permutation_summary(building_observations())
  })
  
  
  # change to TRUE when deployed
  
  # refreshes connection when grey screened
  
  session$allowReconnect(T)
  
}

shinyApp(ui, server)