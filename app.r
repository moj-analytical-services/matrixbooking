library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(lubridate)


source("data_cleaning_functions.R")
source("charting_functions.R")



my_data <- s3tools::read_using(feather::read_feather,"alpha-app-matrixbooking/leeds.feather")
date_list <- lubridate::date(my_data$obs_datetime)


ui <- dashboardPage(
  dashboardHeader(title = "Matrixbooking app v0.0.4"),
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput(inputId = "date_filter",
                     label = "pick a date range",
                     start = min(date_list),
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
                                plotlyOutput(outputId = "occupancy_by_bookedness"))
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
                                plotlyOutput(outputId = "booked_permutation_building"))
                )
                
                
              )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  joined_observations <- reactive({
    my_data %>%
    filter(obs_datetime >= input$date_filter[1],
           obs_datetime <= paste0(input$date_filter[2], " 23:50"))
  })
  
  output$booked_by_occupancy <- renderPlotly({
    bookings_during_occupied_time(joined_observations() %>% filter(roomname == input$room))
  })
  
  output$occupancy_by_bookedness <- renderPlotly({
    occupancy_during_booked_time(joined_observations() %>% filter(roomname == input$room))
  })
  
  output$daily_bookings_by_occupancy_fill <- renderPlotly({
    booking_utilisation_by_date(joined_observations() %>% filter(devicetype %in% input$room_type))
    
  })
  
  output$daily_bookings_by_occupancy_stack <- renderPlotly({
    booking_utilisation_by_date(joined_observations() %>% 
                                  filter(devicetype %in% input$room_type),
                                "stack")
    
  })
  
  output$daily_rooms_by_occupancy <- renderPlotly({
    room_utilisation_by_date(joined_observations() %>%
                               filter(devicetype %in% input$room_type))
  })
  
  
  output$booked_permutation_room <- renderPlotly({
    room_utilisation_permutation(joined_observations() %>%
                                   filter(roomname == input$room), "date")
    
  })
  
  output$booking_length_by_room <- renderPlotly({
    
    room_booking_length_histogram(joined_observations() %>%
                                    filter(roomname == input$room))
  })
  
  
  output$permutation_throughout_day <- renderPlotly({
    occupancy_through_day(joined_observations() %>%
                            filter(roomname == input$room))
  })
  
  output$weekday_rooms_by_occupancy <- renderPlotly({
    room_utilisation_by_weekday(joined_observations() %>%
                                  filter(devicetype %in% input$room_type))
  })
  
  output$smoothing_chart <- renderPlotly({
    smoothing_chart(joined_observations() %>% filter(devicetype %in% input$room_type), 0.5)
  })
  
  output$booked_permutation_building <- renderPlotly({
    room_utilisation_permutation(joined_observations(), input$selected_column)
  })
  
  
  # change to TRUE when deployed
  
  # refreshes connection when grey screened
  
  session$allowReconnect(T)
  
}

shinyApp(ui, server)