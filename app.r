library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)

source("data_cleaning_functions.R")
source("charting_functions.R")


joined_observations <- s3tools::read_using(feather::read_feather,"alpha-app-matrixbooking/leeds.feather")

ui <- dashboardPage(
  dashboardHeader(title = "Matrixbooking app v0.0.2"),
  dashboardSidebar(
    sidebarMenu(
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
                            choices = unique(joined_observations$building)),
                
                dateRangeInput(inputId = "download_date_range", 
                               label = "Select time period to download"),
                
                actionButton(inputId = "download_data", label = "download data")
              )
      ),
      
      tabItem(tabName = "by_room",
              fluidRow(
                selectInput(inputId = "room",
                            label = "Select room",
                            choices = sort(unique(joined_observations$roomname))
                            ),
                plotlyOutput(outputId = "booking_length_by_room"),
                plotlyOutput(outputId = "booked_permutation"),
                plotlyOutput(outputId = "booked_by_occupancy"),
                plotlyOutput(outputId = "occupancy_by_bookedness")
              )
      ),
      
      tabItem(tabName = "by_building",
              fluidRow(
                pickerInput(inputId = "room_type",
                            label = "Select Room Type(s)",
                            choices = sort(unique(joined_observations$devicetype)),
                            selected = unique(joined_observations$devicetype),
                            options = list(`actions-box` = TRUE, `selected-text-format` = "count > 4"),
                            multiple = TRUE),
                plotlyOutput(outputId = "daily_rooms_by_occupancy"),
                plotlyOutput(outputId = "daily_bookings_by_occupancy_fill"),
                plotlyOutput(outputId = "daily_bookings_by_occupancy_stack")
                
                
              )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  output$booked_by_occupancy <- renderPlotly({
    bookings_during_occupied_time(joined_observations %>% filter(roomname == input$room))
  })
  
  output$occupancy_by_bookedness <- renderPlotly({
    occupancy_during_booked_time(joined_observations %>% filter(roomname == input$room))
  })
  
  output$daily_bookings_by_occupancy_fill <- renderPlotly({
    booking_utilisation_by_date(joined_observations %>% filter(devicetype %in% input$room_type))
    
  })
  
  output$daily_bookings_by_occupancy_stack <- renderPlotly({
    booking_utilisation_by_date(joined_observations %>% 
                                  filter(devicetype %in% input$room_type),
                                "stack")
    
  })
  
  output$daily_rooms_by_occupancy <- renderPlotly({
    room_utilisation_by_date(joined_observations %>%
                               filter(devicetype %in% input$room_type))
  })
  
  
  output$booked_permutation <- renderPlotly({
    room_utilisation_permutation(joined_observations %>% filter(roomname == input$room))
    
  })
  
  output$booking_length_by_room <- renderPlotly({
    
    room_booking_length_histogram(joined_observations %>% filter(roomname == input$room))
  })
  
  # change to TRUE when deployed
  
  # refreshes connection when grey screened
  
  session$allowReconnect(TRUE)
  
}

shinyApp(ui, server)