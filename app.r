library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(lubridate)
library(dbtools)
library(DT)

source("data_cleaning_functions.R")
source("charting_functions.R")
source("airflow_queries.R")


my_data <- s3tools::read_using(feather::read_feather, "alpha-app-matrixbooking/leeds.feather")
print("My_data loaded")
bookings <- s3tools::read_using(feather::read_feather, "alpha-app-matrixbooking/bookings.feather")
print("bookings loaded")
locations <- s3tools::read_using(feather::read_feather, "alpha-app-matrixbooking/locations.feather")
print("locations loaded")
date_list <- lubridate::date(my_data$obs_datetime)


ui <- dashboardPage(
  dashboardHeader(title = "Matrixbooking app v0.0.9", titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput(inputId = "date_filter",
                     label = "pick a date range",
                     start = max(date_list) %m-% months(1),
                     end = max(date_list),
                     min = min(date_list),
                     max = max(date_list)),
      menuItem("Data Download", tabName = "data_download"),
      menuItem("Report by room", tabName = "by_room"),
      menuItem("Report by building", tabName = "by_building"),
      menuItem("Top users", tabName = "by_users")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_download",
              fluidRow(
                uiOutput("survey_picker"),
                
                dateRangeInput(inputId = "download_date_range", 
                               label = "Select time period to download",
                               start = today() %m-% months(1)),
                
                actionButton(inputId = "download_data", label = "download data"),
                downloadButton(outputId = "download_report", label = "download word report")
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
                                dataTableOutput(outputId = "permutation_table_room"),
                                plotlyOutput(outputId = "permutation_pie_room"))
                ),
                tabBox(id = "room_data_tabBox",
                       width = 9,
                       tabPanel("bookings data",
                                dataTableOutput(outputId = "bookings_data_room")),
                       tabPanel("locations data",
                                dataTableOutput(outputId = "locations_data_room"))
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
                                plotlyOutput(outputId = "weekday_rooms_by_occupancy"),
                                plotlyOutput(outputId = "bookings_heatmap"),
                                plotlyOutput(outputId = "occupancy_heatmap"),
                                plotlyOutput(outputId = "weekday_throughout_day")),
                       tabPanel("room usage by type",
                                plotlyOutput(outputId = "room_types_by_occupancy")),
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
                                plotlyOutput(outputId = "building_booking_histogram")),
                       tabPanel("Time to booking (cancellations)",
                                plotlyOutput(outputId = "building_cancellations_histogram")),
                       tabPanel("Time from booking to cancellation",
                                plotlyOutput(outputId = "building_time_to_cancellation_histogram"))
                ),
                
                tabBox(id = "building_narrative_tabBox",
                       tabPanel("booking permutation summary",
                                dataTableOutput(outputId = "permutation_table_building"),
                                plotlyOutput(outputId = "permutation_pie_building"))
                       
                ),
                tabBox(id = "building_data_tabBox",
                       width = 9,
                       tabPanel("bookings data",
                                dataTableOutput(outputId = "bookings_data_building")),
                       tabPanel("locations data",
                                dataTableOutput(outputId = "locations_data_building"))
                )
              )
              
      ),
      
      tabItem(
        tabName = "by_users",
        fluidRow(
          tabBox(id = "user_tables",
                 tabPanel("Top bookers",
                          "This table shows the total number of hours booked by user.",
                          dataTableOutput(outputId = "top_bookers")),
                 tabPanel("Out of hours bookers",
                          "This table shows the number of bookings made by users outside working hours (9-5)",
                          dataTableOutput(outputId = "out_of_hours_bookers")),
                 tabPanel("Top no-showers",
                          "This table shows the top users by number of meetings that were cancelled due to no-shows",
                          dataTableOutput(outputId = "no_showers"))
          )
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  RV <- reactiveValues()
  RV$joined_observations <- my_data
  RV$bookings <- bookings
  RV$surveys <- get_surveys()
  
  output$survey_picker <- renderUI({
    selectInput(inputId = "survey_picker",
                label = "Select Occupeye Survey",
                choices = unique(RV$surveys$name))
  })
  
  observeEvent(input$survey_picker, {
    RV$survey_name <- input$survey_picker
    
    RV$selected_survey_id <- RV$surveys %>%
      dplyr::filter(name == input$survey_picker) %>%
      pull(survey_id)
  })
  
  observeEvent(input$download_data, {
    start.time <- Sys.time()
    
    withProgress(message = glue("Downloading data from {input$survey_picker}"), {
      RV$sensor_observations <- get_sensor_observations(RV$selected_survey_id,
                                                        input$download_date_range[[1]],
                                                        input$download_date_range[[2]]) %>%
        change_p_to_person()
      
      RV$bookings <- get_bookings(RV$selected_survey_id,
                                  input$download_date_range[[1]],
                                  input$download_date_range[[2]])
      
      RV$locations <- get_locations(RV$selected_survey_id)
      
      RV$sensorised_bookings <- convert_bookings_to_sensors(RV$bookings)
      
      RV$joined_observations <- get_joined_df(RV$sensor_observations,
                                              RV$sensorised_bookings %>% dplyr::filter(status != "CANCELLED")) %>%
        filter_time_range("09:00","17:00")
      
      
      updateDateRangeInput(session,
                           inputId = "date_filter",
                           min = input$download_date_range[[1]],
                           max = input$download_date_range[[2]],
                           start = input$download_date_range[[1]],
                           end = input$download_date_range[[2]])
      updateSelectInput(session,
                        inputId = "room",
                        choices = sort(unique(RV$joined_observations$roomname)))
      
      updatePickerInput(session,
                        inputId = "room_type",
                        choices = sort(unique(RV$joined_observations$devicetype)),
                        selected = sort(unique(RV$joined_observations$devicetype)))
    })
    
    
    sendSweetAlert(session, "Data download finished")
    
    
    end.time <- Sys.time()
    
    print(end.time - start.time)
    
  })
  
  output$download_report <- downloadHandler(
    filename = "matrixbooking report.docx",
    content = function(file) {
      out_report <- "word_report.rmd"
      
      src <- normalizePath(out_report)
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, out_report, overwrite = TRUE)
      
      word_report_reference <- s3tools::download_file_from_s3("alpha-app-matrixbooking/matrixbooking-report-reference.dotx",
                                                              "matrixbooking-report-reference.dotx",
                                                              overwrite = TRUE)
      withProgress(message = "Generating report...", {
        out <- rmarkdown::render(out_report, 
                                 params = list(start_date = input$date_filter[1],
                                               end_date = input$date_filter[2], 
                                               joined_observations = joined_observations(),
                                               bookings = RV$bookings,
                                               survey_name = RV$survey_name))
        file.rename(out, file)
      })
      
      
    })
  
  # create reactive data object -----------------------------------------------------------
  joined_observations <- reactive({
    RV$joined_observations %>%
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
                                            dplyr::filter(location_id %in% unique(room_observations()$location)
                                            )
    )
  })
  
  output$room_cancellations_histogram <- renderPlotly({
    cancelled_bookings_histogram(bookings %>%
                                   dplyr::filter(status == "CANCELLED",
                                                 location_id %in% unique(room_observations()$location)
                                   )
    )
  })
  
  output$room_time_to_cancellation_histogram <- renderPlotly({
    start_to_cancelled_bookings_histogram(bookings %>%
                                            dplyr::filter(status == "CANCELLED",
                                                          location_id %in% unique(room_observations()$location)
                                            )
    )
  })
  
  
  output$permutation_table_room <- renderDataTable({
    permutation_summary(room_observations())
  })
  
  output$permutation_pie_room <- renderPlotly({
    permutation_summary_pie(room_observations())
  })
  
  output$permutation_throughout_day <- renderPlotly({
    occupancy_through_day(room_observations())
  })
  
  output$bookings_data_room <- renderDataTable({
    DT::datatable(bookings %>%
                    dplyr::filter(location_id %in% unique(room_observations()$location)),
                  filter = list(position = 'top', clear = FALSE))
  })
  
  output$locations_data_room <- renderDataTable({
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
  
  output$room_types_by_occupancy <- renderPlotly({
    room_utilisation_by_type(building_observations())
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
                                            dplyr::filter(location_id %in% unique(building_observations()$location)
                                            )
    )
  })
  
  output$building_cancellations_histogram <- renderPlotly({
    cancelled_bookings_histogram(bookings %>%
                                   dplyr::filter(status == "CANCELLED",
                                                 location_id %in% unique(building_observations()$location)
                                   )
    )
  })
  
  output$building_time_to_cancellation_histogram <- renderPlotly({
    start_to_cancelled_bookings_histogram(bookings %>%
                                            dplyr::filter(status == "CANCELLED",
                                                          location_id %in% unique(building_observations()$location)
                                            )
    )
  })
  
  output$permutation_table_building <- renderDataTable({
    permutation_summary(building_observations())
  })
  
  output$permutation_pie_building <- renderPlotly({
    permutation_summary_pie(building_observations())
  })
  
  output$bookings_heatmap <- renderPlotly({
    time_of_day_heatmap(building_observations(), "is_booked")
  })
  
  output$occupancy_heatmap <- renderPlotly({
    time_of_day_heatmap(building_observations(), "sensor_value")
  })
  
  output$weekday_throughout_day <- renderPlotly({
    time_of_day_bar(building_observations())
    
  })
  
  output$bookings_data_building <- renderDataTable({
    DT::datatable(bookings %>%
                    dplyr::filter(location_id %in% unique(building_observations()$location)),
                  filter = list(position = 'top', clear = FALSE))
  })
  
  output$locations_data_building <- renderDataTable({
    DT::datatable(locations, filter = list(position = 'top', clear = FALSE))
  })
  
  # User abuse charts ------------------------------------------------------
  
  output$top_bookers <- renderDataTable({
    DT::datatable(top_booked_hours_by_user(RV$bookings))
  })
  
  output$out_of_hours_bookers <- renderDataTable({
    DT::datatable(out_of_hours_table(RV$bookings))
  })
  
  output$no_showers <- renderDataTable({
    DT::datatable(top_no_showers(RV$bookings))
  })
  
  
  # change to TRUE when deployed
  
  # refreshes connection when grey screened
  
  session$allowReconnect(T)
  
}

shinyApp(ui, server)