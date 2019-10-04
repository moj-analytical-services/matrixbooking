library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(lubridate)
library(dbtools)
library(DT)

source("data_cleaning_functions.R")
source("charting_functions.R")
source("athena_queries.R")


my_data <- s3tools::read_using(feather::read_feather, "alpha-app-matrixbooking/joined_observations.feather")
print("My_data loaded")
bookings <- s3tools::read_using(feather::read_feather, "alpha-app-matrixbooking/bookings.feather")
print("bookings loaded")
locations <- s3tools::read_using(feather::read_feather, "alpha-app-matrixbooking/locations.feather")
print("locations loaded")
date_list <- lubridate::date(my_data$obs_datetime)


ui <- dashboardPage(
  dashboardHeader(title = "Matrixbooking app v0.3.1", titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
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
                               start = max(min(date_list), today() %m-% months(1)),
                               end = today() - 1),
                uiOutput("start_time"),
                uiOutput("end_time"),
                
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
                                plotOutput(outputId = "booked_permutation_room"),
                                plotOutput(outputId = "permutation_throughout_day")),
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
                                tableOutput(outputId = "permutation_table_room"),
                                plotOutput(outputId = "permutation_pie_room"))
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
                       tabPanel("room usage by weekday",
                                plotlyOutput(outputId = "bookings_heatmap"),
                                plotlyOutput(outputId = "occupancy_heatmap"),
                                plotlyOutput(outputId = "weekday_throughout_day")),
                       tabPanel("Bookings by permutation",
                                selectInput(inputId = "selected_column",
                                            label = "Select grouping column",
                                            choices = c("date",
                                                        "weekday",
                                                        "roomname",
                                                        "devicetype"),
                                            selected = "date"),
                                plotOutput(outputId = "booked_permutation_building")),
                       tabPanel("Time to booking",
                                plotlyOutput(outputId = "building_booking_histogram")),
                       tabPanel("Time to booking (cancellations)",
                                plotlyOutput(outputId = "building_cancellations_histogram")),
                       tabPanel("Time from booking to cancellation",
                                plotlyOutput(outputId = "building_time_to_cancellation_histogram"))
                ),
                
                tabBox(id = "building_narrative_tabBox",
                       tabPanel("booking permutation summary",
                                tableOutput(outputId = "permutation_table_building"),
                                plotOutput(outputId = "permutation_pie_building"))
                       
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
                          "This table shows the total number of hours booked by user. By default, it's sorted by the number of confirmed hours booked",
                          dataTableOutput(outputId = "top_bookers")
                 ),
                 tabPanel("Out of hours bookers",
                          "This table shows the number of bookings made by users outside working hours. Select working hours:",
                          uiOutput(outputId = "out_of_hours_start_time"),
                          uiOutput(outputId = "out_of_hours_end_time"),
                          dataTableOutput(outputId = "out_of_hours_table"),
                          dataTableOutput(outputId = "out_of_hours_bookers")
                 ),
                 tabPanel("Top no-showers",
                          "This table shows the top users by number of meetings that were cancelled due to no-shows",
                          dataTableOutput(outputId = "no_showers")
                 )
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
  
  time_list <- get_time_list()
  
  output$survey_picker <- renderUI({
    selectInput(inputId = "survey_picker",
                label = "Select Occupeye Survey",
                choices = unique(RV$surveys$name))
  })
  
  output$start_time <- renderUI({
    selectInput(inputId = "start_time",
                label = "Start time:",
                choices = time_list,
                selected = "09:00")
  })
  
  output$end_time <- renderUI({
    selectInput(inputId = "end_time",
                label = "End time:",
                choices = time_list,
                selected = "17:00")
  })
  
  output$out_of_hours_start_time <- renderUI({
    selectInput(inputId = "out_of_hours_start_time",
                label = "Start time:",
                choices = time_list,
                selected = "09:00")
  })
  
  output$out_of_hours_end_time <- renderUI({
    selectInput(inputId = "out_of_hours_end_time",
                label = "End time:",
                choices = time_list,
                selected = "17:00")
  })
  
  
  observeEvent(input$survey_picker, {
    RV$survey_name <- input$survey_picker
    
    RV$selected_survey_id <- RV$surveys %>%
      dplyr::filter(name == input$survey_picker) %>%
      pull(survey_id)
    
    start_date <- RV$surveys %>% dplyr::filter(survey_id == RV$selected_survey_id) %>% pull(startdate)
    end_date <- RV$surveys %>% dplyr::filter(survey_id == RV$selected_survey_id) %>% pull(enddate)
    
    updateDateRangeInput(session,
                         inputId = "download_date_range",
                         start = max(as.Date(start_date), today() %m-% months(1)),
                         min = start_date,
                         max = end_date)
    
  })
  
  observeEvent(input$download_data, {
    start.time <- Sys.time()
    
    withProgress(message = glue("Downloading data from {input$survey_picker}"), {
      RV$sensor_observations <- get_sensor_observations(RV$selected_survey_id,
                                                        input$download_date_range[[1]],
                                                        input$download_date_range[[2]]) %>%
        change_p_to_person() %>%
        remove_non_business_days() %>%
        fix_bad_sensor_observations()
      
      
      RV$bookings <- get_bookings(RV$selected_survey_id,
                                  input$download_date_range[[1]],
                                  input$download_date_range[[2]])
      
      RV$locations <- get_locations(RV$selected_survey_id)
      
      RV$sensorised_bookings <- convert_bookings_to_sensors(RV$bookings)
      
      RV$joined_observations <- get_joined_df(RV$sensor_observations,
                                              RV$sensorised_bookings %>% dplyr::filter(status != "CANCELLED")) %>%
        filter_time_range(input$start_time,input$end_time)
      
      
      updateSelectInput(session,
                        inputId = "room",
                        choices = sort(unique(RV$joined_observations$roomname)))
      
      updatePickerInput(session,
                        inputId = "room_type",
                        choices = sort(unique(RV$joined_observations$devicetype)),
                        selected = sort(unique(RV$joined_observations$devicetype)))
      
      feather::write_feather(RV$joined_observations, "joined_observations.feather")
      s3tools::write_file_to_s3("joined_observations.feather",
                                "alpha-app-matrixbooking/joined_observations.feather",
                                overwrite = T)
      
      feather::write_feather(RV$bookings, "bookings.feather")
      s3tools::write_file_to_s3("bookings.feather",
                                "alpha-app-matrixbooking/bookings.feather",
                                overwrite = T)
      
      feather::write_feather(RV$locations, "locations.feather")
      s3tools::write_file_to_s3("locations.feather",
                                "alpha-app-matrixbooking/locations.feather",
                                overwrite = T)
      
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
                                 params = list(start_date = input$download_date_range[1],
                                               end_date = input$download_date_range[2], 
                                               joined_observations = joined_observations(),
                                               bookings = RV$bookings,
                                               survey_name = RV$survey_name))
        file.rename(out, file)
      })
      
      
    })
  
  # create reactive data object -----------------------------------------------------------
  joined_observations <- reactive({
    RV$joined_observations %>%
      dplyr::filter(obs_datetime >= input$download_date_range[1],
                    obs_datetime <= paste0(input$download_date_range[2], " 23:50"))
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
  
  output$booked_permutation_room <- renderPlot({
    room_utilisation_permutation(room_observations(), "date")
    
  })
  
  
  output$booking_length_by_room <- renderPlotly({
    
    room_booking_length_histogram(room_observations())
  })
  
  output$room_booking_histogram <- renderPlotly({
    bookings_created_to_meeting_histogram(RV$bookings %>%
                                            dplyr::filter(location_id %in% unique(room_observations()$location)
                                            )
    )
  })
  
  output$room_cancellations_histogram <- renderPlotly({
    cancelled_bookings_histogram(RV$bookings %>%
                                   dplyr::filter(status == "CANCELLED",
                                                 location_id %in% unique(room_observations()$location)
                                   )
    )
  })
  
  output$room_time_to_cancellation_histogram <- renderPlotly({
    start_to_cancelled_bookings_histogram(RV$bookings %>%
                                            dplyr::filter(status == "CANCELLED",
                                                          location_id %in% unique(room_observations()$location)
                                            )
    )
  })
  
  
  output$permutation_table_room <- renderTable({
    permutation_summary(room_observations())
  })
  
  output$permutation_pie_room <- renderPlot({
    permutation_summary_pie(room_observations())
  })
  
  output$permutation_throughout_day <- renderPlot({
    occupancy_through_day(room_observations())
  })
  
  output$bookings_data_room <- renderDataTable({
    DT::datatable(RV$bookings %>%
                    dplyr::filter(location_id %in% unique(room_observations()$location)),
                  filter = list(position = 'top', clear = FALSE),
                  options = list(scrollX = TRUE))
  })
  
  output$locations_data_room <- renderDataTable({
    DT::datatable(locations,
                  filter = list(position = 'top', clear = FALSE),
                  options = list(scrollX = TRUE))
  })
  
  
  
  
  # by building charts ------------------------------------------------------
  
  
  
  output$booked_permutation_building <- renderPlot({
    room_utilisation_permutation(building_observations(),
                                 input$selected_column)
  })
  
  output$building_booking_histogram <- renderPlotly({
    bookings_created_to_meeting_histogram(RV$bookings %>%
                                            dplyr::filter(location_id %in% unique(building_observations()$location)
                                            )
    )
  })
  
  output$building_cancellations_histogram <- renderPlotly({
    cancelled_bookings_histogram(RV$bookings %>%
                                   dplyr::filter(status == "CANCELLED",
                                                 location_id %in% unique(building_observations()$location)
                                   )
    )
  })
  
  output$building_time_to_cancellation_histogram <- renderPlotly({
    start_to_cancelled_bookings_histogram(RV$bookings %>%
                                            dplyr::filter(status == "CANCELLED",
                                                          location_id %in% unique(building_observations()$location)
                                            )
    )
  })
  
  output$permutation_table_building <- renderTable({
    permutation_summary(building_observations())
  })
  
  output$permutation_pie_building <- renderPlot({
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
    DT::datatable(RV$bookings %>%
                    dplyr::filter(location_id %in% unique(building_observations()$location)),
                  filter = list(position = 'top', clear = FALSE),
                  options = list(scrollX = TRUE))
  })
  
  output$locations_data_building <- renderDataTable({
    DT::datatable(RV$locations, filter = list(position = 'top', clear = FALSE),
                  options = list(scrollX = TRUE))
  })
  
  # User abuse charts ------------------------------------------------------
  
  output$top_bookers <- renderDataTable({
    DT::datatable(top_booked_hours_by_user(RV$bookings), rownames = FALSE)
  })
  
  output$out_of_hours_table <- renderDataTable({
    DT::datatable(out_of_hours_table(RV$bookings,
                                     input$out_of_hours_start_time,
                                     input$out_of_hours_end_time),
                  rownames = FALSE,
                  options = list(scrollX = TRUE))
  })
  
  output$out_of_hours_bookers <- renderDataTable({
    DT::datatable(out_of_hours_bookings(RV$bookings,
                                        input$out_of_hours_start_time,
                                        input$out_of_hours_end_time),
                  rownames = FALSE,
                  options = list(scrollX = TRUE))
  })
  
  
  output$no_showers <- renderDataTable({
    DT::datatable(top_no_showers(RV$bookings), rownames = FALSE)
  })
  
  
  # change to TRUE when deployed
  
  # refreshes connection when grey screened
  
  session$allowReconnect(T)
  
}

shinyApp(ui, server)
