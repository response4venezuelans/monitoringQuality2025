library(shiny)
library(activityinfo)
library(bslib)
library(gridlayout)
library(DT)
library(waiter)
library(readxl)  # For reading Excel files

server <- function(input, output, session) {
  metrics <- reactiveValues(
    total_activities = 0,
    total_errors = 0,
    percent_error = 0
  )
  # Initialize fetchedData as an empty reactive value
  fetchedData <- reactiveVal(data.frame(Message = "No data available", stringsAsFactors = FALSE))
  
  # Observe filter selection changes and update choices
  observe({
    req(input$filterRadioButton)
    updateSelectInput(session, "filterItemSelection",
                      choices = if (input$filterRadioButton == "country") 
                        countryList else 
                          partnerList,
                      selected = "All")
  })
  
  # Fetch data when the button is clicked
  observeEvent(input$getDataFromActivityInfoDB, {
    waiter <- Waiter$new(id = "dataTable") # Attach waiter to dataTable
    waiter$show()
    # Fetch data
    monitoring5WData <- getDataFromAI(input$filterRadioButton, input$filterItemSelection)
    
    # Ensure the fetched data is a valid data frame
    if (is.null(monitoring5WData) || nrow(monitoring5WData) == 0) {
      fetchedData(data.frame(Message = "No data available", stringsAsFactors = FALSE))
    } else {
      fetchedData(monitoring5WData)
      updateActionButton(session, "checkDataFromActivityInfoDB", disabled = FALSE)
    }
    waiter$hide()
  })
  
  # Reactive expression to read the uploaded Excel file
  uploaded_data <- reactive({
    req(input$uploadExcelFile)  # Ensure a file is uploaded
    file <- input$uploadExcelFile$datapath
    read_excel(file)  # Read the Excel file
  })
  
  # Render the preview of the uploaded Excel file
  output$previewXlsTable <- renderDT({
    req(uploaded_data())  # Ensure data is available
    datatable(uploaded_data(), options = list(pageLength = 5))
  })
  
  # Add any additional processing or analysis here
  observeEvent(input$analizeDataFromExcelFile, {
    # Perform QA analysis on the uploaded data
    data <- uploaded_data()
    print('boton_excel')
    # TODO Add your QA analysis code here 
  })
  
  # Add any additional processing or analysis here
  observeEvent(input$checkDataFromActivityInfoDB, {
    # Perform QA analysis on the uploaded data
    # Load the script
    #source("R/QAFromAIDB.R")
    req(fetchedData())
    # Run the script function on the data
    data_to_check <- fetchedData()

    checked_data <- qa_check(data_to_check)
    # Update the reactive value with the checked data (optional)
    fetchedData(checked_data)
    metrics$total_activities<-get_total_activities(checked_data)
    metrics$total_errors<-get_total_activities_to_review(checked_data, "QA_sum")
    metrics$percent_error <- get_percentage_activities(metrics$total_errors,metrics$total_activities)
  })
  
  # Render DataTable safely
  output$dataTable <- renderDT({
    datatable(fetchedData())
  })
  
  output$downloadDataAI <- downloadHandler(
    filename = function() {
      "error_report.xlsx"
    },
    content = function(file) {
      # Write your dataset to the file
      # Example with the writexl package
      writexl::write_xlsx(fetchedData(), path = file)
    }
  )
  
  output$total_activities_box <- renderUI({
    value_box( 
      title = "Activities", 
      value = metrics$total_activities, 
      theme = "info",
      class = "my-valuebox"
    )
  })
  
  output$total_error_box <- renderUI({
    value_box( 
      title = "Total Error", 
      value = metrics$total_errors, 
      theme = "warning",
      class = "my-valuebox"
    )
  })
  
  output$total_percent_box <- renderUI({
    value_box( 
      title = "Total percent", 
      value = metrics$percent_error, 
      theme = "danger",
      class = "my-valuebox"
    )
  })
  
}