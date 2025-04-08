library(shiny)
library(activityinfo)
library(bslib)
library(gridlayout)
library(DT)
library(waiter)
library(readxl)  # For reading Excel files

server <- function(input, output, session) {
  
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
    print('boton_AI')
    # Load the script
    source("R/QAFromAIDB.R")  # Assume it defines a function, e.g., qa_check(data)
    # Run the script function on the data
    data_to_check <- fetchedData()
    # Suppose the script defines a function called qa_check
    #browser()
    checked_data <- qa_check(data_to_check)
    # Update the reactive value with the checked data (optional)
    fetchedData(checked_data)
  })
  
  # Render DataTable safely
  output$dataTable <- renderDT({
    datatable(fetchedData())
  })
}