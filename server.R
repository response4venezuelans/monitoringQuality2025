library(shiny)
library(activityinfo)
library(bslib)
library(gridlayout)
library(DT)
library(waiter)
library(readxl)
library(writexl)
library(purrr)
library(dplyr)
library(stringr)

server <- function(input, output, session) {
  metrics_db <- reactiveValues(
    total_activities = 0,
    total_errors = 0,
    percent_error = 0
  )
  metrics_excel <- reactiveValues(
    total_activities = 0,
    total_errors = 0,
    percent_error = 0
  )
  fetchedData <- reactiveVal(tibble(Message = "No data available"))
  fetchedDataExcel <- reactiveVal(tibble(Message = "No data available"))

  # Observe filter selection changes and update choices
  observe({
    req(input$filterRadioButton)
    updateSelectInput(session, "filterItemSelection",
      choices = if (input$filterRadioButton == "country") country_list else partner_list,
      selected = "All"
    )
  })

  # Fetch data when the button is clicked
  observeEvent(input$getDataFromActivityInfoDB, {
    waiter <- Waiter$new(id = "dataTable")
    waiter$show()
    monitoring5WData <- get_data_from_ai(input$filterRadioButton, input$filterItemSelection)

    if (is.null(monitoring5WData) || nrow(monitoring5WData) == 0) {
      fetchedData(tibble(Message = "No data available"))
    } else {
      fetchedData(monitoring5WData)
      updateActionButton(session, "checkDataFromActivityInfoDB", disabled = FALSE)
      updateActionButton(session, "downloadDataAI", disabled = FALSE)
    }
    waiter$hide()
  })

  # Run QA analysis on the fetched data
  observeEvent(input$checkDataFromActivityInfoDB, {
    req(fetchedData())
    data_to_check <- fetchedData()

    tryCatch({
      checked_data <- qa_check(data_to_check)
      fetchedData(checked_data)
      metrics_db$total_activities <- get_total_activities(checked_data)
      metrics_db$total_errors <- get_total_activities_to_review(checked_data, "QA_sum")
      metrics_db$percent_error <- get_percentage_activities(metrics_db$total_errors, metrics_db$total_activities)
      updateActionButton(session, "downloadDataAI", disabled = FALSE)
    }, error = function(e) {
      showNotification(
        str_c("QA check error: ", conditionMessage(e)),
        type = "error",
        duration = 15
      )
    })
  })

  output$dataTable <- renderDT({
    datatable(fetchedData())
  })

  output$downloadDataAI <- downloadHandler(
    filename = function() "error_report.xlsx",
    content = function(file) {
      error_data <- fetchedData() |> filter(QA_sum == 1) |> select(-QA_sum)
      qa_cols_with_errors <- error_data |>
        select(starts_with("QA_")) |>
        select(where(~ any(. == 1, na.rm = TRUE))) |>
        names()
      writexl::write_xlsx(
        error_data |> select(-starts_with("QA_"), all_of(qa_cols_with_errors)),
        path = file
      )
    }
  )

  output$downloadDataExcel <- downloadHandler(
    filename = function() "error_report.xlsx",
    content = function(file) {
      writexl::write_xlsx(fetchedDataExcel(), path = file)
    }
  )

  ### Metric Boxes

  output$total_activities_box <- renderUI({
    value_box(
      title = "Activities",
      showcase = activities_icon,
      value = metrics_db$total_activities,
      theme = "info",
      class = "my-valuebox"
    )
  })

  output$total_activities_box_xlsx <- renderUI({
    value_box(
      title = "Activities",
      showcase = activities_icon,
      value = metrics_excel$total_activities,
      theme = "info",
      class = "my-valuebox"
    )
  })

  output$total_error_box <- renderUI({
    value_box(
      title = "Total Errors",
      showcase = error_icon,
      value = metrics_db$total_errors,
      theme = "warning",
      class = "my-valuebox"
    )
  })

  output$total_error_box_xlsx <- renderUI({
    value_box(
      title = "Total Errors",
      showcase = error_icon,
      value = metrics_excel$total_errors,
      theme = "warning",
      class = "my-valuebox"
    )
  })

  output$total_percent_box <- renderUI({
    value_box(
      title = "Percentage of errors",
      showcase = percent_icon,
      value = metrics_db$percent_error,
      theme = "danger",
      class = "my-valuebox"
    )
  })
  output$total_percent_box_xlsx <- renderUI({
    value_box(
      title = "Percentage of errors",
      showcase = percent_icon,
      value = metrics_excel$percent_error,
      theme = "danger",
      class = "my-valuebox"
    )
  })

  # Reactive expression to read the uploaded Excel file
  uploaded_data <- reactive({
    req(input$uploadExcelFile)
    input$uploadExcelFile$datapath |> read_excel()
  })

  output$previewXlsTable <- renderDT({
    req(fetchedDataExcel())
    datatable(fetchedDataExcel(), options = list(pageLength = 5))
  })

  observeEvent(input$analizeDataFromExcelFile, {
    data <- uploaded_data()
    is_template_valid <- check_dataframe_structure(data, "www/template_5w_2025.xlsx", sheet = 1)
    if (!is_template_valid) {
      showModal(
        modalDialog(
          title = "Error: Data structure check failed!!",
          easy_close = TRUE,
          "The structure of the uploaded data does not match the expected template."
        )
      )
      return(NULL)
    }
    checked_data <- data |>
      rename_columns() |>
      add_platform_column() |>
      add_indicator_type(indicator_df) |>
      add_country_iso_codes(country_list_df) |>
      add_population_sums() |>
      qa_check()

    fetchedDataExcel(checked_data)
    metrics_excel$total_activities <- get_total_activities(checked_data)
    metrics_excel$total_errors <- get_total_activities_to_review(checked_data, "QA_sum")
    metrics_excel$percent_error <- get_percentage_activities(metrics_excel$total_errors, metrics_excel$total_activities)
  })
}
