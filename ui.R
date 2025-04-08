library(shiny)
library(activityinfo)
library(bslib)
library(gridlayout)
library(DT)
library(waiter)
ui <- page_navbar(
  title = "Activity Info Quality RMRP 2025/2026",
  selected = "About",
  collapsible = TRUE,
  theme = R4Vtheme,
  header = tags$head(
    tags$link(rel = "shortcut icon", href = "img/logo.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "img/r4vLogo.png"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans|Source+Sans+Pro")
  ),
  
  # Home panel
  nav_panel(
    title = "About",
    tags$div(
      style = "text-align: center;",
      tags$img(src = "img/r4vLogo.png", height = "100px"),
      tags$h1("Quality Control of Monitoring Data ActivityInfo - 5W 2025"),
      tags$div(
        style = "text-align: justify;",
        tags$p(
          "This app is designed to check existing data in ActivityInfo or to
           quickly check offline data before bulk-uploading it.
           After Quality Assurance (QA), users can then download a version
           of their dataset according for the creation of the R4V Consolidated
           Report on reached population by sector."
        ),
        tags$p(
          tags$i(
            "To get started navigate to the following tabs to perform Quatity Assurance
             or Analyze data from excel file previous to uploading it into the regional Database"
          )
        ),
        tags$p(
          tags$i("Any comments to this app please contact the R4V regional IM TEAM")
        )
      )
    )
  ),
  # QA Panel
  nav_panel(
    title = "QA from database",
    layout_sidebar(
      open = TRUE,
      sidebar = tagList(
        tags$h4("Filter Options"),
        tags$p("Use bellow filters to analyze a subset of the data"),
        radioButtons(
          "filterRadioButton",
          "Select option for filter",
          choices = list("Country" = "country", "Partner" = "partner"),
          selected = "country"
        ),
        
        selectInput(
          "filterItemSelection",
          "Select a value for the filter:",
          choices = NULL,
          selected = "All"
        ),
        use_waiter(),
        actionButton("getDataFromActivityInfoDB", "Pull data from ActivityInfo"),
        waiter::waiter_preloader(html = spin_folding_cube()),
        actionButton("checkDataFromActivityInfoDB", "QA analysis", disabled = TRUE),
        waiter::waiter_preloader(html = spin_folding_cube())
      ),
      tags$div(
        tags$h3("Data read from Regional ActivityInfo Database"),
        tags$p("Select a country from the sidebar to filter the database."),
        tags$p("After data is displayed please use the QA button to get the results")
      ),
      DTOutput("dataTable")
    )
  ),
  nav_panel(
    title = "QA from Excel file",
    layout_sidebar(
      open = TRUE,
      sidebar = tagList(
        tags$h4("How to use this QA Test?"),
        tags$p(
          "To upload your Excel file to Activity Info, ensure it complies with the
        reporting template. Once the data is loaded, press the button to run the
        QA test. The results will be displayed on the screen, and you can download
        them in a new Excel file to make corrections. After fixing the issues,
        upload the corrected file back into the Activity Info Database."
        ),
        
      ),
      tags$div(
        tags$h3("Database QA from excel"),
        tags$p(
        "Please upload an excel file in agreement with the following template,
        ressults of the data quality can be reviewed in the results tab"
        ),
        fileInput(
          "uploadExcelFile",
          "Upload a compliant file",
          multiple = FALSE,
          accept = ".xlsx",
        ),
        actionButton(
          "analizeDataFromExcelFile",
          "Excecute QA analysis from excel file"
        ),
        DTOutput("previewXlsTable")
      )
    )
  ),
  nav_panel(
    title = "Results",
    layout_sidebar(
      open = TRUE,
      sidebar = tagList(
        tags$h4("How to interpret these results sheet"),
        tags$p(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur nec felis
          vehicula, egestas odio ac, tristique augue. Nam scelerisque velit a orci
          venenatis, non dapibus sapien aliquet. Suspendisse potenti, et malesuada
          magna eu, vulputate lorem. Nullam dictum risus ut nisi fermentum, nec
          interdum felis commodo. Aenean vel tortor in ligula accumsan dapibus et
          vel est. Proin ut lectus euismod, dapibus dui ut, dictum mi"
        ),
        
      ),
      tags$div(
        tags$h3("QA Results"),
        tags$p("--Select a country from the sidebar to filter the database."),
        DTOutput("previewXlsTable")
      )
    )
  )
)
