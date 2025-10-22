box::use(
  shiny[tagList, fluidRow, column, br, div, moduleServer, NS, selectInput, renderPrint, textOutput, tags, dateRangeInput, checkboxInput,
        reactiveValues, observeEvent, observe, reactive, updateDateRangeInput, reactiveValuesToList,updateSelectInput,
        uiOutput, renderUI],
  bslib[layout_column_wrap],
  lubridate[make_date, year, years]
)


#' @export
box::use(
  app/logic/datasets[load_data],
  app/logic/vars[sites, last_3_years, start_date, end_date]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, uiOutput(ns("station"))),
      column(3, dateRangeInput(ns("daterange"),
                               "DATE RANGE:",
                               min = start_date,
                               max = end_date,
                               start = last_3_years,
                               end = end_date,
                               width = "100%"))#,
     # column(6, br(), checkboxInput(ns("only_crosses"), "Select only crosses done between this date range", width = "100%"))
    )
  )
}

#' @export
server <- function(id, tab, res_auth) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    v1 <- reactiveValues(
      site = NULL,
      daterange = NULL,
      only_crosses = FALSE
    )

    output$station <- renderUI({
      station <- reactiveValuesToList(res_auth)$station
      if(length(station)>0){
        if(station == "All"){
          location <- c(sites, "All")
        } else {
          location <- station
        }

        selectInput(ns("site"), "SELECT SITE", choices = location, multiple = FALSE, selected = location[1])
      }
    })

    observeEvent( input$site , {
      if(isTRUE(tab$nav == "overview")){
        if(isTRUE(!is.null(input$site))){
          v1$site <- input$site
        }
      }
    })


    observeEvent( input$daterange , {
      if(isTRUE(tab$nav == "overview")){
        v1$daterange <- input$daterange
      }
    })


    observeEvent( input$only_crosses , {
      if(isTRUE(tab$nav == "overview")){
        v1$only_crosses <- input$only_crosses
      }
    })

    v1

  })
}
