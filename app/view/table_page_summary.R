box::use(
  shiny[tagList, sidebarLayout,sidebarPanel, mainPanel , div, moduleServer, NS, selectInput, tags,reactiveValuesToList,
    updateDateRangeInput, dateRangeInput, downloadButton, downloadHandler, br, reactive, observe, observeEvent, p, req],
  DT[DTOutput, renderDT, datatable, JS],
  shinycssloaders[withSpinner],
  bslib[card, card_header],
  shinyjs[hidden]
)
#' @export
box::use(
  #app/logic/load_datasets[overall_banana]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        dateRangeInput(ns("daterange"), "First pollination date:"), br(),
        selectInput(ns("aggregate_by"),
                    "Aggregate by:",
                    choices = c("Location","Crossnumber", "Cross_Type", "Female_Genotype", "Female_Ploidy",
                                "Male_Genotype", "Male_Ploidy", "Year_of_Pollination", "Month_of_Pollination"),
                    selected = "Location",
                    multiple = TRUE)
      ),

      mainPanel(
        width = 9,
        card(
          DTOutput(ns("table")) |>
            withSpinner(type = 7, size = 1)
        ),

        p("Click any row in table above for more details."), br(),

        div(id = ns("drilldown"),
            card(
              card_header("DRILL-DOWN DETAILS"),
              DTOutput(ns("table2")) |>
                withSpinner(type = 7, size = 1)
            )
        ) |> shinyjs::hidden(),
        br(), br(), br(), br()
      )
    )
  )
}

#' @export
server <- function(id, tab, res_auth) {
  moduleServer(id, function(input, output, session) {

    observe({
      if(isTRUE(tab$data == "Summary Table")){
        req(res_auth)
        location <- reactiveValuesToList(res_auth)$station
        data <- readRDS(paste0("app/data/", tolower(location),"_overall_banana.rds"))
        dt <- unique(data$First_Pollination_Date)
        updateDateRangeInput(session, "daterange", "First pollination date:",
                             min=min(dt, na.rm = T), max = max(dt, na.rm = T), start = min(dt, na.rm = T), end = max(dt, na.rm = T))
      }
    })


    data_input <- reactive({
      location <- reactiveValuesToList(res_auth)$station
      data <- readRDS(paste0("app/data/", tolower(location),"_overall_banana.rds"))

      result <- data |>
        dplyr::filter(dplyr::between(First_Pollination_Date, input$daterange[1], input$daterange[2]))
      result
    })

    summary_data <- reactive({
      req(res_auth)
      result <- data_input()

      if(length(input$aggregate_by)>0){
        result |>
          dplyr::group_by(!!!rlang::syms(input$aggregate_by)) |>
          dplyr::summarise(Number_of_Crosses = dplyr::n(),
                           Banana_Bunches = sum(Banana_Bunches),
                           Total_Seeds = sum(as.integer(Total_Seeds), na.rm = T),
                           Good_Seeds = sum(as.integer(Good_Seeds), na.rm = T),
                           Number_of_Embryo_Rescued = sum(as.integer(Number_of_Embryo_Rescued), na.rm = T),
                           Number_of_Embryo_Germinating = sum(as.integer(Number_of_Embryo_Germinating), na.rm = T),
                           Subcultures = sum(as.integer(Number_of_Subcultures), na.rm = T),
                           Number_Rooting = sum(as.integer(Number_Rooting), na.rm = T),
                           Number_Sent_Out = sum(as.integer(Number_Sent_Out), na.rm = T),
                           Weaning_2_Plantlets = sum(as.integer(Weaning_2_Plantlets), na.rm = T),
                           Number_in_Screenhouse = sum(as.integer(Number_in_Screenhouse), na.rm = T),
                           Number_in_hardening = sum(as.integer(Number_in_hardening), na.rm = T),
                           Number_in_Openfield = sum(as.integer(Number_in_Openfield), na.rm = T)
                           , .groups = 'drop')
      } else {
        result
      }
    })

    output$table <- renderDT({
      dt <- summary_data()
      # colnames(dt) <- gsub("_", " ", names(dt))
      datatable(dt,
                rownames = FALSE,
                filter="top",
                selection = "multiple",
                extensions = c('Buttons','Scroller'),
                options = list( pageLength = 10,
                                lengthMenu = c(5, 10, 25, 50, 100, 500, 1000, 5000, 10000, 20000, 50000, 100000),
                                autoWidth = TRUE,
                                scrollX = TRUE,
                                dom = 'lBfrtip',
                                buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print'),
                                options = list(deferRender = TRUE, scrollY = 400, scroller = TRUE),
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#4682B4', 'color': '#fff'});",
                                  "}")
                ),
                escape = FALSE
      )
    })


    # Drill down

    summary_drill <- reactive({
      # Retrieve selected row data from summary_data
      selectedRow <- summary_data()[as.integer(input$table_rows_selected), ]

      # If no row is selected, return an empty data frame
      if (nrow(selectedRow) == 0) {
        return(data.frame())
      }

      # Extract relevant filters from the selected row
      filters <- list(
        Location = selectedRow$Location,
        Female_Genotype = selectedRow$Female_Genotype,
        Male_Genotype = selectedRow$Male_Genotype,
        Cross_Type = selectedRow$Cross_Type,
        Female_Ploidy = selectedRow$Female_Ploidy,
        Male_Ploidy = selectedRow$Male_Ploidy,
        Female_Sub_Group = selectedRow$Female_Sub_Group,
        Male_Sub_Group = selectedRow$Male_Sub_Group,
        Crossnumber = selectedRow$Crossnumber
      )
      # Remove NULL values from filters
      filters_clean <- purrr::keep(filters, ~ !is.null(.x))

      # Start with the base data
      result <- data_input()

      # Apply filters dynamically
      for (col in names(filters_clean)) {
        result <- result |>
            dplyr::filter(.data[[col]] %in% filters_clean[[col]])
      }

      # Remove empty columns
      janitor::remove_empty(result, "cols")

    })

    observeEvent( input$table_rows_selected , {
      if(is.null(input$table_rows_selected)){
        shinyjs::hide("drilldown")
      } else {
        shinyjs::show("drilldown")
      }
    })

    output$table2 <- renderDT({
      dt <- summary_drill()
      colnames(dt) <- gsub("_", " ", names(dt))
      datatable(dt,
                rownames = FALSE,
                filter="top",
                selection = "multiple",
                extensions = c('Buttons','Scroller'),
                options = list( pageLength = 10,
                                lengthMenu = c(5, 10, 25, 50, 100, 500, 1000, 5000, 10000, 20000, 50000, 100000),
                                autoWidth = TRUE,
                                scrollX = TRUE,
                                dom = 'lBfrtip',
                                buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print'),
                                options = list(deferRender = TRUE, scrollY = 400, scroller = TRUE),
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#4682B4', 'color': '#fff'});",
                                  "}")
                ),
                escape = FALSE
      )
    })


  })
}
