box::use(
  shiny[tagList, fluidRow, tableOutput, renderTable, column, br, div, moduleServer,
        NS, selectInput, renderPrint, textOutput, tags, dateRangeInput, checkboxInput,
        bindCache, req, reactiveValues, reactiveVal, observeEvent, observe, reactive],
  bslib[card, card_header],
  shinycssloaders[withSpinner],
  highcharter
)

#' @export
box::use(
  app/logic/datasets[load_data],
  app/logic/vars[r]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(

      card(
        card_header(
          selectInput(ns("stage"),"", c("Crosses"="crosses",
                                        "Banana Bunches"="bunches",
                                        "Seeds extracted" = "extracted_seeds",
                                        "Embryo rescued" = "embryo_rescue",
                                        "Embryo germination" = "germination",
                                        "Plant in openfield" = "openfield"
                                        ), width = "100%")
            ),
        fluidRow(
          column(4, offset = 8,
            shinyWidgets::awesomeRadio(
              ns("group"), "",
              c("yearly", "monthly", "daily"),
              selected = "yearly",
              inline = TRUE
            )
          ),
          column(12,
                 highcharter$highchartOutput(ns("totals"), height = 250)  |>
                   withSpinner(type = 7, size = 1),
                 textOutput(ns("tbl"))
                 )

          )
     )
    )
  )
}

#' @export
server <- function(id, controls, tab) {
  moduleServer(id, function(input, output, session) {

    js_bar_clicked <- highcharter$JS("function(event) {Shiny.onInputChange('app-overview_charts-bar_clicked', [event.point.category]);}")

    stage <- reactiveValues(v=NULL)

    observe({
      stage$v <- input$stage
    })

    data_input <- reactive({
      data <- readRDS(paste0("app/data/",tolower(controls$site),"_", input$stage, ".rds"))
      # First, identify which column contains date information
      date_col_name <- grep("Date", names(data), value = TRUE)[1]
      num_col <- ifelse(input$stage == "extracted_seeds", "Total_Seeds", grep("Number", names(data), value = TRUE))
      # Convert the identified column to Date type and handle filtering
      data <- data |>
        # Convert the string date column to an actual Date object
        dplyr::mutate(across(all_of(date_col_name), ~as.Date(.))) |>
        # Filter between start and end dates
        dplyr::filter(dplyr::between(.data[[date_col_name]], controls$daterange[1], controls$daterange[2])) |>
        # Extract date components
        dplyr::mutate(
          Yearly = lubridate::year(.data[[date_col_name]]),
          Monthly = lubridate::month(.data[[date_col_name]]),
          Daily = lubridate::day(.data[[date_col_name]])
        ) |>
        dplyr::rename(Date = date_col_name)

      if(input$stage %in%  c("crosses", "bunches") ){
        data$Number <- rep(1, times=nrow(data))
      } else {
        data$Number <- as.numeric(data[[num_col]])
        data <- data  |>
          dplyr::filter(!is.na(num_col))
      }

      data

    }) #|> bindCache(input$stage, input$group)

    output$totals <- highcharter$renderHighchart({
      req(controls$site)
      data <- data_input()
      if(input$group == "daily"){
        data <- data |>
          dplyr::group_by(Location, Date) |>
          dplyr::summarize(number = sum(Number, na.rm = T), .groups = 'drop')
        data$Time <- data$Date
        group_name <- "Daily"
      } else if(input$group == "monthly"){
        data <- within(data, Time <- sprintf("%d-%02d", Yearly, Monthly))
        data <- data |>
          dplyr::group_by(Location,Time) |>
          dplyr::summarize(number = sum(Number, na.rm = T), .groups = 'drop')
        group_name = "Monthly"
      }  else if(input$group == "yearly"){
        data <- data |>
          dplyr::group_by(Location, Yearly) |>
          dplyr::summarize(number = sum(Number, na.rm = T), .groups = 'drop')
        data$Time = data$Yearly
        group_name = "Yearly"
      }

      hc <- highcharter$hchart(data, "column",
                               highcharter$hcaes(x = Time, y = number, group = Location),
                   events = list(click = js_bar_clicked))

      hc |>
        highcharter$hc_exporting(enabled = TRUE) |>
        highcharter$hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", valueDecimals=0,
                   shared = TRUE, borderWidth = 2) |>
        highcharter$hc_add_theme(highcharter$hc_theme_elementary())
    })# |> bindCache(input$stage, input$group)


    # Return stage
    stage

  })
}
