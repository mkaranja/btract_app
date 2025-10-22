box::use(
  shiny[tagList, fluidRow, tableOutput, renderTable, column, br, div, moduleServer, NS, actionLink, selectInput,
        renderPrint, textOutput, tags, dateRangeInput, checkboxInput, HTML,showModal, modalDialog,
        reactiveValues, observeEvent, observe, reactive, renderText, req, bindCache ],
  bslib[card, card_header],
  shinycssloaders[withSpinner],
  highcharter,
  shinyWidgets[actionBttn],
  DT[renderDT, datatable]
)

#' @export
box::use(
  app/logic/load_datasets[banana, germinating_embryos, openfield],
  app/logic/datasets[load_data, highchart_data, grouped_data]
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
    tags$style(HTML("
  .inline {
    display: inline-block;
    vertical-align: middle;
  }
  .card-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
")),

    card(
      card_header(
        div(class = "inline", textOutput(ns("title"))),  # Title with inline class
        div(
          class = "inline",
          style = "float: right;",
            actionBttn(
              ns("all"),
              "View all genotypes",
              size = "xs",
              style = "bordered",
              color = "primary")
            )
      ),

      highcharter$highchartOutput(ns("chart"), height = 350)  |>
        withSpinner(type = 7, size = 1)

    )
  )
}

#' @export
server <- function(id, controls, genotypes_col, stage) {
  moduleServer(id, function(input, output, session) {

    js_bar_clicked <- highcharter$JS("function(event) {Shiny.onInputChange('app-overview_charts-bar_clicked', [event.point.category]);}")


    data_input <- reactive({

      data <- readRDS(paste0("app/data/",tolower(controls$site),"_", stage$v, ".rds"))
      # First, identify which column contains date information
      date_col_name <- grep("Date", names(data), value = TRUE)[1]
      num_col <- ifelse(stage$v == "extracted_seeds", "Total_Seeds", grep("Number", names(data), value = TRUE))
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

      if(stage$v %in%  c("crosses", "bunches") ){
        data$Number <- rep(1, times=nrow(data))
      } else {
        data$Number <- as.numeric(data[[num_col]])
        data <- data  |>
          dplyr::filter(!is.na(num_col))
      }

      data

    })# |> bindCache(input$stage)


    chart_input <- reactive({
      data_input() |>
        dplyr::group_by(!!rlang::sym(genotypes_col)) |>
        dplyr::tally() |>
        dplyr::arrange(desc(n))
    })

    output$chart <- highcharter$renderHighchart({
      req(controls$site)

      result <- chart_input() |>
        dplyr::slice_head(n=20) |>
        dplyr::collect()

      highcharter$highchart() |>
        highcharter$hc_add_series(
          data = result$n,
          type = "bar",
          name = "",
          events = list(click = js_bar_clicked),
          showInLegend = FALSE,
          pointPadding = 0.1,  # Space between bars within each category
          groupPadding = 0.2   # Space between groups of bars
        ) |>
        highcharter$hc_xAxis(
          categories = result[[genotypes_col]],  # Ensure correct labels for each bar
          labels = list(
            rotation = 0,                     # Set to 0 degrees to display labels horizontally
            style = list(fontSize = "10px"),    # Adjust font size to ensure all labels fit
            step = 1                            # Ensure every label is displayed
          ),
          tickInterval = 1                     # Display every label
        ) |>
        highcharter$hc_yAxis(
          labels = list(style = list(fontSize = "10px"))  # Optional: Adjust Y-axis label font size
        ) |>
        highcharter$hc_exporting(enabled = TRUE) |>
        highcharter$hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          valueDecimals = 0,
          shared = TRUE,
          borderWidth = 2
        ) |>
        highcharter$hc_title(text = "") |>
        highcharter$hc_add_theme(highcharter$hc_theme_elementary())
    }) # |> bindCache(input$stage)



    output$title <- renderText({
      paste0("Top 20 ", gsub("_", " ", genotypes_col),"s")
    })


    # View all
    observeEvent( input$all , {
      showModal(
        modalDialog(
          title = paste0(gsub("_", " ", genotypes_col),"s"),
          size = "l",
          easyClose = TRUE,
          fade = TRUE,

          renderDT(
            chart_input(),
            server = TRUE
          )
        )
      )
    })


  })
}
