box::use(
  shiny[
    tagList, sidebarLayout, sidebarPanel, mainPanel, div, moduleServer, NS,
    selectInput, tags, downloadButton, downloadHandler, br, reactive, req,
    bindCache, observe, reactiveValuesToList, showNotification
  ],
  reactable[reactableOutput, renderReactable, reactable, colDef, JS, getReactableState, reactableTheme, reactableLang],
  DT[DTOutput, renderDT, datatable, JS],
  shinycssloaders[withSpinner],
  bslib[card],
  dplyr[select, everything],
  cachem[cache_mem],
  utils[write.csv],
  shinypop[nx_notify_error, use_notiflix_notify]
)

box::use(
  app/logic/load_datasets[
    flowering, banana, plantlets, subcultures, rooting, weaning1, weaning2,
    screenhouse, hardening, openfield
  ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    use_notiflix_notify(
      position = "right-bottom",
      timeout = 30,
      closeButton = TRUE,
      width = "300px"
      ),

    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput(ns("dataset"), "Select dataset:",
                    choices = c(
                      #"Flowering" = "flowering",
                      "Crosses" = 'banana',
                      "Plantlets" = "plantlets",
                      "Direct germination" = "directgermination",
                      "Subcultures" = "subcultures",
                      "Rooting" = 'rooting',
                      "Weaning 1" = "weaning1",
                      "Weaning 2" = "weaning2",
                      "Screenhouse" = "screenhouse",
                      "Hardening" = "hardening",
                      "Open-field" = "openfield",
                      "Ploidy Analysis" = "ploidy",
                      "Contamination" = "contamination"
                    ),
                    selected = "banana"
        )#, br(), downloadButton(ns("download_data"), "Download CSV")
      ),
      mainPanel(
        width = 10,
        card(
          full_screen = TRUE,
          DTOutput(ns("table")) |>
            withSpinner(type = 7, size = 1)
        ), br(), br()
      )
    )
  )
}

#' @export
server <- function(id, tab, res_auth) {
  moduleServer(id, function(input, output, session) {
    # Create cache for dataset loading
    data_cache <- cache_mem(max_size = 500 * 1024^2)

    # Reactive data loader with caching
    # data_input <- reactive({
    #   req(tab$nav == "data" && tab$data == "Raw Data Table")
    #   location <- reactiveValuesToList(res_auth)$station
    #   data <- readRDS(paste0("app/data/",tolower(location),"_",input$dataset, ".rds"))
    # }) |>
    #   bindCache(input$dataset, tab$nav, tab$data)

    data_input <- reactive({
      req(tab$nav == "data" && tab$data == "Raw Data Table")
      location <- reactiveValuesToList(res_auth)$station
      file_path <- paste0("app/data/", tolower(location), "_", input$dataset, ".rds")

      # Check if file exists
      if (file.exists(file_path)) {
        data <- readRDS(file_path)
      } else {
        # Display message if file doesn't exist
        nx_notify_error("Oops: There is no data available for the selected dataset.")
        data <- NULL  # or you can set this to a default empty data structure
      }

      data
    }) |> bindCache(input$dataset, tab$nav, tab$data)

    output$table <- renderDT({
      req(data_input())
      dt <- data_input()
      colnames(dt) <- gsub("_"," ", names(dt))
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


    selected <- reactive(getReactableState("table", "selected"))

    download_input <- reactive({
      if(is.null(selected())){
        dt <- data_input()
      } else {
        dt <- data_input()[selected(),]
      }
    })

    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste0(input$dataset, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(download_input(), file, row.names = FALSE)
      }
    )
  })
}
