box::use(
  shiny[tagList, sidebarLayout, sidebarPanel, mainPanel, p, h4, div, moduleServer, NS, selectInput, dateRangeInput, radioButtons, tags, numericInput, br, downloadButton,downloadHandler, uiOutput, renderUI,reactiveValuesToList,
    conditionalPanel, reactive, req, renderText, textOutput, renderPrint, HTML, updateSelectInput, observe, observeEvent, reactiveVal, verbatimTextOutput,],
  DT[DTOutput, renderDT, datatable],
  shinycssloaders[withSpinner],
  bslib[card, card_header],
  shinypop[nx_notify_error, use_notiflix_notify]
)

box::use(
  app/logic/datasets[tc_embryo_data2,tc_embryo_germination, tc_plantlet_data ],
  app/data_prep/fcts[clean_genotype_columns],
  app/logic/vars[sites]
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
      width = 4,
      uiOutput(ns("station")),
      # selectInput(ns("site"), "SITE:", choices = c("Arusha", "Ibadan", "Kawanda", "Sendusu")),
      selectInput(ns("dataset"),
                  "DATASET:",
                  choices = c("Crosses (Embryo Rescue)" = "embryo_rescue",
                               "Germinating Embryo" = "germination_ids",
                               "Direct germination" = "directgermination",
                               "Subcultures" = "subcultures",
                               "Rooting" = "rooting",
                               "Weaning 1/ Sending out" = "weaning1",
                               "Weaning 2" = "weaning2",
                               "Screenhouse Transfer" = "screenhouse",
                               "Hardening" = "hardening",
                               "Open-field" = "openfield"),
                  selected = "germinating_embryos"), br(),

      dateRangeInput(
        ns("daterange"),
        label = HTML('<span>DATE RANGE <span style="color: red; font-size: 12px; font-weight: normal;">(Adjust the date range to focus on specific time periods.)</span></span>'),
        max = Sys.Date(),
        start = lubridate::floor_date(lubridate::ymd(Sys.Date()), 'month'),
        end = Sys.Date()
      ),
      br(),
      shiny::radioButtons(ns("number_per_tube"), "Number of plants/ seeds per test tube/ jar",
                          choices = c(
                            "Plantlet ID / test tube (all plantlets with the same ID to be planted in one jag)" = 1,
                            "1 plant per test tube (each plantlet planted individually in its own jag)" = 2,
                            "3 plants per test tube" = 3,
                            "6 plants per test tube" = 6
                          )),


      numericInput(ns("number_of_copies"), "Number of barcode labels (copies)", value=1, min=1),
      downloadButton(ns("download"), "Download")
    ),
    mainPanel(
      width = 8,
      card(
        height = "700px",
        card_header(textOutput(ns("title"))),
        textOutput(ns("txt")),
        selectInput(ns("scanId"), "Scan barcode(s)", choices = c(""), multiple = T, width = "100%"),
        DTOutput(ns("table")) |>
          withSpinner(type = 7, size = 1),
        DTOutput(ns("table2")),
      ), br(), br(), br(), br()
    )
    )
  )
}

#' @export
server <- function(id, tab, res_auth) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    id_col <- reactiveVal(NULL)

    output$title <- renderText({
      if(isTRUE(tab$nav == "Tissue Culture Barcodes")){
        toupper(paste(input$site, ": ", gsub("_", " ", input$dataset)))
      }
    })

    output$station <- renderUI({
      station <- reactiveValuesToList(res_auth)$station
      if(length(station)>0){
        if(station == "All"){
          location <- sites
        } else {
          location <- station
        }
        selectInput(ns("site"),
                    label = HTML('<span>SITE <span style="color: red; font-size: 12px; font-weight: normal;">(You can olny see data from the authorized sites)</span></span>'),
                    choices = location, multiple = FALSE, selected = location[1])
      }

    })

    observe({
      if(isTRUE(tab$nav == "Tissue Culture Barcodes")){
        if(input$dataset =='germination'){
          shinyjs::hide("number_per_tube")
        } else {
          shinyjs::show("number_per_tube")
        }
      }
    })



    data_input <- reactive({
      req(input$site)

      if (isTRUE(tab$nav == "Tissue Culture Barcodes")) {

        dataset_name <- paste0(tolower(input$site), "_", input$dataset)
        file_path <- paste0("app/data/", dataset_name, ".rds")

        # Check if file exists
        if (file.exists(file_path)) {
          dt <- readRDS(file_path) |>
            dplyr::select(-dplyr::contains("URL")) |>
            clean_genotype_columns()

          date_col <- grep("Date", names(dt), value = TRUE)[1]

          dt <- dt |>
            dplyr::mutate(!!date_col := as.Date(.data[[date_col]])) |>
            dplyr::filter(dplyr::between(.data[[date_col]], input$daterange[1], input$daterange[2])) |>
            dplyr::arrange(desc(.data[[date_col]]))
          dt[,c("source_file", "processing_date", "dataset_type")] <- NULL

          dt
        } else {
          # Display message if file doesn't exist
          nx_notify_error("Oops: There is no data available for the selected dataset.")
          return(NULL)  # Return NULL or some default value if file is not found
        }
      }
    })

    observe({
      id_col <- ifelse(input$dataset == "embryo_rescue", "Crossnumber", "PlantletID")
      id <- unique(data_input()[[id_col]])
      updateSelectInput(session, "scanId",  "Scan IDs to show", choices = c('', id))
      id_col(id_col)
    })

    filtered_data <- reactive({
      if(isTRUE(!is.null(data_input()))){
        dt <- data_input() |>
          dplyr::select(-Location)

        if(length(input$scanId) > 0){
          dt <- dt |>
            dplyr::filter(!!rlang::sym(id_col()) %in% input$scanId)
        }
        dt
      }

    })

    # Data-table
    output$table <- renderDT({
      input$site
      input$dataset
      if(isTRUE(!is.null(filtered_data()))){
        dt <- filtered_data()
        colnames(dt) <- gsub("_"," ", names(dt))
        datatable(
          dt, rownames=TRUE,
          options = list(scrollX = TRUE)
        )
      }
    })


    # Download labels

    download_input <- reactive({
      dt <- filtered_data()

      embryo_col <- names(dplyr::select_if(dt, is.numeric))

      if(input$dataset == "embryo_rescue"){
        dt <- tc_embryo_data2(dt = dt,
                              number_per_tube = input$number_per_tube,
                              number_of_copies = input$number_of_copies,
                              #rows_all = input[["table_rows_all"]],
                              rows_selected = input$table_rows_selected)
      } else if( input$dataset %in% c("germination_ids", "earlygermination")){
        dt <- tc_embryo_germination(dt = dt,
                                    number_of_copies = input$number_of_copies,
                                    rows_selected = input$table_rows_selected)
      } else {
        dt <- tc_plantlet_data(dt=dt,
                               embryo_col=embryo_col,
                               number_per_tube=input$number_per_tube,
                               number_of_copies=input$number_of_copies,
                               rows_selected = input$table_rows_selected)
      }
      dt
    })


# on click download
  output$download <- downloadHandler(
    filename = function(){
      paste0(input$site,"-", input$dataset,"-", Sys.Date(),".xls")
    },
    content = function(file) {
      writexl::write_xlsx(download_input(), file)
    }
  )



  })
}
