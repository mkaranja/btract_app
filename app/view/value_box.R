box::use(
  shiny[tagList, fluidRow, HTML, column,p, br, div, moduleServer, NS, selectInput, textOutput, renderText, tags, dateRangeInput, checkboxInput, icon, observe,
        downloadButton, observeEvent, reactive, showModal, modalDialog,modalButton, bindCache, req],
  shinydashboard[valueBoxOutput, renderValueBox, valueBox],
  bslib[value_box, card, card_header],
  shinycssloaders[withSpinner],
  shinyBS[bsModal, ],
  DT[DTOutput, renderDT, datatable],
  data.table[as.data.table],
  datasets[mtcars],
  shinyWidgets[actionBttn],
  reactable[reactableOutput, renderReactable, reactable, colDef, JS, getReactableState, reactableTheme, reactableLang],
  memoise[memoise],
  cachem[cache_mem, ],
  stringr[str_extract]
)

box::use(
  app/data_prep/data_fcts[clean_genotype_columns]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      value_box(
        title = textOutput(ns("title")),
        value = textOutput(ns("value")),
        theme = "primary",
        textOutput(ns("description1")),
        textOutput(ns("description2")),
        textOutput(ns("description3")),
        showcase = tagList(
          actionBttn(
            inputId = ns("my_button"),
            label = "Details",
            style = "bordered",
            color = "default",
            size = "xs",
            block = TRUE,
            no_outline = TRUE
          )
        ),
        showcase_layout = "bottom" # Position the button at the bottom
      )
    ),

    bsModal("modalExample", "Data Table", "my_button", size = "large",
         DTOutput(ns("table")))
  )
}

#' @export
server <- function(id, title, data_file, controls, date_col, seed_column ) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    # data <- reactiveVal(NULL)
    #
    # observe({
    #   if (!is.null(controls$site) && controls$site !="") {
    #     data(data_input())
    #   }
    # })


    # Create a memory cache (adjust size as needed, 200MB shown here)
    data_cache <- cache_mem(max_size = 200 * 1024^2)

    # data_input <- reactive({
    #   data <- readRDS(paste0("app/data/",tolower(controls$site), "_", data_file,".rds"))
    #   if (controls$site !="All") {
    #     data <- data |>
    #       dplyr::filter(Location == controls$site)
    #   }
    #
    #   start_ <- lubridate::ymd(controls$daterange[1])
    #   end_ <- lubridate::ymd(controls$daterange[2])
    #   date_col <- rlang::sym(date_col)
    #   data |>
    #     clean_genotype_columns() |>
    #     dplyr::filter(dplyr::between(!!date_col, start_, end_))
    # })

    data_input <- reactive({
      file_path <- paste0("app/data/", tolower(controls$site), "_", data_file, ".rds")

      # Try to read the file, return empty df if it doesn't exist
      if (file.exists(file_path)) {
        data <- readRDS(file_path)
      } else {
        # Create empty df with expected columns
        # You should adjust the column names and types to match your actual data structure
        data <- data.frame(
          Location = character(),
          Female_Genotype = character(),
          Male_Genotype = character(),
          stringsAsFactors = FALSE
        )
        # Add date column if needed (assuming date_col is defined elsewhere)
        if (exists("date_col") && !is.null(date_col)) {
          data[[date_col]] <- as.Date(character())
        }

        if (exists("seed_column") && !is.null(seed_column)) {
          data[[seed_column]] <- as.integer(character())
        }

      }

      if (controls$site != "All" && "Location" %in% names(data)) {
        data <- data |>
          dplyr::filter(Location == controls$site)
      }

      if (exists("date_col") && !is.null(date_col) && date_col %in% names(data)) {
        start_ <- lubridate::ymd(controls$daterange[1])
        end_ <- lubridate::ymd(controls$daterange[2])
        date_col_sym <- rlang::sym(date_col)
data <- subset(data, get(date_col) >= start_ & get(date_col) <= end_)
        #data <- data |>
        #  dplyr::filter(dplyr::between(!!date_col_sym, start_, end_))
      }

      # Only run clean_genotype_columns if the function exists and data isn't empty
      if (nrow(data) > 0) {
        data <- data |>
          clean_genotype_columns()
      }

      data
    })

    uniq_input <- reactive({
      seed_column <- rlang::sym(seed_column)
      data_input() |>
        dplyr::group_by(Female_Genotype, Male_Genotype) |>
        dplyr::summarise(n = sum(!!seed_column, na.rm = TRUE), .groups = "drop")
    })

    output$title<- renderText({
      req(controls$site)
      title
    })

    output$value<- renderText({
      req(controls$site)
      sum(uniq_input()$n)
    })

    output$description1<- renderText({
      req(controls$site)
      paste(nrow(uniq_input())," Unique combinations")
    })


    # table
    observeEvent(input$my_button, {
      # Trigger the modal when the action button is clicked
      showModal(modalDialog(
        title = title,
        renderDT({
          datatable(data_input(),
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
        }),

        size = "xl",
        easyClose = TRUE,
        footer = modalButton("Dismiss"),
        fade = TRUE  # No footer buttons
      ))
    })


  })
}
