box::use(
  shiny[tagList, fluidRow, HTML, column,p, br, div, moduleServer, NS, selectInput, textOutput, renderText, tags, dateRangeInput, checkboxInput,
        downloadButton, observeEvent, reactive, showModal, modalDialog,modalButton, bindCache, req  ],
  shinydashboard[valueBoxOutput, renderValueBox, valueBox],
  bslib[value_box],
  shinycssloaders[withSpinner],
  shinyBS[bsModal, toggleModal, ],
  DT[DTOutput, renderDT, datatable],
  datasets[mtcars],
  shinyWidgets[actionBttn],
  reactable[reactableOutput, renderReactable, reactable, colDef, JS, getReactableState, reactableTheme, reactableLang],
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
server <- function(id, title, controls ) {
  moduleServer(id, function(input, output, session) {

    data_input <- reactive({
      data <- readRDS(paste0("app/data/",tolower(controls$site), "_bunches.rds"))

      if (controls$site !="All") {
        data <- data |>
          dplyr::filter(Location == controls$site)
        } else {
          data <- data
        }

        start_ <- lubridate::ymd(controls$daterange[1])
        end_ <- lubridate::ymd(controls$daterange[2])

        data |>
          clean_genotype_columns() |>
          dplyr::filter(dplyr::between(Bunch_Harvest_Date, start_, end_))
    })


    uniq_input <- reactive({
      data_input() |>
        dplyr::group_by(Female_Genotype, Male_Genotype) |>
        dplyr::tally()
    })

    output$title<- renderText({
      req(controls$site)
      title
    })
    output$value<- renderText({
      req(controls$site)
      nrow(data_input())
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
