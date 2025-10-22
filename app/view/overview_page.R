box::use(
  shiny[tagList, fluidRow, column, br, div, moduleServer, NS, selectInput, renderPrint, textOutput, tags, dateRangeInput, checkboxInput,
        reactiveVal, observeEvent, observe, reactive, sliderInput],
  bslib[layout_column_wrap],
  shinyjs[useShinyjs, show, hide, hidden, enable, disable, disabled],
  bs4Dash[box]
)

#' @export
box::use(
  app/view/value_box,
  app/view/value_box1,
  app/view/value_box2,
  app/view/overview_filters,
  app/view/overview_chart1,
  app/view/overview_genotypes
)

#' @export
box::use(
  app/logic/load_datasets[banana, overall_banana, crosses, bunches,
                          extracted_seeds, embryo_rescued, germinating_embryos,
                          openfield]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow( overview_filters$ui(ns("controls"))),

    fluidRow(
      bslib::layout_column_wrap(
        width = "240px",  # You can adjust this width based on your box size
        value_box2$ui(ns("crosses")),
        value_box1$ui(ns("bunches")),
        value_box$ui(ns("seeds")),
        value_box$ui(ns("rescue")),
        value_box$ui(ns("germination")),
        value_box$ui(ns("openfiled"))
      )
    ),

    fluidRow(
      column(6, overview_chart1$ui(ns("charts"))),
      column(3, overview_genotypes$ui(ns("female"))),
      column(3, overview_genotypes$ui(ns("male")))
    ), br(), br(), br(), br()
  )
}

#' @export
server <- function(id, tab, res_auth) {
  moduleServer(id, function(input, output, session) {

    # Top level data filters
    controls <- overview_filters$server("controls", tab, res_auth)

    # Info boxes
    crosses <- value_box2$server(id = "crosses", title = "Crosses", controls = controls)
    value_box1$server(id = "bunches", title = "Bunches", controls = controls)

    value_box$server(id = "seeds",
                    title = "Extracted seeds",
                    data_file = "extracted_seeds",
                    controls = controls,
                    date_col = "Seed_Extraction_Date",
                    seed_column="Total_Seeds"
                    )

    value_box$server(id = "rescue",
                    title = "Embryo rescued",
                    data_file = "embryo_rescue",
                    controls = controls,
                    date_col = "Embryo_Rescue_Date",
                    seed_column="Number_of_Embryo_Rescued"
                    )

    value_box$server(id = "germination",
                    title = "Embryo germination",
                    data_file = "germination",
                    controls = controls,
                    date_col = "Germination_Date",
                    seed_column="Number_of_Embryo_Germinating"
                    )

    value_box$server(id = "openfiled",
                    title = "Plants sent to open-field",
                    data_file = "openfield",
                    controls = controls,
                    date_col = "Openfield_Transfer_Date",
                    seed_column = "Number_in_Openfield"
                    )


   # # # Chart
   overview1 <- overview_chart1$server("charts", controls, tab)

   # # Female Genotypes
   overview_genotypes$server("female", controls, genotypes_col="Female_Genotype", stage = overview1)

   # Male Genotypes
   overview_genotypes$server("male", controls, genotypes_col="Male_Genotype", stage = overview1)

  })
}
