box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
)

box::use(
  app/view/app_ui
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    app_ui$ui("app")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
