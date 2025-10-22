#' @export
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, p, span, a,img, tags, HTML, textOutput, renderText, reactiveValues, observeEvent, observe, includeHTML,
        reactiveValuesToList],
  bslib[bs_theme, navbar_options, font_google, page_navbar, navset_pill, nav_spacer, nav_panel, nav_menu, navset_pill_list,
        page_sidebar, navset_tab, ],
)

#' @export
box::use(
  app/view/overview_page,
  app/view/tc_labels,
  app/view/table_page_raw,
  app/view/table_page_summary,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

 bootstrapPage(
    page_navbar(
      id = ns("nav"),
      title = "BTracT",
      window_title = "BTracT",
      theme = bslib::bs_theme(version = 5, bootswatch = "lumen"),
      navbar_options = navbar_options(
        #class = "bg-dark",
        theme = "dark",
        position = "static-top",
        fluid = T,
        inverse = F,
        fillable = TRUE,
        fillable_mobile = TRUE,
        underline=F
      ),

      nav_spacer(),
      nav_panel(
        "OVERVIEW",
        value = "overview",
        overview_page$ui(ns("overview_page")),
      ),

      nav_panel(
        "DATA TABLES",
        value = "data",
        navset_pill(
          id = ns("data_tabs"),
          nav_panel(
            "Raw Data Table",
            table_page_raw$ui(ns("raw"))
          ),
          nav_panel(
            "Summary Table",
            table_page_summary$ui(ns("summary"))
          )
        )

      ),
      nav_menu(
        "BARCODE LABELS",
        nav_panel(
          "Tissue Culture Barcodes",
          tc_labels$ui(ns("tc_labels"))
        )
        # nav_panel(
        #   "Other Barcodes",
        #   value = "other_labels"
        # )
      ),

      nav_menu(
        "ABOUT",
        nav_panel(tags$a("Using BTracT", href="static/docs/using_btract.html", target="_blank"))
      ),

      nav_spacer()
    ),

 HTML(paste("<script>var parent = document.getElementsByClassName('navbar-nav');
         parent[0].insertAdjacentHTML('afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"disabled\"><a href=\"#\" class=\"navbar-text\"><strong>",
            textOutput(ns('user')), "</strong></a></li></ul>' );</script>")),
 tags$div(  # Wrapper div to ensure full width
   tags$br(), tags$br(),
   style = "position: fixed; bottom: 0; left: 0; right: 0;",
   tags$footer(
     style = "font-size: 0.75rem;
                 padding: 15px 30px;
                 width: 100vw;
                 margin-left: calc(-50vw + 50%);
                 background-color: #f8f9fa;
                 border-top: 1px solid #dee2e6;
                 box-sizing: border-box;",

     div(
       style = "display: flex;
                   justify-content: space-between;
                   align-items: center;
                   width: 100%;
                   max-width: 1400px;
                   margin: 0 auto;",

       # Left section - Copyright + Support (aligned left)
       div(
         # style = "flex: 1; min-width: 300px;",
         style = "flex-shrink: 50;",
         p(style = "margin: 0; color: #6c757d;",
           HTML("&copy; ", lubridate::year(Sys.Date()), "BTracT | "),
           span(style = "white-space: nowrap;", "IITA - Kenya"),
           tags$br(),
           "Need help? Contact: ",
           a(
             href = "mailto:m.karanja@cgiar.org?Subject=RTBEAGEL%20-%20Problem",
             target = "_top",
             style = "color: #007bff; text-decoration: none;",
             "Support"
           )
         )
       ),

       # Right section - Partners and Funders (spaced from center to right)
       div(
         style = "flex: 2;
                     display: flex;
                     justify-content: flex-end;
                     align-items: center;
                     gap: 40px;",

         # Partners section (pushed right)
         div(
           style = "display: flex;
                       align-items: center;
                       gap: 20px;
                       margin-right: 40px;",

           div(
             style = "display: flex;
                         align-items: center;
                         gap: 20px;",
             a(
               href = "https://breedingbetterbananas.org", target = "_blank",
               title = "Breeding Better Bananas",
               img(
                 src = "https://breedingbetterbananas.org/wp-content/uploads/2020/01/banLogo.png",
                 style = "height: 40px; opacity: 0.9; transition: opacity 0.3s;",
                 onmouseover = "this.style.opacity=1",
                 onmouseout = "this.style.opacity=0.9"
               )
             ),
             a(
               href = "https://www.iita.org", target = "_blank",
               title = "IITA",
               img(
                 src = "https://www.iita.org/wp-content/themes/iita/images/IITA-TAA-smallnew.png",
                 style = "height: 40px; opacity: 0.9; transition: opacity 0.3s;",
                 onmouseover = "this.style.opacity=1",
                 onmouseout = "this.style.opacity=0.9"
               )
             ),
             a(
               href = "https://www.naro.co.ug", target = "_blank",
               title = "NARO",
               img(
                 src = "https://naro.go.ug/wp-content/uploads/2024/01/NARO_LOGO_BIG_OFFICIAL_2024.png",
                 style = "height: 35px; opacity: 0.9; transition: opacity 0.3s;"
               )
             ),
             a(
               href = "https://btiscience.org/", target = "_blank",
               title = "BTI",
               img(
                 src = "https://btiscience.org/wp-content/uploads/BTI-Primary-2color-1.png",
                 style = "height: 35px; opacity: 0.9; transition: opacity 0.3s;"
               )
             )
           )
         )
       )
     )
   )
 )
 # tags$footer(HTML("
 #                   <!-- Footer -->
 #                   <footer class='page-footer font-large indigo' style='position: fixed; bottom: 0; width: 100%; background-color: #f8f9fa;'>
 #                       <div class='container-fluid'> <!-- Use container-fluid for full width -->
 #                           <!-- Row for Footer Content -->
 #                           <div class='row d-flex'>
 #                               <!-- Copyright -->
 #                               <div class='footer-copyright text-left py-3' style='font-size:10pt'>
 #                               <span id='copyright-year'>Copyright © 2025 </span> <br>
 #                               For any question, please contact <a href='mailto:m.karanja@cgiar.org?Subject=BTracT%20Tracker%20-%20Problem' target='_top'>administrator</a>.
 #                               </div>
 #                               <!-- Copyright -->
 #                           </div> <!-- End of Bootstrap row -->
 #                       </div>
 #
 #                   </footer>
 #                   <!-- Footer -->"
 #     )
 #    )
 )
}

#' @export
server <- function(id, res_auth) {
  moduleServer(id, function(input, output, session) {

    tab <- reactiveValues(
      nav = NULL, # overview, data, tc, other_labels, using_btract
      data = NULL # data_tabs = "Raw Data Table","Summary Table"
    )

    observeEvent( input$nav , {
      tab$nav <- input$nav
    })

    observeEvent( input$data_tabs , {
      tab$data <- input$data_tabs
    })

    output$user <- renderText({
      paste0(stringr::str_to_title(reactiveValuesToList(res_auth)$user), " | ", reactiveValuesToList(res_auth)$station)
    })

    overview_page$server("overview_page", tab, res_auth)
    table_page_raw$server("raw", tab, res_auth)
    table_page_summary$server("summary", tab, res_auth)
    tc_labels$server("tc_labels", tab, res_auth)

  })
}
