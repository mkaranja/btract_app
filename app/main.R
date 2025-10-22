box::use(
  shiny[bootstrapPage, p,div, moduleServer, NS, renderUI, tags, span, a, HTML, br, reactiveVal, observeEvent, reactiveValuesToList, img, includeHTML],
  shinymanager[secure_app, secure_server, check_credentials, set_labels],
  bslib[bs_theme, bs_theme_update, font_google, card, card_header],
  waiter[use_waiter, waiter_show, waiter_hide]
)

#' @export
box::use(
  app/view/app_ui,
)


set_labels(
  language = "en",
  "Please authenticate" = ""
)


#' @export
ui <- secure_app(
  enable_admin = TRUE,
  fab_position = "bottom-right",
  theme = bs_theme(version = 5, bootswatch = "yeti"),
  # Top Section with Logos
  #background = "img(src='app/static/images/bunch.jpg');",

  tags_top = tags$div(
    style = "display: flex; flex-direction: column; align-items: center; padding-top: 20px;",

    # Welcome message
    tags$div(
      style = "text-align: center; margin-top: 20px;",
      tags$h2(
        style = "font-weight: 300; color: #2c3e50; margin-bottom: 5px;",
        "Banana Tracking Tool (BTracT)"
      )
    )
    ),

  # Bottom Footer Section
  tags_bottom = tags$div(
    tags$p(style = "color: red; font-size: 14px; font-family: Arial, sans-serif;", "Login Instructions"),
    tags$ul(style = "font-size: 12px; font-family: Arial, sans-serif;",
            tags$li("For first-time login, use your name (in all lowercase letters) as the username."),
            tags$li("Enter the password:", tags$strong("xyz123")),
            tags$li("After logging in, you will be prompted to reset your password."),
            tags$li("Make sure to choose a secure password that you can remember and store it for future use.")
    ),

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
  ),

  # Main app content placeholder
  bootstrapPage(
    app_ui$ui("app")
  )
)

#' @export
server <-  function(input, output, session){

  res_auth <- secure_server(
    check_credentials = check_credentials(db = "app/static/pg_template.yml"),
    inputs_list = list(
      station = list(
        fun = "selectInput",
        args = list(choices = c("Arusha", "Ibadan", "Kawanda", "Onne", "Sendusu", "All"), multiple = FALSE))
    ),
    timeout = 15,
    keep_token = TRUE
  )



  app_ui$server("app", res_auth)
}
