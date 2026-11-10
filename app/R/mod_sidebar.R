### sidebar module -------------------------------------------------------------
# The year slider, the national-view controls, and (once an MSA is selected)
# the MSA/city tabs, change table, and time-lapse GIF.
#
# Returns the reactives the map module needs:
#   year_range(), metric(), dev_class(), selected_city()

sidebarUI <- function(id, years) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::sliderInput(
      inputId = ns("year_range"),
      label   = "Year range:",
      min     = years[1],
      max     = years[2],
      value   = years,
      step    = 1,
      sep     = ""
    ),
    shiny::br(),
    # everything in here is scaled to fit the window (see app.R)
    shiny::div(
      class = "sidebar-wrap",
      # Static, and only shown/hidden: these inputs used to be rebuilt by a
      # renderUI on every slider move, which re-fired them and redrew the map.
      shiny::conditionalPanel("!output.msa_selected", ns = ns, national_controls(ns)),
      shiny::conditionalPanel(
        "output.msa_selected", ns = ns,
        shiny::uiOutput(ns("geo_tabs")),
        shiny::uiOutput(ns("change_panel")),
        shiny::uiOutput(ns("gif"))
      )
    )
  )
}

national_controls <- function(ns) {
  shiny::tagList(
    shiny::radioButtons(
      inputId  = ns("metric"),
      label    = "Circle metric:",
      choices  = metric_choices,
      selected = "pop"
    ),

    shiny::tags$hr(style = "margin: 4px 0;"),

    shiny::tags$small("For developed land metrics, choose a single NLCD class:"),
    shiny::selectInput(
      inputId  = ns("dev_class"),
      label    = NULL,
      choices  = dev_class_choices,
      selected = 0
    ),
    shiny::tags$small(
      style = "display:block; margin-top:2px; font-size:0.75rem;",
      "Note: For date ranges where population or land cover declines, circles are colored red"
    ),

    shiny::br(),

    shiny::wellPanel(
      shiny::h4("Land Cover Definitions"),
      shiny::tags$div(
        style = "font-size: 0.85rem; line-height: 1.2;",
        shiny::tags$p(
          shiny::tags$strong("Open Space (21): "),
          "Mostly lawn and other planted vegetation with some built surfaces. ",
          "Impervious cover < 20%. Commonly large-lot housing, parks, golf courses, ",
          "and landscaped areas in developed settings."
        ),
        shiny::tags$p(
          shiny::tags$strong("Low Intensity (22): "),
          "Mix of buildings and vegetation. Impervious cover 20–49%. ",
          "Most commonly single-family housing areas."
        ),
        shiny::tags$p(
          shiny::tags$strong("Medium Intensity (23): "),
          "More built surfaces than vegetation. Impervious cover 50–79%. ",
          "Includes denser residential neighborhoods and mixed development."
        ),
        shiny::tags$p(
          shiny::tags$strong("High Intensity (24): "),
          "Highly developed areas where people live or work in high numbers. ",
          "Impervious cover 80–100%. Includes apartments, rowhouses, and ",
          "commercial/industrial districts."
        )
      )
    ),

    shiny::wellPanel(
      shiny::h4("Data Sources"),
      shiny::tags$div(
        style = "font-size: 0.85rem; line-height: 1.2;",
        shiny::p("U.S. Census Bureau, 2000 Census of Population and Housing (SF1)"),
        shiny::p("U.S. Census Bureau, 2020 Census of Population and Housing"),
        shiny::p("U.S. Census Bureau, American Community Survey 1-Year Estimates, 2005–2019, 2021–2024"),
        shiny::p("U.S. Geological Survey, National Land Cover Database (NLCD), Annual Land Cover, 1999–2024")
      )
    )
  )
}

sidebarServer <- function(id, app_data, selected_msa) {
  shiny::moduleServer(id, function(input, output, session) {
    selection_flag(output, selected_msa)

    year_range <- shiny::reactive({
      shiny::req(input$year_range)
      sort(input$year_range)
    })

    cities <- shiny::reactive(msa_cities(app_data$pop_dev, shiny::req(selected_msa())))

    # NULL on the MSA tab. input$geo_tab can briefly hold a tab from the
    # previously selected MSA until the new tabset renders, hence the check.
    selected_city <- shiny::reactive({
      tab <- input$geo_tab
      if (is.null(selected_msa()) || is.null(tab)) return(NULL)
      geoid <- sub("^city_", "", tab)
      if (geoid %in% cities()$CITY_GEOID) geoid else NULL
    })

    # Depends on the selected MSA only, so moving the slider keeps the current
    # tab. The tabs are just a selector; the table lives in change_panel.
    output$geo_tabs <- shiny::renderUI({
      cty <- cities()
      city_tabs <- lapply(seq_len(nrow(cty)), function(i) {
        shiny::tabPanel(title = cty$city_name[i], value = paste0("city_", cty$CITY_GEOID[i]))
      })
      do.call(
        shiny::tabsetPanel,
        c(
          list(id = session$ns("geo_tab"), type = "tabs", selected = "MSA_tab"),
          list(shiny::tabPanel(title = "MSA", value = "MSA_tab")),
          city_tabs
        )
      )
    })

    output$change_panel <- shiny::renderUI({
      msa  <- shiny::req(selected_msa())
      yrs  <- year_range()
      city <- selected_city()
      place <- if (is.null(city)) {
        "Entire MSA"
      } else {
        cities()$city_name[cities()$CITY_GEOID == city][1]
      }

      shiny::wellPanel(
        shiny::h4(style = "font-size: 0.95rem; margin-top: 0; margin-bottom: 4px;", msa),
        shiny::p(place),
        shiny::p(yrs[1], "–", yrs[2]),
        shiny::tags$hr(style = "border-color:#555;"),
        change_table(
          pop_change(app_data$pop_dev, msa, city, yrs[1], yrs[2]),
          class_change(app_data$pop_dev, msa, city, yrs[1], yrs[2]),
          yrs[1], yrs[2]
        )
      )
    })

    output$gif <- shiny::renderUI({
      msa  <- shiny::req(selected_msa())
      file <- app_data$msa_shapes$gif_file[app_data$msa_shapes$MSA == msa][1]
      has_gif <- !is.na(file) && file.exists(file.path("www", "gifs", file))

      shiny::wellPanel(
        shiny::h4("Time-lapse"),
        if (has_gif) {
          shiny::tags$img(src = paste0("gifs/", file), style = "width: 100%;")
        } else {
          shiny::p("No image could be found for this MSA.")
        }
      )
    })

    list(
      year_range    = year_range,
      metric        = shiny::reactive(input$metric %||% "pop"),
      dev_class     = shiny::reactive(as.integer(input$dev_class %||% 0L)),
      selected_city = selected_city
    )
  })
}

# output.msa_selected drives the conditionalPanel()s in a module's UI. It must
# keep updating while hidden, or a panel could never be shown again.
selection_flag <- function(output, selected_msa) {
  output$msa_selected <- shiny::reactive(!is.null(selected_msa()))
  shiny::outputOptions(output, "msa_selected", suspendWhenHidden = FALSE)
}
