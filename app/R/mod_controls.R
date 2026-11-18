### controls module ------------------------------------------------------------
# The national map's controls (year range, land type), in one panel under the
# national map. (The data sources are in the side panel, side_panel.R.)
#
# The panel belongs to the national view: app.R hides it (along with the
# national map) while an MSA is selected, and the MSA view takes the page.
# Its two controls sit side by side as one centred pair, a fixed gap apart:
#   year range slider  <- controls_gap_px ->  land type list
# On a window too narrow for both, the list wraps beneath, still centred.
#
# Returns the reactives the map module needs:
#   year_range(), dev_class()

# Widths of the pair, and the gap between them. Fixed, so the gap stays the
# same at any window width; the explicit widths also replace Shiny's default
# 300px width for inputs.
year_slider_width  <- "360px"
dev_class_width    <- "220px"   # the land type list; fits "Medium intensity"
controls_gap_px    <- 48

controlsUI <- function(id, years) {
  ns <- shiny::NS(id)

  # As wide as the national map above it: both fill the map column of the
  # national row (national_row(), side_panel.R), so their edges line up.
  # margin-bottom:0 lets its bottom line up with the side panel's.
  shiny::wellPanel(
    style = "width:100%; margin:12px 0 0; box-sizing:border-box;",
    # The pair, centred in the panel. flex:none on each keeps both at their
    # fixed widths, so neither can squeeze the other.
    shiny::div(
      style = paste0(
        "display:flex; justify-content:center; align-items:flex-start;",
        " flex-wrap:wrap; gap:", controls_gap_px, "px;"
      ),
      shiny::div(
        style = "flex:none;",
        # Static inputs: these used to be rebuilt by a renderUI on every slider
        # move, which re-fired them and redrew the map.
        shiny::sliderInput(
          inputId = ns("year_range"),
          label   = "Year range:",
          min     = years[1],
          max     = years[2],
          value   = years,
          step    = 1,
          sep     = "",
          width   = year_slider_width
        )
      ),
      shiny::div(style = "flex:none;", national_controls(ns))
    )
  )
}

national_controls <- function(ns) {
  # The national map's single design: size = new developed land of the class
  # chosen here, colour = new residents per square mile of that new land.
  # The map's own legends explain both, so there is no note here.
  # Radio buttons, so every option is visible and one is chosen; control_css()
  # (constants.R) draws them as a stacked list with the chosen one in amber.
  # Its value is the class as text ("21"), as the dropdown's was.
  shiny::radioButtons(
    inputId  = ns("dev_class"),
    label    = "Type of Developed Land",
    choices  = dev_class_choices,
    selected = 0,
    width    = dev_class_width
  )
}

controlsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    year_range <- shiny::reactive({
      shiny::req(input$year_range)
      sort(input$year_range)
    })

    list(
      year_range = year_range,
      dev_class  = shiny::reactive(as.integer(input$dev_class %||% 0L))
    )
  })
}

# output.msa_selected drives the two conditionalPanel()s in app.R that switch
# the page between the national and MSA views. It must keep updating while
# hidden, or a panel could never be shown again.
selection_flag <- function(output, selected_msa) {
  output$msa_selected <- shiny::reactive(!is.null(selected_msa()))
  shiny::outputOptions(output, "msa_selected", suspendWhenHidden = FALSE)
}
