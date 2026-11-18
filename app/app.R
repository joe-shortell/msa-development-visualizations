library(shiny)
library(leaflet)
library(shinythemes)
library(dplyr)
library(sf)
library(tidyr)     # pivot_longer() in chart_msa_ranges.R
library(ggplot2)   # the MSA view's comparison chart

# Helpers and modules live in R/ and are sourced automatically by Shiny:
#   data.R        load_app_data()
#   constants.R   colors, NLCD classes, input choices
#   stats.R       change calculations (pure functions, tested in tests/)
#   format.R      number formatting and the change table
#   side_panel.R  the national view's left panel: how to read the map, data sources
#   mod_controls.R  the panel under the national map: year range, land type
#   mod_map.R     the national leaflet map
#   mod_msa_view.R  the selected MSA: dot map and land cover images (year slider) side by side,
#                   the comparison chart beneath
#   chart_msa_ranges.R  that chart: where the MSA ranks among all 30 over the national year range

# API keys (CARTO_API_KEY). R reads .Renviron only from the directory it starts
# in — app/ on shinyapps.io, but usually the project root locally — so load it
# here explicitly. runApp() has already made app/ the working directory.
if (file.exists(".Renviron")) readRenviron(".Renviron")

app_data <- load_app_data()

# Make the bundled Marcellus font available to the comparison chart (constants.R):
# svglite measures its labels with it when drawing the chart as SVG (svg_chart()).
register_chart_font()

ui <- fluidPage(
  theme = shinythemes::shinytheme("darkly"),

  # Page fonts: Marcellus, and Federo for the titles, from Google Fonts, applied by app_font_css()
  # (constants.R). display=swap shows a fallback font until it arrives.
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet", href = app_font_url),
    tags$style(HTML(app_font_css())),
    # controls (land type list, sliders, play button, checkboxes): amber accents
    tags$style(HTML(control_css()))
  ),

  # Two whole-page views, switched by output.msa_selected:
  #   national  the app's title, then a row: the side panel (guidance, data
  #             sources) at the left, and the national map with the controls
  #             panel (year range, land type) underneath it at the right
  #   MSA       once a circle is clicked, titled with just that MSA's name: its
  #             dot map and land cover images across the page, with the
  #             comparison chart beneath
  # The title sits inside the national panel so it shows only there.
  # windowTitle keeps the browser tab's title the same in both views.
  conditionalPanel(
    "!output.msa_selected",
    # the app's title, left-aligned with the side panel's left edge (mod_map.R)
    national_title("Growth and Development in the Nation's 30 Largest MSAs"),
    national_row(shiny::tagList(
      mapUI("map"),
      controlsUI("controls", years = range(app_data$msa_pop_dev$year))
    ))
  ),
  conditionalPanel(
    "output.msa_selected",
    msaViewUI(
      "msa",
      ppd   = app_data$ppd,
      years = range(app_data$lc_years),
      # the title is sized so this name exactly spans the two panels
      longest_title = app_data$msa_shapes$MSA[which.max(nchar(app_data$msa_shapes$MSA))]
    )
  )
)

server <- function(input, output, session) {
  # NULL = national view; otherwise the MSA name. Shared by all modules.
  selected_msa <- reactiveVal(NULL)
  selection_flag(output, selected_msa)   # drives the page's two views

  controls <- controlsServer("controls")
  mapServer("map", app_data, selected_msa, controls)
  msaViewServer("msa", app_data, selected_msa, controls)   # chart follows the national year range and land type
}

shinyApp(ui, server)
