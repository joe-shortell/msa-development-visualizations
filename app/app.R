library(shiny)
library(leaflet)
library(shinythemes)
library(dplyr)
library(sf)

# Helpers and modules live in R/ and are sourced automatically by Shiny:
#   data.R        load_app_data()
#   constants.R   colors, NLCD classes, input choices
#   stats.R       change calculations (pure functions, tested in tests/)
#   format.R      number formatting and the change table
#   mod_sidebar.R slider, national controls, MSA/city tabs, GIF
#   mod_map.R     the leaflet map

# API keys (CARTO_API_KEY). R reads .Renviron only from the directory it starts
# in — app/ on shinyapps.io, but usually the project root locally — so load it
# here explicitly. runApp() has already made app/ the working directory.
if (file.exists(".Renviron")) readRenviron(".Renviron")

app_data <- load_app_data()

ui <- fluidPage(
  titlePanel("Growth and Development in the Nation's 30 Largest MSAs"),
  theme = shinythemes::shinytheme("darkly"),

  # ---- global CSS + JS to scale sidebar content --------------------
  tags$head(
    tags$style(HTML("
      .sidebar-wrap {
        transform-origin: top left;
      }
    ")),
    tags$script(HTML("
      function scaleSidebar() {
        var el = document.querySelector('.sidebar-wrap');
        if (!el) return;
        // reset scale to measure natural height
        el.style.transform = 'scale(1)';
        var bounds = el.getBoundingClientRect();
        var available = window.innerHeight - bounds.top - 20; // margin at bottom
        if (available <= 0) return;
        var scale = Math.min(1, available / bounds.height);
        el.style.transform = 'scale(' + scale + ')';
      }
      document.addEventListener('DOMContentLoaded', scaleSidebar);
      window.addEventListener('resize', scaleSidebar);
      document.addEventListener('shiny:idle', scaleSidebar);
    "))
  ),

  sidebarLayout(
    sidebarPanel(
      sidebarUI("sidebar", years = range(app_data$msa_pop_dev$year))
    ),
    mainPanel(
      mapUI("map", ppd = app_data$ppd)
    )
  )
)

server <- function(input, output, session) {
  # NULL = national view; otherwise the MSA name. Shared by both modules.
  selected_msa <- reactiveVal(NULL)

  controls <- sidebarServer("sidebar", app_data, selected_msa)
  mapServer("map", app_data, selected_msa, controls)
}

shinyApp(ui, server)
