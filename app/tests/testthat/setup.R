library(shiny)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)

# make app/R/ (helpers + modules) visible to the tests
shiny::loadSupport("../..", renv = globalenv())

# the module tests render the map; they don't need a real basemap key
if (!nzchar(Sys.getenv("CARTO_API_KEY"))) Sys.setenv(CARTO_API_KEY = "test-key")

# the chart's bundled font (register_chart_font(), constants.R), so the chart
# tests draw with it; paths are relative to app/
font_path <- file.path("../..", "www", "fonts", "Marcellus-Regular.ttf")
if (file.exists(font_path)) register_chart_font(font_path)
