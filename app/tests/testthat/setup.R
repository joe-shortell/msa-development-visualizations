library(shiny)
library(dplyr)
library(sf)

# make app/R/ (helpers + modules) visible to the tests
shiny::loadSupport("../..", renv = globalenv())

# the module tests render the map; they don't need a real basemap key
if (!nzchar(Sys.getenv("CARTO_API_KEY"))) Sys.setenv(CARTO_API_KEY = "test-key")
