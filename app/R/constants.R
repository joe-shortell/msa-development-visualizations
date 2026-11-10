### constants ------------------------------------------------------------------
# Files in app/R/ are sourced before app.R runs its library() calls, so anything
# that needs a package is namespaced or wrapped in a function.

# --- circle colors ------------------------------------------------------------
colors <- list(
  pop_gain  = "#006400",  # dark green
  pop_loss  = "#8B0000",  # dark red
  area      = "#4B0082",  # dark purple
  ratio     = "#89CFF0",  # baby blue
  no_change = "#999999",
  dot_gain  = "#3CB371",
  dot_loss  = "#B22222"
)

# --- NLCD developed classes (0 = all developed classes combined) ---------------
dev_classes <- c(0L, 21L, 22L, 23L, 24L)

dev_labels <- c(
  "0"  = "Total Developed Land",
  "21" = "Open Space",
  "22" = "Low Intensity",
  "23" = "Medium Intensity",
  "24" = "High Intensity"
)

dev_class_choices <- c(
  "Total developed (0)"   = 0,
  "Open space (21)"       = 21,
  "Low intensity (22)"    = 22,
  "Medium intensity (23)" = 23,
  "High intensity (24)"   = 24
)

metric_choices <- c(
  "Population growth"                                = "pop",
  "Developed land change (by type)"                  = "area",
  "Developed land change per new resident (by type)" = "ratio"
)

# --- basemap ------------------------------------------------------------------
# CARTO's Positron tiles need an API key, read from CARTO_API_KEY. Keep it in
# app/.Renviron (git-ignored; deployApp() uploads it with the app). The key is
# visible in the tile URLs visitors' browsers request, so restrict it to the
# app's domain in the CARTO dashboard.
basemap_url <- function(key = Sys.getenv("CARTO_API_KEY")) {
  if (!nzchar(key)) {
    warning(
      "CARTO_API_KEY is not set; the basemap will show 'API key required'.\n",
      "  Add CARTO_API_KEY=... to app/.Renviron and restart R.",
      call. = FALSE
    )
  }
  paste0("https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=", key)
}

basemap_attribution <- paste(
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors,',
  '&copy; <a href="https://carto.com/attributions">CARTO</a>'
)

# --- shared leaflet styling ---------------------------------------------------
msa_label_options <- function() {
  leaflet::labelOptions(
    direction = "auto",
    offset    = c(0, -8),
    textsize  = "11px",
    style = list(
      "color"       = "#ffffff",
      "font-weight" = "bold",
      "text-shadow" = "0 0 3px #000000"
    )
  )
}
