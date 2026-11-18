### constants ------------------------------------------------------------------
# Files in app/R/ are sourced before app.R runs its library() calls, so anything
# that needs a package is namespaced or wrapped in a function.

# --- map colors ---------------------------------------------------------------
colors <- list(
  no_change = "#BDBDBD",  # circle fill where density is undefined
  dot_gain  = "#009392",  # teal, the growth end of TealRose
  dot_loss  = "#D0587E",  # rose, the loss end of TealRose
  median    = "#5BC0EB",  # sky blue: the median tick and label in the comparison chart
  selected  = "#F5B841",  # amber: the selected MSA in the comparison chart,
                          # apart from the teal/rose scheme so it reads as a highlight
  panel_grey = "#303030"  # the darkly theme's .well grey (the chart bar); also the
                          # ribbons under both MSA maps (step 09 uses the same grey)
)

# National circles are filled on a continuous scale of new residents per square
# mile of new developed land (see density_palette() in stats.R), using CARTO's
# TealRose diverging palette, ordered from loss to growth: rose where
# population fell, cream at zero, teal where it grew. The same teal and rose
# mark gain and loss dots in the MSA view, so the whole app reads as one scheme.
density_colors <- c("#D0587E", "#D98994", "#E5B9AD", "#F1EAC8", "#B1C7B3", "#72AAA1", "#009392")

# --- time-lapse GIF frame size --------------------------------------------------
# Pixel size of the GIFs R/08 renders (its gif_w_px x gif_h_px: a square map
# with the title ribbon beneath it), and so of the yearly images step 10
# splits them into. The MSA view sizes both of its panels to this shape, so if
# step 09's frame size changes, update it here too.
gif_size <- c(width = 800, height = 992)

# Play button speed: milliseconds per year when the year slider under the land
# cover image plays through the years on its own.
lc_play_ms <- 1000

# Height of the GIF's grey ribbon as a share of the map's width (the map is
# square): R/09's ribbon_height (130pt) over its frame_w_in (7.5in = 540pt),
# about 24%. The dot map's control ribbon uses the same share so the two
# ribbons, and the two square maps above them, line up; update it if step 09's
# ribbon or frame size changes.
gif_ribbon_frac <- 130 / (7.5 * 72)

# The Developed Land legend the app draws over the land cover image's ribbon:
# one row per developed class, in display order from top to bottom (High
# Intensity on top, so intensity rises up the legend). The colours MUST match
# `sunset_pal` in R/09_create_and_run_gifs.R, which colours those classes on
# the map; the labels are the NLCD class names minus "Developed, ".
lc_legend <- data.frame(
  class = c(24L, 23L, 22L, 21L),
  color = c("#F3E79B", "#F8A07E", "#CE6693", "#5C53A5"),
  label = c("High Intensity", "Medium Intensity", "Low Intensity", "Open Space")
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

# The land type list under the national map (national_controls(),
# mod_controls.R): label = value (the NLCD class; 0 = all developed classes).
dev_class_choices <- c(
  "Total developed"  = 0,
  "Open space"       = 21,
  "Low intensity"    = 22,
  "Medium intensity" = 23,
  "High intensity"   = 24
)

# --- page font ----------------------------------------------------------------
# The app's text is set in Marcellus, a classical serif from Google Fonts,
# except the two page titles (the app's title in the national view and the
# MSA's name in the MSA view), which are set in Federo, a display face. Both
# are loaded by the stylesheet link in app.R's tags$head and have one weight
# only, so "bold" and "italic" text is synthesised by the browser.
#
# app_font_css() points the page's text elements at Marcellus: the body and
# headings, form controls (buttons, inputs, selectize dropdowns), the year
# sliders (.irs), and Leaflet's labels, legends and hover text, which Leaflet
# otherwise sets to Helvetica. It names elements explicitly rather than using a
# catch-all `*` selector, which would also replace the icon font that draws the
# year slider's play/pause button. A second, more specific rule sets the two
# titles to Federo: .page-title (the span app.R puts in titlePanel())
# and the MSA view's title (.msa-title-block h2). The national map's place
# labels keep their own map-style fonts (basemap_css() in mod_map.R).
app_font_family   <- "'Marcellus', serif"
title_font_name   <- "Federo"   # bare name: msa_title_fit_js waits for it to load
title_font_family <- paste0("'", title_font_name, "', sans-serif")

app_font_url <- "https://fonts.googleapis.com/css2?family=Federo&family=Marcellus&display=swap"

app_font_css <- function() {
  selectors <- c(
    "body", "h1", "h2", "h3", "h4", "h5", "h6",
    ".btn", "input", "select", "textarea", "label",
    ".selectize-input", ".selectize-dropdown",
    ".irs",
    ".leaflet-container", ".leaflet-tooltip", ".leaflet-control"
  )
  paste0(
    paste(selectors, collapse = ", "), " { font-family: ", app_font_family, "; }\n",
    "h2 .page-title, .msa-title-block h2 { font-family: ", title_font_family, "; }"
  )
}

# The comparison chart's font. R draws the chart as SVG (svg_chart(),
# chart_msa_ranges.R), whose text the browser renders in the page's own
# Marcellus. svglite still measures each label on the server to place it, so
# Marcellus also ships with the app as a file (www/fonts/Marcellus-Regular.ttf,
# with its SIL Open Font License, OFL.txt) and is registered here with
# systemfonts under the name "Marcellus"; msa_range_plot() asks for
# family = "Marcellus". app.R
# calls this once at startup, with app/ as the working directory. If the file
# is missing the labels are measured with a default font rather than failing.
chart_font_family <- "Marcellus"

register_chart_font <- function(path = file.path("www", "fonts", "Marcellus-Regular.ttf")) {
  if (!file.exists(path)) {
    warning("Chart font not found at ", path, "; the chart will use the default font.", call. = FALSE)
    return(invisible(FALSE))
  }
  systemfonts::register_font(name = chart_font_family, plain = path)
  invisible(TRUE)
}

# --- control styling --------------------------------------------------------
# The app's controls restyled on the dark page, with accents in the same amber
# that marks the selected MSA in the comparison chart (colors$selected):
#   - land type list (radio buttons, #controls-dev_class only): every option
#     shown as a line of small white text, the chosen one's text in amber (no
#     boxes or fill). The round radio dots are hidden visually but kept, so
#     keyboard (Tab, arrow keys) and screen readers still work; keyboard focus
#     shows an amber outline
#   - year sliders (ion.rangeSlider's "shiny" skin, .irs--shiny): a thin grey
#     track with the selected range and the round handles in amber, and the
#     values as plain amber text instead of blue bubbles
#   - the slider's play button: amber, full strength
#   - checkboxes (Loss/Gain in the dot map ribbon): amber ticks via accent-color
# Each rule starts with `body` so it outranks Shiny's and the theme's own
# stylesheets whatever order they load in. Only these controls' own classes are
# targeted, so nothing else on the page changes.
#
# Slider geometry: the track and selected range are 3px tall at top 27px and
# the 14px handles sit at top 21.5px, so all three share one centre line
# (28.5px from the slider's top). ion.rangeSlider measures the handles' width
# when it draws, so smaller handles still land on the right values.
control_colors <- list(
  track   = "#555555",   # slider track
  text    = "#FFFFFF",   # land type option text
  muted   = "#AAAAAA",   # slider min/max and tick labels
  accent  = colors$selected
)

control_css <- function(cc = control_colors) {
  paste0(
    # --- land type list ------------------------------------------------------
    # Shiny's markup: #controls-dev_class > label (the title), then
    # .radio > label > input + span for each option.
    # more room between the title and the first option (Bootstrap's is 5px)
    "body #controls-dev_class > .control-label { margin-bottom:10px; }\n",
    "body #controls-dev_class .radio { margin:0; }\n",
    "body #controls-dev_class .radio label { display:block; padding:0; min-height:0; }\n",
    # hide the dot without display:none, which would drop it from keyboard focus
    "body #controls-dev_class input[type=radio] {",
    " position:absolute; opacity:0; width:1px; height:1px; margin:0; }\n",
    # each option: plain white text
    "body #controls-dev_class input[type=radio] + span {",
    " display:block; padding:2px 0; font-size:12px; line-height:1.3;",
    " color:", cc$text, "; cursor:pointer; }\n",
    # hover: faint amber, a hint that it's clickable
    "body #controls-dev_class label:hover input[type=radio] + span { color:", cc$accent, "B3; }\n",
    # the chosen option: amber text, nothing else, so the list never shifts
    "body #controls-dev_class input[type=radio]:checked + span { color:", cc$accent, "; }\n",
    # keyboard focus: an amber outline around the option
    "body #controls-dev_class input[type=radio]:focus-visible + span {",
    " outline:2px solid ", cc$accent, "; outline-offset:1px; }\n",

    # --- year sliders ------------------------------------------------------
    "body .irs--shiny .irs-line { top:27px; height:3px; background:", cc$track, ";",
    " border:none; border-radius:2px; }\n",
    "body .irs--shiny .irs-bar { top:27px; height:3px; background:", cc$accent, ";",
    " border:none; }\n",
    "body .irs--shiny .irs-bar--single { border-radius:2px 0 0 2px; }\n",
    "body .irs--shiny .irs-handle { top:21.5px; width:14px; height:14px;",
    " background:", cc$accent, "; border:2px solid #222222; border-radius:50%; box-shadow:none; }\n",
    "body .irs--shiny .irs-handle.state_hover, body .irs--shiny .irs-handle:hover {",
    " background:", cc$accent, "; transform:scale(1.2); }\n",
    "body .irs--shiny .irs-from, body .irs--shiny .irs-to, body .irs--shiny .irs-single {",
    " background:none; color:", cc$accent, "; font-size:11px; }\n",
    "body .irs--shiny .irs-from:before, body .irs--shiny .irs-to:before,",
    " body .irs--shiny .irs-single:before { display:none; }\n",
    "body .irs--shiny .irs-min, body .irs--shiny .irs-max {",
    " background:none; color:", cc$muted, "; }\n",
    "body .irs--shiny .irs-grid-text { color:", cc$muted, "; }\n",
    "body .irs--shiny .irs-grid-pol { background-color:", cc$muted, "; }\n",

    # --- play button and checkboxes -----------------------------------------
    "body .slider-animate-button { color:", cc$accent, "; opacity:1; }\n",
    "body input[type=checkbox] { accent-color:", cc$accent, "; }\n"
  )
}

# --- basemap ------------------------------------------------------------------
# CARTO's Dark Matter tiles (the MSA dot map's basemap; the national map is
# vector) need an API key, read from CARTO_API_KEY. Keep it in
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
  paste0("https://basemaps.cartocdn.com/rastertiles/dark_all/{z}/{x}/{y}.png?key=", key)
}

basemap_attribution <- paste(
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors,',
  '&copy; <a href="https://carto.com/attributions">CARTO</a>'
)

# --- national view projection -------------------------------------------------
# The national view is drawn in USA Contiguous Albers Equal Area (EPSG:5070),
# the standard for US thematic maps: unlike Web Mercator it doesn't inflate the
# northern states. Leaflet does this through the Proj4Leaflet plugin, which
# projects the EPSG:4326 data in the browser. `resolutions` sets the metres per
# screen pixel at each zoom level (zoom 0 = 8,192 m/px, halving per level).
albers_crs <- function() {
  leaflet::leafletCRS(
    crsClass    = "L.Proj.CRS",
    code        = "EPSG:5070",
    proj4def    = paste(
      "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5",
      "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
    ),
    resolutions = 8192 / 2^(0:8)
  )
}

# lower-48 extent (lng1, lat1, lng2, lat2) for fitting the national view
national_bounds <- c(-124.8, 24.5, -66.9, 49.4)

# How many whole zoom levels closer than that fit the national map opens.
# fitBounds() lands on the largest whole level that shows all of the lower 48;
# one level closer doubles the scale, so the opening view crops the edges a
# little but shows the metros larger. Applied in national_basemap()'s render hook.
national_zoom_in <- 1

# Nudge the opening national view southeast so all of the lower 48 fits.
# In Albers the country's visual centre sits south-east of the lng/lat box's
# centre, so at the closer zoom Florida and the Atlantic coast were cropped.
# Shares of the map's width (east) and height (south) to pan by, after the zoom,
# in national_basemap()'s render hook; positive = east/south. Tune if still off.
national_view_shift <- c(east = 0.0764, south = 0.0711)

# Vector basemap colours for the national view, close to CARTO Positron.
basemap_colors <- list(
  background    = "#DCE3E8",  # water: oceans, Gulf, Great Lakes (pale grey-blue)
  land          = "#F2F2F2",  # the lower 48
  neighbor_land = "#E4E4E4",  # Canada and Mexico, a shade darker to recede
  state_border  = "#FFFFFF",
  msa_border    = "#A8A8A8"   # the dot map's MSA outline (the national map's is gold, control_colors$accent)
)

# Shaded relief under the national map's land: Natural Earth's Gray Earth,
# reprojected to Albers by step 10 and served from www/basemap/relief.jpg.
# It is blended with multiply (basemap_css() in mod_map.R), so it only darkens.
# relief_opacity sets how strongly the terrain shows: 0 = flat grey land as
# before, 1 = the full Gray Earth tones. Change it here; no step-10 re-run needed.
relief_url     <- "basemap/relief.jpg"
relief_opacity <- 0.5

# Fixed-position place labels for the national view (lng/lat), styled by class
# in mapUI(): countries in spaced caps, water in italic blue-grey.
# (The Pacific and Atlantic are curved labels instead, ocean_labels below.)
place_labels <- data.frame(
  label = c("CANADA", "MEXICO", "Gulf of Mexico"),
  lng   = c(-101, -104.5, -90.5),
  lat   = c(50.6, 27.0, 26.7),
  class = c("country-label", "country-label", "water-label")
)

# The two ocean labels: each set along a gentle arc, mostly up-and-down, just off
# its coast (drawn by ocean_label_html(), mod_map.R). Parameters per ocean:
#   lng, lat  where the middle of the label sits
#   angle     tilt of the arc's chord, in degrees clockwise from horizontal,
#             which is also the reading direction: positive reads downward
#             (Pacific, north to south), negative upward (Atlantic, south-west
#             to north-east), so neither is ever upside down
#   bend      how far the arc's middle bows out from its chord, in px; positive
#             bows to the left of the reading direction, negative to the right
#   length    length of the chord, in px (room for the text)
# All in screen pixels, so the labels keep their shape at every zoom.
ocean_labels <- data.frame(
  label  = c("Pacific Ocean", "Atlantic Ocean"),
  id     = c("pacific", "atlantic"),
  lng    = c(-119.4, -74.6),
  lat    = c(32.1, 33.6),
  angle  = c(72, -58),
  bend   = c(-10, 10),
  length = c(170, 180)
)

# --- shared leaflet styling ---------------------------------------------------
# The national map's hover label. Its position is set by national_basemap()'s
# hover script (beside the MSA's outline and circle), so it opens to the right
# with no offset; the script switches it to the left near the map's right edge.
# Not sticky: leaflet's labels follow the pointer by default (sticky = TRUE),
# which on the circle would move the label to the pointer on every mouse move
# and undo that placement.
msa_label_options <- function() {
  leaflet::labelOptions(
    sticky    = FALSE,
    direction = "right",
    offset    = c(0, 0),
    textsize  = "11px",
    # Minimal: just the name in black on a faint white backing (70% white, so
    # the map shows through), with Leaflet's border, shadow and wide padding
    # removed. The pointer triangle is hidden by basemap_css() (mod_map.R),
    # via this class.
    className = "msa-label",
    opacity   = 1,
    style = list(
      "color"            = "#000000",
      "font-weight"      = "bold",
      "background-color" = "rgba(255, 255, 255, 0.7)",
      "border"           = "none",
      "box-shadow"       = "none",
      "border-radius"    = "2px",
      "padding"          = "1px 4px"
    )
  )
}
