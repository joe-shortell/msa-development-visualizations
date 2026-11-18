### map module -----------------------------------------------------------------
# The national map: one circle per MSA, sized by new developed land (of the class
# chosen in the controls panel below it) and coloured by new residents per new square mile.
# Clicking a circle (or an MSA outline) selects that MSA; app.R then swaps this
# map for the two-panel MSA view (mod_msa_view.R).
#
# Drawn in Albers Equal Area with vector state outlines and no tiles. The widget
# re-renders when the selection changes, so returning from an MSA always gets a
# freshly sized map; the circles are drawn on top by their own observer once
# the new widget reports it has finished drawing (input$map_ready).

# The national view's title, left-aligned with the side panel's left edge.
# titlePanel() is wrapped in a block as wide as the national row (side panel +
# map, national_row_width() in side_panel.R) and centred like it, so the
# title's text starts where the side panel does. The span lets app_font_css() set the title in
# Federo; windowTitle (the browser tab) is plain text, and titlePanel() puts it
# in the page head wherever this sits.
national_title <- function(text) {
  shiny::div(
    style = paste0("width:", national_row_width(), "; margin:0 auto;"),
    shiny::titlePanel(
      title       = shiny::tags$span(class = "page-title", text),
      windowTitle = text
    )
  )
}

mapUI <- function(id) {
  ns <- shiny::NS(id)

  # Fills the map column of the national row (national_row(), side_panel.R),
  # which is as wide as the MSA view's two panels together (msa_pair_width())
  # when the window has room. The map is drawn when shown and fits the lower
  # 48 to its actual size.
  shiny::div(
    style = "position:relative; width:100%;",
    # the national view has no tiles, so colour the empty map behind the land
    # (it reads as water) and style the basemap's text labels
    shiny::tags$style(basemap_css(ns("map"))),
    leaflet::leafletOutput(ns("map"), height = "70vh")
  )
}

mapServer <- function(id, app_data, selected_msa, controls) {
  shiny::moduleServer(id, function(input, output, session) {
    national <- shiny::reactive({
      yrs <- controls$year_range()
      msa_map_data(
        app_data$msa_centers, app_data$msa_pop_dev, app_data$pop_dev,
        yrs[1], yrs[2], controls$dev_class()
      )
    })

    national_mode <- shiny::reactive(is.null(selected_msa()))

    # Reading selected_msa() makes the map re-render on the way back from an
    # MSA, so it is drawn at the size it is shown at, not while hidden.
    output$map <- leaflet::renderLeaflet({
      selected_msa()
      national_basemap(app_data$state_shapes, app_data$neighbor_shapes, app_data$relief_bounds)
    })

    # --- national circles ----------------------------------------------------
    # Draw the circles once the map has finished drawing, and again when the
    # year range or land class changes.
    # Circles go to the live map with leafletProxy(). Coming back from an MSA,
    # the map re-renders (above) in the same update; circles sent then reached
    # the old widget, which the fresh one immediately replaced, so the map came
    # back empty. So this waits for input$map_ready, which national_basemap()'s
    # onRender hook sets each time the widget finishes drawing (first load and
    # every return from an MSA), as the dot map does with input$dot_map_ready.
    shiny::observeEvent(list(input$map_ready, national(), controls$dev_class()), {
      shiny::req(input$map_ready)
      if (!is.null(selected_msa())) return()
      leaflet::leafletProxy("map") |>
        draw_national(app_data$msa_shapes, national(), controls$dev_class())
    })

    # --- selecting an MSA ----------------------------------------------------
    select_from_click <- function(click) {
      if (isTRUE(click$id %in% app_data$msa_shapes$MSA)) selected_msa(click$id)
    }
    shiny::observeEvent(input$map_shape_click,  select_from_click(input$map_shape_click))
    shiny::observeEvent(input$map_marker_click, select_from_click(input$map_marker_click))

    list(national_mode = national_mode)
  })
}

### drawing helpers --------------------------------------------------------------

# The national base map: Albers Equal Area with vector land and text labels.
#
# CARTO's raster tiles only exist in Web Mercator and can't be shown in this
# projection, so the basemap is drawn here: Canada and Mexico a shade darker
# than the lower-48 states, the map background showing through as water, and
# labels for states (postal codes), the two countries, and the oceans/Gulf.
#
# Everything sits in custom panes stacked below Leaflet's overlay pane
# (z-index 400), where the MSA outlines and circles go, so labels never cover a
# circle. Nothing here is interactive, so clicks fall through to the MSAs.
#
# Shaded relief (when step 10 has built it; relief_bounds = NULL leaves it out):
# a greyscale image of Natural Earth's Gray Earth, water already white, laid
# over the grey land in its own pane with a multiply blend (basemap_css()), so
# the land keeps its grey and only the terrain darkens it. The render hook adds
# it with Proj4Leaflet's L.Proj.imageOverlay, which places an image by its
# corners in EPSG:5070 metres (relief_bounds), at relief_opacity.
# A blend only mixes with what is drawn inside the map's own layer, not the
# page background behind it, so a water rectangle in the background colour is
# drawn under the land: without it the image's white water would wash out the
# water colour instead of leaving it unchanged.
#
# The view is first fitted to the lower 48 (national_bounds), then the render
# hook zooms national_zoom_in whole levels closer (about the map's centre) and
# pans slightly southeast (national_view_shift) so all of the lower 48 fits,
# since Albers puts the country's visual centre south-east of the box's. The
# same hook then tells the server the map is drawn (input$map_ready), the cue
# to send the circles.
national_basemap <- function(state_shapes, neighbor_shapes, relief_bounds = NULL) {
  land <- function(map, data, fill) {
    leaflet::addPolygons(
      map,
      data        = data,
      group       = "basemap",
      color       = basemap_colors$state_border,
      weight      = 1,
      opacity     = 1,
      fillColor   = fill,
      fillOpacity = 1,
      options     = leaflet::pathOptions(pane = "basemap", interactive = FALSE)
    )
  }
  labels <- function(map, lng, lat, text, class) {
    leaflet::addLabelOnlyMarkers(
      map,
      lng          = lng,
      lat          = lat,
      label        = text,
      group        = "basemap",
      options      = leaflet::markerOptions(pane = "basemap_labels", interactive = FALSE),
      labelOptions = leaflet::labelOptions(
        permanent = TRUE,
        textOnly  = TRUE,
        direction = "center",
        className = paste("basemap-label", class),
        pane      = "basemap_labels"
      )
    )
  }

  map <- leaflet::leaflet(options = leaflet::leafletOptions(crs = albers_crs(), minZoom = 0, maxZoom = 6)) |>
    leaflet::addMapPane("basemap", zIndex = 380) |>
    leaflet::addMapPane("relief", zIndex = 385) |>
    leaflet::addMapPane("basemap_labels", zIndex = 390)

  # The relief image's footprint, in the water colour, under the land (see above).
  # Its edges are segmentized so they follow the image's straight Albers edges
  # once Leaflet projects the lng/lat corners back.
  if (!is.null(relief_bounds)) {
    water <- sf::st_bbox(
      c(xmin = relief_bounds[[1]], ymin = relief_bounds[[2]],
        xmax = relief_bounds[[3]], ymax = relief_bounds[[4]]),
      crs = sf::st_crs(5070)
    ) |>
      sf::st_as_sfc() |>
      sf::st_segmentize(50000) |>
      sf::st_transform(4326)
    map <- leaflet::addPolygons(
      map,
      data        = water,
      group       = "basemap",
      stroke      = FALSE,
      fillColor   = basemap_colors$background,
      fillOpacity = 1,
      options     = leaflet::pathOptions(pane = "basemap", interactive = FALSE)
    )
  }
  relief <- if (is.null(relief_bounds)) NULL else list(
    url     = relief_url,
    opacity = relief_opacity,
    bounds  = unname(as.numeric(relief_bounds))
  )

  map |>
    land(neighbor_shapes, basemap_colors$neighbor_land) |>
    land(state_shapes, basemap_colors$land) |>
    labels(state_shapes$label_lng, state_shapes$label_lat, state_shapes$STUSPS, "state-label") |>
    labels(place_labels$lng, place_labels$lat, place_labels$label, place_labels$class) |>
    labels(ocean_labels$lng, ocean_labels$lat,
           lapply(seq_len(nrow(ocean_labels)), function(i) ocean_label_html(ocean_labels[i, ])),
           "water-label") |>
    leaflet::fitBounds(national_bounds[1], national_bounds[2], national_bounds[3], national_bounds[4]) |>
    # While the pointer is over an MSA's area or its circle, show its outline
    # and its circle's label (population and land-use change), with the label
    # placed beside the MSA rather than on top of it.
    # Leaflet's own highlightOptions can't do this: each circle sits above its
    # outline, so moving onto the circle leaves the outline and would hide it.
    # Instead every layer carrying an `msa` option (draw_national()) gets hover
    # handlers as it is added, including later leafletProxy() redraws:
    #   - either layer shows/hides that MSA's outline polygon;
    #   - the outline opens/closes the circle's label (the circle's own hover
    #     already does), so the label shows anywhere in the MSA;
    #   - whenever the label opens (tooltipopen), placeLabel() moves it 6px
    #     right of whichever reaches further right, the outline or the circle
    #     (screen pixels, from the outline's projected vertices and the
    #     circle's radius), level with the circle's centre; if it would run off
    #     the map's right edge it goes left of both instead.
    # Then: add (or replace) the shaded relief, apply the opening view (fit the
    # lower 48, zoom closer, nudge southeast; deferred until the map is shown
    # if it is hidden, see applyView in the hook), then tell the server the widget is drawn (input$<id>_ready,
    # i.e. input$map_ready), the cue to send the circles. panBy() takes
    # pixels, so the shift is a share of the map's current size and looks the
    # same on any screen.
    htmlwidgets::onRender(
      "function(el, x, opts) {
         var map = this;
         // The widget keeps one Leaflet map across renders (each return from
         // an MSA re-renders it), so anything added here directly is set up
         // once per map, or replaced, never stacked.
         if (!map._national) {
           map._national = {outlines: {}, circles: {}};
           var outlines = map._national.outlines, circles = map._national.circles;
           function edges(layer) {
             var xs = [];
             (function walk(ll) {
               if (ll.length === undefined) { xs.push(map.latLngToContainerPoint(ll).x); return; }
               for (var i = 0; i < ll.length; i++) walk(ll[i]);
             })(layer.getLatLngs());
             return [Math.min.apply(null, xs), Math.max.apply(null, xs)];
           }
           function placeLabel(id, tip) {
             var c = circles[id], p = map.latLngToContainerPoint(c.getLatLng()), r = c.getRadius();
             var left = p.x - r, right = p.x + r;
             if (outlines[id]) {
               var e = edges(outlines[id]);
               left = Math.min(left, e[0]); right = Math.max(right, e[1]);
             }
             var gap = 6, width = tip.getElement() ? tip.getElement().offsetWidth : 0;
             var toRight = right + gap + width <= map.getSize().x;
             tip.options.direction = toRight ? 'right' : 'left';
             tip.setLatLng(map.containerPointToLatLng([toRight ? right + gap : left - gap, p.y]));
           }
           map.on('layeradd', function(e) {
             var id = e.layer.options && e.layer.options.msa;
             if (!id) return;
             var isOutline = e.layer instanceof L.Polygon;
             if (isOutline) outlines[id] = e.layer;
             else {
               circles[id] = e.layer;
               e.layer.on('tooltipopen', function(ev) { placeLabel(id, ev.tooltip); });
             }
             e.layer.on('mouseover', function() {
               if (outlines[id]) outlines[id].setStyle({opacity: 1});
               if (isOutline && circles[id]) circles[id].openTooltip();
             });
             e.layer.on('mouseout', function() {
               if (outlines[id]) outlines[id].setStyle({opacity: 0});
               if (isOutline && circles[id]) circles[id].closeTooltip();
             });
           });
         }
         // the shaded relief: replace the previous render's, don't add another
         if (map._national.relief) map.removeLayer(map._national.relief);
         map._national.relief = null;
         if (opts.relief) {
           var b = opts.relief.bounds;
           map._national.relief = L.Proj.imageOverlay(opts.relief.url, L.bounds([b[0], b[1]], [b[2], b[3]]),
                               {pane: 'relief', opacity: opts.relief.opacity, interactive: false}).addTo(map);
         }
         // The opening view: fit the lower 48, zoom in, nudge southeast.
         // It needs the map's real size. On first load the map is showing; on
         // a return from an MSA it is re-rendered while still hidden (0 x 0),
         // and the widget postpones its own render (including its fitBounds)
         // until the map is shown. So if hidden, wait for the map to get its
         // size: the widget's resize fires Leaflet's 'resize' and then runs
         // the postponed render, so the timeout lets that finish and this
         // view is applied last, the same as on first load.
         // The opening zoom is also the furthest out the reader can zoom:
         // the limit is cleared first (the fit may need a lower zoom if the
         // window has changed size since), then set to the zoom just applied.
         function applyView() {
           var bb = opts.bounds;
           map.setMinZoom(0);
           map.fitBounds(L.latLngBounds([bb[1], bb[0]], [bb[3], bb[2]]), {animate: false});
           map.setZoom(map.getZoom() + opts.zoomIn, {animate: false});
           var size = map.getSize();
           map.panBy([size.x * opts.shift.east, size.y * opts.shift.south], {animate: false});
           map.setMinZoom(map.getZoom());
         }
         if (map.getSize().x > 0 && map.getSize().y > 0) applyView();
         else map.once('resize', function() { setTimeout(applyView, 0); });
         if (HTMLWidgets.shinyMode) {
           Shiny.setInputValue(el.id + '_ready', Date.now());
         }
       }",
      data = list(
        bounds = unname(national_bounds),   # lng1, lat1, lng2, lat2 of the lower 48
        zoomIn = national_zoom_in, shift = as.list(national_view_shift), relief = relief
      )
    )
}

# CSS for the national basemap: the map background (read as water), the
# relief pane's multiply blend (see national_basemap()), and the three label
# styles. Each label gets a white text-shadow halo so it stays
# legible where it crosses a border.
#
# The labels follow cartographic convention rather than the page's own fonts: water names in a serif italic, states and countries in a plain
# sans-serif. Both are system font stacks, so nothing is downloaded. The labels
# are Leaflet tooltips, and app_font_css() (constants.R) also sets
# .leaflet-tooltip; prefixing each selector with .leaflet-tooltip (two classes
# beat one) makes these fonts win whatever order the two style blocks load in.
basemap_label_serif <- "Georgia, 'Times New Roman', serif"
basemap_label_sans  <- "'Helvetica Neue', Helvetica, Arial, sans-serif"

# One curved ocean label (a row of ocean_labels, constants.R) as inline SVG.
#
# Leaflet labels can't curve, so the label holds a small SVG that writes the
# name along an arc with <textPath>. The text stays real text in the water
# labels' style (basemap_css(): serif italic, blue), with ocean-style wide
# letter spacing.
# The arc is a quadratic Bezier, built flat and then turned:
#   1. chord from (-length/2, 0) to (length/2, 0); the control point at
#      (0, -2 * bend) makes the curve's middle bow out by `bend` px (a
#      quadratic's middle sits halfway to its control point). SVG's y runs
#      down, so -y is to the left of the left-to-right reading direction.
#   2. rotate every point by `angle` degrees (clockwise on screen).
# The SVG is sized to the turned curve plus room for the text's height, and
# centred on (0, 0), so Leaflet's centred label puts the label's middle on its
# lng/lat. The text is centred along the curve (startOffset 50%).
ocean_label_html <- function(o, text_pad = 14) {
  turn <- function(x, y) {
    a <- o$angle * pi / 180
    c(x * cos(a) - y * sin(a), x * sin(a) + y * cos(a))
  }
  p0 <- turn(-o$length / 2, 0)
  p1 <- turn(0, -2 * o$bend)
  p2 <- turn(o$length / 2, 0)
  pts <- rbind(p0, p1, p2)
  half_w <- ceiling(max(abs(pts[, 1])) + text_pad)
  half_h <- ceiling(max(abs(pts[, 2])) + text_pad)
  f <- function(v) sprintf("%.1f", v)
  path_id <- paste0("ocean-path-", o$id)
  htmltools::HTML(paste0(
    '<svg width="', 2 * half_w, '" height="', 2 * half_h, '"',
    ' viewBox="', -half_w, " ", -half_h, " ", 2 * half_w, " ", 2 * half_h, '"',
    ' style="overflow:visible; display:block;">',
    '<path id="', path_id, '" fill="none" d="M ', f(p0[1]), " ", f(p0[2]),
    " Q ", f(p1[1]), " ", f(p1[2]), " ", f(p2[1]), " ", f(p2[2]), '"/>',
    '<text style="letter-spacing:3px;" dominant-baseline="middle">',
    '<textPath href="#', path_id, '" xlink:href="#', path_id, '" startOffset="50%" text-anchor="middle">',
    htmltools::htmlEscape(o$label), "</textPath></text></svg>"
  ))
}

basemap_css <- function(map_id) {
  paste0(
    "#", map_id, " { background: ", basemap_colors$background, "; }\n",
    ".leaflet-relief-pane { mix-blend-mode: multiply; }\n",
    ".basemap-label { color: #8A8A8A; text-shadow: 0 0 2px #FFFFFF, 0 0 2px #FFFFFF;",
    " pointer-events: none; white-space: nowrap; }\n",
    ".leaflet-tooltip.state-label { font-family: ", basemap_label_sans, ";",
    " font-size: 9px; font-weight: 600; }\n",
    ".leaflet-tooltip.country-label { font-family: ", basemap_label_sans, ";",
    " font-size: 12px; font-weight: 600; letter-spacing: 3px; color: #9A9A9A; }\n",
    ".leaflet-tooltip.water-label { font-family: ", basemap_label_serif, ";",
    # a darker blue: legible on the sky-blue water (basemap_colors$background)
    " font-size: 12px; font-style: italic; color: #2F5F86;",
    " text-shadow: none; letter-spacing: 1px; }\n",
    # the curved ocean labels (ocean_label_html()): SVG text takes the label's colour
    ".leaflet-tooltip.water-label text { fill: currentColor; }\n",
    # the MSA hover label (msa_label_options()): no pointer triangle
    ".leaflet-tooltip.msa-label:before { display: none; }\n"
  )
}

national_legend_ids <- c("density_legend", "size_legend")

# Draw the national graduated-symbol map: one circle per MSA.
#
# Size encodes |new developed land| of `dev_class` (square-root scaled, see
# circle_radius()); fill encodes new residents per new square mile on a
# diverging TealRose scale (density_palette()), negative where the metro lost
# people. Metros where the class did not grow (or shrank) are grey. Circles
# have no outline (stroke = FALSE); overlaps show through the 0.85 fill
# opacity. Big circles are drawn first so small ones stay
# clickable on top of them. Everything goes in group "national" and only that
# group is cleared, so the state-outline basemap stays.
# Both legends (colour bar, then size key) stack in the map's bottom-left corner.
#
# MSA outlines (gold, the year slider's accent) are invisible (opacity 0) until
# the pointer is over that MSA's area or its circle; national_basemap()'s render
# hook shows and hides them, and also opens the circle's label then, placed
# beside the MSA. They stay clickable while invisible. Outlines and
# circles both carry their MSA's name as a custom Leaflet option, `msa`, which
# is how the hook pairs a circle with its outline. Leaflet splits vector
# options per feature, as it does for the circles' fillColor.
draw_national <- function(map, msa_shapes, circles, dev_class) {
  map <- map |>
    leaflet::clearGroup("national") |>
    leaflet::removeControl(national_legend_ids) |>
    leaflet::addPolygons(
      data        = msa_shapes,
      group       = "national",
      layerId     = ~MSA,
      # the year slider's gold (control_colors$accent), so the two always match
      color       = control_colors$accent,
      weight      = 0.8,
      opacity     = 0,   # shown only on hover (see above)
      fillColor   = "transparent",
      fillOpacity = 0,
      options     = c(leaflet::pathOptions(), list(msa = msa_shapes$MSA))
    )
  circles <- circles[!is.na(circles$area_diff), ]
  if (nrow(circles) == 0) return(map)

  circles <- circles[order(-abs(circles$area_diff)), ]
  x_max   <- max(abs(circles$area_diff))
  pal     <- density_palette(circles$density)

  map <- leaflet::addCircleMarkers(
    map,
    data         = circles,
    group        = "national",
    layerId      = ~MSA,
    radius       = circle_radius(circles$area_diff, x_max),
    stroke       = FALSE,
    fillOpacity  = 0.85,
    fillColor    = pal$fill(circles$density),
    options      = c(leaflet::pathOptions(), list(msa = circles$MSA)),
    label        = national_labels(circles),
    labelOptions = msa_label_options()
  )

  if (pal$has_values) {
    map <- leaflet::addControl(
      map,
      html     = density_legend_html(pal),
      position = "bottomleft",
      layerId  = "density_legend"
    )
  }
  leaflet::addControl(
    map,
    html     = size_legend_html(x_max, dev_class),
    position = "bottomleft",
    layerId  = "size_legend"
  )
}

# Hover text for each national circle: just the MSA's name. The figures are
# left to the legends and the MSA view, so the label stays small.
national_labels <- function(circles) {
  lapply(htmltools::htmlEscape(circles$MSA), htmltools::HTML)
}

# Horizontal colour bar for density, drawn to a linear scale.
#
# leaflet::addLegend() only draws vertical bars, so this builds one in HTML:
# the title, then the bar, then one row of tick numbers under it. Every
# position along the bar is bar_fraction() of its width, so the length on each
# side of zero is proportional to the range of values there.
#   - the bar is a CSS linear-gradient through density_palette()'s stops, which
#     are the exact colours the circles get
#   - the ticks are 0, which marks where the colours turn from rose (population
#     loss) to teal (growth), plus density_palette()'s round numbers. Round
#     numbers within 12% of the bar's width of zero are dropped so their
#     labels can't overlap the 0. Most metros grew, so zero usually sits near
#     the bar's left end, where a centred "0" would be clipped: within 6% of
#     either end its label is anchored to that side instead of centred.
#   - a small grey note under the numbers saying red shades mean population
#     loss, since the bar no longer labels its ends
density_legend_html <- function(pal) {
  pct <- function(f) sprintf("%.2f%%", 100 * f)

  gradient <- paste(sprintf("%s %s", pal$stops$colour, pct(pal$stops$fraction)), collapse = ", ")
  bar <- paste0(
    '<div style="height:12px; border:1px solid #CCCCCC; box-sizing:border-box;',
    " background:linear-gradient(to right, ", gradient, ');"></div>'
  )

  # the tick values: 0, and the round numbers far enough from it
  zero  <- bar_fraction(0, pal$lo, pal$hi)
  round <- pal$ticks[abs(bar_fraction(pal$ticks, pal$lo, pal$hi) - zero) >= 0.12]
  ticks <- sort(c(0, round))
  frac  <- bar_fraction(ticks, pal$lo, pal$hi)

  # how each label sits on its tick: centred, except 0 near an end of the bar
  label_shift <- ifelse(
    ticks == 0 & frac < 0.06, "transform:translateX(0);",
    ifelse(ticks == 0 & frac > 0.94, "transform:translateX(-100%);", "transform:translateX(-50%);")
  )

  tick_row <- paste0(
    '<div style="position:relative; height:16px; font-size:10px;">',
    paste(sprintf(
      paste0(
        '<div style="position:absolute; left:%s; top:0; width:1px; height:4px; background:#555;"></div>',
        '<div style="position:absolute; left:%s; top:4px; %s">%s</div>'
      ),
      pct(frac), pct(frac), label_shift, fmt_int(ticks)
    ), collapse = ""),
    "</div>"
  )

  paste0(
    '<div style="width:220px;">',
    '<div style="font-size:11px; font-weight:bold; margin-bottom:4px;">',
    "New residents per new sq mi developed</div>",
    bar, tick_row,
    '<div style="font-size:9px; font-style:italic; color:#777; margin-top:2px;">',
    "Red shades indicate population loss</div>",
    "</div>"
  )
}

# A size key: three nested reference circles drawn at the same scale as the map.
#
# Leaflet has no built-in proportional-symbol legend, so this builds one in
# HTML. Reference values are the largest change and roughly a quarter and a
# twentieth of it, rounded to one significant figure; each is drawn with
# circle_radius() against the map's own maximum, so sizes match exactly.
size_legend_html <- function(x_max, dev_class) {
  refs  <- unique(signif(x_max * c(1, 0.25, 0.05), 1))
  radii <- circle_radius(refs, x_max)
  box   <- ceiling(2 * max(radii)) + 2

  rings <- vapply(seq_along(refs), function(i) {
    r <- radii[i]
    sprintf(
      paste0(
        '<div style="position:absolute; left:%.1fpx; bottom:0; width:%.1fpx; height:%.1fpx;',
        ' border:1px solid #999; border-radius:50%%;"></div>',
        '<div style="position:absolute; left:%dpx; bottom:%.1fpx; font-size:10px;',
        ' line-height:10px; white-space:nowrap;">%s</div>'
      ),
      box / 2 - r, 2 * r, 2 * r,
      box + 6, 2 * r - 5, fmt_num(refs[i], if (refs[i] < 10) 1 else 0)
    )
  }, character(1))

  paste0(
    '<div style="font-size:11px; font-weight:bold; margin-bottom:4px;">',
    "Sq mi of ", tolower(dev_labels[[as.character(dev_class)]]), " gained or lost</div>",
    '<div style="position:relative; width:', box + 50, "px; height:", box, 'px;">',
    paste(rings, collapse = ""),
    "</div>"
  )
}
