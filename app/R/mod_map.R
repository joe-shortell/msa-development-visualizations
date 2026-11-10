### map module -----------------------------------------------------------------
# National view: one circle per MSA for the chosen metric.
# MSA view: the MSA outline, population-change dots, and the outline of the
# city selected in the sidebar.
#
# Each piece of the map is drawn by one observer, so nothing depends on which
# input happens to fire first:
#   national circles   <- selected_msa() is NULL, plus year/metric/class
#   zoom to an MSA     <- selected_msa() set (priority 1: runs before the dots)
#   dots               <- selected MSA + Loss/Gain checkboxes
#   city outline       <- sidebar's selected city tab

mapUI <- function(id, ppd) {
  ns <- shiny::NS(id)

  shiny::div(
    style = "position:relative;",
    leaflet::leafletOutput(ns("map"), height = "80vh"),

    shiny::conditionalPanel(
      "output.msa_selected", ns = ns,
      shiny::absolutePanel(
        top = 15, right = 15, width = 260,
        style = "z-index: 500;
                 background: rgba(0,0,0,0.7);
                 padding: 8px;
                 border-radius: 4px;
                 color: #ffffff;",
        shiny::checkboxGroupInput(
          inputId  = ns("type"),
          label    = shiny::tagList(
            "Population Change 2000–2020",
            shiny::tags$br(),
            shiny::tags$small(paste0("each dot represents ", ppd, " people"))
          ),
          choices  = c("Loss", "Gain"),
          selected = c("Loss", "Gain")
        ),
        shiny::actionButton(
          inputId = ns("reset"),
          label   = "Back to all MSAs",
          class   = "btn btn-sm btn-light"
        )
      )
    )
  )
}

mapServer <- function(id, app_data, selected_msa, controls) {
  shiny::moduleServer(id, function(input, output, session) {
    selection_flag(output, selected_msa)

    national <- shiny::reactive({
      yrs <- controls$year_range()
      msa_map_data(
        app_data$msa_centers, app_data$msa_pop_dev, app_data$pop_dev,
        yrs[1], yrs[2], controls$dev_class()
      )
    })

    msa_dots <- shiny::reactive({
      msa <- shiny::req(selected_msa())
      app_data$dots[app_data$dots$MSA == msa, ]
    })

    # no boxes ticked -> no dots (input$type is NULL then)
    shown_dots <- shiny::reactive({
      d <- msa_dots()
      d[d$change_cat %in% input$type, ]
    })

    dot_pal <- leaflet::colorFactor(
      palette = c("Gain" = colors$dot_gain, "Loss" = colors$dot_loss, "No Change" = colors$no_change),
      domain  = c("Gain", "Loss", "No Change")
    )

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 3)) |>
        leaflet::addTiles(
          urlTemplate = basemap_url(),
          attribution = basemap_attribution,
          options     = leaflet::tileOptions(noWrap = TRUE, maxZoom = 20)
        ) |>
        leaflet::setMaxBounds(lng1 = -130, lat1 = 22, lng2 = -65, lat2 = 52) |>
        national_view()
    })

    # --- national circles ----------------------------------------------------
    # Also redraws on reset, since selected_msa() going NULL invalidates it.
    shiny::observe({
      if (!is.null(selected_msa())) return()
      leaflet::leafletProxy("map") |>
        draw_national(app_data$msa_shapes, national(), controls$metric(), controls$dev_class())
    })

    # --- selecting an MSA ----------------------------------------------------
    select_from_click <- function(click) {
      if (isTRUE(click$id %in% app_data$msa_shapes$MSA)) selected_msa(click$id)
    }
    shiny::observeEvent(input$map_shape_click,  select_from_click(input$map_shape_click))
    shiny::observeEvent(input$map_marker_click, select_from_click(input$map_marker_click))

    shiny::observeEvent(input$reset, {
      selected_msa(NULL)
      leaflet::leafletProxy("map") |> national_view()
    })

    shiny::observeEvent(selected_msa(), priority = 1, {
      shape <- app_data$msa_shapes[app_data$msa_shapes$MSA == selected_msa(), ]
      bb <- unname(sf::st_bbox(shape))

      leaflet::leafletProxy("map") |>
        leaflet::clearMarkers() |>
        leaflet::clearShapes() |>
        leaflet::addPolygons(
          data        = shape,
          color       = "white",
          weight      = 1,
          fillColor   = "transparent",
          fillOpacity = 0
        ) |>
        leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4])
    })

    # --- dots ----------------------------------------------------------------
    shiny::observe({
      d <- shown_dots()
      map <- leaflet::leafletProxy("map") |> leaflet::clearGroup("dots")
      if (nrow(d) == 0) return()
      leaflet::addCircleMarkers(
        map,
        data        = d,
        group       = "dots",
        radius      = 2,
        stroke      = FALSE,
        fillOpacity = 0.6,
        fillColor   = ~dot_pal(change_cat)
      )
    })

    # --- city outline --------------------------------------------------------
    shiny::observe({
      msa  <- selected_msa()
      city <- controls$selected_city()
      map  <- leaflet::leafletProxy("map") |> leaflet::removeShape("selected_city")
      if (is.null(msa) || is.null(city)) return()

      shapes <- app_data$city_shapes
      geom <- shapes[which(shapes$MSA == msa & shapes$CITY_GEOID == city), ]
      if (nrow(geom) == 0) return()

      leaflet::addPolygons(
        map,
        data        = geom,
        layerId     = "selected_city",
        color       = "yellow",
        weight      = 3,
        fillColor   = "transparent",
        fillOpacity = 0
      )
    })

    list(msa_dots = msa_dots, shown_dots = shown_dots)
  })
}

### drawing helpers --------------------------------------------------------------

national_view <- function(map) {
  leaflet::setView(map, lng = -98, lat = 39, zoom = 4)
}

draw_national <- function(map, msa_shapes, circles, metric, dev_class) {
  map <- map |>
    leaflet::clearMarkers() |>
    leaflet::clearShapes() |>
    leaflet::addPolygons(
      data        = msa_shapes,
      layerId     = ~MSA,
      color       = "white",
      weight      = 1,
      fillColor   = "transparent",
      fillOpacity = 0
    )
  if (nrow(circles) == 0) return(map)

  if (metric != "ratio") {
    val <- if (metric == "pop") circles$pop_diff else circles$area_diff
    return(add_msa_circles(map, circles, circle_radius(val), circle_colors(val, metric, dev_class)))
  }

  # developed land per new resident: a circle where the ratio is meaningful,
  # an X where the MSA has a ratio but lost developed land
  ok   <- ratio_ok(circles)
  good <- circles[ok, ]
  bad  <- circles[!ok & !is.na(circles$ratio), ]

  if (nrow(good) > 0) {
    map <- add_msa_circles(map, good, circle_radius(good$ratio), colors$ratio)
  }
  if (nrow(bad) > 0) {
    map <- leaflet::addAwesomeMarkers(
      map,
      data    = bad,
      layerId = ~MSA,
      icon    = leaflet::makeAwesomeIcon(
        icon        = "times",
        library     = "fa",
        markerColor = "red",
        iconColor   = "white"
      ),
      label        = ~MSA,
      labelOptions = msa_label_options()
    )
  }
  map
}

add_msa_circles <- function(map, data, radius, fill) {
  leaflet::addCircleMarkers(
    map,
    data         = data,
    layerId      = ~MSA,
    radius       = radius,
    stroke       = TRUE,
    color        = "white",
    weight       = 1,
    fillOpacity  = 0.9,
    fillColor    = fill,
    label        = ~MSA,
    labelOptions = msa_label_options()
  )
}
