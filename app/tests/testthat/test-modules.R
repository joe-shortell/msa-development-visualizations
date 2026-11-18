alpha_beta <- fixture_msas[1]
gamma      <- fixture_msas[2]

test_that("the controls return the year range in order and the land class", {
  testServer(controlsServer, {
    session$setInputs(year_range = c(2020, 2010), dev_class = "21")   # slider order shouldn't matter
    expect_equal(year_range(), c(2010, 2020))
    expect_identical(session$getReturned()$dev_class(), 21L)
  })
})

map_controls <- function() {
  list(
    year_range    = reactive(c(2010, 2020)),
    dev_class     = reactive(0L)
  )
}

test_that("clicking a circle or outline selects that MSA; other shapes don't", {
  selected <- reactiveVal(NULL)
  testServer(mapServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    expect_true(national_mode())
    session$setInputs(map_marker_click = list(id = gamma))
    expect_equal(selected(), gamma)
    expect_false(national_mode())

    session$setInputs(map_shape_click = list(id = alpha_beta))
    expect_equal(selected(), alpha_beta)

    # a click on anything that isn't an MSA leaves the selection alone
    session$setInputs(map_shape_click = list(id = "not_an_msa"))
    expect_equal(selected(), alpha_beta)
  })
})

test_that("the comparison chart renders for a growing and a shrinking MSA", {
  selected <- reactiveVal(alpha_beta)
  testServer(msaViewServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    # sent as SVG, drawn at the slot's reported size
    expect_match(output$range_chart$html, "<svg", fixed = TRUE)
    selected(gamma)                           # lost people: the per-resident note
    session$flushReact()
    expect_match(output$range_chart$html, "population fell", fixed = TRUE)
  })
})

test_that("Back to all MSAs clears the selection", {
  selected <- reactiveVal(gamma)
  testServer(msaViewServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    expect_equal(output$title, gamma)
    session$setInputs(reset = 1)
    expect_null(selected())
  })
})

test_that("the land cover panel falls back to a message when the images are missing", {
  selected <- reactiveVal(alpha_beta)
  testServer(msaViewServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    # the fixture's www/frames/alpha/ doesn't exist
    expect_match(output$lc_frames$html, "No time-lapse image")
  })
})

test_that("yearly image paths follow step 10's www/frames/<dir>/<year>.jpg", {
  expect_equal(lc_frame_srcs("alpha", 1999:2000),
               c("frames/alpha/1999.jpg", "frames/alpha/2000.jpg"))
})

test_that("all years' images load stacked, with only the slider's year visible", {
  # a stand-in www/frames/alpha/ in a temporary app folder
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "www", "frames", "alpha"), recursive = TRUE)
  file.create(file.path(tmp, "www", lc_frame_srcs("alpha", 1999:2001)))

  selected <- reactiveVal(alpha_beta)
  testServer(msaViewServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    withr::local_dir(tmp)
    session$setInputs(year = 2000)
    html <- output$lc_frames$html
    expect_equal(lengths(regmatches(html, gregexpr("<img", html))), 3)
    expect_match(html, 'src="frames/alpha/2000.jpg" data-year="2000"[^>]*visibility:visible')
    expect_match(html, 'data-year="1999"[^>]*visibility:hidden')
    # moving the slider afterwards is handled by the message, not a rebuild
    expect_no_error(session$setInputs(year = 2001))
  })
})

test_that("the dot map opens on the GIF's frame and pans only over the dot area", {
  shape <- fixture_app_data()$msa_shapes[2, ]
  msa   <- msa_basemap(shape)

  # fitted exactly to the stored frame (no padding), with unsnapped zoom
  # (leaflet stores bounds as lat1, lng1, lat2, lng2)
  expect_equal(unlist(msa$x$fitBounds[1:4]),
               c(shape$frame_lat1, shape$frame_lng1, shape$frame_lat2, shape$frame_lng2))
  expect_null(msa$x$fitBounds[[5]]$padding)
  expect_equal(msa$x$options$zoomSnap, 0)
  expect_true(msa$x$options$preferCanvas)

  # panning limited to the dot area
  max_bounds <- Filter(function(cl) cl$method == "setMaxBounds", msa$x$calls)[[1]]$args
  expect_equal(unlist(max_bounds), c(shape$dots_lat1, shape$dots_lng1, shape$dots_lat2, shape$dots_lng2))

  # re-fits to the frame on resize, with the frame passed to the JS hook
  hook <- msa$jsHooks$render[[1]]
  expect_match(hook$code, "map.on('resize'", fixed = TRUE)
  # and tells the server it has finished drawing, the cue to send the dots
  expect_match(hook$code, "Shiny.setInputValue(el.id + '_ready'", fixed = TRUE)
  expect_equal(unlist(hook$data), c(lng1 = shape$frame_lng1, lat1 = shape$frame_lat1,
                                    lng2 = shape$frame_lng2, lat2 = shape$frame_lat2))
})

test_that("Reset view sits in the ribbon and re-fits the selected MSA's frame", {
  html <- as.character(msaViewUI("msa", ppd = 500, years = c(1999, 2024)))
  expect_gt(regexpr('id="msa-reset_view"', html), regexpr('id="msa-ribbon"', html))
  expect_lt(regexpr('id="msa-reset_view"', html), regexpr("msa-lc_frames", html))

  selected <- reactiveVal(gamma)
  testServer(msaViewServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    expect_no_error(session$setInputs(reset_view = 1))
  })
  expect_equal(msa_shape_row(fixture_app_data(), gamma)$frame_lng1, 4.9)
})

test_that("add_dots() draws the dots as one layer, and nothing when there are none", {
  fx  <- fixture_app_data()
  map <- msa_basemap(fx$msa_shapes[1, ]) |> add_dots(fx$dots[1:3, ])
  methods <- vapply(map$x$calls, `[[`, "", "method")
  expect_equal(sum(methods == "addCircleMarkers"), 1)
  markers <- map$x$calls[[which(methods == "addCircleMarkers")]]$args
  expect_true("dots" %in% unlist(markers))

  # no dots ticked -> no marker layer
  empty <- msa_basemap(fx$msa_shapes[1, ]) |> add_dots(fx$dots[0, ])
  expect_false("addCircleMarkers" %in% vapply(empty$x$calls, `[[`, "", "method"))
})

test_that("dots wait for the map to report it is drawn", {
  selected <- reactiveVal(alpha_beta)
  testServer(msaViewServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    # before the map is ready, ticking boxes sends nothing (and doesn't fail)
    expect_no_error(session$setInputs(type = c("Loss", "Gain")))
    expect_no_error(session$setInputs(dot_map_ready = 1))
    expect_no_error(session$setInputs(type = NULL))
  })
})

test_that("dots are limited to the selected MSA and the ticked types", {
  selected <- reactiveVal(alpha_beta)
  testServer(msaViewServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    session$setInputs(type = c("Loss", "Gain"))
    expect_equal(nrow(msa_dots()), 3)
    expect_equal(nrow(shown_dots()), 3)

    session$setInputs(type = "Loss")
    expect_equal(shown_dots()$change_cat, "Loss")

    session$setInputs(type = NULL)
    expect_equal(nrow(shown_dots()), 0)
  })
})

test_that("the national map reports when it has drawn, and circles wait for it", {
  d <- fixture_app_data()
  # the widget tells the server it has finished drawing (input$map_ready)
  hook <- national_basemap(d$state_shapes, d$neighbor_shapes, d$national_pan_bounds)$jsHooks$render[[1]]
  expect_match(hook$code, "Shiny.setInputValue(el.id + '_ready'", fixed = TRUE)

  # the circles observer runs on that cue, before and after a trip to an MSA
  selected <- reactiveVal(NULL)
  testServer(mapServer, args = list(app_data = d, selected_msa = selected, controls = map_controls()), {
    expect_no_error(session$setInputs(map_ready = 1))
    selected(gamma)
    selected(NULL)                                   # Back to all MSAs
    expect_no_error(session$setInputs(map_ready = 2)) # the re-rendered map is ready
  })
})

test_that("the hover label shows just the MSA's name", {
  d <- fixture_app_data()
  circles <- msa_map_data(d$msa_centers, d$msa_pop_dev, d$pop_dev, 2010, 2020, 0L)
  labels <- vapply(national_labels(circles), as.character, character(1))
  expect_equal(labels, circles$MSA)
})

test_that("the MSA view's chart follows the national year range and land type", {
  selected <- reactiveVal(fixture_msas[1])
  yrs   <- reactiveVal(c(2010, 2020))
  class <- reactiveVal(0L)
  controls <- list(year_range = yrs, dev_class = class)
  testServer(msaViewServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = controls), {
    land <- function() range_data()$value[range_data()$MSA == fixture_msas[1] &
                                          range_data()$measure == "New developed land"]
    expect_equal(land(), 20)    # total developed, 100 -> 120 sq mi
    class(21L)
    expect_equal(land(), -5)    # open space, 50 -> 45 sq mi
    yrs(c(2010, 2010))          # a single year: nothing to compare
    expect_error(range_data(), "at least two years")
  })
})

test_that("the Pacific and Atlantic labels are curved SVG text, never upside down", {
  for (i in seq_len(nrow(ocean_labels))) {
    o    <- ocean_labels[i, ]
    html <- as.character(ocean_label_html(o))
    expect_match(html, "<textPath", fixed = TRUE)
    expect_match(html, paste0(">", o$label, "</textPath>"), fixed = TRUE)
    # the path's start and end (M x0 y0 Q cx cy x2 y2): SVG y runs down
    nums <- as.numeric(strsplit(sub(".* d=\"M ([^\"]*)\".*", "\\1", html), "[ Q]+")[[1]])
    x0 <- nums[1]; y0 <- nums[2]; x2 <- nums[5]; y2 <- nums[6]
    expect_gt(x2, x0)   # reads left to right, so never upside down
    if (o$id == "pacific")  expect_gt(y2, y0)   # reads downward, north to south
    if (o$id == "atlantic") expect_lt(y2, y0)   # reads upward, south-west to north-east
  }
  expect_false(any(c("Pacific Ocean", "Atlantic Ocean") %in% place_labels$label))
  expect_match(basemap_css("map-map"), ".leaflet-tooltip.water-label text { fill: currentColor; }", fixed = TRUE)
})

test_that("the national view is applied once the map is visible, and re-renders don't stack layers", {
  d <- fixture_app_data()
  hook <- national_basemap(d$state_shapes, d$neighbor_shapes, d$relief_bounds)$jsHooks$render[[1]]
  # fit, zoom in and nudge together, from the lower 48's bounds
  expect_match(hook$code, "function applyView()", fixed = TRUE)
  expect_match(hook$code, "map.fitBounds(L.latLngBounds([bb[1], bb[0]], [bb[3], bb[2]])", fixed = TRUE)
  expect_equal(unlist(hook$data$bounds), unname(national_bounds))
  # hidden (re-rendered on the way back from an MSA): wait until it is shown
  expect_match(hook$code, "if (map.getSize().x > 0 && map.getSize().y > 0) applyView();", fixed = TRUE)
  expect_match(hook$code, "map.once('resize', function() { setTimeout(applyView, 0); });", fixed = TRUE)
  # the relief replaces the previous render's; hover handlers are set up once per map
  expect_match(hook$code, "if (map._national.relief) map.removeLayer(map._national.relief);", fixed = TRUE)
  expect_match(hook$code, "if (!map._national) {", fixed = TRUE)
})

test_that("the reader can't zoom out past the opening view", {
  d <- fixture_app_data()
  hook <- national_basemap(d$state_shapes, d$neighbor_shapes, d$relief_bounds)$jsHooks$render[[1]]
  # cleared before the fit, then set to the zoom the opening view lands on
  expect_match(hook$code, "map.setMinZoom(0);\n           map.fitBounds(", fixed = TRUE)
  expect_match(hook$code, "map.setMinZoom(map.getZoom());\n         }", fixed = TRUE)
})

test_that("leaving and reopening MSAs, or a one-year range, never leaves a drawing device open", {
  open_devices <- function() length(grDevices::dev.list())
  before   <- open_devices()
  selected <- reactiveVal(alpha_beta)
  yrs      <- reactiveVal(c(2010, 2020))
  controls <- list(year_range = yrs, dev_class = reactive(0L))
  testServer(msaViewServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = controls), {
    for (i in 1:5) {
      expect_match(output$range_chart$html, "<svg", fixed = TRUE)
      selected(NULL);  session$flushReact()                # Back to all MSAs
      selected(gamma); session$flushReact()
    }
    yrs(c(2010, 2010)); session$flushReact()               # one-year range: message, no chart
    expect_error(output$range_chart)
    expect_equal(open_devices(), before)
  })
})
