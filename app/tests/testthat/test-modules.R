alpha_beta <- fixture_msas[1]
gamma      <- fixture_msas[2]

test_that("sidebar change table follows the year range", {
  selected <- reactiveVal(alpha_beta)
  testServer(sidebarServer, args = list(app_data = fixture_app_data(), selected_msa = selected), {
    session$setInputs(year_range = c(2020, 2010))   # slider order shouldn't matter
    expect_equal(year_range(), c(2010, 2020))
    expect_match(output$change_panel$html, "1,500")

    session$setInputs(year_range = c(2010, 2010))
    expect_no_match(output$change_panel$html, "1,500")
  })
})

test_that("moving the slider does not rebuild the city tabs", {
  selected <- reactiveVal(alpha_beta)
  testServer(sidebarServer, args = list(app_data = fixture_app_data(), selected_msa = selected), {
    session$setInputs(year_range = c(2010, 2020), geo_tab = "city_002")
    tabs_before <- output$geo_tabs$html   # tab anchors get random ids per render

    session$setInputs(year_range = c(2010, 2015))
    expect_identical(output$geo_tabs$html, tabs_before)
    expect_equal(selected_city(), "002")
  })
})

test_that("a city tab left over from another MSA is ignored", {
  selected <- reactiveVal(alpha_beta)
  testServer(sidebarServer, args = list(app_data = fixture_app_data(), selected_msa = selected), {
    session$setInputs(year_range = c(2010, 2020), geo_tab = "city_002")
    selected(gamma)
    expect_null(selected_city())

    session$setInputs(geo_tab = "MSA_tab")
    expect_null(selected_city())
  })
})

map_controls <- function() {
  list(
    year_range    = reactive(c(2010, 2020)),
    metric        = reactive("pop"),
    dev_class     = reactive(0L),
    selected_city = reactive(NULL)
  )
}

test_that("clicks select an MSA; the city outline and reset don't misfire", {
  selected <- reactiveVal(NULL)
  testServer(mapServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    session$setInputs(map_marker_click = list(id = gamma))
    expect_equal(selected(), gamma)

    # the highlighted city outline is a shape too; clicking it must not
    # "select" an MSA called selected_city
    session$setInputs(map_shape_click = list(id = "selected_city"))
    expect_equal(selected(), gamma)

    session$setInputs(reset = 1)
    expect_null(selected())
  })
})

test_that("dots are limited to the selected MSA and the ticked types", {
  selected <- reactiveVal(alpha_beta)
  testServer(mapServer, args = list(app_data = fixture_app_data(), selected_msa = selected, controls = map_controls()), {
    session$setInputs(type = c("Loss", "Gain"))
    expect_equal(nrow(msa_dots()), 3)
    expect_equal(nrow(shown_dots()), 3)

    session$setInputs(type = "Loss")
    expect_equal(shown_dots()$change_cat, "Loss")

    session$setInputs(type = NULL)
    expect_equal(nrow(shown_dots()), 0)
  })
})
