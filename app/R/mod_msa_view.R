### MSA view module ------------------------------------------------------------
# What the page shows once an MSA is selected, across the full width (app.R
# hides the national map and its controls panel): two panels side by side.
#   left   the population-change dot map (Web Mercator, CARTO Dark Matter
#          tiles, the MSA outline), with a grey ribbon beneath it holding
#          the title, Loss/Gain checkboxes and Reset view (dot_map_ribbon())
#   right  the MSA's developed land, one image per year (1999-2024), with a
#          grey ribbon holding the Developed Land legend and a year slider
#          whose play button steps through the years (lc_ribbon()). The
#          images are the frames of step 09's time-lapse GIF, split into
#          yearly JPEGs by step 10.
# Both maps are Web Mercator and frame the same square, centred on the MSA's
# circle (msa_frame() in R/utils.R), so they line up. The dot map's dots are
# placed by 2010 block group and cover every block group in view, inside the
# MSA or not, since the GIF shows all developed land in the frame.
# A header row above them, exactly as wide as the two panels together
# (msa_pair_width()), holds the view's title, just the MSA's name (the app's
# own title shows only in the national view), left-aligned with the dot map's
# left border, and on the same line "Back to all MSAs", flush with the land
# cover image's right border. The title's size is set so the longest MSA name
# fills the width left of the button and its gap (msa_title_fit_js); a bar
# below them (chart_slot) holds the comparison chart: where this MSA ranks
# among all 30 on new land, population growth and land per new resident over
# the national map's year range and land type (chart_msa_ranges.R).
# Both panels are boxes of the same size and the GIF's shape (msa_panel_style()),
# so the dot map is exactly as wide and tall as the GIF beside it.
#
# The dot map is its own Leaflet widget, separate from the national map, so
# neither one has to change projection. It sits in a conditionalPanel (app.R)
# and Shiny doesn't render hidden outputs, so it first renders once the panel
# is visible and the map has its real size, which fitBounds() needs to centre
# the MSA correctly.

# Tallest the two panels may be. Leaves room below for the chart bar, so the
# whole view fits on one screen.
# The year slider sits inside the panel, in the image's ribbon, so the panel
# can be this tall and the view still fit.
msa_panel_height <- "66vh"

# Height of the comparison chart's bar under the two panels: room for its
# three stacked lines, while the view still fits on one screen.
msa_chart_height <- "22vh"

# Gap between the two panels. The header above and the chart bar below span
# both panels and this gap (msa_pair_width()), so their edges line up with the
# panels' outer edges.
msa_panel_gap_px <- 48

# CSS for one side of the view (a column), shared by the dot map and the land
# cover image so they match. The panel box sits at its top (msa_panel_style());
# anything under the box would go below it in the column.
#
# The column's width is the smaller of
#   - half the row, less half the gap (so two columns always fit side by side)
#   - the width that makes the box msa_panel_height tall at the GIF's ratio
# On a typical screen the height limit wins; in a narrow window both shrink
# together. (The width is set on the column, not the box, because a percentage
# width is measured against the parent: the row here.)
msa_column_style <- function() {
  paste0(
    "flex:none; display:flex; flex-direction:column;",
    " width:", msa_column_width(), ";"
  )
}

# The column width rule above as a CSS expression, shared with the chart bar so
# the two are computed identically.
msa_column_width <- function() {
  paste0(
    "min(calc(50% - ", msa_panel_gap_px / 2, "px),",
    " calc(", msa_panel_height, " * ", gif_size[["width"]], " / ", gif_size[["height"]], "))"
  )
}

# The distance from the dot map's left border to the land cover image's right
# border: two columns plus the gap between them, as a CSS expression. The
# header and the chart bar both take this width and centre themselves like the
# panel row, so their edges line up with the panels' outer edges at any window
# size. They share the panel row's parent, so the 50% inside
# msa_column_width() measures the same width in all three.
# The national view uses it too, for the national map (mapUI()) and the
# controls panel below it (controlsUI()): both views sit in the same page
# container (app.R's two conditionalPanels), so the national map's edges are
# exactly where the dot map's left border and the image's right border are.
msa_pair_width <- function() {
  paste0("calc(2 * ", msa_column_width(), " + ", msa_panel_gap_px, "px)")
}

# Minimum space between the end of the title and "Back to all MSAs", even for
# the longest MSA name.
msa_title_button_gap_px <- 32

# CSS for the header row above the panels: the pair's width, centred, with the
# title and the Back button side by side and vertically centred on each other.
msa_header_style <- function() {
  paste0(
    "width:", msa_pair_width(), "; margin:12px auto 8px;",
    " display:flex; align-items:center;"
  )
}

# CSS for the box holding the title: it takes all the row's width the button
# and its gap don't use (flex:1; min-width:0 lets it be narrower than the
# title's natural width). container-type:inline-size lets the title size
# itself in cqw, 1% of this box's width.
msa_title_box_style <- function() {
  "flex:1 1 auto; min-width:0; container-type:inline-size;"
}

# CSS for the title. Its font size is its box's width (100cqw) divided by the
# longest name's width in ems (--msa-title-em, measured by msa_title_fit_js and
# inherited from the header row), so the longest name exactly fills the space
# left of the button, and every other name is set at that same size,
# left-aligned. Until the browser has measured the name, the fallback of 48 ems
# (wider than the longest name in the title font) keeps the title from
# running into the button.
# nowrap stops a rounding error from wrapping the longest name onto two lines.
msa_title_style <- function() {
  "margin:0; white-space:nowrap; font-size:calc(100cqw / var(--msa-title-em, 48));"
}

# CSS for the Back button: never shrinks, keeps at least
# msa_title_button_gap_px from the title, and has no right margin, so its right
# edge is the land cover image's right border.
msa_back_button_style <- function() {
  paste0("flex:none; margin:0 0 0 ", msa_title_button_gap_px, "px;")
}

# Left and right padding of the chart bar, in px.
msa_chart_pad_px <- 12

# CSS for the chart bar under the panels: the pair's width, centred, so its
# left and right edges line up with the panels' outer edges. border-box keeps
# the bar's padding and border inside that width.
# It stacks the period line over the plot (a flex column), and is a size
# container like the ribbons, so the period line can be sized in cqw.
msa_chart_style <- function() {
  paste0(
    "width:", msa_pair_width(), ";",
    " margin:12px auto 0; height:", msa_chart_height, ";",
    " padding:6px ", msa_chart_pad_px, "px;",
    " box-sizing:border-box; display:flex; flex-direction:column;",
    " container-type:inline-size;"
  )
}

# The chart's period line ("2005-2015 · Open space (21)"), styled like the dot
# map's title ("Population Change 2000-2020", dot_map_ribbon()): white, bold, and
# 2.4% of one panel's width.
# The title's 2.4cqw is measured in its panel; here cqw is the chart bar's
# content width, which spans both panels and the gap less the bar's padding:
#   100cqw = 2 x panel + gap - 2 x pad,  so  panel = (100cqw + 2 x pad - gap) / 2
# and 2.4% of a panel is (100cqw + 2 x pad - gap) x 0.012. Built from the
# same constants as the layout, so it stays equal to the title at any size.
msa_chart_period_style <- function() {
  paste0(
    "flex:none; white-space:nowrap; font-weight:bold; color:#FFFFFF; line-height:1.15;",
    " font-size:calc((100cqw - ", msa_panel_gap_px - 2 * msa_chart_pad_px, "px) * 0.012);"
  )
}

# Width of the year slider in the land cover ribbon, in cqw (% of the panel's
# width). It sits at the ribbon's right, leaving the left for the legend.
lc_slider_width_cqw <- 52

# The play button centred on its own line under the year slider. Shiny's own
# rule right-aligns it and pulls it 9px up into the slider; this overrides
# that for the year slider only (its wrapper has class lc-year-slider).
lc_play_button_css <- ".lc-year-slider .slider-animate-container { text-align:center; margin-top:2px; }"

# CSS for one panel box: the column's full width, with the GIF's aspect ratio
# (gif_size), which derives the box's height from that width.
msa_panel_style <- function() {
  paste0(
    "position:relative; width:100%;",
    " aspect-ratio:", gif_size[["width"]], " / ", gif_size[["height"]], ";"
  )
}

# The land cover image's ribbon: the legend and year slider, drawn by the app
# over the plain grey band step 09 leaves under each image, and laid out like
# the dot map's ribbon beside it (dot_map_ribbon()).
# An absolutely positioned layer covers the image's band exactly: anchored to
# the bottom of the panel box, gif_ribbon_frac of the panel's width tall (cqw;
# the box is an inline-size container). It has its own solid panel grey, so
# it matches the dot map's ribbon exactly (the GIF's grey is slightly
# speckled by its limited palette).
#   left   "Developed Land" (bold, like the dot map's title), a 1cqw gap, one
#          row per developed class (lc_legend, High Intensity on top): a
#          square in the class colour with its label to the right. (Data
#          sources are credited in the national view's side panel.)
#   right  the year slider, lc_slider_width_cqw wide, with its play button
#          centred beneath it (lc_play_button_css)
# Each side is its own full-height layer, centred top-to-bottom, like the two
# sides of the dot map's ribbon. The slider's value is the only year shown:
# the images don't print it.
lc_ribbon <- function(ns, years) {
  ribbon_h <- paste0(round(100 * gif_ribbon_frac, 3), "cqw")

  legend_row <- function(color, label) {
    shiny::div(
      style = "display:flex; align-items:center; margin-top:0.4cqw;",
      shiny::span(style = paste0(
        "flex:none; width:1.8cqw; height:1.8cqw; margin-right:0.8cqw; background:", color, ";"
      )),
      shiny::span(style = "font-size:1.6cqw; line-height:1.2; white-space:nowrap;", label)
    )
  }

  shiny::div(
    class = "lc-ribbon",
    style = paste0(
      "position:absolute; left:0; right:0; bottom:0; height:", ribbon_h, "; z-index:1;",
      " background:", colors$panel_grey, "; color:#ffffff;"
    ),
    # left: the Developed Land legend, centred top-to-bottom
    shiny::div(
      style = paste0(
        "position:absolute; top:0; bottom:0; left:1.85cqw;",
        " display:flex; flex-direction:column; justify-content:center;"
      ),
      shiny::div(
        style = "font-weight:bold; font-size:2.4cqw; line-height:1.15; white-space:nowrap; margin-bottom:0.6cqw;",
        "Developed Land"
      ),
      shiny::tagList(Map(legend_row, lc_legend$color, lc_legend$label, USE.NAMES = FALSE))
    ),
    # right: the year slider, centred top-to-bottom. Shiny's animate option
    # adds the play button, which advances one year every lc_play_ms and stops
    # at the end (pressing it again at the last year starts over).
    shiny::tags$style(shiny::HTML(lc_play_button_css)),
    shiny::div(
      class = "lc-year-slider",
      style = paste0(
        "position:absolute; top:0; bottom:0; right:1.85cqw; width:", lc_slider_width_cqw, "cqw;",
        " display:flex; flex-direction:column; justify-content:center;"
      ),
      shiny::sliderInput(
        inputId = ns("year"),
        label   = NULL,
        min     = years[1],
        max     = years[2],
        value   = years[1],
        step    = 1,
        sep     = "",
        ticks   = TRUE,
        width   = "100%",
        animate = shiny::animationOptions(interval = lc_play_ms, loop = FALSE)
      )
    )
  )
}

# The dot map's control ribbon: a grey band (colors$panel_grey, the chart
# bar's grey) under the square map, as tall (relative to the map's width) as
# the GIF's ribbon beside it (gif_ribbon_frac), so the two panels read as a
# pair and their maps line up. The title and its subtitle form one block at
# the left (data sources are credited in the national view's side panel), and the Gain/Loss checkboxes and Reset view one
# at the right; each is centred top-to-bottom in the whole ribbon.
#
# Laid out like the GIF's ribbon: the title at the left ("Population Change
# 2000-2020", with the people-per-dot note beneath), and at the right the
# Gain and Loss checkboxes stacked (Gain on top), each led by a dot in its map
# colour so they double as the legend, then a "Reset view" button. Its height
# and text are sized in cqw (1% of the panel's width; the panel is an
# inline-size container), so they scale with the panel the way the GIF's
# ribbon scales with the image.
# The scoped <style> sets the checkboxes' spacing in place of Bootstrap's:
# no outer margins, a small gap between the two rows, and the label's text
# (dot first) starting 3.4cqw in, so there is clear space between each
# checkbox and its dot. Shiny gives stacked (non-inline) inputs a fixed 300px
# width, which squeezed the title until it wrapped and spilled over the map;
# the style resets that width to auto, and the title block is flex:none with
# nowrap so the controls can never squeeze it again.
dot_map_ribbon <- function(ns, ppd) {
  ribbon_h   <- paste0(round(100 * gif_ribbon_frac, 3), "cqw")
  ribbon_id  <- ns("ribbon")
  key_dot <- function(color) {
    shiny::tags$span(style = paste0(
      "display:inline-block; width:1.4cqw; height:1.4cqw; border-radius:50%;",
      " margin-right:0.6cqw; vertical-align:middle; background:", color, ";"
    ))
  }

  shiny::tagList(
    shiny::tags$style(shiny::HTML(paste0(
      "#", ribbon_id, " .shiny-input-container { width:auto; }\n",
      "#", ribbon_id, " .form-group { margin:0 0 0 2.4cqw; }\n",
      "#", ribbon_id, " .checkbox { margin:0; }\n",
      "#", ribbon_id, " .checkbox + .checkbox { margin-top:0.5cqw; }\n",
      "#", ribbon_id, " .checkbox label { padding-left:3.4cqw; font-size:1.8cqw; line-height:1.2; min-height:0; }\n",
      "#", ribbon_id, " .checkbox input { margin-left:-3.4cqw; margin-top:0.3cqw; }"
    ))),
    shiny::div(
      id    = ribbon_id,
      style = paste0(
        "position:relative; height:", ribbon_h, "; background:", colors$panel_grey, "; color:#ffffff;"
      ),
      # Title and subtitle: one block in a layer spanning the
      # whole ribbon's height at its left (1.85cqw in), stacked in a column
      # that is centred top-to-bottom, like the controls layer at the right.
      shiny::div(
        style = paste0(
          "position:absolute; top:0; bottom:0; left:1.85cqw;",
          " display:flex; flex-direction:column; justify-content:center;"
        ),
        # title and subtitle; flex:none + nowrap so nothing can squeeze them.
        # Gap: title -> subtitle is the line-height plus the subtitle's
        # 0.8cqw margin.
        shiny::div(
          style = "line-height:1.15; flex:none; white-space:nowrap;",
          shiny::div(style = "font-weight:bold; font-size:2.4cqw;", "Population Change 2000–2020"),
          shiny::div(style = "margin-top:0.8cqw; font-size:1.6cqw;", paste0("each dot represents ", ppd, " people"))
        )
      ),
      # Gain/Loss and Reset view: a layer spanning the whole ribbon's height at
      # its right (on the band's 1.85cqw padding), so both are centred
      # top-to-bottom in the grey ribbon, independent of the title above.
      shiny::div(
        style = "position:absolute; top:0; bottom:0; right:1.85cqw; display:flex; align-items:center;",
        shiny::checkboxGroupInput(
          inputId      = ns("type"),
          label        = NULL,
          inline       = FALSE,   # stacked, Gain above Loss
          choiceNames  = list(
            shiny::tagList(key_dot(colors$dot_gain), "Gain"),
            shiny::tagList(key_dot(colors$dot_loss), "Loss")
          ),
          choiceValues = c("Gain", "Loss"),
          selected     = c("Loss", "Gain")
        ),
        # snaps a panned/zoomed map back to the GIF's frame (msaViewServer)
        shiny::actionButton(
          inputId = ns("reset_view"),
          label   = "Reset view",
          style   = paste0(
            "margin-left:2.4cqw; padding:0.3cqw 1cqw; font-size:1.6cqw; line-height:1.2;",
            " background:transparent; color:#ffffff; border:1px solid #ffffff; border-radius:3px;"
          )
        )
      )
    )
  )
}

# Shows one year's land cover image and hides the rest.
# The server sends {id, year} each time the year slider moves (msaViewServer);
# every year's image is already loaded and stacked in the box, so changing
# year is instant and playback never waits on a download.
lc_year_js <- "
Shiny.addCustomMessageHandler('show-lc-year', function(msg) {
  var box = document.getElementById(msg.id);
  if (!box) return;
  box.querySelectorAll('img[data-year]').forEach(function(img) {
    img.style.visibility = (img.dataset.year == msg.year) ? 'visible' : 'hidden';
  });
});
"

# Measures how wide the longest MSA name is in the title's font, in ems, and
# stores it on the header block as --msa-title-em, which sizes the title
# (msa_title_style()).
# The MSA view is hidden when the page loads, and hidden elements have no width,
# so the name is measured with a canvas instead of in the page: the canvas is
# given the title's own font (family, weight, style, read from its computed
# style, which hidden elements still have) at 100px, and the text's width / 100
# is its width in ems. This waits for the title font (title_font_name,
# constants.R) to load, since measuring with the fallback font would give the
# wrong width.
msa_title_fit_js <- paste0("
function msaTitleFit() {
  document.querySelectorAll('.msa-title-block').forEach(function(block) {
    var title = block.querySelector('h2');
    var name  = block.dataset.longest;
    if (!title || !name) return;
    var cs  = window.getComputedStyle(title);
    var ctx = document.createElement('canvas').getContext('2d');
    ctx.font = cs.fontStyle + ' ' + cs.fontWeight + ' 100px ' + cs.fontFamily;
    var em = ctx.measureText(name).width / 100;
    if (em > 0) block.style.setProperty('--msa-title-em', em);
  });
}
document.addEventListener('DOMContentLoaded', function() {
  if (document.fonts && document.fonts.load) {
    document.fonts.load('100px ", title_font_name, "').then(msaTitleFit, msaTitleFit);
  } else {
    msaTitleFit();
  }
});
")

# `years` is the first and last land cover year, the slider's range.
# `longest_title` is the longest MSA name: the title is sized so that it
# exactly spans the two panels (app.R passes it from app_data).
msaViewUI <- function(id, ppd, years, longest_title = NULL) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$script(shiny::HTML(lc_year_js)),
    shiny::tags$script(shiny::HTML(msa_title_fit_js)),
    # Header: one row as wide as the two panels and centred like them. The
    # title starts at the dot map's left border; "Back to all MSAs" sits on the
    # same line, vertically centred on it and flush with the land cover image's
    # right border.
    shiny::div(
      class          = "msa-title-block",
      `data-longest` = longest_title,
      style          = msa_header_style(),
      shiny::div(
        style = msa_title_box_style(),
        shiny::h2(style = msa_title_style(), shiny::textOutput(ns("title"), inline = TRUE))
      ),
      shiny::actionButton(
        inputId = ns("reset"),
        label   = "Back to all MSAs",
        class   = "btn btn-sm btn-light",
        style   = msa_back_button_style()
      )
    ),
    # The two panels side by side and centred, each in an identical box.
    shiny::div(
      style = paste0(
        "display:flex; justify-content:center; align-items:flex-start;",
        " gap:", msa_panel_gap_px, "px;"
      ),
      # left: the dot map in a square the width of the box, the control
      # ribbon beneath it; the same layout as the image beside it
      shiny::div(
        style = msa_column_style(),
        shiny::div(
          style = paste(msa_panel_style(), "container-type:inline-size;"),
          shiny::div(
            style = "width:100%; aspect-ratio:1;",
            leaflet::leafletOutput(ns("dot_map"), height = "100%")
          ),
          dot_map_ribbon(ns, ppd)
        )
      ),
      # right: the land cover image, filling a box of the same size, with its
      # ribbon (legend and year slider, lc_ribbon()) laid over the image's
      # grey band. container-type lets the ribbon size itself in cqw.
      shiny::div(
        style = msa_column_style(),
        shiny::div(
          style = paste(msa_panel_style(), "container-type:inline-size;"),
          shiny::uiOutput(ns("lc_frames"), style = "position:relative; height:100%;"),
          lc_ribbon(ns, years)
        )
      )
    ),
    # Comparison chart: a bar under the two panels, aligned with their outer
    # edges (msa_chart_style()), holding the three number lines from
    # msa_range_plot() (chart_msa_ranges.R). The plot fills the bar (height
    # 100%); the well's padding is trimmed so the bar's height goes to the chart.
    shiny::div(
      id    = ns("chart_slot"),
      class = "well",
      style = msa_chart_style(),
      # the period line, then the chart filling the rest of the bar's height.
      # The chart arrives as SVG (svg_chart(), chart_msa_ranges.R). The
      # shiny-report-size class makes Shiny report this slot's width and height
      # to the server, as it does for plots, so the SVG is drawn at its exact
      # size; the CSS makes the <svg> fill the slot (svglite sizes it in points).
      shiny::tags$style(shiny::HTML(paste0("#", ns("range_chart"), " svg { display:block; width:100%; height:100%; }"))),
      shiny::div(style = msa_chart_period_style(), shiny::textOutput(ns("chart_period"), inline = TRUE)),
      shiny::div(
        style = "flex:1; min-height:0;",
        shiny::uiOutput(ns("range_chart"), class = "shiny-report-size", style = "height:100%;")
      )
    )
  )
}

msaViewServer <- function(id, app_data, selected_msa, controls) {
  shiny::moduleServer(id, function(input, output, session) {
    msa_dots <- shiny::reactive({
      msa <- shiny::req(selected_msa())
      app_data$dots[app_data$dots$MSA == msa, ]
    })

    # no boxes ticked -> no dots (input$type is NULL then)
    shown_dots <- shiny::reactive({
      d <- msa_dots()
      d[d$change_cat %in% input$type, ]
    })

    output$title <- shiny::renderText(shiny::req(selected_msa()))

    # going back shows the national map again (app.R toggles the panels)
    shiny::observeEvent(input$reset, selected_msa(NULL))

    # --- dot map -------------------------------------------------------------
    # The outline and starting view are part of the render, not leafletProxy()
    # calls afterwards, so the view is applied once the map has its final size.
    # Dots are left out: drawn in the same render, while the conditionalPanel
    # is still revealing the map, they left the whole map blank.
    output$dot_map <- leaflet::renderLeaflet({
      msa_basemap(msa_shape_row(app_data, shiny::req(selected_msa())))
    })

    # Reset view: after panning or zooming, fit the map back to the GIF's
    # frame, so the two panels show the same ground again.
    shiny::observeEvent(input$reset_view, {
      shape <- msa_shape_row(app_data, shiny::req(selected_msa()))
      leaflet::leafletProxy("dot_map") |>
        leaflet::fitBounds(shape$frame_lng1, shape$frame_lat1, shape$frame_lng2, shape$frame_lat2)
    })

    # Draw the dots once the map has finished drawing, and again when
    # Loss/Gain changes, redrawing only the dots, not the whole map.
    # Dots go to the live map with leafletProxy(), which silently drops them
    # if the map isn't there yet (as when an MSA has just been clicked). So
    # this waits for input$dot_map_ready, which msa_basemap()'s onRender hook
    # sets each time the map finishes drawing (once per MSA opened), rather
    # than reacting to the selection itself. ignoreNULL = FALSE lets
    # unticking both boxes (input$type NULL) clear the dots.
    shiny::observeEvent(list(input$dot_map_ready, input$type), ignoreNULL = FALSE, {
      shiny::req(input$dot_map_ready)
      leaflet::leafletProxy("dot_map") |>
        leaflet::clearGroup("dots") |>
        add_dots(shown_dots())
    })

    # --- land cover images and year slider -------------------------------
    # Load all of the MSA's yearly images at once, stacked in its box, and
    # show only the slider's year; moving the slider just switches which one
    # is visible (see lc_year_js), so it is instant even while playing.
    # Step 10 writes the images to www/frames/<frame_dir>/<year>.jpg. They
    # fill a box that already has their shape and the dot map's size
    # (msa_panel_style()); object-fit:contain only guards against an image of
    # another shape being stretched. isolate(): the year is read once, for
    # the image to show first; later moves go through the observer below,
    # not a rebuild. Opening another MSA keeps the year the slider is on.
    output$lc_frames <- shiny::renderUI({
      msa <- shiny::req(selected_msa())
      dir <- app_data$msa_shapes$frame_dir[app_data$msa_shapes$MSA == msa][1]
      srcs <- lc_frame_srcs(dir, app_data$lc_years)

      if (is.na(dir) || !file.exists(file.path("www", srcs[1]))) {
        # fills the same box, so the layout doesn't change without images
        return(shiny::wellPanel(
          style = "height:100%; margin:0;",
          shiny::p("No time-lapse image could be found for this MSA.")
        ))
      }

      shown <- shiny::isolate(input$year) %||% app_data$lc_years[1]
      shiny::tagList(Map(function(src, yr) {
        shiny::tags$img(
          src         = src,
          `data-year` = yr,
          alt         = paste0("Developed land, ", msa, ", ", yr),
          style       = paste0(
            "position:absolute; inset:0; width:100%; height:100%; object-fit:contain;",
            " visibility:", if (yr == shown) "visible" else "hidden", ";"
          )
        )
      }, srcs, app_data$lc_years, USE.NAMES = FALSE))
    })

    # Moving the slider (or the play button) shows that year's image.
    shiny::observeEvent(input$year, {
      session$sendCustomMessage(
        "show-lc-year",
        list(id = session$ns("lc_frames"), year = input$year)
      )
    })

    # --- comparison chart ----------------------------------------------------
    # Covers the same years and land type as the national map, read from its
    # controls (controlsServer(), mod_controls.R). The three measures for all
    # 30 MSAs depend only on those, so they are recomputed when the range or
    # land type changes, not when another MSA is opened; opening another MSA
    # only moves the highlight. A range of a single year has no change to
    # show, so the chart bar says so instead.
    # bg = "transparent" lets the chart bar's colour show through.
    chart_years <- shiny::reactive(controls$year_range())

    range_data <- shiny::reactive({
      yrs <- chart_years()
      shiny::validate(shiny::need(
        yrs[1] < yrs[2], "Choose a year range of at least two years to compare metros."
      ))
      msa_range_data(app_data$pop_dev, yrs[1], yrs[2], controls$dev_class())
    })

    # Drawn as SVG at the slot's size, read from what Shiny reports for it
    # (it redraws when the window, and so the slot, is resized).
    output$range_chart <- shiny::renderUI({
      id <- session$ns("range_chart")
      w  <- session$clientData[[paste0("output_", id, "_width")]]
      h  <- session$clientData[[paste0("output_", id, "_height")]]
      shiny::req(w > 0, h > 0)
      # build the plot first, so a req()/validate() stop happens before
      # svg_chart() opens its drawing device
      p <- msa_range_plot(range_data(), shiny::req(selected_msa()))
      shiny::HTML(svg_chart(p, w, h))
    })

    # the period line above the chart (msa_chart_period_style())
    output$chart_period <- shiny::renderText(
      chart_period_label(chart_years()[1], chart_years()[2], controls$dev_class())
    )

    list(msa_dots = msa_dots, shown_dots = shown_dots, range_data = range_data)
  })
}

### drawing helpers --------------------------------------------------------------

# The MSA base map: Web Mercator with CARTO Dark Matter tiles and the MSA outline,
# opened on the same frame the time-lapse GIF shows.
#
# `shape` is one row of app_data$msa_shapes. Its frame_* corners are the square
# step 09 drew the GIF in, and dots_* the larger area steps 06-07 filled with
# dots; both come from msa_frame() in R/utils.R via step 10. The view is fitted
# to the frame with no padding, and zoomSnap = 0 lets Leaflet use any
# fractional zoom, so it fits the frame exactly. (Any snapping rounds the zoom
# down: whole levels can show up to twice the area, steps of 0.1 up to ~7%
# more.) The +/- buttons still step one whole level.
#
# When the map's box changes size (the window is resized, and the GIF beside
# it scales with it), Leaflet would keep its zoom and so show a different area
# from the GIF. The onRender() hook listens for Leaflet's "resize" event and
# fits the frame again. The same hook then tells the server the map is drawn
# (input$<id>_ready, e.g. input$dot_map_ready), the cue to send the dots.
# Panning stops at the
# dot area, so the map never shows ground that has no dots. preferCanvas draws
# the dots on one canvas instead of one SVG element each, which keeps maps with
# ten thousand or more dots responsive.
msa_basemap <- function(shape) {
  leaflet::leaflet(options = leaflet::leafletOptions(
    minZoom            = 3,
    zoomSnap           = 0,
    preferCanvas       = TRUE,
    maxBoundsViscosity = 1
  )) |>
    leaflet::addTiles(
      urlTemplate = basemap_url(),
      attribution = basemap_attribution,
      options     = leaflet::tileOptions(noWrap = TRUE, maxZoom = 20)
    ) |>
    leaflet::setMaxBounds(shape$dots_lng1, shape$dots_lat1, shape$dots_lng2, shape$dots_lat2) |>
    leaflet::addPolygons(
      data        = shape,
      color       = basemap_colors$msa_border,
      weight      = 1.5,
      fillColor   = "transparent",
      fillOpacity = 0
    ) |>
    leaflet::fitBounds(shape$frame_lng1, shape$frame_lat1, shape$frame_lng2, shape$frame_lat2) |>
    htmlwidgets::onRender(
      "function(el, x, frame) {
         var map = this;
         var bounds = L.latLngBounds([frame.lat1, frame.lng1], [frame.lat2, frame.lng2]);
         map.on('resize', function() { map.fitBounds(bounds, {animate: false}); });
         if (HTMLWidgets.shinyMode) {
           Shiny.setInputValue(el.id + '_ready', Date.now());
         }
       }",
      data = list(
        lng1 = shape$frame_lng1, lat1 = shape$frame_lat1,
        lng2 = shape$frame_lng2, lat2 = shape$frame_lat2
      )
    )
}

# Dot colours by change category.
dot_pal <- leaflet::colorFactor(
  palette = c("Gain" = colors$dot_gain, "Loss" = colors$dot_loss, "No Change" = colors$no_change),
  domain  = c("Gain", "Loss", "No Change")
)

# Add population-change dots to a dot map, as one "dots" group.
# Works on both a new map (leaflet()) and a leafletProxy(), so the first
# render and the Loss/Gain redraws draw dots the same way. Adds nothing when
# there are no dots (e.g. both boxes unticked).
add_dots <- function(map, dots) {
  if (nrow(dots) == 0) return(map)
  leaflet::addCircleMarkers(
    map,
    data        = dots,
    group       = "dots",
    radius      = 1,   # screen pixels (2 px across), the same at every zoom
    stroke      = FALSE,
    fillOpacity = 0.6,
    fillColor   = ~dot_pal(change_cat)
  )
}

# Web paths of an MSA's yearly land cover images, in year order: step 10
# writes them to www/frames/<frame_dir>/<year>.jpg, served at frames/...
lc_frame_srcs <- function(frame_dir, years) {
  paste0("frames/", frame_dir, "/", years, ".jpg")
}

# The row of app_data$msa_shapes for one MSA (its outline and frame corners).
msa_shape_row <- function(app_data, msa) {
  app_data$msa_shapes[app_data$msa_shapes$MSA == msa, ]
}
