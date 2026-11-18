library(sf)
library(dplyr)
library(here)

source(here("R", "utils.R"))   # msa_frame()

### load block-group population data ---------------------------
# from step 06: one row per 2010 block group per MSA view it falls in
bgs_2010_msa_pop_data <- readRDS(
  here("data", "processed", "geographies", "bg_2010_msa_pop_data.rds")
)

crs <- st_crs(bgs_2010_msa_pop_data)

# convert to sf object
bgs_2010_msa_pop_data <- st_as_sf(
  bgs_2010_msa_pop_data,
  sf_column_name = "geometry",          # adjust if different
  crs = crs        # borrow CRS from the block groups
)


### trivariate dot density map --------------------------------------------------------

# people per dot. Saved alongside the dots (script 10) so the app's legend
# reads this value instead of hardcoding it — the legend used to say 250.
ppd <- 500

# 1) samples per block group. abs(): losses get dots too. This had become
# pmax(0, chg / ppd), which gives every losing unit zero dots — re-running it
# would have emptied the app's "Loss" layer (the saved dots predate that).
n <- round(abs(bgs_2010_msa_pop_data$chg_2000_2020) / ppd)
n[is.na(n)] <- 0

set.seed(123)

# sample each block group on its own and attach its attributes by row index.
# Two reasons not to sample all at once and spatially join back:
#   - script 06 keeps one row per block group *per MSA view* it falls in, so
#     a spatial join would give every dot in a shared one both MSAs (duplicate
#     dots); sampled per row, each view gets its own set;
#   - st_sample(exact = TRUE) over many polygons does not reliably return
#     exactly sum(n) points, so positions can't be mapped back by count.
geom <- st_geometry(bgs_2010_msa_pop_data)
idx  <- which(n > 0)
pts  <- lapply(idx, function(i) st_sample(geom[i], size = n[i], exact = TRUE))

dots <- st_sf(
  st_drop_geometry(bgs_2010_msa_pop_data)[rep(idx, lengths(pts)), , drop = FALSE],
  geometry = do.call(c, pts)
)

# Trim each MSA's dots to its dot area (msa_frame(scale = 1.25), as in 06).
# Block groups were sampled whole so each one's dot count matches its whole
# population change; dots from the part of an edge block group outside the area
# could never be seen in that view, so they are dropped here.
# The test is done in Web Mercator, where the areas' edges are straight lines
# (in lng/lat, s2 would treat them as great circles), one MSA at a time.
dot_areas <- msa_frame(
  readRDS(here("data", "processed", "geographies", "msa_geom.rds")),
  scale = 1.25
)
dots_merc <- st_geometry(st_transform(dots, 3857))

inside <- logical(nrow(dots))
for (m in unique(dots$MSA)) {
  i <- which(dots$MSA == m)
  inside[i] <- lengths(st_intersects(dots_merc[i], dot_areas[dot_areas$MSA == m, ])) > 0
}
dots <- dots[inside, ]

# classify the change for discrete colors
dots <- dots %>%
  mutate(
    change_cat = case_when(
      chg_2000_2020 < 0 ~ "Loss",
      chg_2000_2020 > 0 ~ "Gain",
      TRUE              ~ "No Change"
    )
  )

saveRDS(ppd, here::here("data", "processed", "dots_ppd.rds"))


### save dots -------------------------------------------------------
saveRDS(
  dots,
  here::here("data", "processed", "dots_bg_change.rds")
)