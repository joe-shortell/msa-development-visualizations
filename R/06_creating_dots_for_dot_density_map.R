library(sf)
library(dplyr)
library(here)


### load tract population data --------------------------------
tracts_2010_msa_pop_data <- readRDS(
  here("data", "processed", "geographies", "tracts_2010_msa_pop_data.rds")
)

crs <- st_crs(tracts_2010_msa_pop_data)

# convert to sf object
tracts_2010_msa_pop_data <- st_as_sf(
  tracts_2010_msa_pop_data,
  sf_column_name = "geometry",          # adjust if different
  crs = crs        # borrow CRS from original tracts
)


### trivariate dot density map --------------------------------------------------------

# people per dot. Saved alongside the dots (script 09) so the app's legend
# reads this value instead of hardcoding it — the legend used to say 250.
ppd <- 500

# 1) samples per tract. abs(): losses get dots too. This had become
# pmax(0, chg / ppd), which gives every losing tract zero dots — re-running it
# would have emptied the app's "Loss" layer (the saved dots predate that).
n <- round(abs(tracts_2010_msa_pop_data$chg_2000_2020) / ppd)
n[is.na(n)] <- 0

set.seed(123)

# sample each tract on its own and attach its attributes by row index.
# Two reasons not to sample all at once and spatially join back:
#   - script 05 keeps one row per tract *per MSA* it touches, so a spatial
#     join would give every dot in a shared tract both MSAs (duplicate dots);
#   - st_sample(exact = TRUE) over many polygons does not reliably return
#     exactly sum(n) points, so positions can't be mapped back by count.
geom <- st_geometry(tracts_2010_msa_pop_data)
idx  <- which(n > 0)
pts  <- lapply(idx, function(i) st_sample(geom[i], size = n[i], exact = TRUE))

dots <- st_sf(
  st_drop_geometry(tracts_2010_msa_pop_data)[rep(idx, lengths(pts)), , drop = FALSE],
  geometry = do.call(c, pts)
)

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
  here::here("data", "processed", "dots_tract_change.rds")
)