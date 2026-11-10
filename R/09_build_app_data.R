library(sf)
library(dplyr)
library(here)

source(here("R", "utils.R"))   # slugify()

### build the app's self-contained inputs ---------------------------------------
# The app is deployed as the app/ folder alone, so it cannot reach data/ or
# output/. This step writes everything it reads into app/:
#   app/data/app_data.rds   one named list, already in EPSG:4326
#   app/www/gifs/*.gif      served by Shiny at gifs/<file>
# It also does the joins and transforms the app used to redo on every launch.

### load ------------------------------------------------------------------------
msa_shapes <- readRDS(here("data", "processed", "geographies", "msa_geom.rds"))
city_shapes <- readRDS(here("data", "processed", "geographies", "city_geom.rds"))
pop_dev <- readRDS(
  here("data", "processed", "combined_population_development_data_lc_classes.rds")
)
dots <- readRDS(here("data", "processed", "dots_tract_change.rds"))
ppd  <- readRDS(here("data", "processed", "dots_ppd.rds"))

### geographies -----------------------------------------------------------------
# gif_file is the only place the app needs slugify(); computing it here keeps
# slugify() defined once, in R/utils.R.
msa_shapes <- msa_shapes |>
  st_transform(4326) |>
  mutate(gif_file = paste0(slugify(MSA), "_developed.gif"))

st_agr(msa_shapes) <- "constant"   # MSA attributes hold at the centroid
msa_centers <- st_centroid(msa_shapes)

# attach CITY_GEOID + MSA so the city tabs can find their outline
city_shapes <- city_shapes |>
  st_transform(4326) |>
  left_join(distinct(pop_dev, CITY, CITY_GEOID, MSA), by = "CITY")

### tables ----------------------------------------------------------------------
# one row per MSA-year (MSA totals are repeated on every city/class row)
msa_pop_dev <- pop_dev |>
  group_by(MSA, year) |>
  slice(1) |>
  ungroup() |>
  select(MSA_GEOID, MSA, year, msa_pop, msa_area_mi2)

dots <- dots |>
  st_transform(4326) |>
  select(MSA, change_cat)

### write -----------------------------------------------------------------------
dir.create(here("app", "data"), showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(
    msa_shapes  = msa_shapes,
    msa_centers = msa_centers,
    city_shapes = city_shapes,
    pop_dev     = pop_dev,
    msa_pop_dev = msa_pop_dev,
    dots        = dots,
    ppd         = ppd
  ),
  here("app", "data", "app_data.rds")
)

gif_dir <- here("app", "www", "gifs")
dir.create(gif_dir, showWarnings = FALSE, recursive = TRUE)
gifs <- here("output", "gifs", msa_shapes$gif_file)
missing <- !file.exists(gifs)
if (any(missing)) {
  warning("No GIF for: ", paste(msa_shapes$MSA[missing], collapse = "; "))
}
invisible(file.copy(gifs[!missing], gif_dir, overwrite = TRUE))
