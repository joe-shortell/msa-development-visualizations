library(terra)
library(sf)
library(dplyr)
library(here)

source(here("R", "utils.R"))   # slugify(), lc_years

### developed land by MSA and year ---------------------------------------------------
# How much of each MSA is developed, by NLCD developed class, in every land cover
# year, measured inside the MSA's boundary.
#
# Every year in lc_years (1999-2024): step 08 pairs these with the MSA
# populations from step 05 for the national map and the MSA view's comparison
# chart, which both cover any range of these years.
#
# For each MSA and year, the crop from step 02 is masked to the MSA outline and
# its developed pixels (classes 21-24) are counted and converted to area.
# Output: one row per MSA x year x class, where class 0 is all four combined:
#   msa, year, lc_class, area_mi2
developed_classes <- 21:24

# per-MSA crops from step 02: data/processed/landcover_crops/<msa_slug>/nlcd_<year>.tif
crop_dir <- here("data", "processed", "landcover_crops")

terraOptions(todisk = TRUE)

msa_geom <- readRDS(here("data", "processed", "geographies", "msa_geom.rds"))

# The crops share one projected CRS (metres); outlines are transformed to it
# once, from the first crop found.
first_crop <- file.path(crop_dir, slugify(msa_geom$MSA[1]), sprintf("nlcd_%d.tif", lc_years[1]))
msa_geom   <- st_transform(msa_geom, crs(rast(first_crop)))

results <- list()

for (yr in lc_years) {
  message("=== Year ", yr, " ===")

  for (i in seq_len(nrow(msa_geom))) {
    msa_name <- msa_geom$MSA[i]
    r_path   <- file.path(crop_dir, slugify(msa_name), sprintf("nlcd_%d.tif", yr))
    if (!file.exists(r_path)) {
      warning(msa_name, ": no land cover crop for ", yr, "; skipped")
      next
    }

    # Mask the crop to the MSA's true outline, then count pixels by class.
    msa_vect <- vect(msa_geom[i, ])
    r_msa    <- mask(crop(rast(r_path), msa_vect), msa_vect)
    cell_m2  <- prod(res(r_msa))   # pixel area, CRS in metres

    by_class <- as.data.frame(freq(r_msa, digits = 0)) |>
      filter(value %in% developed_classes) |>
      transmute(msa = msa_name, year = yr, lc_class = as.integer(value),
                area_m2 = count * cell_m2)

    # class 0 = all developed classes together
    results[[length(results) + 1]] <- bind_rows(
      by_class,
      summarise(by_class, msa = msa_name, year = yr, lc_class = 0L, area_m2 = sum(area_m2))
    )

    rm(r_msa); gc()
  }
}

### save ------------------------------------------------------------------------------
developed_area <- bind_rows(results) |>
  transmute(msa, year, lc_class, area_mi2 = area_m2 / 1e6 * 0.386102) |>
  arrange(msa, lc_class, year)

saveRDS(developed_area, here("data", "processed", "developed_area_by_geo_and_year.rds"))
