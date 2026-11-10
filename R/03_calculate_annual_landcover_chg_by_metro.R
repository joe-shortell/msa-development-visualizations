library(terra)
library(sf)
library(dplyr)
library(here)

source(here("R", "utils.R"))   # slugify()

# ----------------- parameters ----------------------------------------

years <- setdiff(2005:2024, 2020)   # 2005–2024, skipping 2020
developed_classes <- 21:24

# base folder with per-MSA crop rasters:
#   data/processed/landcover_crops/<msa_slug>/nlcd_<year>.tif
out_base <- here("data", "processed", "landcover_crops")

terraOptions(todisk = TRUE)

# ----------------- load geometries -----------------------------------

# MSAs (must have a column "MSA" that matches your folder naming logic)
template_area <- readRDS(
  here("data", "processed", "geographies", "msa_geom.rds")
)
msas <- template_area$MSA

# Cities within MSAs (adjust path / column names if different)
# Must have:
#   - column "MSA" that matches template_area$MSA
#   - column "CITY_NAME" (or change below)
cities_raw <- readRDS(
  here("data", "processed", "geographies", "city_geom.rds")
)

# We'll reproject geometries once we know the raster CRS
msas_tr   <- NULL
cities_tr <- NULL

all_results <- list()

# ----------------- main loop -----------------------------------------

for (yr in years) {
  message("=== Year ", yr, " ===")
  
  # Find a raster for *some* MSA in this year to get CRS, if not set yet
  if (is.null(msas_tr)) {
    message("  Initializing CRS from first available raster...")
    
    crs_r <- NULL
    for (msa_name in msas) {
      msa_slug <- slugify(msa_name)
      r_path   <- file.path(out_base, msa_slug, sprintf("nlcd_%d.tif", yr))
      if (file.exists(r_path)) {
        crs_r <- crs(rast(r_path))
        break
      }
    }
    
    if (is.null(crs_r)) {
      warning("No rasters found for year ", yr, "; skipping this year.")
      next
    }
    
    msas_tr   <- st_transform(template_area, crs_r)
    cities_tr <- st_transform(cities_raw,   crs_r)
  }
  
  # Loop over MSAs
  for (i in seq_len(nrow(msas_tr))) {
    msa_name <- msas_tr$MSA[i]
    msa_slug <- slugify(msa_name)
    
    r_path <- file.path(out_base, msa_slug, sprintf("nlcd_%d.tif", yr))
    if (!file.exists(r_path)) {
      message("  [", msa_name, "] no raster for ", yr, " → skipping MSA.")
      next
    }
    
    message("  Processing MSA: ", msa_name, " (", yr, ")")
    r <- rast(r_path)
    
    # --- MSA-level area ----------------------------------------------
    msa_geom <- msas_tr[i, ]
    msa_vect <- vect(msa_geom)
    
    # crop+mask to true MSA boundary
    r_msa <- mask(crop(r, msa_vect), msa_vect)
    
    # pixel area in m^2 (assuming projected CRS in meters)
    cell_area_m2 <- prod(res(r_msa))
    
    # get full frequency table, then filter to developed classes
    fr_msa <- freq(
      r_msa,
      digits = 0
    )
    
    msa_results <- tibble()
    if (!is.null(fr_msa) && nrow(fr_msa) > 0) {
      df_msa_classes <- as.data.frame(fr_msa) |>
        dplyr::filter(value %in% developed_classes) |>
        dplyr::transmute(
          year      = yr,
          level     = "msa",
          msa       = msa_name,
          city      = NA_character_,
          lc_class  = as.integer(value),          # 21, 22, 23, 24
          area_m2   = count * cell_area_m2
        )
      
      if (nrow(df_msa_classes) > 0) {
        df_msa_total <- df_msa_classes |>
          dplyr::summarise(
            year     = unique(year),
            level    = "msa",
            msa      = unique(msa),
            city     = NA_character_,
            lc_class = 0L,                        # 0 = all developed
            area_m2  = sum(area_m2)
          )
        
        msa_results <- dplyr::bind_rows(df_msa_classes, df_msa_total)
      }
    }
    
    # --- City-level area within this MSA -----------------------------
    cities_this_msa <- cities_tr |>
      dplyr::filter(.data$MSA == msa_name)
    
    city_results_list <- list()
    
    if (nrow(cities_this_msa) > 0) {
      for (j in seq_len(nrow(cities_this_msa))) {
        city_geom <- cities_this_msa[j, ]
        # column name from your saved city_geom
        city_name <- city_geom$CITY
        
        city_vect <- vect(city_geom)
        r_city    <- mask(crop(r, city_vect), city_vect)
        
        fr_city <- freq(
          r_city,
          digits = 0
        )
        
        if (is.null(fr_city) || nrow(fr_city) == 0) next
        
        df_city_classes <- as.data.frame(fr_city) |>
          dplyr::filter(value %in% developed_classes) |>
          dplyr::transmute(
            year      = yr,
            level     = "city",
            msa       = msa_name,
            city      = city_name,
            lc_class  = as.integer(value),
            area_m2   = count * cell_area_m2
          )
        
        if (nrow(df_city_classes) == 0) next
        
        df_city_total <- df_city_classes |>
          dplyr::summarise(
            year     = unique(year),
            level    = "city",
            msa      = unique(msa),
            city     = unique(city),
            lc_class = 0L,
            area_m2  = sum(area_m2)
          )
        
        city_results_list[[length(city_results_list) + 1]] <-
          dplyr::bind_rows(df_city_classes, df_city_total)
      }
    }
    
    city_results <- if (length(city_results_list) > 0) {
      bind_rows(city_results_list)
    } else {
      tibble()
    }
    
    # combine MSA + city for this MSA-year
    msa_city_results <- bind_rows(msa_results, city_results)
    
    if (nrow(msa_city_results) > 0) {
      all_results[[length(all_results) + 1]] <- msa_city_results
    }
    
    rm(r, r_msa); gc()
  }
}

# ----------------- final area + change data frames -------------------

developed_area_df <- bind_rows(all_results) |>
  mutate(
    area_km2 = area_m2 / 1e6,
  ) %>%
  mutate(
    are_mi2 = area_km2 * 0.386102
      ) %>%
  arrange(level, msa, city, lc_class, year) %>%
  group_by(level, msa, city, lc_class)

### save -------------------------------------------------------------------
saveRDS(
  developed_area_df,
  here::here("data", "processed", "developed_area_by_geo_and_year.rds")
)
