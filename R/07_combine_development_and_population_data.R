library(dplyr)
library(here)

### load data ------------------------------------------------------------------
developed_area_df <- readRDS(
  here::here("data", "processed", "developed_area_by_geo_and_year.rds")
)

msa_city_long <- readRDS(
  here::here("data", "processed", "acs_data", "msa_city_acs1_2005_2019_2021_2024.rds")
)

### clean and merge data -------------------------------------------------------

# restict developed area to just "total development"
developed_area_df_select <- developed_area_df

# split into separate msa and city datasets
developed_msa <- developed_area_df_select %>%
  ungroup() %>%
  filter(level=="msa") %>%
  rename("msa_area_mi2"="are_mi2") %>%
  select(year, msa, lc_class, msa_area_mi2)
developed_city <- developed_area_df_select %>%
  ungroup() %>%
  filter(level=="city") %>%
  rename("city_area_mi2"="are_mi2") %>%
  select(year, city, lc_class, city_area_mi2)

# merge msa: many-to-many by design. Each city-year picks up its MSA's row for
# every NLCD class, and each MSA-year-class row is shared by the MSA's cities.
combined_data <- left_join(msa_city_long,
                           developed_msa,
                           by = c("MSA"="msa", "year"),
                           relationship = "many-to-many"
                           )

# merge city
combined_data <- left_join(combined_data,
                           developed_city,
                           by = c("CITY"="city", "year", "lc_class")
)

### save combined data ---------------------------------------------------------
saveRDS(
  combined_data,
  here::here("data", "processed", "combined_population_development_data_lc_classes.rds")
)