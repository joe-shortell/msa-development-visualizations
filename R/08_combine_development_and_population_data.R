library(dplyr)
library(here)

### population and developed land, by MSA and year ----------------------------------
# The national map's table: each MSA's population (step 05) next to its
# developed land (step 03) for every year 1999-2024.
#
# Both inputs are on the same, current MSA boundaries for every year: the
# population sums the counties in the OMB 2023 delineation, and the land is
# measured inside the MSA outline from step 01. Joined by MSA name and year.
# The population repeats on each of an MSA-year's land class rows.
#
# Output: data/processed/combined_population_development_data_lc_classes.rds,
# one row per MSA x year x land class (0 = all developed, 21-24 = NLCD classes):
#   MSA_GEOID, MSA, year, lc_class, msa_pop, msa_area_mi2
msa_population <- readRDS(here("data", "processed", "msa_population_1999_2024.rds"))
developed_area <- readRDS(here("data", "processed", "developed_area_by_geo_and_year.rds"))

pop_dev <- msa_population |>
  inner_join(developed_area, by = c("MSA" = "msa", "year"), relationship = "one-to-many") |>
  select(MSA_GEOID, MSA, year, lc_class, msa_pop, msa_area_mi2 = area_mi2) |>
  arrange(MSA, year, lc_class)

# Every MSA-year should have both a population and a land measurement.
unmatched <- anti_join(distinct(msa_population, MSA, year), pop_dev, by = c("MSA", "year"))
if (nrow(unmatched) > 0) {
  warning("No developed land for ", nrow(unmatched), " MSA-years, e.g. ",
          unmatched$MSA[1], " ", unmatched$year[1], ". Re-run step 03.")
}

saveRDS(pop_dev, here("data", "processed", "combined_population_development_data_lc_classes.rds"))
