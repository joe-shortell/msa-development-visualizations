library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(sf)
library(here)

source(here("R", "utils.R"))   # lc_years

### MSA population by year, on today's MSA boundaries -----------------------------
# The national map's population: one figure per MSA per land cover year
# (lc_years, 1999-2024), all measured on the same, current MSA boundaries, so a
# change between two years is growth and never a boundary redefinition.
#
# Method: MSAs are made of whole counties, so an MSA's population in any year
# is the sum of its counties' populations that year.
#   1. Which counties: the OMB 2023 delineation (list1_2023.xlsx), the current
#      county-to-MSA list, applied to every year.
#   2. County populations: the Census Bureau's Population Estimates Program
#      county totals for July 1 of each year, taking each year from the best
#      series available for it:
#        1999        1990-2000 intercensal   stch-icen1999.txt
#        2000-2009   2000-2010 intercensal   co-est00int-tot.csv
#        2010-2019   2010-2020 intercensal   cc-est2020int-alldata.csv
#        2020-2024   Vintage 2025            co-est2025-alldata.csv
#      Intercensal series are revised to agree with the census at both ends of
#      their decade; the post-2020 years are the latest vintage's estimates.
#   3. Fill the one gap (Broomfield County, CO in 1999; see below).
#   4. Check that every MSA county has a figure in every year.
#   5. Sum the county figures by MSA and year.
#
# These are July 1 estimates, so they run slightly above the April 1 census
# counts behind the dot map and the MSA view's chart (by ~0.3-0.7% in 2000 and
# 2020). The two are kept separate on purpose: the chart pairs with the dots.
#
# Inputs download on first run into data/raw/census_popest/ and are reused
# after that (delete a file to re-download it). The Broomfield fill reads step
# 04's cache of 2000 census blocks, which is why this step runs after step 04.
#
# Output: data/processed/msa_population_1999_2024.rds, one row per MSA-year:
#   MSA_GEOID, MSA, year, msa_pop, source (which series the year came from)

### inputs ------------------------------------------------------------------------
# Source files and where the Census Bureau publishes them.
popest_dir <- here("data", "raw", "census_popest")
dir.create(popest_dir, showWarnings = FALSE, recursive = TRUE)

census_files <- c(
  delineation = "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2023/delineation-files/list1_2023.xlsx",
  icen_1999   = "https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen1999.txt",
  icen_2000s  = "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-tot.csv",
  icen_2010s  = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/intercensal/county/asrh/cc-est2020int-alldata.csv",
  vintage     = "https://www2.census.gov/programs-surveys/popest/datasets/2020-2025/counties/totals/co-est2025-alldata.csv"
)

# Download any input not already on disk; return local paths named like census_files.
# The 2010s file is ~180 MB, so the timeout is raised from R's 60 s default.
fetch <- function(urls, dir) {
  paths <- file.path(dir, basename(urls))
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old))
  for (i in which(!file.exists(paths))) {
    message("Downloading ", basename(urls[i]))
    download.file(urls[i], paths[i], mode = "wb", quiet = TRUE)
  }
  setNames(paths, names(urls))
}
src <- fetch(census_files, popest_dir)

# The 30 MSAs, from step 01.
msa_geom <- read_rds(here("data", "processed", "geographies", "msa_geom.rds"))
msas <- st_drop_geometry(msa_geom) |> select(MSA_GEOID, MSA)

# Build a 5-digit county FIPS as text, so leading zeros survive.
county_fips <- function(state, county) {
  paste0(sprintf("%02d", as.integer(state)), sprintf("%03d", as.integer(county)))
}

### 1. which counties make up each MSA ----------------------------------------------
# The delineation lists one row per county in a CBSA (metro or micro area).
# Keep metropolitan areas only, and only our 30.
msa_counties <- read_excel(src[["delineation"]], skip = 2, col_types = "text") |>
  filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area",
         `CBSA Code` %in% msas$MSA_GEOID) |>
  transmute(MSA_GEOID = `CBSA Code`,
            fips      = county_fips(`FIPS State Code`, `FIPS County Code`),
            county    = paste0(`County/County Equivalent`, ", ", `State Name`))

missing_msas <- setdiff(msas$MSA_GEOID, msa_counties$MSA_GEOID)
if (length(missing_msas) > 0) {
  stop("Not in the 2023 delineation: ", paste(missing_msas, collapse = ", "))
}

### 2. county population, July 1 of each year ----------------------------------------
# Each series is reduced to the same long table: fips, year, pop, source.

# 1999: the 1990s intercensal file has no header and splits each county by age
# group, race-sex and Hispanic origin, so its rows are summed to county.
# Columns: 2-digit year, county FIPS, age group, race-sex, Hispanic origin, population.
pop_1999 <- read_table(src[["icen_1999"]],
                       col_names = c("yr", "fips", "age", "race_sex", "hisp", "pop"),
                       col_types = "ccciid") |>
  group_by(fips) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  mutate(year = 1999L, source = "1990-2000 intercensal")

# 2000-2009: one row per county (SUMLEV 50; 40 is a state total), with a
# POPESTIMATE<year> column per year. SUMLEV is compared as a number because
# some files write it as text ("050").
pop_2000s <- read_csv(src[["icen_2000s"]], locale = locale(encoding = "latin1"),
                      show_col_types = FALSE) |>
  filter(as.integer(SUMLEV) == 50) |>
  transmute(fips = county_fips(STATE, COUNTY), across(num_range("POPESTIMATE", 2000:2009))) |>
  pivot_longer(-fips, names_to = "year", names_prefix = "POPESTIMATE",
               names_transform = as.integer, values_to = "pop") |>
  mutate(source = "2000-2010 intercensal")

# 2010-2019: one row per county x age group x YEAR code. AGEGRP 0 is all ages.
# YEAR codes are dates, not years: 1 = April 1 2010 (census base), 2-11 =
# July 1 2010-2019, 12 = April 1 2020 (census). So July 1 year = code + 2008.
pop_2010s <- read_csv(src[["icen_2010s"]], locale = locale(encoding = "latin1"),
                      col_select = c(STATE, COUNTY, YEAR, AGEGRP, TOT_POP),
                      show_col_types = FALSE) |>
  filter(AGEGRP == 0, YEAR %in% 2:11) |>
  transmute(fips = county_fips(STATE, COUNTY), year = as.integer(YEAR + 2008),
            pop = TOT_POP, source = "2010-2020 intercensal")

# 2020-2024: the latest vintage, laid out like the 2000s file.
pop_2020s <- read_csv(src[["vintage"]], locale = locale(encoding = "latin1"),
                      show_col_types = FALSE) |>
  filter(as.integer(SUMLEV) == 50) |>
  transmute(fips = county_fips(STATE, COUNTY), across(num_range("POPESTIMATE", 2020:2024))) |>
  pivot_longer(-fips, names_to = "year", names_prefix = "POPESTIMATE",
               names_transform = as.integer, values_to = "pop") |>
  mutate(source = "Vintage 2025 estimates")

county_pop <- bind_rows(pop_1999, pop_2000s, pop_2010s, pop_2020s) |>
  filter(year %in% lc_years)
stopifnot(!anyDuplicated(county_pop[c("fips", "year")]))

### 3. the one gap: Broomfield County, CO in 1999 ------------------------------------
# Fills the 1999 population of the parts of Broomfield that the 1999 total
# would otherwise miss.
#
# Broomfield (08014) was created in 2001 from parts of Adams, Boulder, Jefferson
# and Weld counties, so the 1990s file has no row for it. Its Adams and
# Jefferson parts are counted anyway (both counties are wholly in Denver's MSA,
# and in 1999 they still contained those parts). Its Boulder and Weld parts
# are not: those counties are in other MSAs.
#
# The 2000 census was taken before Broomfield existed, so its blocks are still
# coded to the old counties. Summing the Boulder- and Weld-coded blocks whose
# interior point lies inside today's Broomfield gives those parts' population
# on April 1, 2000 (21,642). Each part is then scaled back to July 1, 1999 by
# its parent county's July-1999 estimate / April-2000 census ratio (the same
# extent at both dates), giving 21,239. Splitting edge blocks by area instead
# of by interior point changes this by about 20 people.
broomfield_parents <- c("08013", "08123")   # Boulder, Weld

blocks_2000_co <- here("data", "raw", "census_blocks", "2000_08.rds")
if (!file.exists(blocks_2000_co)) {
  stop("Step 04's cache of Colorado's 2000 blocks (", blocks_2000_co, ") is missing; run step 04 first.")
}
pop_blocks <- read_rds(blocks_2000_co) |>
  mutate(fips = substr(GEOID, 1, 5)) |>
  filter(fips %in% broomfield_parents)

block_shapes <- tigris::blocks("08", county = substr(broomfield_parents, 3, 5),
                               year = 2000, progress_bar = FALSE) |>
  select(GEOID = BLKIDFP00)
broomfield <- tigris::counties("08", year = 2023, progress_bar = FALSE) |>
  filter(COUNTYFP == "014")

# Interior points found in a planar CRS (UTM 13N) so they are exact.
st_agr(block_shapes) <- "constant"
in_broomfield <- block_shapes |>
  st_transform(26913) |>
  st_point_on_surface() |>
  st_filter(st_transform(broomfield, 26913), .predicate = st_within) |>
  pull(GEOID)

broomfield_1999 <- pop_blocks |>
  group_by(fips) |>
  summarise(census_2000 = sum(pop),
            part_2000   = sum(pop[GEOID %in% in_broomfield]),
            .groups = "drop") |>
  left_join(select(pop_1999, fips, july_1999 = pop), by = "fips") |>
  mutate(part_1999 = part_2000 * july_1999 / census_2000)

message("Broomfield 1999 fill: ", round(sum(broomfield_1999$part_1999)), " people (",
        sum(broomfield_1999$part_2000), " on April 1, 2000)")

county_pop <- bind_rows(
  county_pop,
  tibble(fips = "08014", year = 1999L, pop = round(sum(broomfield_1999$part_1999)),
         source = "Broomfield 1999 fill (2000 census blocks)")
)

### 4. check every county has a figure every year -----------------------------------
# Every MSA county must have a population in every year: a gap would silently
# understate that MSA. A new gap means a county changed (renamed, merged or
# split) between a source file and the delineation.
county_years <- msa_counties |>
  cross_join(tibble(year = lc_years)) |>
  left_join(county_pop, by = c("fips", "year"))

gaps <- filter(county_years, is.na(pop))
if (nrow(gaps) > 0) {
  stop("Counties with no population:\n",
       paste0("  ", gaps$county, " (", gaps$fips, "), ", gaps$year, collapse = "\n"))
}

### 5. sum by MSA and year ------------------------------------------------------------
# A year's source is the same for all its counties, except the
# Broomfield fill, so the MSA row takes its counties' sources joined together.
msa_population <- county_years |>
  group_by(MSA_GEOID, year) |>
  summarise(msa_pop = sum(pop),
            source  = paste(sort(unique(source)), collapse = "; "),
            .groups = "drop") |>
  inner_join(msas, by = "MSA_GEOID") |>
  select(MSA_GEOID, MSA, year, msa_pop, source) |>
  arrange(MSA, year)

stopifnot(nrow(msa_population) == nrow(msas) * length(lc_years))

# A jump between adjacent years far above normal growth would point to a seam
# between two source files (or a parsing error) rather than real growth.
jumps <- msa_population |>
  group_by(MSA) |>
  mutate(change = msa_pop / lag(msa_pop) - 1) |>
  filter(abs(change) > 0.08)
if (nrow(jumps) > 0) {
  warning("Year-over-year change above 8%:\n",
          paste0("  ", jumps$MSA, " ", jumps$year, ": ", round(100 * jumps$change, 1), "%",
                 collapse = "\n"))
}

### save ---------------------------------------------------------------------------
saveRDS(msa_population, here("data", "processed", "msa_population_1999_2024.rds"))
