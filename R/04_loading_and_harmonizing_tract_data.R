library(tidycensus)
library(dplyr)
library(purrr)
library(readr)
library(here)

### load tract crosswalks -------------------------
crosswalks_2000_2010 <- read_csv(here("data","raw","crosswalks","nhgis_tr2000_tr2010.csv"))
crosswalks_2020_2010 <- read_csv(here("data","raw","crosswalks","nhgis_tr2020_tr2010.csv"))

#--------------------------------------------------
# 1. Census API key
#--------------------------------------------------
# CENSUS_API_KEY in ~/.Renviron (see utils.R)
source(here("R", "utils.R"))
census_api_key(census_key(), install = FALSE)

# Vector of state abbreviations (incl. DC etc.)
states <- read_rds(here("data", "processed", "geographies", "state_abbr.rds"))
# or:
# states <- unique(fips_codes$state)[fips_codes$state != ""]

#--------------------------------------------------
# 2. Decennial years and total population vars
#--------------------------------------------------
decennial_vars <- c(
  "2000" = "P001001",  # SF1 total population
  "2010" = "P001001",  # SF1 total population
  "2020" = "P1_001N"   # 2020 PL total population
)
decennial_years <- as.numeric(names(decennial_vars))

#--------------------------------------------------
# 3. Safe wrapper (optional but helpful)
#--------------------------------------------------
safe_get_decennial <- function(...) {
  tryCatch(
    {
      Sys.sleep(0.2)   # be nice to the API
      get_decennial(...)
    },
    error = function(e) {
      message("   get_decennial() failed: ", conditionMessage(e))
      tibble(
        GEOID    = character(),
        value    = numeric(),
        variable = character(),
        NAME     = character()
      )
    }
  )
}

#--------------------------------------------------
# 4. Pull tract-level population for the country
#--------------------------------------------------
decennial_tracts_all <- map_dfr(
  decennial_years,
  function(year) {
    census_var <- decennial_vars[as.character(year)]
    message("===== Year ", year, " (var: ", census_var, ") =====")
    
    map_dfr(
      states,
      function(st_abbr) {
        message("  State: ", st_abbr)
        
        safe_get_decennial(
          geography = "tract",
          year      = year,
          state     = st_abbr,
          variables = census_var,
          geometry  = FALSE
        )
      }
    ) %>%
      rename(estimate = value) %>%
      mutate(
        year     = year,
        variable = "Total_Population"
      )
  }
)

#--------------------------------------------------
# 4. Separate into individual years
#--------------------------------------------------

tract_2000 <- decennial_tracts_all %>%
  filter(year == 2000) %>%
  select(GEOID00 = GEOID, pop_2000 = estimate)

tract_2010 <- decennial_tracts_all %>%
  filter(year == 2010) %>%
  select(GEOID10 = GEOID, pop_2010 = estimate, NAME_2010 = NAME)

tract_2020 <- decennial_tracts_all %>%
  filter(year == 2020) %>%
  select(GEOID20 = GEOID, pop_2020 = estimate)

#--------------------------------------------------
# 5. create crosswalk keys
#--------------------------------------------------

cw00_10 <- crosswalks_2000_2010 %>%
  transmute(
    GEOID00 = tr2000ge,   # source (2000)
    GEOID10 = tr2010ge,   # target (2010)
    wt_pop
  )

cw20_10 <- crosswalks_2020_2010 %>%
  transmute(
    GEOID20 = tr2020ge,   # source (2020)
    GEOID10 = tr2010ge,   # target (2010)
    wt_pop
  )

#--------------------------------------------------
# 6. harmonize
#--------------------------------------------------

harm_2000_to_2010 <- tract_2000 %>%
  inner_join(cw00_10, by = "GEOID00") %>%
  mutate(pop_2000_wt = pop_2000 * wt_pop) %>%
  group_by(GEOID10) %>%
  summarise(
    pop_2000 = sum(pop_2000_wt, na.rm = TRUE),
    .groups = "drop"
  )

harm_2020_to_2010 <- tract_2020 %>%
  inner_join(cw20_10, by = "GEOID20") %>%
  mutate(pop_2020_wt = pop_2020 * wt_pop) %>%
  group_by(GEOID10) %>%
  summarise(
    pop_2020 = sum(pop_2020_wt, na.rm = TRUE),
    .groups = "drop"
  )

#--------------------------------------------------
# 7. Combine with 2010 counts
#--------------------------------------------------

tract_2010_clean <- tract_2010 %>%
  # keep one NAME per GEOID10
  group_by(GEOID10) %>%
  slice(1) %>%
  ungroup()

tracts_2010_harmonized <- tract_2010_clean %>%
  select(GEOID10, NAME_2010, pop_2010)

#--------------------------------------------------
# 8. join together
#--------------------------------------------------

tracts_pop_2010geom_wide <- tracts_2010_harmonized %>%
  full_join(harm_2000_to_2010, by = "GEOID10") %>%
  full_join(harm_2020_to_2010, by = "GEOID10")

#--------------------------------------------------
# 9. create change variable
#--------------------------------------------------

tracts_pop_2010geom_wide <- tracts_pop_2010geom_wide %>%
  mutate(chg_2000_2010 = pop_2010 - pop_2000,
         chg_2010_2020 = pop_2020 - pop_2010,
         chg_2000_2020 = pop_2020 - pop_2000
         )

#--------------------------------------------------
# 10. save 
#--------------------------------------------------

saveRDS(
  tracts_pop_2010geom_wide,
  here::here("data", "processed", "tracts_pop_2010geom_wide.rds")
)