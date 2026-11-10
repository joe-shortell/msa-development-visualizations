# MSA Growth Map

An interactive map of population growth and land development in the 30 largest U.S. metropolitan statistical areas (MSAs). It compares where people were added or lost with where land was developed to hold them.

<!-- Link to the live app, plus one screenshot or GIF of the map in use. -->

## Motivation

Since the start of the new millenium, America's population has increased by over 60 million which, along with internal migrations, has fueled the rapid growth of some cities and spurred the decline in others.

Much of this growth has occured in suburbs and exurbs, rapidly expanding the footrpint of cities and reclaiming both agricultural and wildlands on their periphery. We are all familiar with the subdevelopments, strip malls, and logistics parks which ring american cities, re

This projects seeks to visualize how development patterns differed between the nation's biggest metro areas.

## Features

- The main visualization is a national-level map showing new land developed per new resident for the nation's 30 largest MSAs. The user can also toggle this map to see the absolute change in both population and developed land area.
- By clicking on an MSA, the user can see a dot desnity map of population growth/decline within that metro for the period between the 2000 and 202 Censuses. 
- The user can also see a time lapse of developed land within each MSA over the past two decades.

## Key findings

TO-DO

## Methodology

1. **Pick the metros.** The 30 largest MSAs by 2024 ACS population, using 2024 boundaries for every year. Each MSA's principal cities are the places named in its title (e.g. Dallas, Fort Worth and Arlington).
2. **Population.** Annual ACS 1-year totals for each MSA and principal city.
3. **Developed land.** Annual NLCD rasters are cropped to each MSA and city boundary, and the 30 m pixels in the four developed classes are counted and converted to square miles.
4. **Land per new resident.** For the selected years, the change in developed area divided by the change in population. This is only shown for MSAs that gained people.
5. **Dot-density map.** 2000 and 2020 tract populations are moved onto 2010 tract boundaries with the NHGIS crosswalks, so each tract is compared on the same footprint. Each tract gets one dot per 500 people gained or lost, placed at random within the tract, with water clipped out.
6. **Time-lapse.** One frame per year of developed land in each MSA, 1999–2024.

## Data sources

### Population: U.S. Census Bureau

All Census data is pulled through the Census API with [tidycensus](https://walker-data.com/tidycensus/).

| Dataset | Years | Used for |
|------------------------|------------------------|------------------------|
| American Community Survey 1-Year Estimates, table B01003 (Total Population) | 2005–2019, 2021–2024 | Annual population of each MSA (CBSA) and its principal cities (places). These drive the year slider and the change tables. 2020 is skipped because the Census Bureau did not release standard 1-year ACS estimates that year. |
| 2000 Census, Summary File 1, P001001 (Total Population) | 2000 | Tract population at the start of the dot-density map |
| 2010 Census, Summary File 1, P001001 (Total Population) | 2010 | Tract population on the 2010 tract boundaries that all years are mapped to |
| 2020 Census, Redistricting Data (P.L. 94-171), P1_001N (Total Population) | 2020 | Tract population at the end of the dot-density map |

The 30 largest MSAs are chosen by their 2024 ACS population.

### Boundaries: U.S. Census Bureau TIGER/Line

Downloaded with [tigris](https://github.com/walkerke/tigris) and tidycensus.

- **MSA (CBSA) and place boundaries, 2024.** These are the MSA outlines and city outlines on the map.
- **Census tracts, 2010.** Population dots are scattered within these.
- **Counties and area hydrography (water).** Water is clipped out of the tracts so dots don't land in lakes, rivers or bays.

### Tract crosswalks: IPUMS NHGIS

[NHGIS geographic crosswalks](https://www.nhgis.org/geographic-crosswalks) re-weight 2000 and 2020 tract populations onto 2010 tract boundaries (population weights), so change from 2000 to 2020 is measured on one consistent geography.

- `nhgis_tr2000_tr2010.csv`
- `nhgis_tr2020_tr2010.csv`

> Steven Manson, Jonathan Schroeder, David Van Riper, Katherine Knowles, Tracy Kugler, Finn Roberts, and Steven Ruggles. IPUMS National Historical Geographic Information System. Minneapolis, MN: IPUMS. <http://doi.org/10.18128/D050.V19.0> *(check the current version number on the NHGIS citation page)*

### Land cover: USGS Annual National Land Cover Database

[Annual NLCD](https://www.usgs.gov/centers/eros/science/annual-national-land-cover-database), Collection 1, version 1.0, land cover product for the contiguous U.S. (`Annual_NLCD_LndCov_<year>_CU_C1V1.tif`), 1999–2024. The project keeps the four developed classes:

| Class | Name                        | Impervious surface |
|-------|-----------------------------|--------------------|
| 21    | Developed, Open Space       | \< 20%             |
| 22    | Developed, Low Intensity    | 20–49%             |
| 23    | Developed, Medium Intensity | 50–79%             |
| 24    | Developed, High Intensity   | 80–100%            |

These give the developed area in each MSA and city for each year (and "total developed" as the sum of all four), plus the time-lapse GIFs.

### Basemap: CARTO

The map's background tiles are [CARTO Positron](https://carto.com/basemaps), built on © [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors. They require a CARTO API key.

## Limitations

<!-- Caveats a careful reader should know: ACS 1-year sampling error and the
     65,000-population threshold for places, no 2020 ACS 1-year, decennial dots
     cover 2000–2020 only, NLCD classification error, fixed 2024 boundaries. -->

## Project structure

<!-- Short annotated tree of the top-level folders (R/, app/, data/, output/) and
     what each pipeline step produces. Keep it brief; link to scripts for detail. -->

## Getting started

<!-- Steps for someone cloning the repo. -->

### Prerequisites

<!-- R version and the main packages; system libraries needed by sf/terra
     (GDAL, GEOS, PROJ). -->

### API keys

<!-- Which keys are needed (Census, CARTO), where to get them, and where each one
     goes (~/.Renviron vs app/.Renviron). Make clear that keys are never committed. -->

### Running the pipeline

<!-- Where to download the raw data (NLCD, NHGIS crosswalks) and where to put it,
     the command to run all steps, and rough runtimes. -->

### Running the app

<!-- The one command to launch the app locally, and that it needs app/data built
     by step 09. -->

## Testing

<!-- What the tests cover and the command to run them. -->

## Deployment

<!-- How the app is published to shinyapps.io and what gets uploaded. -->

## Built with

<!-- Main tools and packages (R, Shiny, Leaflet, sf, terra, tidycensus, tigris),
     with links. -->

## Acknowledgments

<!-- Credit for data providers (citation formats required by NHGIS etc.), and
     anyone or anything that informed the project. -->

## License

<!-- The license for the code (e.g. MIT), plus a note that the data keeps its
     providers' terms. Add a matching LICENSE file. -->

## Author

<!-- Your name, a one-line bio, and links (portfolio, LinkedIn, email). -->
