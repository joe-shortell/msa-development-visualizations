# MSA Growth Map

An interactive map of population growth and land development in the 30 largest U.S. metropolitan statistical areas (MSAs). It compares where people were added or lost with where land was developed to hold them.

<!-- Link to the live app, plus one screenshot or GIF of the map in use. -->

## Motivation

Since the start of the new millenium, America's population has increased by over 60 million which, along with internal migrations, has fueled the rapid growth of some cities and spurred the decline in others.

Much of this growth has occured in suburbs and exurbs, rapidly expanding the footrpint of cities and reclaiming both agricultural and wildlands on their periphery. We are all familiar with the subdevelopments, strip malls, and logistics parks which ring american cities, re

This projects seeks to visualize how development patterns differed between the nation's biggest metro areas.

## Features

- The main visualization is a national-level map of the nation's 30 largest MSAs: each circle's size shows how much land was newly developed, and its colour how many new residents arrived per square mile of that new land. The user can choose the years and the type of developed land.
- By clicking on an MSA, the user can see a dot desnity map of population growth/decline within that metro for the period between the 2000 and 2020 Censuses. 
- The user can also see a time lapse of developed land within each MSA over the past two decades.

## Key findings

TO-DO

## Methodology

1. **Pick the metros.** The 30 largest MSAs by 2024 ACS population, using current boundaries for every year.
2. **Population.** Each MSA's population on July 1 of each year, 1999–2024, is the sum of its counties' Census population estimates, using the counties in the 2023 OMB delineation for every year. So all years share the same boundaries, and change between years is growth rather than redefinition. One county, Broomfield, CO (created in 2001, in the Denver MSA), has no 1999 estimate; the part of it the 1999 figures would miss is filled from 2000 census blocks.
3. **Developed land.** Annual NLCD rasters are cropped to each MSA boundary, and the 30 m pixels in the four developed classes are counted and converted to square miles.
4. **Land per new resident.** For the selected years, the change in developed area divided by the change in population. This is only shown for MSAs that gained people.
5. **Dot-density map.** 2000 and 2020 block populations are summed into 2010 block groups with the NHGIS block crosswalks, so each block group is compared on the same footprint. Each block group gets one dot per 500 people gained or lost, placed at random within the block group, with water clipped out.
6. **Time-lapse.** One frame per year of developed land in each MSA, 1999–2024.

## Data sources

### Population: U.S. Census Bureau

Census counts and the ACS are pulled through the Census API with [tidycensus](https://walker-data.com/tidycensus/). The county population estimates and the delineation file are downloaded from the Census Bureau's website.

| Dataset | Years | Used for |
|------------------------|------------------------|------------------------|
| Population Estimates Program, county population estimates (July 1): 1990–2000, 2000–2010 and 2010–2020 intercensal series; Vintage 2025 | 1999–2024 | Annual population of each MSA, summed from its counties. These drive the national map and its year slider. |
| OMB metropolitan statistical area delineation file (`list1_2023.xlsx`) | 2023 | Which counties make up each MSA, applied to every year |
| American Community Survey 1-Year Estimates, table B01003 (Total Population) | 2024 | Ranking the MSAs to choose the 30 largest |
| 2000 Census, Summary File 1, P001001 (Total Population) | 2000 | Block population at the start of the dot-density map (and the Broomfield, CO fill for 1999) |
| 2010 Census, Summary File 1, P001001 (Total Population) | 2010 | Block-group population on the 2010 block groups that 2000 and 2020 are mapped to |
| 2020 Census, Redistricting Data (P.L. 94-171), P1_001N (Total Population) | 2020 | Block population at the end of the dot-density map |

The 30 largest MSAs are chosen by their 2024 ACS population.

### Boundaries: U.S. Census Bureau TIGER/Line

Downloaded with [tigris](https://github.com/walkerke/tigris) and tidycensus.

- **MSA (CBSA) boundaries, 2024.** These are the MSA outlines on the map.
- **Census block groups, 2010.** Population dots are scattered within these.
- **Counties and area hydrography (water).** Water is clipped out of the block groups so dots don't land in lakes, rivers or bays.

### Block crosswalks: IPUMS NHGIS

[NHGIS geographic crosswalks](https://www.nhgis.org/geographic-crosswalks) re-weight 2000 and 2020 block populations into 2010 block groups (population weights), so change from 2000 to 2020 is measured on one consistent geography.

- `nhgis_blk2000_bg2010.csv`
- `nhgis_blk2020_bg2010.csv`

> Steven Manson, Jonathan Schroeder, David Van Riper, Katherine Knowles, Tracy Kugler, Finn Roberts, and Steven Ruggles. IPUMS National Historical Geographic Information System. Minneapolis, MN: IPUMS. <http://doi.org/10.18128/D050.V19.0> *(check the current version number on the NHGIS citation page)*

### Land cover: USGS Annual National Land Cover Database

[Annual NLCD](https://www.usgs.gov/centers/eros/science/annual-national-land-cover-database), Collection 1, version 1.0, land cover product for the contiguous U.S. (`Annual_NLCD_LndCov_<year>_CU_C1V1.tif`), 1999–2024. The project keeps the four developed classes:

| Class | Name                        | Impervious surface |
|-------|-----------------------------|--------------------|
| 21    | Developed, Open Space       | \< 20%             |
| 22    | Developed, Low Intensity    | 20–49%             |
| 23    | Developed, Medium Intensity | 50–79%             |
| 24    | Developed, High Intensity   | 80–100%            |

These give the developed area in each MSA for each year (and "total developed" as the sum of all four), plus the time-lapse GIFs.

### Basemap: CARTO

The MSA view's dot map uses [CARTO Dark Matter](https://carto.com/basemaps) tiles, built on © [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors. They require a CARTO API key. (The national map has no tiles: its land, labels and shaded relief are drawn by the app.)

## Limitations

<!-- Caveats a careful reader should know: the national map's populations are
     July 1 estimates (between censuses they are modelled, not counted) while the
     dots and comparison chart use April 1 census counts, so the two differ
     slightly; decennial dots cover 2000–2020 only; NLCD classification error;
     fixed current boundaries. -->

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
     by step 10. -->

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
