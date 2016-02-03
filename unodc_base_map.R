# Useful links:
#   https://rud.is/b/2015/07/09/faceted-world-population-by-income-choropleths-in-ggplot/
#   http://gis.stackexchange.com/q/44387
#   https://github.com/geocomPP/sdv/blob/master/S3.md
# And my StackExchange question: 
#   http://gis.stackexchange.com/q/157139/56265
#
# Shapefiles:
#   Admin 0 – Countries 2.0
#     http://www.naturalearthdata.com/downloads/110m-cultural-vectors/
#   Land 2.0
#     http://www.naturalearthdata.com/downloads/110m-physical-vectors/

# ----------------
# Load libraries
# ----------------
library(dplyr)
library(readr)
library(countrycode)

# Map stuff
# You must install geos (http://trac.osgeo.org/geos/) and 
# gdal (http://www.gdal.org/) first. 
# Easy to do on OS X: `brew install geos gdal`
# Then install these packages from source
# install.packages(c("rgeos", "rgdal"), type="source")
library(rgeos)
library(rgdal)

source("shared_functions.R")


# -----------
# Load data
# -----------
# Load map information
countries.map <- readOGR("map_data", "ne_110m_admin_0_countries")
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99))) %>%  # Get rid of Antarctica and NAs
  mutate(id = ifelse(id == "GRL", "DNK", id))  # Greenland is part of Denmark

continents.map <- readOGR("map_data", "ne_110m_land")
continents.robinson <- spTransform(continents.map, CRS("+proj=robin"))
continents.ggmap <- fortify(continents.robinson) %>%
  # Antarctica and its islands are ids 0-7 (the mainland is 7)
  # fortify() coerces id to character, so the filter command is a little wonky
  filter(!(id %in% as.character(0:7)))

# Determine which countries to plot
all.countries <- data_frame(id = unique(as.character(countries.ggmap$id))) 

countries.covered <- read_csv("data/unodc_covered.csv") %>%
  mutate(id = countrycode(country_name, "country.name", "iso3c"))


# -----------
# Plot data
# -----------
country.color <- "grey90"
base.map <- ggplot() +
  geom_polygon(data=continents.ggmap, aes(long, lat, group=group),
               colour=country.color, fill="white", size=0.3) + 
  geom_map(data=countries.covered, aes(map_id=id),
           map=countries.ggmap, colour=country.color, fill=country.color, size=0.2) + 
  coord_equal() +  # Use when manually converting to Robinson
  theme_blank_map()
  # No need for this...
  # expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  # Best if not manually converting to Robinson
  # coord_map(xlim=c(-180, 180), ylim=c(-60, 90), projection="gall", param=0) + 
base.map
ggsave(base.map, filename="figures/unodc_base_map.pdf")
