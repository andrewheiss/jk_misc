library(dplyr)
library(ggplot2)
library(rgdal)

# Get Natural Earth shapefiles
download.file(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip", "ne_110m_admin_0_countries.zip", "auto")
unzip("ne_110m_admin_0_countries.zip")
file.remove("ne_110m_admin_0_countries.zip")

# Load and fortify regular data
world <- readOGR(".", "ne_110m_admin_0_countries")
continents.regular <- fortify(world, region="continent")

# Convert to Robinson and fortify
world.robinson <- spTransform(world, CRS("+proj=robin"))
continents.robinson <- fortify(world.robinson, region="continent")

continents <- readOGR("map_data", "ne_110m_land")
continents.rob <- spTransform(continents, CRS("+proj=robin"))
continents.new <- fortify(continents.rob)

all.continents <- data_frame(id = unique(as.character(continents.new$id)))
map.regular <- ggplot() +
  geom_map(data=all.continents, aes(map_id=id),
           map=continents.new, colour="grey50", fill="white", size=0.4) + 
  geom_map(data=countries.covered, aes(map_id=id),
           map=world.ggmap, colour="grey50", fill="grey50", size=0.2) + 
  expand_limits(x=world.ggmap$long, y=world.ggmap$lat) + 
  coord_equal()
map.regular
#


# Continents to plot
all.continents <- data_frame(id = unique(as.character(continents.regular$id)))

# This works
map.regular <- ggplot(data=all.continents, aes(map_id=id)) +
  geom_map(map=continents.regular, colour="grey50", fill="white", size=0.4) + 
  expand_limits(x=continents.regular$long, y=continents.regular$lat) + 
  coord_equal()
map.regular

# This has weird artifacts
map.robinson <- ggplot(data=all.continents, aes(map_id=id)) +
  geom_map(map=continents.robinson, colour="grey50", fill="white", size=0.4) + 
  expand_limits(x=continents.robinson$long, y=continents.robinson$lat) + 
  coord_equal()
map.robinson





library(dplyr)
library(ggplot2)
library(rgdal)

# Get Natural Earth shapefiles
download.file(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip", "ne_110m_land.zip", "auto")
unzip("ne_110m_land.zip")
file.remove("ne_110m_land.zip")

# Load and fortify regular data
world.land <- readOGR(".", "ne_110m_land")
land.robinson <- spTransform(world.land, CRS("+proj=robin"))
continents.simple <- fortify(land.robinson)

# Continents to plot
all.continents <- data_frame(id = unique(as.character(continents.simple$id)))

# This works
map.fixed <- ggplot(data=all.continents, aes(map_id=id)) +
  geom_map(map=continents.simple, colour="grey50", fill="white", size=0.4) + 
  expand_limits(x=continents.simple$long, y=continents.simple$lat) + 
  coord_equal()
map.fixed
