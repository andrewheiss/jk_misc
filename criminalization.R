# ----------------
# Load libraries
# ----------------
library(dplyr)
library(haven)
library(countrycode)
library(ggplot2)
library(grid)
library(Cairo)
library(rgdal)


# ------------------
# Useful functions
# ------------------
theme_blank_map <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border=element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          axis.title=element_blank(), axis.text=element_blank(),
          legend.text=element_text(size=rel(0.7), family="Source Sans Pro Light"),
          legend.title=element_text(size=rel(0.9), family="Source Sans Pro Semibold"))
  ret
}


# -----------
# Load data
# -----------
crim <- read_stata("original_files/kelley_simmons_ajps_2014_replication.dta") %>%
  select(name, year, cowcode, adjbicrimlevel) %>%
  mutate(id = countrycode(cowcode, "cown", "iso3c"),
         crim.level = factor(adjbicrimlevel,
                             levels=c(0, 2),
                             labels=c("No criminalization", 
                                      "Full criminalization"),
                             ordered=TRUE)) %>%
  filter(year %in% c(2001, 2011))

# Load map information
countries.map <- readOGR("map_data", "ne_110m_admin_0_countries")
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99)))  # Get rid of Antarctica and NAs

# All possible countries (to fix the South Sudan issue)
possible.countries <- expand.grid(id = unique(as.character(countries.ggmap$id)),
                                  year = c(2001, 2011), stringsAsFactors=FALSE)

all.countries <- possible.countries %>% 
  left_join(crim, by=c("id", "year"))


# -----------
# Plot data
# -----------
# TODO: BW colors
# TODO: Better facet titles
# TODO: Nicer legend
ggplot(all.countries, aes(fill=crim.level)) +
  geom_map(aes(map_id=id), map=countries.ggmap, size=0) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  facet_wrap(~ year, ncol=1) + 
  scale_fill_manual(values=c("#FF4136", "#2ECC40"), na.value="grey90") +
  theme_blank_map()
