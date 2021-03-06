# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(haven)
library(countrycode)
library(maptools)
library(rgdal)
library(readr)

source("shared_functions.R")


# ------------------
# Useful functions
# ------------------
# Carry the most recent post-2011 value forward to 2014 if 2014 is missing
impute.crim <- function(year, adjcrimlevel) {
  df <- data_frame(year, adjcrimlevel)
  
  crim.2014.actual <- filter(df, year == 2014)$adjcrimlevel
  
  # If 2014 is missing, check for a value each year back to 2011 and use that
  if (is.na(crim.2014.actual)) {
    for (y in 2013:2011) {
      crim.level <- filter(df, year == y)$adjcrimlevel
      
      if (!(is.na(crim.level))) {
        crim.2014.new <- crim.level
        break
      } else {
        # Needs to be wrapped in as.integer() if dealing with integers
        crim.2014.new <- as.integer(NA)
      }
    }
    
    return(crim.2014.new)
  } else {
    return(crim.2014.actual)
  }
}

# If a country has criminalization laws in 2014 but has missing data for 2001, 
# we can assume that they did not have laws then, so retroactively assign a 0.
# Function expects a two-value vector, after 
#   filter(year %in% c(2001, 2014)) %>% group_by(id))
# in a dplyr chain
retroactively.criminalize <- function(crim) {
  if (is.na(crim[1] && !(is.na(crim[2])))) {
    return(c(0, crim[2]))
  } else {
    return(as.numeric(crim))
  }
}


# -----------
# Load data
# -----------
crim <- read_stata("original_files/Criminalization Data UpdatedJK.dta") %>%
  select(country, year, ccode, adjcrimlevel) %>%
  mutate(id = countrycode(ccode, "cown", "iso3c")) %>%
  arrange(id, year)

crim.all <- crim %>% 
  expand(id, year) %>%  # Get all combinations of countries and years
  left_join(crim, by=c("id", "year")) %>%
  group_by(id) %>%
  # Impute/carry forward the criminalization level for the last few years
  mutate(crim.imputed = impute.crim(year, adjcrimlevel)) %>%
  filter(year %in% c(2001, 2014)) %>%
  # Use the imputed level if needed
  mutate(crim.imputed = ifelse(is.na(adjcrimlevel) & year == 2014, 
                               crim.imputed, adjcrimlevel),
         crim.imputed = retroactively.criminalize(crim.imputed)) %>%
  select(id, year, crim = adjcrimlevel, crim.imputed)

# Manually add data for large missing countries
updated.countries <- data_frame(id = rep(c("SOM", "SSD", "SRB", "PRK"), each=2),
                                year = rep(c(2001, 2014), times=4),
                                crim = NA,
                                crim.imputed = c(0, 0, 0, 0, 0, 2, 0, 0))

crim.all <- bind_rows(crim.all, updated.countries) %>%
  mutate(crim.imputed = ifelse(id == "SOM", 0, crim.imputed))

# Load map information
countries.map <- readOGR("map_data", "ne_110m_admin_0_countries")
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99))) %>%  # Get rid of Antarctica and NAs
  mutate(id = ifelse(id == "GRL", "DNK", id))  # Greenland is part of Denmark

# All possible countries (to fix the South Sudan issue)
possible.countries <- expand.grid(id = unique(as.character(countries.ggmap$id)),
                                  year = c(2001, 2014), stringsAsFactors=FALSE)

all.countries <- possible.countries %>% 
  left_join(crim.all, by=c("id", "year")) %>%
  mutate(crim.level.num = ifelse(is.na(crim.imputed), 0, crim.imputed),
         crim.level = factor(crim.level.num,
                             levels=c(2, 1, 0),
                             labels=c("Full criminalization   ",
                                      "Partial criminalization    ",
                                      "No criminalization"),
                             ordered=TRUE))

# Which countries are missing?
# all.countries %>%
#   filter(crim.level.num == -1) %>%
#   distinct(id, year) %>%
#   mutate(countryname = countrycode(id, "iso3c", "country.name"))

# For the map, we only care about fixing the countries that are visible
# (sorry tiny islands), so Somalia, South Sudan, Serbia, and North Korea
#
# SOM = No in 2001, No in 2014
# SSD = No (well, non-existent) in 2001, No in 2014
# SRB = No in 2001, Yes in 2014 (criminalized in 2003)
# PRK = No in 2001, No in 2014
#
# These are added manually above as `updated.countries`


# -----------
# Plot data
# -----------
crim.map <- ggplot(all.countries, aes(fill=crim.level, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show.legend=FALSE) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  facet_wrap(~ year, ncol=1) + 
  scale_fill_manual(values=c("grey30", "grey60", "white"), name="",
                    guide=guide_legend(override.aes=list(colour="black", size=0.1))) +
  theme_blank_map(base_size=10) +
  theme(legend.position="top", legend.key.size=unit(0.65, "lines"),
        legend.key = element_blank(),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))
crim.map
# ggsave(crim.map, filename="figures/map_criminalization.pdf", device=cairo_pdf)
# ggsave(crim.map, filename="figures/map_criminalization.png")
# ,
# guide = guide_legend(override.aes=list(size = 0.1))

# ------------------------------------
# Export underlying plot data to CSV
# ------------------------------------
to.csv <- all.countries %>%
  mutate(ccode = countrycode(id, "iso3c", "cown")) %>%
  select(iso3 = id, year, ccode, crim, crim_level = crim.level)

write_csv(to.csv, path="data/map_criminalization.csv")
