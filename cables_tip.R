# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(haven)
library(foreign)
library(countrycode)
library(rgdal)

source("shared_functions.R")


# ---------------------
# Load and clean data
# ---------------------
cables <- read_csv("original_files/Cables Trafficking.csv")
embassies <- readRDS("data/embassies_final.rds") %>% select(-geocoded)

# Combine embassy details with cables
cables.geocoded <- cables %>% 
  rename(cables.month = `Actual number of cables that month`,
         trafficking.cables.year = `Trafficking cables this year`) %>%
  left_join(embassies, by="Embassy") %>%
  filter(!is.na(country))

# Calculate the number of cables published each year for all countries
cables.per.year <- cables.geocoded %>%
  group_by(Year) %>%
  summarise(total.all.countries = sum(cables.month))

# Calculate cable-related variables
# Creates multiple variables:
#   * cables: the total number of cables that year
#   * tip.cables: the total number of TIP-related cables that year
#   * prop.tip: tip.cables / cables
#   * prop.cables.that.year: number of cables that year / total number of cables
#      for all countries (to get a sense of how much coverage a country-year got 
#      in the Wikileak dump)
cables.tip <- cables.geocoded %>%
  group_by(country, Year) %>%
  summarise(cables = sum(cables.month),
            tip.cables = sum(trafficking.cables.year, na.rm=TRUE)) %>%
  mutate(prop.tip = tip.cables / cables) %>%
  left_join(cables.per.year, by="Year") %>%
  mutate(prop.cables.that.year = cables / total.all.countries) %>%
  select(-total.all.countries)

# Expand to full country-year panel format
cables.panel <- cables.tip %>% 
  expand(country, Year) %>%  # Magic dataframe expansion
  left_join(cables.tip, by=c("country", "Year")) %>%
  mutate(cow = countrycode(country, "country.name", "cown")) %>%
  select(country, cow, year = Year, cables, tip_cables = tip.cables, 
         prop_tip = prop.tip, prop_cables_that_year = prop.cables.that.year)


# ----------------
# Write to Stata
# ----------------
# Add fancy Stata labels
labs <- c("Country name", "COW code", "Year", 
          "Number of cables originating from country", 
          "Number of cables related to TIP", 
          "Proportion of cables related to TIP", 
          "Proportion of total number of cables that year")

cables.panel %<>% add_labels(labs)  # For haven and RStudio
attr(cables.panel, "var.labels") <- labs  # For foreign

# write_dta(cables.panel, "data/cables_panel.dta")
write.dta(cables.panel, "data/cables_panel.dta")


# ------------------------
# Load all data for maps
# ------------------------
# Load map information
countries.map <- readOGR("map_data", "ne_110m_admin_0_countries")
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99))) %>%  # Get rid of Antarctica and NAs
  mutate(id = ifelse(id == "GRL", "DNK", id))  # Greenland is part of Denmark

# All possible countries (to fix the South Sudan issue)
possible.countries <- data_frame(id = unique(as.character(countries.ggmap$id)))

# Calculate aggregate proportions of TIP cables for each country and cut into bins
tip.effort <- cables.tip %>%
  group_by(country) %>%
  summarize(avg.effort = mean(prop.tip),
            med.effort = median(prop.tip)) %>%
  mutate(id = countrycode(country, "country.name", "iso3c"),
         bins = cut(avg.effort, c(0, 0.00001, seq(0.05, .25, .05)), 
                    include.lowest=TRUE, ordered_result=TRUE))

# Extract the ranges from cut(), increase the lower bound by 1, and make a nice label
# Converts [0,0.05] to 0-5% and (0.05,0.1] to 6-10%
bins.df <- data_frame(bins = levels(tip.effort$bins),
                      lower = as.numeric(gsub("[\\(\\[](.+),.*", "\\1", bins)),
                      upper = as.numeric(gsub("[^,]*,([^]]*)\\]", "\\1", bins)),
                      lower.clean = ifelse(grepl("^\\(", bins), lower + .01, lower),
                      bin.clean = paste0(lower.clean * 100, "-", 
                                         upper * 100, "%    ")) %>%
  mutate(bin.clean = ifelse(lower >= 0.15, "> 15%", bin.clean),
         bin.clean = ifelse(lower == 0, "0%    ", bin.clean),
         bin.clean = ifelse(upper == 0.05, "0.01-5%    ", bin.clean),
         bin.clean = factor(bin.clean, levels=unique(bin.clean), ordered=TRUE))

# Merge all the data together
effort.full <- possible.countries %>%
  left_join(tip.effort, by="id") %>%
  left_join(bins.df, by="bins")

# Convert embassy coordinates to Robinson projection and add to df
embassies.robinson <- project(as.matrix(embassies %>% select(long, lat)), 
                              proj="+proj=robin")
embassies.to.plot <- bind_cols(embassies, as.data.frame(embassies.robinson)) %>%
  rename(long.robinson = V1, lat.robinson = V2) %>%
  filter(!(is.na(iso)))


# --------------------------
# Finally plot everything!
# --------------------------
# Map of proportions with a gradient fill
effort.map <- ggplot(effort.full, aes(fill=avg.effort, map_id=id)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  geom_point(data=embassies.to.plot, 
             aes(x=long.robinson, y=lat.robinson, fill=NULL, map_id=NULL), 
             colour="black", size=0.5, show_guide=FALSE) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(high="black", low="white", na.value="white",
                      guide="colorbar", labels=percent, name="") +
  theme_blank_map() + 
  theme(legend.position="bottom")
effort.map
ggsave(effort.map, filename="figures/map_avg_tip_effort.pdf", device=cairo_pdf)
ggsave(effort.map, filename="figures/map_avg_tip_effort.png")

# Map of proportions with bins
effort.map.binned <- ggplot(effort.full, aes(fill=bin.clean, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show_guide=FALSE) + 
  geom_point(data=embassies.to.plot, 
             aes(x=long.robinson, y=lat.robinson, fill=NULL, map_id=NULL), 
             colour="black", size=0.5, show_guide=FALSE) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_manual(values=c("white", "grey90", "grey60", "grey30", "black"), name="") +
  theme_blank_map() + 
  theme(legend.position="bottom", legend.key.size=unit(0.5, "lines"))
effort.map.binned
ggsave(effort.map.binned, 
       filename="figures/map_avg_tip_effort_binned.pdf", 
       device=cairo_pdf)
ggsave(effort.map.binned, 
       filename="figures/map_avg_tip_effort_binned.png")


# ------------------------------------
# Export underlying plot data to CSV
# ------------------------------------
to.csv <- effort.full %>%
  mutate(ccode = countrycode(id, "iso3c", "cown")) %>%
  select(iso3 = id, ccode, avg_effort = avg.effort, 
         median_effort = med.effort, bin = bin.clean)

write_csv(to.csv, path="data/map_avg_tip_effort.csv")

embassies.to.csv <- embassies.to.plot %>%
  mutate(ccode = countrycode(iso, "iso3c", "cown")) %>%
  select(embassy_name_in_cable = Embassy, country, iso, ccode, city, lat, long)

write_csv(embassies.to.csv, path="data/map_approximate_embassy_locations.csv")
