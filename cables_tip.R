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
library(maptools)
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
  filter(!is.na(country)) %>%
  rename(mrn = `Full MRN of last cable of that month`) %>%
  # Sometimes the first or last cable of the year gets put in the previous year. 
  # Extract the actual year from the MRN record and use *that* as the year.
  mutate(mrn.year = as.integer(substr(mrn, 1, 2)),
         Year = ifelse(mrn.year < 50, mrn.year + 2000, mrn.year + 1900))

# Get the predicted cables per day for each country-year
# TODO: Some of these are way off, like Egypt 2008, because of weirdness in the estimate
# Imputation kind of based on Gill, Michael, and Arthur Spirling. 2014. 
#  "Estimating the Severity of the WikiLeaks United States Diplomatic 
#  Cables Disclosure."
per.day <- cables.geocoded %>%
  group_by(Embassy, Year) %>%
  summarize(estimated.per.day = last(`Per day that year`)) %>%
  left_join(select(embassies, Embassy, country), by="Embassy") %>%
  group_by(country, Year) %>%
  summarize(estimated.per.day = sum(estimated.per.day, na.rm=TRUE)) %>%
  mutate(estimated.cables.year = estimated.per.day * 365)

# Calculate the number of cables published each year for all countries
estimated.per.year <- per.day %>%
  group_by(Year) %>%
  summarise(estimated.all.countries = sum(estimated.cables.year))

cables.per.year <- cables.geocoded %>%
  group_by(Year) %>%
  summarise(total.all.countries = sum(cables.month)) %>%
  left_join(estimated.per.year, by="Year")

# Calculate cable-related variables
# Creates multiple variables:
#   * cables.in.wl: the total number of cables that year present in the 
#      Wikileaks dump
#   * tip.cables.in.wl: the total number of TIP-related cables that year 
#      from those in the Wikileaks dump
#   * prop.tip.wl: cables.in.wl / tip.cables.in.wl
#   * prop.cables.year.wl: number of cables that year / total number of cables
#      for all countries (to get a sense of how much coverage a country-year got 
#      in the Wikileaks dump)
#   * estimated.per.day: the probable number of cables expected each day 
#      from that country that year
#   * estimated.cables.year: estimated.per.day * 365
#   * prop.tip.estimated: tip.cables.in.wl / estimated.cables.year
#   * prop.cables.year.estimated: estimated.cables.year / estimated.all.countries
cables.tip <- cables.geocoded %>%
  group_by(country, Year) %>%
  summarise(cables.in.wl = sum(cables.month),
            tip.cables.in.wl = sum(trafficking.cables.year, na.rm=TRUE)) %>%
  left_join(per.day, by=c("country", "Year")) %>%
  left_join(cables.per.year, by="Year") %>%
  mutate(prop.tip.wl = tip.cables.in.wl / cables.in.wl,
         prop.tip.estimated = tip.cables.in.wl / estimated.cables.year,
         prop.cables.year.wl = cables.in.wl / total.all.countries,
         prop.cables.year.estimated = 
           estimated.cables.year / estimated.all.countries) %>%
  select(-c(total.all.countries, estimated.all.countries))

# Expand to full country-year panel format
cables.panel <- cables.tip %>% 
  ungroup() %>%
  expand(Year, country) %>%  # Magic dataframe expansion
  left_join(cables.tip, by=c("country", "Year")) %>%
  mutate(cow = countrycode(country, "country.name", "cown")) %>%
  select(country, cow, year = Year, cables_in_wl = cables.in.wl, 
         tip_cables_in_wl = tip.cables.in.wl, 
         estimated_per_day = estimated.per.day,
         estimated_cables_year = estimated.cables.year,
         prop_tip_wl = prop.tip.wl, 
         prop_cables_year_wl = prop.cables.year.wl,
         prop_tip_estimated = prop.tip.estimated,
         prop_cables_year_estimated = prop.cables.year.estimated)


# ----------------
# Write to Stata
# ----------------
# # Add fancy Stata labels
# labs <- c("Country name", "COW code", "Year", 
#           "Number of cables originating from country (in Wikileaks)", 
#           "Number of cables related to TIP (in Wikileaks)", 
#           "Estimated number of cables per day",
#           "Estimated number of cables that year",
#           "Proportion of cables related to TIP (in Wikileaks)", 
#           "Proportion of total number of cables that year (in Wikileaks)",
#           "Proportion of cables related to TIP (estimated)", 
#           "Proportion of total number of cables that year (estimated)")
# 
# cables.panel %<>% add_labels(labs)  # For haven and RStudio
# attr(cables.panel, "var.labels") <- labs  # For foreign
# 
# # write_dta(cables.panel, "data/cables_panel.dta")
# write.dta(cables.panel, "data/cables_panel.dta")


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
  summarize(avg.effort = mean(prop.tip.estimated) * 1000,
            med.effort = median(prop.tip.estimated) * 1000) %>%
  mutate(id = countrycode(country, "country.name", "iso3c"),
         # Make intervals with about the same number of obs. in groups
         bins.raw = cut_number(avg.effort, 5, ordered_result=TRUE),
         bins = cut(avg.effort, c(0, 1.5, 2.5, 4, 6.5, 50),
                    ordered_result=TRUE))

# Extract the ranges from cut_number(), increase the lower bound by 1, and 
# make a nice label
# Converts [0,0.05] to 0-5% and (0.05,0.1] to 6-10%
bins.df <- data_frame(bins = levels(tip.effort$bins),
                      lower = as.numeric(gsub("[\\(\\[](.+),.*", "\\1", bins)),
                      upper = as.numeric(gsub("[^,]*,([^]]*)\\]", "\\1", bins)),
                      lower.clean = ifelse(grepl("^\\(", bins), lower + .1, lower),
                      bin.clean = paste0(lower.clean, "-", 
                                         upper, "    ")) %>%
  mutate(bin.clean = ifelse(lower >= 6.5, "6.5+", bin.clean),
         bin.clean = ifelse(lower == 0, "0    ", bin.clean),
         bin.clean = ifelse(upper == 1.5, "0-1.5    ", bin.clean),
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
# Map of proportions with bins
# TIP-related cables per 1,000 estimated/imputed cables
effort.map.binned <- ggplot(effort.full, aes(fill=bin.clean, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show.legend=FALSE) + 
  geom_point(data=embassies.to.plot, 
             aes(x=long.robinson, y=lat.robinson, fill=NULL, map_id=NULL), 
             colour="black", size=0.15, show.legend=FALSE, alpha=0.35) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_manual(values=c("white", "grey90", "grey60", "grey30", "black"),
                    guide = guide_legend(title=NULL,
                                         override.aes=list(size = 0.1),
                                         keywidth=0.75, keyheight=0.75)) +
  theme_blank_map() + 
  theme(legend.position="bottom")
effort.map.binned
ggsave(effort.map.binned, 
       filename="figures/map_avg_tip_effort_adjusted.pdf", 
       device=cairo_pdf)
ggsave(effort.map.binned, 
       filename="figures/map_avg_tip_effort_adjusted.png")


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
