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
library(gridExtra)
library(WDI)
library(readxl)
library(stargazer)

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
#   * prop.present: cables.in.wl / estimated.cables.year (to guess how many 
#       cables are missing)
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
         prop.present = cables.in.wl / estimated.cables.year,
         prop.tip.estimated = tip.cables.in.wl / estimated.cables.year,
         prop.cables.year.wl = cables.in.wl / total.all.countries,
         prop.cables.year.estimated = 
           estimated.cables.year / estimated.all.countries) %>%
  select(-c(total.all.countries, estimated.all.countries))

# Expand to full country-year panel format
cables.panel <- cables.tip %>% 
  ungroup() %>%
  filter(Year >= 2000) %>%
  expand(Year, country) %>%  # Magic dataframe expansion
  left_join(cables.tip, by=c("country", "Year")) %>%
  mutate(cow = countrycode(country, "country.name", "cown")) %>%
  select(country, cow, year = Year, cables_in_wl = cables.in.wl, 
         tip_cables_in_wl = tip.cables.in.wl, 
         estimated_per_day = estimated.per.day,
         estimated_cables_year = estimated.cables.year,
         prop_tip_wl = prop.tip.wl, 
         prop_present = prop.present,
         prop_cables_year_wl = prop.cables.year.wl,
         prop_tip_estimated = prop.tip.estimated,
         prop_cables_year_estimated = prop.cables.year.estimated) %>%
  # Add unofficial COW codes for these countries
  mutate(cow = case_when(
    .$country == "Hong Kong" ~ as.integer(715),
    .$country == "Bermuda" ~ as.integer(1003),
    .$country == "Serbia" ~ as.integer(340),
    .$country == "Curacao" ~ as.integer(1006),
    TRUE ~ .$cow)) %>%
  mutate(iso2 = countrycode(cow, "cown", "iso2c")) %>%
  mutate(iso2 = case_when(
    .$country == "Hong Kong" ~ "HK",
    .$country == "Bermuda" ~ "BM",
    .$country == "Serbia" ~ "RS",
    .$country == "Kosovo" ~ "XK",
    .$country == "Curacao" ~ "CW",
    TRUE ~ .$iso2))

# World Bank World Development Indicators (WDI)
# http://data.worldbank.org/data-catalog/world-development-indicators
# Use WDI::WDI() to access the data
wdi.indicators <- c("NY.GDP.PCAP.KD",  # GDP per capita (constant 2005 US$)
                    "NY.GDP.MKTP.KD",  # GDP (constant 2005 US$)
                    "SP.POP.TOTL",     # Population, total
                    "DT.ODA.ALLD.CD")  # Net ODA and official aid received (current US$)
wdi.countries <- unique(cables.panel$iso2)
wdi.raw <- WDI(country="all", wdi.indicators, extra=TRUE, start=2000, end=2016)

wdi.clean <- wdi.raw %>%
  filter(iso2c %in% wdi.countries) %>%
  rename(gdpcap = NY.GDP.PCAP.KD, gdp = NY.GDP.MKTP.KD, 
         population = SP.POP.TOTL, oda = DT.ODA.ALLD.CD) %>%
  mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
         population.log = log(population)) %>%
  mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
         population.log = log(population)) %>%
  # Ignore negative values of oda
  mutate(oda.log = sapply(oda, FUN=function(x) ifelse(x < 0, NA, log1p(x)))) %>%
  mutate(region = factor(region),  # Get rid of unused levels first
         region = factor(region, labels = 
                           gsub(" \\(all income levels\\)", "", levels(region)))) %>%
  select(-c(country, capital, longitude, latitude, income, lending))

# Democracy (Freedom House)
if (!file.exists(file.path("original_files", "freedom_house.xlsx"))) {
  fh.url <- paste0("https://freedomhouse.org/sites/default/files/", 
                   "Country%20Ratings%20and%20Status%2C%20",
                   "1973-2016%20%28FINAL%29_0.xlsx")
  fh.tmp <- file.path("original_files", "freedom_house.xlsx")
  download.file(fh.url, fh.tmp)
}

fh.raw <- read_excel(file.path("original_files", "freedom_house.xlsx"), 
                     skip=6)

# Calculate the number of years covered in the data (each year has three columns)
num.years <- (ncol(fh.raw) - 1)/3

# Create combinations of all the variables and years
var.years <- expand.grid(var = c('PR', 'CL', 'Status'), 
                         year = 1972:(1972 + num.years - 1))

colnames(fh.raw) <- c('country', paste(var.years$var, var.years$year, sep="_"))

# Split columns and convert to long
fh <- fh.raw %>%
  gather(var.year, value, -country) %>%
  separate(var.year, into=c("indicator", "year"), sep="_") %>%
  filter(!is.na(country)) %>%
  spread(indicator, value) %>%
  mutate(year = as.numeric(year),
         CL = suppressWarnings(as.integer(CL)),
         PR = suppressWarnings(as.integer(PR)),
         Status = factor(Status, levels=c("NF", "PF", "F"), 
                         labels=c("Not free", "Partially free", "Free"),
                         ordered=TRUE),
         total.freedom = CL + PR,
         country.clean = countrycode(country, "country.name", "country.name")) %>%
  filter(!is.na(CL) & !is.na(PR)) %>%
  # All the cases we're interested in are after 2000, so we can remove these
  # problematic double countries
  filter(!(country %in% c("Germany, E.", "Germany, W.", "USSR", "Vietnam, N.", 
                          "Vietnam, S.", "Yemen, N.", "Yemen, S."))) %>%
  # Again, because we only care about post-2000 Serbia, merge with Yugoslavia
  mutate(country.clean = ifelse(country.clean == "Yugoslavia", 
                                "Serbia", country.clean)) %>%
  select(-country, country=country.clean)

fh.summary <- fh %>%
  filter(year >= 2000) %>%
  group_by(country, year) %>%
  summarize(total.freedom = mean(total.freedom, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(iso2 = countrycode(country, "country.name", "iso2c")) %>%
  mutate(iso2 = ifelse(country == "Kosovo", "XK", iso2)) %>%
  select(-country)

# Model cable presence
cables.panel.all <- cables.panel %>%
  left_join(wdi.clean, by=c("iso2" = "iso2c", "year")) %>%
  left_join(fh.summary, by=c("year", "iso2")) %>%
  mutate(prop_present100 = prop_present * 100) %>%
  # Get rid of crazy outliers (Syria 2007 and Turkey 2009)
  filter(prop_present100 < 100)

model.simple <- lm(prop_present100 ~ gdpcap.log + oda.log + total.freedom,
            data=cables.panel.all)

model.fe <- lm(prop_present100 ~ gdpcap.log + oda.log + total.freedom +
                 region + as.factor(year),
               data=cables.panel.all)

extra.lines <- list(c("Year fixed effects", c("No", "Yes")),
                    c("Country fixed effects", c("No", "Yes")))

stargazer(model.simple, model.fe,
          type="text", no.space=TRUE,
          dep.var.caption="Percent of estimated cables actually present",
          omit="\\.factor|region",
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines)


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
saveRDS(cables.panel, file="final_figures/data_figureA_cables.rds")


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
            med.effort = median(prop.tip.estimated) * 1000,
            num.cables = sum(cables.in.wl, na.rm=TRUE),
            estimated.per.year = sum(estimated.cables.year, na.rm=TRUE),
            prop.present = num.cables / estimated.per.year) %>%
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
  mutate(bins = as.character(bins)) %>%
  left_join(bins.df, by="bins")

# Convert embassy coordinates to Robinson projection and add to df
embassies.robinson <- project(as.matrix(embassies %>% select(long, lat)), 
                              proj="+proj=robin") %>%
  as.data.frame %>% rename(long.robinson = long, lat.robinson = lat)
embassies.to.plot <- bind_cols(embassies, as.data.frame(embassies.robinson)) %>%
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

# Actual number of cables
actual.cables.map <- ggplot(effort.full, aes(fill=num.cables, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show.legend=FALSE) + 
  geom_point(data=embassies.to.plot, 
             aes(x=long.robinson, y=lat.robinson, fill=NULL, map_id=NULL), 
             colour="black", size=0.15, show.legend=FALSE, alpha=0.35) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(high="black", low="white", na.value="white",
                      labels=comma, name="",
                      limits=c(0, max(effort.full$num.cables)),
                      guide=guide_colorbar(draw.llim=TRUE, barwidth=15, 
                                           barheight=0.5, ticks=FALSE)) +
  labs(title="Actual number of cables") +
  theme_blank_map(base_size=10) + 
  theme(legend.position="bottom",
        plot.title=element_text(hjust=0.5, size=rel(1)))
actual.cables.map

# Estimated number of cables
estimated.cables.map <- ggplot(effort.full, aes(fill=estimated.per.year, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show.legend=FALSE) + 
  geom_point(data=embassies.to.plot, 
             aes(x=long.robinson, y=lat.robinson, fill=NULL, map_id=NULL), 
             colour="black", size=0.15, show.legend=FALSE, alpha=0.35) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(high="black", low="white", na.value="white",
                      labels=comma, name="",
                      limits=c(0, max(effort.full$estimated.per.year)),
                      guide=guide_colorbar(draw.llim=TRUE, barwidth=15, 
                                           barheight=0.5, ticks=FALSE)) +
  labs(title="Estimated number of cables") +
  theme_blank_map(base_size=10) + 
  theme(legend.position="bottom",
        plot.title=element_text(hjust=0.5, size=rel(1)))
estimated.cables.map

# Proporition of estimated cables that exist
prop.present.map <- ggplot(effort.full, aes(fill=prop.present, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show.legend=FALSE) + 
  geom_point(data=embassies.to.plot, 
             aes(x=long.robinson, y=lat.robinson, fill=NULL, map_id=NULL), 
             colour="black", size=0.15, show.legend=FALSE, alpha=0.35) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(high="black", low="white", na.value="white",
                      labels=percent, name="",
                      limits=c(0, max(effort.full$prop.present)),
                      guide=guide_colorbar(draw.llim=TRUE, barwidth=15, 
                                           barheight=0.5, ticks=FALSE)) +
  labs(title="Proporition of estimated cables that exist") +
  theme_blank_map(base_size=10) + 
  theme(legend.position="bottom",
        plot.title=element_text(hjust=0.5, size=rel(1)))
prop.present.map

cables.map <- arrangeGrob(estimated.cables.map, actual.cables.map,
                          prop.present.map, ncol=1)
grid.draw(cables.map)
ggsave(cables.map, 
       filename="figures/map_cables.pdf", 
       device=cairo_pdf, width=4.5, height=7, units="in")
ggsave(cables.map, 
       filename="figures/map_cables.png",
       width=4.5, height=5, units="in")


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
