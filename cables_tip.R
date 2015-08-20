# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(foreign)
library(countrycode)


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

# Rather than fight with lapply or dplyr::mutate_each, just use a dumb for loop
for (i in 1:ncol(cables.panel)) {
  attr(cables.panel[[i]], "label") <- labs[i]  # For haven and RStudio
}

# For foreign
attr(cables.panel, "var.labels") <- labs

# write_dta(cables.panel, "data/cables_panel.dta")
write.dta(cables.panel, "data/cables_panel.dta")
