library(dplyr)
library(tidyr)
library(readr)
library(countrycode)


# -----------
# Load data
# -----------
# Clean data first
# cleaned <- read_csv("data/year_joined_original.csv") %>%
#   mutate(actual.name = countrycode(country_name, "country.name", "country.name"),
#          cow = countrycode(actual.name, "country.name", "cown")) %>%
#   select(country_name = actual.name, cow, start_year)
# write_csv(cleaned, "data/year_joined.csv")

joined <- read_csv("data/year_joined.csv") %>%
  mutate(id = countrycode(cow, "cown", "iso3c"),
         joined = TRUE)

all.possibilities <- expand.grid(id = countrycode_data$iso3c,
                                 report_year = 2001:2010, 
                                 stringsAsFactors=FALSE)

full.data <- all.possibilities %>% 
  left_join(joined, by=c("id", "report_year" = "start_year"))
