library(dplyr)
library(tidyr)
library(readr)
library(countrycode)


# Determine a country's presence in the annual TIP report
# Fun logic to get this working:
#   1. Take the `joined` column, which just has a TRUE in the year a country 
#      joined, convert to numeric and calculate the cumulative sum (x.num)
#   2. Calculate the cumulative sum again to find how many years the country 
#      has been part of the report (x.final)
#   3. Convert to factor: 
#      Not present: x.final = 0, Newly added: x.final = 1, 
#      Already present: x.final > 1
#
# Example of all intermediate columns:
#
#     id report_year joined x.num x.final x.text
# 1  AFG        2001     NA     0       0     No
# 2  AFG        2002   TRUE     1       1    New
# 3  AFG        2003     NA     1       2    Yes
# 4  AFG        2004     NA     1       3    Yes
# 5  AFG        2005     NA     1       4    Yes
#
# Function only returns the final text
calc.tip.presence <- function(x) {
  possible.levels <- c("No", "New", "Yes", "Missing")
  if (all(is.na(x))) {
    return(factor("Missing", levels=possible.levels))
  } else{
    x.num <- cumsum(ifelse(is.na(x), FALSE, x))
    x.final <- cumsum(x.num)
    x.text <- cut(x.final, breaks=c(0, 0.9, 1, Inf), include.lowest=TRUE, 
                  labels=c("No", "New", "Yes"))
    return(x.text) 
  }
}

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
  left_join(joined, by=c("id", "report_year" = "start_year")) %>%
  arrange(id, report_year) %>%
  group_by(id) %>%
  mutate(in.report = calc.tip.presence(joined))

