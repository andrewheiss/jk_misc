# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(lubridate)
library(foreign)
library(countrycode)


# ------------------------
# Clean super messy data
# ------------------------
raw.years <- read.csv("original_files/funding_original.csv", 
                      stringsAsFactors=FALSE) %>%
  select(grant_year = fiscal.year)

raw.data <- read.csv("original_files/funding_master.csv", stringsAsFactors=FALSE) %>%
  slice(-c(1000, 3157, 4287:4290)) %>%  # Remove these rows
  bind_cols(raw.years) %>%  # Bring in years from other CSV
  mutate(amount = as.numeric(gsub("\\D", "", amount)),
         grant = as.numeric(ifelse(!(grant %in% c(0, 1)), NA, grant)),
         Prevention = as.numeric(ifelse(!(Prevention %in% c(0, 1)), NA, Prevention)),
         protection = as.numeric(ifelse(!(protection %in% c(0, 1)), NA, protection)),
         prosecution = as.numeric(ifelse(!(prosecution %in% c(0, 1)), NA, prosecution)),
         research = as.numeric(gsub("\\D", "", gsub("^o$", "0", research))),
         year_actual = ymd(paste0(grant_year, "-01-01"))) %>%
  select(country = Country, grant_year, year_actual, cowcode, grant, recipient, 
         subgrantee = Subgrantee, prevention = Prevention, protection, 
         prosecution, research, amount, region1 = Regional., 
         region2 = Regional..1, recipient_type = recipient.1)

# Write to Stata
# TODO: Take care of empty strings, long strings
labs <- c("Country name", "Year of grant", "Year (formatted as date)", 
          "COW code", "Grant", "Grant recipient", "Subgrantee", "Prevention", 
          "Protection", "Prosecution", "Research", "Amount given", "Region", 
          "Region (alternate)", "Type of recipient")

write.dta(raw.data, "~/Desktop/example.dta")
