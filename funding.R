# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(lubridate)
library(foreign)
library(countrycode)


# ------------------
# Useful functions
# ------------------
# Shrink long strings
truncate <- function(x) {
  if (nchar(x) > 80) {
    paste0(strtrim(x, 80), "...")
  } else {
    x
  }
}


# ------------------------
# Clean super messy data
# ------------------------
raw.years <- read.csv("original_files/funding_original.csv", 
                      stringsAsFactors=FALSE) %>%
  select(grant_year = fiscal.year)

funding.clean <- read.csv("original_files/funding_master.csv", stringsAsFactors=FALSE) %>%
  slice(-c(1000, 3157)) %>%  # Remove these two extra rows
  bind_cols(raw.years) %>%  # Bring in years from other CSV
  mutate(amount = as.numeric(gsub("\\D", "", amount)),
         grant = as.numeric(ifelse(!(grant %in% c(0, 1)), NA, grant)),
         Prevention = as.numeric(ifelse(!(Prevention %in% c(0, 1)), NA, Prevention)),
         protection = as.numeric(ifelse(!(protection %in% c(0, 1)), NA, protection)),
         prosecution = as.numeric(ifelse(!(prosecution %in% c(0, 1)), NA, prosecution)),
         research = as.numeric(gsub("\\D", "", gsub("^o$", "0", research))),
         year_actual = ymd(paste0(grant_year, "-01-01")),
         recipient.1 = factor(recipient.1),
         id = 1:nrow(.)) %>%
  select(id, country = Country, grant_year, year_actual, cowcode, grant, recipient, 
         subgrantee = Subgrantee, prevention = Prevention, protection, 
         prosecution, research, amount, region1 = Regional., 
         region2 = Regional..1, recipient_type = recipient.1) %>%
  mutate(recipient = ifelse(recipient == "", NA, recipient),
         subgrantee = ifelse(subgrantee == "", NA, subgrantee),
         region1 = ifelse(region1 == "", NA, region1),
         region2 = ifelse(region2 == "", NA, region2))


# ----------------
# Write to Stata
# ----------------
# Stata (or at least write.dta?) gets mad at long character columns
funding.clean.stata <- funding.clean %>%
  select(-year_actual) %>%
  rowwise() %>%
  mutate(recipient = truncate(recipient),
         subgrantee = truncate(subgrantee)) %>%
  ungroup()
  
labs <- c("Row ID", "Country name", "Year of grant", 
          "COW code", "Grant", "Grant recipient", "Subgrantee", "Prevention", 
          "Protection", "Prosecution", "Research", "Amount given", "Region", 
          "Region (alternate)", "Type of recipient")
attr(funding.clean.stata, "var.labels") <- labs

write.dta(funding.clean.stata, "data/funding_clean.dta")
system("stata-se -b do funding_clean_stata.do")
system("rm funding_clean_stata.log")


# --------------
# Stats to run
# --------------
# the number of grants and the total amount that the US Office to Combat Trafficking in Persons has awarded to various intergovernmental organizations
# the above, broken down by IGOs
# 
# share of all grants awarded to IGOs
# 
# breakdown of purpose of grants awarded to IGOs
# 
# the breakdown of IGO funding by country. 
