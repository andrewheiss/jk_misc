library(dplyr)
library(readr)
library(countrycode)

# List of countries with bureaucracies dedicated to tracking EDB requirements 
# and reforms. Comes from page 22 in the 2015 EDB report:
# http://www.doingbusiness.org/~/media/GIAWB/Doing%20Business/Documents/Annual-Reports/English/DB15-Chapters/DB15-Report-Overview.pdf
countries.with.edb.office <-
  c(
    "Indonesia",
    "South Korea",
    "Malaysia",
    "Philippines",
    "Sri Lanka",
    "Algeria",
    "Kuwait",
    "Morocco",
    "Saudi Arabia",
    "United Arab Emirates",
    "Azerbaijan",
    "Croatia",
    "Czech Republic",
    "Georgia",
    "Kazakhstan",
    "Kosovo",
    "Kyrgyzstan",
    "Macedonia",
    "Moldova",
    "Montenegro",
    "Poland",
    "Russia",
    "Tajikistan",
    "Ukraine",
    "United Kingdom",
    "Uzbekistan",
    "Botswana",
    "Burundi",
    "Central African Republic",
    "Comoros",
    "Democratic Republic of Congo",
    "Republic of Congo",
    "Cote d'Ivoire",
    "Guinea",
    "Kenya",
    "Liberia",
    "Malawi",
    "Mali",
    "Nigeria",
    "Rwanda",
    "Sierra Leone",
    "Togo",
    "Zambia",
    "Chile",
    "Colombia",
    "Costa Rica",
    "Dominican Republic",
    "Guatemala",
    "Mexico",
    "Panama",
    "Peru"
  )

# Standardize country names and codes
df.final <- data_frame(country_name_raw = countries.with.edb.office,
                       ISO3 = countrycode(country_name_raw, 
                                          "country.name", "iso3c"),
                       cowcode = countrycode(country_name_raw, 
                                             "country.name", "cown"),
                       country = countrycode(country_name_raw, 
                                             "country.name", "country.name")) %>%
  mutate(ISO3 = ifelse(country_name_raw == "Kosovo", "XKX", ISO3)) %>%
  select(country, ISO3, cowcode)

# Save final list
write_csv(df.final, path="EDB/data/countries_with_edb_bureau.csv")
