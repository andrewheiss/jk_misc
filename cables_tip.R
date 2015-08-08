# TASK: calculate VAR1 the percent of available cables that are on TIP
# Calculate VAR2: the percent of cables available
# Create country year dataset with cow code , year and these two variables
# create map of density of TIP effort as defined by var1:the percent of available cables that are on TIP

# ----------------
# Load libraries
# ----------------
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(grid)
library(Cairo)
library(ggmap)
library(countrycode)


# ------------------
# Useful functions
# ------------------
# geocode(..., output="more") chokes when the type of address component is not 
# a character (like when it is an empty list), so this is a custom parser to 
# only extract the necessary columns AND manually fix strange addresses
parse.city <- function(x) {
  if (x$status == "ZERO_RESULTS") {
    df <- data_frame(formmated_address = NA, lat = NA, long = NA,
                     loctype = NA, address = NA, country = NA,
                     admin_level_1 = NA, admin_level_2 = NA,
                     admin_level_3 = NA, locality = NA)
    return(df)
  }
  
  NULLtoNA <- function(x) {
    if (length(x) == 0) 
      return(NA)
    x
  }
  
  results <- x$results[[1]]
  
  # If the address component type is not a character, force it to be
  for (i in 1:length(results$address_components)) {
    if (class(results$address_components[[i]]$types) != "character") {
      results$address_components[[i]]$types <- ""
    }
  }
  
  # lapply(as.data.frame) %>% rbind_all() is a replacement for plyr:ldply()
  address.parts <- results$address_components %>% 
    lapply(as.data.frame, stringsAsFactors=FALSE) %>% 
    rbind_all() %>%
    filter(types != "political")
  
  geometry_parts <- results$geometry %>%
    lapply(as.data.frame, stringsAsFactors=FALSE) %>%
    rbind_all() %>%
    rename(type = `X[[i]]`) %>%
    summarise_each(funs(max(., na.rm=TRUE)))
  
  filter(address.parts, types == "country")$long_name
  
  df <- data_frame(formatted_address = NULLtoNA(results$formatted_address),
                   lat = NULLtoNA(geometry_parts$lat),
                   long = NULLtoNA(geometry_parts$lng),
                   loctype = tolower(NULLtoNA(geometry_parts$type)),
                   address = tolower(NULLtoNA(results$formatted_address)),
                   country = NULLtoNA(filter(address.parts, 
                                             types == "country")$long_name),
                   admin_level_1 = 
                     NULLtoNA(filter(address.parts, 
                                     types == "administrative_area_level_1")$long_name),
                   admin_level_2 = 
                     NULLtoNA(filter(address.parts,
                                     types == "administrative_area_level_2")$long_name),
                   admin_level_3 = 
                     NULLtoNA(filter(address.parts, 
                                     types == "administrative_area_level_3")$long_name),
                   locality = NULLtoNA(filter(address.parts, 
                                              types == "locality")$long_name))
  df
}


# -----------
# Load data
# -----------
# Regex searches to clean up the cable data
# Remove State department prefixes
state.dept.prefixes <- c("^Efto" = "", "^Noforn" = "")

# Specify countries for ambiguous embassy names
city.updates <- c("Alexandria" = "Alexandria, Egypt", 
                  "Georgetown" = "Georgetown, Guyana", 
                  "Hamilton" = "Hamilton, Bermuda", 
                  "Kolonia" = "Kolonia, Micronesia", 
                  "Melbourne" = "Melbourne, Australia", 
                  "Naples" = "Napoli", 
                  "Nicosia" = "Metochiou & Ploutarchou Street, 2407, Engomi",
                  "Nogales" = "Nogales, AZ", 
                  "Nssau" = "Nassau, Bahamas",
                  "Pristina" = "Ahmet Krasniqi, Prishtinë 10000, Kosovo",
                  "Sanjose" = "San Jose, Costa Rica", 
                  "Santiago" = "Santiago, Chile", 
                  "Stpetersburg" = "St. Petersburg, Russia", 
                  "Vancouver" = "Vancouver, BC")

# Cities that have the same name as the country, or that Google struggles to find
city.countries <- c("Belize" = "Belmopan, Belize",
                    "Brasilia" = "Brasilia, Brazil",
                    "Curacao" = "Willemstad, Curacao",
                    "Djibouti" = "Djibouti, Djibouti",
                    "Grenada" = "St. George's, Grenada",
                    "Guatemala" = "Guatemala City, Guatemala",
                    "Hongkong" = "Hong Kong, Kong Kong",
                    "Kuwait" = "Kuwait City, Kuwait",
                    "Luxembourg" = "Luxembourg City, Luxembourg", 
                    "Mexico" = "Mexico City, Mexico",
                    "Panama" = "Panama City, Panama",
                    "Singapore" = "Singapore, Singapore",
                    "Tunis" = "Tunis, Tunisia",
                    "Vatican" = "Vatican City, Vatican City")

# Iran regional presence offices
iran.offices <- c("Iranrpodubai" = "Tehran, Iran", "Rpodubai" = "Tehran, Iran")

# IGOs and other miscellaneous embassy designations that won't be geocoded
misc <- c("Unrome" = "UN Rome", "Unvievienna" = "UN Vienna", 
          "Useubrussels" = "US to EU Brussels", 
          "Usnato" = "US to NATO Brussels", 
          "Usosce" = "US to OSCE Vienna", 
          "Usunnewyork" = "US to UN New York",
          "State" = "Secretary of State",
          "Parto" = "US Delegation, Secretary")

# Original cable data
cables <- read_csv("original_files/Cables Trafficking.csv")

# Extract a list of unique embassies, clean up names
embassies <- cables %>% distinct(Embassy) %>% select(Embassy) %>%
  mutate(em.clean = Embassy,
         to.geocode = ifelse(em.clean %in% names(misc), FALSE, TRUE),
         em.clean = str_replace_all(em.clean, c(state.dept.prefixes, city.updates,
                                                city.countries, iran.offices)),
         em.clean = str_replace_all(em.clean, misc))

# Geocode embassies
embassies.to.geocode <- embassies %>% 
  filter(to.geocode == TRUE)

geocode.cache <- "data/embassies_geocoded.rds"
if (file.exists(geocode.cache)) {
  embassies.geocoded <- readRDS(geocode.cache)
} else {
  embassies.geocoded <- geocode(embassies.to.geocode$em.clean, 
                                output="all", messaging=FALSE)
  saveRDS(embassies.geocoded, geocode.cache)
  # geocodeQueryCheck()
}

# Convert crazy nested list returned by Google to a dataframe
# bind_rows(..., parse.city(...)) creates the dataframe
# bind_cols joins that dataframe to original list of embassies to geocode
embassies.parsed <- embassies.to.geocode %>% 
  bind_cols(bind_rows(lapply(1:length(embassies.geocoded), FUN=function(x) 
    parse.city(embassies.geocoded[[x]]))))

# Merge parsed embassy data with the full list (which includes non-geocoded 
# embassies, like the UN missions)
embassies.full <- embassies %>% #select(Embassy) %>%
  left_join(embassies.parsed, by=c("Embassy", "em.clean", "to.geocode"))

# Manually check that embassies were geolocated correctly
# check.me <- embassies.full %>%
#   select(Embassy, country, formatted_address)

embassies.final <- embassies.full %>%
  mutate(city = ifelse(is.na(locality), admin_level_1, locality)) %>%
  select(Embassy, country, city, 
         lat, long, geocoded = to.geocode) %>%
  mutate(iso = countrycode(country, "country.name", "iso3c"),
         country = countrycode(iso, "iso3c", "country.name")) %>%
  # Deal with Kosovo
  mutate(country = ifelse(Embassy == "Pristina", "Kosovo", country),
         iso = ifelse(Embassy == "Pristina", "KOS", iso))
