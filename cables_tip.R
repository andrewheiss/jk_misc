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
cables <- read_csv("original_files/Cables Trafficking.csv")

# TODO: Geocode embassies; determine country
embassies <- cables %>% distinct(Embassy) %>% select(Embassy)
# embassies.geocoded <- geocode(embassies$Embassy, output="all",
#                               messaging=FALSE)
# saveRDS(embassies.geocoded, "data/embassies_geocoded.rds")
# geocodeQueryCheck()
embassies.geocoded <- readRDS("data/embassies_geocoded.rds")

embassies.full <- embassies %>% 
  bind_cols(bind_rows(lapply(1:length(embassies.geocoded), FUN=function(x) 
    parse.city(embassies.geocoded[[x]]))))

check.me <- embassies.full %>%
  select(Embassy, country, formatted_address)


# Remove State department prefixes
state.dept.prefixes <- c("^Efto" = "", "^Noforn" = "")

# Specify countries for ambiguous embassy names
city.updates <- c("Alexandria" = "Alexandria, Egypt", 
                  "Georgetown" = "Georgetown, Guyana", 
                  "Hamilton" = "Hamilton, Bermuda", 
                  "Kolonia" = "Kolonia, Micronesia", 
                  "Melbourne" = "Melbourne, Australia", 
                  "Naples" = "Naples, Italy", 
                  "Nogales" = "Nogales, AZ", 
                  "Nssau" = "Nassau, Bahamas", 
                  "Sanjose" = "San Jose, Costa Rica", 
                  "Santiago" = "Santiago, Chile", 
                  "Stpetersburg" = "St. Petersburg, Russia", 
                  "Vancouver" = "Vancouver, BC")

# Google struggles with these cities, so just get the countries
just.countries <- c("Nicosia" = "Cyprus", "Pristina" = "Kosovo")

# Clean up embassy names
embassies.fixed <- embassies.full %>%
  mutate(em.clean = Embassy,
         em.clean = str_replace_all(em.clean, c(state.dept.prefixes, 
                                                city.updates, just.countries)))

# 186 Parto                   Iran
# 221 State                   Belgium?

# 105 Iranrpodubai (Regional presence office)
# 208 Rpodubai (Regional presence office)

# 242 Unrome
# 243 Unvievienna
# 244 Useubrussels
# 245 Usnato
# 246 Usosce
# 247 Usunnewyork
