library(dplyr)
library(readr)
library(lubridate)
library(rvest)
library(countrycode)
# library(WDI)
# library(aiddata)

# Remove non-aid-eligible countries (get OECD DAC list online) from models with aid in them
# Aid eligible countries: https://www.oecd.org/dac/stats/documentupload/DAC%20List%20of%20ODA%20Recipients%202014%20final.pdf
# http://stats.oecd.org/
#

# Control for US military aid
# http://www.securityassistance.org/data/country/military/country/1996/2017/is_all/Global


# Add FDI
# is there any relationship between tier ratings and FDI? Is FDI less likely in countries with worse ratings, once one controls for other economic and political factors? I suspect not, but it would be nice to test. Similarly for trade. 
# FDI ~ tier level + controls


# Add BITS
# on human trafficking, Layna wondered if there is any effect of FDI from the US, BITs with the US or PTAs with the US. Can we check?
# https://ustr.gov/trade-agreements/bilateral-investment-treaties
#
# BITS ~ tier level + controls

bits.url <- "https://icsid.worldbank.org/apps/ICSIDWEB/resources/Pages/BITDetails.aspx?state=ST181"
bits.table.xpath <- '//*[@id="ctl00_m_g_4238daa0_aae6_4bbc_b65b_a6998b248617_ctl00_gvTreatiesbyCountry"]'

us.bits <- read_html(bits.url) %>%
  html_nodes(xpath=bits.table.xpath) %>%
  html_table() %>% bind_rows() %>%
  select(bit.partner = Party, sig.date = `Signature Date`,
         start.date = `Entry into Force Date`) %>%
  mutate(sig.date = mdy(sig.date),
         sig.year = year(sig.date),
         start.date = mdy(start.date),
         start.year = year(start.date),
         bit.partner = countrycode(bit.partner, "country.name", "country.name"),
         bit.partner.cc = countrycode(bit.partner, "country.name", "cown"),
         bit.partner.cc = ifelse(bit.partner == "Serbia", 555, bit.partner.cc),
         bit.partner.iso = countrycode(bit.partner, "country.name", "iso3c"))
write_csv(us.bits, path="data/us_bits.csv")

us.bits.panel <- expand.grid(bit.partner = us.bits$bit.partner, 
                             year = seq(min(us.bits$sig.year), 
                                        max(us.bits$start.year, na.rm=TRUE), 1),
                             stringsAsFactors=FALSE) %>%
  right_join(us.bits, by="bit.partner") %>%
  mutate(has.bit.sig.with.us = year >= sig.year,
         has.bit.start.with.us = year >= start.year)

# US aid as a percent of total aid (get both US and global aid from OECD)

# IMF direction of trade statistics

# Green book vs. OECD ODA?
# Green book counts more than OECD does
# OECD ODA: http://data.worldbank.org/indicator/DT.ODA.ALLD.CD
# Greenbook: https://catalog.data.gov/dataset/us-overseas-loans-and-grants-greenbook-usaid-1554
# Foreign aid explorer: https://explorer.usaid.gov/
# AidData: http://aiddata.org/









# df <- get_aid(donor="US", start=1990, end=2016)

# World Bank World Development Indicators (WDI)
# http://data.worldbank.org/data-catalog/world-development-indicators
# Use WDI::WDI() to access the data
wdi.indicators <- c("BX.KLT.DINV.CD.WD",  # FDI, net inflows (current US$)
                    "DT.ODA.ALLD.CD")  # Net ODA and official aid received (current US$)
wdi.raw <- WDI(country="all", wdi.indicators, extra=TRUE, start=1981, end=2016)

wdi.clean <- wdi.raw %>%
  rename(fdi = BX.KLT.DINV.CD.WD, oda = DT.ODA.ALLD.CD)

# wdi.clean <- wdi.raw %>%
#   filter(iso2c %in% wdi.countries) %>%
#   rename(gdpcap = NY.GDP.PCAP.KD, gdp = NY.GDP.MKTP.KD, 
#          population = SP.POP.TOTL, oda = DT.ODA.ALLD.CD) %>%
#   mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
#          population.log = log(population)) %>%
#   mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
#          population.log = log(population)) %>%
#   # Ignore negative values of oda
#   mutate(oda.log = sapply(oda, FUN=function(x) ifelse(x < 0, NA, log1p(x)))) %>%
#   mutate(cow = countrycode(iso2c, "iso2c", "cown"),
#          region = factor(region),  # Get rid of unused levels first
#          region = factor(region, labels = 
#                            gsub(" \\(all income levels\\)", "", levels(region)))) %>%
#   select(-c(iso2c, iso3c, country, capital, longitude, latitude, income, lending))
