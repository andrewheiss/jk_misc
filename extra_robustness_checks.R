library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(rvest)
library(countrycode)
library(stringr)
library(WDI)
library(feather)


# Load original data
setwd("~/Research/•Judith/jk_misc/chapter_5")
source("clean_data.R")
setwd("~/Research/•Judith/jk_misc")

df.full <- df.complete.with.lags.correct

# Remove non-aid-eligible countries from models with aid in them
# Aid eligible countries: https://www.oecd.org/dac/stats/historyofdaclistsofaidrecipientcountries.htm
# http://stats.oecd.org/
#
oecd.dac <- read_csv("data/oecd_dac_countries.csv") %>%
  mutate(ccode = countrycode(country, "country.name", "cown"),
         ccode = ifelse(country == "Serbia", 555, ccode),
         dac_eligible = TRUE)


# Control for US military aid
# http://www.securityassistance.org/data/country/military/country/1996/2017/is_all/Global
military.aid <- read_csv("original_files/Military and Police Aid by Country.csv") %>%
  select(-`year (Year)`) %>%
  filter(!str_detect(country, "Regional")) %>%
  filter(!(country %in% c("African Union", "Global",
                          "Panama Canal Area Military School"))) %>%
  # TODO: Fix this; right now this just combines Serbia and Montenegro...
  mutate(country = ifelse(country == "Serbia and Montenegro", "Serbia", country)) %>%
  mutate(country = countrycode(country, "country.name", "country.name")) %>%
  gather(year, amount, -country) %>%
  # There are some duplicate countries, but all only have one amount
  group_by(country, year) %>%
  summarise(amount = sum(amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c"),
         cowcode = countrycode(country, "country.name", "cown"),
         cowcode = ifelse(country == "Serbia", 555, cowcode),
         year = as.numeric(year))


# Add FDI and ODA
# is there any relationship between tier ratings and FDI? Is FDI less likely in countries with worse ratings, once one controls for other economic and political factors? I suspect not, but it would be nice to test. Similarly for trade. 
# FDI ~ tier level + controls
wdi.indicators <- c("BN.KLT.DINV.CD",  # FDI, net inflows (current US$)
                    "DT.ODA.ALLD.CD")  # Net ODA and official aid received (current US$)
wdi.raw <- WDI(country="all", wdi.indicators, extra=TRUE, start=1991, end=2016)
wdi.countries <- countrycode(na.exclude(unique(df.full$cowcode)), "cown", "iso2c")

wdi.clean <- wdi.raw %>%
  filter(iso2c %in% wdi.countries) %>%
  rename(fdi = BN.KLT.DINV.CD, oda = DT.ODA.ALLD.CD) %>%
  mutate(oda.log = sapply(oda, FUN=function(x) ifelse(x < 0, NA, log1p(x))),
         fdi.log = sapply(fdi, FUN=function(x) ifelse(x < 0, NA, log1p(x)))) %>%
  mutate(cowcode = countrycode(iso2c, "iso2c", "cown"),
         cowcode = ifelse(country == "Serbia", 555, cowcode),
         region = factor(region),  # Get rid of unused levels first
         region = factor(region, labels =
                           gsub(" \\(all income levels\\)", "", levels(region)))) %>%
  select(-c(iso2c, capital, longitude, latitude, lending))


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
         bit.partner.cow = countrycode(bit.partner, "country.name", "cown"),
         bit.partner.cow = ifelse(bit.partner == "Serbia", 555, bit.partner.cow),
         bit.partner.iso = countrycode(bit.partner, "country.name", "iso3c"))
write_csv(us.bits, path="data/us_bits.csv")

us.bits.panel <- expand.grid(bit.partner = us.bits$bit.partner, 
                             year = seq(min(us.bits$sig.year), 
                                        max(us.bits$start.year, na.rm=TRUE), 1),
                             stringsAsFactors=FALSE) %>%
  right_join(us.bits, by="bit.partner") %>%
  mutate(has.bit.sig.with.us = year >= sig.year,
         has.bit.start.with.us = year >= start.year)


# Imports to the US
# IMF direction of trade statistics
# http://data.imf.org/regular.aspx?key=61013712
imports.us <- read_excel("original_files/External_Trade_by_Counterpart.xls", 
                         sheet=2, skip=6) %>%
  select(country = 1, everything()) %>%
  mutate_each(funs(asdf = ifelse(. == "-" | . == "...", NA, .))) %>%
  gather(year, amount, -country) %>%
  mutate(year = as.numeric(year),
         amount = as.numeric(amount) * 1000000,
         amount.log = log1p(amount),
         country = ifelse(country == "Serbia & Montenegro" | 
                            country == "Serbia & Montenegro n.s.", 
                          "Serbia", country),
         country = countrycode(country, "country.name", "country.name")) %>%
  filter(!is.na(country)) %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c"),
         cowcode = countrycode(country, "country.name", "cown"),
         cowcode = ifelse(country == "Serbia", 555, cowcode)) %>%
  arrange(country, year)
  

# Green book vs. OECD ODA?
# Green book counts more than OECD does
# Greenbook: https://catalog.data.gov/dataset/us-overseas-loans-and-grants-greenbook-usaid-1554
# Foreign aid explorer: https://explorer.usaid.gov/
# AidData: http://aiddata.org/
#
# US aid as a percent of total aid (get both US and global aid from OECD)
aidata <- read_csv("original_files/AidDataCoreDonorRecipientYear_ResearchRelease_Level1_v3.0.csv") %>%
  filter(!str_detect(recipient, "Regional")) %>%
  mutate(recipient.country = countrycode(recipient, "country.name", "country.name"),
         recipient.country = ifelse(recipient == "Serbia and Montenegro", 
                                    "Serbia", recipient.country)) %>%
  filter(!is.na(recipient.country))

aid.all <- aidata %>%
  group_by(recipient.country, year) %>%
  summarise(aid.total = sum(commitment_amount_usd_constant_sum, na.rm=TRUE))

aid.us <- aidata %>%
  filter(donor == "United States") %>%
  group_by(recipient.country, year) %>%
  summarise(aid.us = sum(commitment_amount_usd_constant_sum, na.rm=TRUE))

aid.us.total <- aid.all %>%
  left_join(aid.us, by=c("recipient.country", "year")) %>%
  mutate(aid.us.total.perc = aid.us / aid.total,
         aid.total.log = log1p(aid.total),
         aid.us.log = log1p(aid.us),
         recipient.iso = countrycode(recipient.country, "country.name", "iso3c"),
         recipient.cowcode = countrycode(recipient.country, "country.name", "cown"),
         recipient.cowcode = ifelse(recipient.country == "Serbia",
                                    as.integer(555), recipient.cowcode)) %>%
  ungroup()


robustness.df <- df.full %>% ungroup() %>%
  expand(cowcode, year) %>%
  left_join(select(oecd.dac, year, cowcode = ccode, dac_status, dac_abbr, dac_eligible), 
            by=c("cowcode", "year")) %>%
  left_join(select(military.aid, year, cowcode, us.military.aid = amount),
            by=c("cowcode", "year")) %>%
  left_join(select(wdi.clean, year, cowcode, fdi, oda, fdi.log, oda.log),
            by=c("cowcode", "year")) %>%
  left_join(select(us.bits.panel, year, cowcode = bit.partner.cow, 
                   has.bit.sig.with.us, has.bit.start.with.us),
            by=c("cowcode", "year")) %>%
  left_join(select(imports.us, year, cowcode, trade.to.us = amount, 
                   trade.to.us.log = amount.log),
            by=c("cowcode", "year")) %>%
  left_join(select(aid.us.total, year, cowcode = recipient.cowcode, 
                   aid.total, aid.total.log, aid.us, aid.us.log, 
                   aid.us.total.perc),
            by=c("cowcode", "year")) %>%
  group_by(cowcode) %>%
  mutate_each(funs(lag = lag))
  
write_feather(robustness.df, "data/robustness_df.feather")
