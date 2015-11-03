library(dplyr)
library(readr)
library(readxl)
library(countrycode)

master.list <- read_excel("/Users/andrew/Research/••Projects/Human trafficking/Master NGO list.xlsx", na=".") 

ngo.count <- master.list %>%
  group_by(ISO3) %>%
  summarise(ht_ngos = n()) %>%
  filter(!is.na(ISO3)) %>%
  mutate(cowcode = countrycode(ISO3, "iso3c", "cown")) %>%
  filter(!is.na(cowcode)) %>%
  select(cowcode, ht_ngos)

write_csv(ngo.count, path="data/ngo_count.csv")
# TODO: Fix Serbia
# TODO: Stop using cowcodes
