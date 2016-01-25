# ----------------
# Load libraries
# ----------------
library(dplyr)
library(lubridate)
library(haven)
library(readr)
library(countrycode)

source("shared_functions.R")


# -----------
# Load data
# -----------
# Load and clean original data
# 555 = missing; 666 = special case
tiers.orig <- read_dta("original_files/mergedChoTierCrimfix.dta") %>%
  select(countryname = name, year, cow=ccode, tier = tierupdated) %>%
  mutate(tier = ifelse(tier %in% c(555, 666), NA, tier),
         countryname = ifelse(countryname == "CentralAfRep", 
                              "Central African Republic",  countryname),
         countryname = ifelse(countryname == "DomRep", 
                              "Dominican Republic",  countryname),
         countryname = ifelse(countryname == "SaoT&P", 
                              "Sao Tome",  countryname),
         countryname = ifelse(countryname == "SolomanIs", 
                              "Solomon Islands",  countryname),
         countryname = ifelse(countryname == "UArabEmir", 
                              "United Arab Emirates",  countryname))

# Load 2015 data
tiers.2015 <- read_csv("data/tiers_2015.csv", col_types="cd") %>%
  mutate(tier = ifelse(tier %in% c(555, 666), NA, tier),
         year = 2015,
         cow = countrycode(countryname, "country.name", "cown"))

# Combine original data and 2015 data and clean stuff up
tiers <- bind_rows(tiers.orig, tiers.2015) %>% arrange(countryname, year) %>%
  mutate(iso = countrycode(countryname, "country.name", "iso3c"),
         iso = ifelse(countryname == "Kosovo", "KOS", iso),
         # Use clean country names
         countryname = countrycode(iso, "iso3c", "country.name"),
         countryname = ifelse(iso == "KOS", "Kosovo", countryname)) %>%
  mutate(tier = factor(tier, levels=c(1, 2, 2.5, 3), 
                       labels=c("Tier 1    ", "Tier 2    ", "Watchlist    ", "Tier 3"), 
                       ordered=TRUE))

present.2001 <- tiers %>% 
  filter(year == 2001, !is.na(tier)) %>% select(iso) %>% 
  c %>% unlist %>% unname

present.2005 <- tiers %>% 
  filter(year == 2005, !is.na(tier)) %>% select(iso) %>% 
  c %>% unlist %>% unname


# Calculate the percentage of tier assignments for each year
tiers.summary <- tiers %>%
  filter(!(is.na(tier))) %>%
  count(year, tier) %>%
  group_by(year) %>%
  mutate(pct = n / sum(n),
         year.actual = ymd(paste0(year, "-01-01")))

tiers.summary.2001 <- tiers %>%
  filter(iso %in% present.2001) %>%
  filter(!(is.na(tier))) %>%
  count(year, tier) %>%
  group_by(year) %>%
  mutate(pct = n / sum(n),
         year.actual = ymd(paste0(year, "-01-01")))

tiers.summary.2005 <- tiers %>%
  filter(iso %in% present.2005) %>%
  filter(!(is.na(tier))) %>%
  count(year, tier) %>%
  group_by(year) %>%
  mutate(pct = n / sum(n),
         year.actual = ymd(paste0(year, "-01-01")))


# -----------
# Plot data
# -----------
tier.plot <- ggplot(tiers.summary, 
                    aes(x=year.actual, y=pct, colour=tier, linetype=tier)) + 
  geom_line(size=0.75) + 
  labs(x=NULL, y="Percent assigned to tier") + 
  scale_y_continuous(labels=percent) + 
  scale_colour_manual(values=c("grey80", "grey50", "grey50", "black"), name="") + 
  scale_linetype_manual(values=c("solid", "solid", "dashed", "solid"), name="") + 
  scale_x_datetime(limits=ymd(c("2000-01-01", "2015-01-01"))) + 
  theme_clean(10) + theme(legend.key = element_blank(), 
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))
tier.plot
# ggsave(tier.plot, filename="figures/tier_percents.pdf", 
#        width=6, height=3, units="in", device=cairo_pdf)
# ggsave(tier.plot, filename="figures/tier_percents.png", 
#        width=6, height=3, units="in")

tier.plot.2001 <- ggplot(tiers.summary.2001, 
                         aes(x=year.actual, y=pct, colour=tier, linetype=tier)) + 
  geom_line(size=0.75) + 
  labs(x=NULL, y="Percent assigned to tier") + 
  scale_y_continuous(labels=percent) + 
  scale_colour_manual(values=c("grey80", "grey50", "grey50", "black"), name="") + 
  scale_linetype_manual(values=c("solid", "solid", "dashed", "solid"), name="") + 
  scale_x_datetime(limits=ymd(c("2000-01-01", "2015-01-01"))) + 
  theme_clean(10) + theme(legend.key = element_blank(), 
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))
tier.plot.2001

tier.plot.2005 <- ggplot(tiers.summary.2005, 
                         aes(x=year.actual, y=pct, colour=tier, linetype=tier)) + 
  geom_line(size=0.75) + 
  labs(x=NULL, y="Percent assigned to tier") + 
  scale_y_continuous(labels=percent) + 
  scale_colour_manual(values=c("grey80", "grey50", "grey50", "black"), name="") + 
  scale_linetype_manual(values=c("solid", "solid", "dashed", "solid"), name="") + 
  scale_x_datetime(limits=ymd(c("2000-01-01", "2015-01-01"))) + 
  theme_clean(10) + theme(legend.key = element_blank(), 
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))
tier.plot.2005


# ------------------------------------
# Export underlying plot data to CSV
# ------------------------------------
tiers.to.csv <- tiers %>%
  select(countryname, iso, year, ccode = cow, tier)

write_csv(tiers.to.csv, path="data/map_tiers.csv")

tiers.summary.to.csv <- tiers.summary %>%
  arrange(year, tier) %>%
  select(year, tier, num_countries = n, percent = pct)

write_csv(tiers.summary.to.csv, path="data/map_tiers_summary.csv")
