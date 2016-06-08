library(dplyr)
library(readr)
library(tidyr)
library(haven)
library(countrycode)
library(ggplot2)

source("shared_functions.R")

base.folder <- "final_figures"

cases <- c("ARM", "IDN", "ECU", "MOZ", "KAZ", "ARG", "ISR", 
           "ARE", "NGA", "OMN", "HND", "JPN", "TCD", "ZWE", "MYS")

# Count of prosecutions, convictions, and investigations
crim.activities <- read_csv("original_files/crime_data.csv", na=".") %>%
  select(-contains("original")) %>%
  mutate(iso = countrycode(Country, "country.name", "iso3c"),
         Country = countrycode(iso, "iso3c", "country.name")) %>%
  gather(activity.type, number, Prosecutions, Convictions, `Opened investigations`) %>%
  filter(activity.type != "Opened investigations")

# Criminalization data
df.cho <- readRDS("data/policy_index.rds") %>%
  filter(iso %in% cases) %>%
  group_by(iso) %>% slice(1) %>% ungroup() %>%
  select(iso, year, year.full, year.partial) %>%
  gather(crim.type, crim.year, year.full, year.partial) %>%
  mutate(crim.type = factor(crim.type, levels=c("year.partial", "year.full"),
                            ordered=TRUE),
         Country = countrycode(iso, "iso3c", "country.name"))

# Plot all cases
fig.crim.activities <- ggplot(crim.activities, 
                              aes(x=Year, y=number, colour=activity.type)) +
  geom_vline(data=df.cho, 
             aes(xintercept=crim.year, linetype=crim.type),
             size=0.5, color="grey50") +
  geom_line() + 
  scale_x_continuous(limits=c(2000, 2015)) + 
  scale_linetype_manual(values=c("dotted", "solid"), guide=FALSE) +
  scale_colour_manual(values=c("black", "grey65"), name=NULL) +
  labs(x=NULL, y="Count") + 
  facet_wrap(~ Country, scales="free", ncol=3) + 
  theme_clean(10) + 
  theme(legend.key.size=unit(0.65, "lines"),
        legend.key = element_blank(),
        legend.margin = unit(0.25, "lines"),
        panel.grid.minor=element_blank(),
        strip.text=element_text(size=rel(0.8)))

filename <- "figure_cases_crim_activities"
width <- 4.5
height <- 6
ggsave(fig.crim.activities, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig.crim.activities, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)
