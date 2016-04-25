# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(haven)
library(readr)
library(ggplot2)
library(lubridate)
library(countrycode)
library(zoo)

source("shared_functions.R")

base.folder <- "~/Desktop/misc_graphs"

# -------------------------------------
# Manipulate and create new variables
# -------------------------------------
# Explanation of the p index:
# http://www.economics-human-trafficking.net/anti-trafficking-3p.html
# Prosecution Index Score: 1 (no compliance) - 5 (full compliance)
# Prevention Index Score: 1 (no compliance) - 5 (full compliance)
# Protection Index Score: 1 (no compliance) - 5 (full compliance)
# 3P Anti-trafficking Policy Index Score (p): 3 (no compliance for any of the 
#   three areas) - 15 (full compliance for all of the three areas)

cho.orig <- read_dta("original_files/mergedChoTierCrimfix.dta") %>%
  select(countryname = name, year, cow=ccode, tier = tierupdated, 
         p, prosecution, prevention, protection) %>%
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
                              "United Arab Emirates",  countryname),
         cow = ifelse(cow == 1001, 347, cow))  # Deal with Kosovo

year.joined <- read_csv("data/year_joined.csv")

year.criminalized.all <- read_stata("original_files/Criminalization Data UpdatedJK.dta") %>%
  mutate(iso = countrycode(ccode, "cown", "iso3c"),
         countryname = countrycode(iso, "iso3c", "country.name")) %>%
  filter(adjcrimlevel > 0) %>%
  select(iso, year, crimlevel)


# Variables to make:
#   time.in.report = years present in report
#   change.since.entering = p_t - p_{entered report}
#   change/time
# TODO: Take missing values into account (like Afghanistan)
p.index <- cho.orig %>% left_join(year.joined, by="cow") %>%
  left_join(year.criminalized.all, by=c("iso", "year")) %>%
  group_by(cow) %>%
  # Make boolean for if the country is in the report and cumulatively sum it
  mutate(in.report = ifelse(year >= start_year, TRUE, FALSE),
         time.in.report = cumsum(in.report),
         change.since.entering = p - p[in.report & year == start_year],
         # Don't calculate change for years where country wasn't in report
         # as.numeric required because of this issue:
         #   https://github.com/hadley/dplyr/issues/1036#issuecomment-87162231
         change.since.entering = as.numeric(ifelse(in.report, 
                                                   change.since.entering, NA)),
         change.time.report = change.since.entering / time.in.report) %>%
  # Carry last known criminalization value forward
  mutate(criminalized = na.locf(crimlevel, na.rm=FALSE),
         criminalized = ifelse(is.na(criminalized), as.integer(0), criminalized),
         criminalization.type = factor(criminalized, levels=c(0, 1, 2),
                                       labels=c("None    ", "Partial    ", "Full"),
                                       ordered=TRUE)) %>%
  ungroup()
# write_csv(p.index, "data/policy_index.csv")

# -------------------
# Plot changes in p
# -------------------
countries.to.plot <- c("ARM", "IDN", "ECU", "MOZ", "KAZ", "ARG", "ISR", 
                       "ARE", "NGA", "OMN", "HND", "JPN", "TCD", "ZWE", "MYS")

plot.data <- p.index %>% 
  filter(iso %in% countries.to.plot) %>% 
  mutate(p.while.in.tip = ifelse(in.report == TRUE, p, NA))

plot.baselines <- plot.data %>%
  filter(year == min(year[in.report]))

year.criminalized <- read_stata("original_files/Criminalization Data UpdatedJK.dta") %>%
  mutate(iso = countrycode(ccode, "cown", "iso3c"),
         countryname = countrycode(iso, "iso3c", "country.name")) %>%
  filter(iso %in% countries.to.plot,
         adjcrimlevel > 0) %>%
  group_by(iso, crimlevel) %>% 
  arrange(desc(crimlevel)) %>%
  group_by(iso) %>%
  slice(1) %>%
  mutate(criminalization.type = factor(crimlevel, levels=c(1, 2),
                                       labels=c("Partial", "Full")))

# Cho Chang!  (⌐○Ϟ○)
fig.cho.changes <- ggplot(plot.data, aes(x=year)) + 
  geom_hline(data=plot.baselines, aes(yintercept=p.while.in.tip),
             size=0.25, color="grey50", linetype="dashed") + 
  geom_vline(data=year.criminalized, 
             aes(xintercept=year, linetype=criminalization.type),
             size=0.5, color="grey50") +
  geom_line(aes(y=p), size=0.75) +
  geom_line(aes(y=p.while.in.tip), size=0.75) + 
  labs(x=NULL, y="Anti-TIP policy index") + 
  scale_y_continuous(limits=c(0, 15)) + 
  scale_x_continuous(limits=c(2000, 2015)) + 
  scale_linetype_manual(values=c("dotted", "solid"), guide=FALSE) +
  facet_wrap(~ countryname, scales="free", ncol=3) + 
  theme_clean(10) + 
  theme(panel.grid.minor=element_blank(), strip.text=element_text(size=rel(0.8)))
# fig.cho.changes

# Plot average changes for 15 case study countries vs. all countries (without 
# 15 case study countries)
all.countries <- p.index %>% 
  filter(year != 2014, year != 1999) %>%
  filter(!(iso %in% countries.to.plot)) %>%
  group_by(year) %>% summarise(avg.all = mean(p, na.rm=TRUE))

just.cases <- plot.data %>%
  filter(year != 2014, year != 1999) %>%
  group_by(year) %>% summarise(avg.cases = mean(p, na.rm=TRUE))

plot.data <- all.countries %>% left_join(just.cases, by="year") %>%
  gather(Variable, Value, -year) %>%
  mutate(Variable = factor(Variable, levels=c("avg.cases", "avg.all"),
                           labels=c("Selected case countries    ", "All other countries"),
                           ordered=TRUE))

fig.cho.all.vs.cases <- ggplot(plot.data, 
                               aes(x=year, y=Value, colour=Variable)) + 
  geom_line(size=0.75) + 
  labs(x=NULL, y="Anti-TIP policy index") + 
  scale_colour_manual(values=c("grey30", "grey80"), name=NULL) +
  theme_clean(10) + theme(panel.grid.minor=element_blank(),
                          legend.key.size=unit(0.65, "lines"),
                          legend.key = element_blank(),
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))

# TODO: Verify measure of changes / time in report
# TODO: Save new variable to CSV/Stata


# ------------------------------------------------------
# What is the effect of criminalization on Cho scores?
# ------------------------------------------------------
# Not possible to subtract out criminalization element from Cho subcomponents, 
# since criminalizaiton policy falls under each of the 3 Ps (prosecution, 
# prevention, protection)

# Effect of criminalization on overall Cho score
cho.crim <- p.index %>% 
  group_by(criminalization.type) %>%
  summarise(avg.score = mean(p, na.rm=TRUE))

plot.cho.crim <- ggplot(cho.crim, aes(x=criminalization.type, y=avg.score)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Level of criminalization", y="Anti-TIP policy index") + 
  theme_clean(10) + theme(panel.grid.minor=element_blank())
plot.cho.crim

filename <- "cho_crim_full_score"
width <- 4.5
height <- 3
ggsave(plot.cho.crim, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(plot.cho.crim, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Effect of criminalization on each of the Cho subcomponents
cho.crim.sub <- p.index %>%
  gather(subcomponent.type, subcomponent.value,
         c(prosecution, prevention, protection)) %>%
  group_by(criminalization.type, subcomponent.type) %>%
  summarise(avg.score = mean(subcomponent.value, na.rm=TRUE))

plot.cho.crim.sub <- ggplot(cho.crim.sub, aes(x=subcomponent.type, y=avg.score, 
                                              fill=criminalization.type)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x="3P index subcomponent", y="Anti-TIP policy index") + 
  scale_fill_manual(values=c("black", "grey30", "grey80"),
                    name="Level of criminalization") +
  theme_clean(10) + theme(panel.grid.minor=element_blank(),
                          legend.key.size=unit(0.65, "lines"),
                          legend.key = element_blank(),
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))
plot.cho.crim.sub

filename <- "cho_crim_subcomponents"
width <- 4.5
height <- 3
ggsave(plot.cho.crim.sub, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(plot.cho.crim.sub, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

