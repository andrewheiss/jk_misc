# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(haven)
library(readr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(countrycode)
library(zoo)

source("shared_functions.R")

base.folder <- "final_figures/additional_figures"

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

years.criminalized.only <- year.criminalized.all %>%
  group_by(iso, crimlevel) %>%
  slice(1) %>%
  ungroup()

years.criminalized.only.wide <- years.criminalized.only %>%
  mutate(crimlevel = ifelse(crimlevel == 1, "year.partial", 
                            ifelse(crimlevel == 2, "year.full", 
                                   crimlevel))) %>%
  spread(crimlevel, year)

# Lots of countries partially or fully criminalized before 2000, so make a
# complete panel of all possible country years for calculating running years
all.country.years <- expand.grid(year = min(year.criminalized.all$year):2016,
                                 iso = unique(year.criminalized.all$iso),
                                 stringsAsFactors=FALSE)

# Calculate running tally of years since full criminalization
criminalizations.full <- all.country.years %>%
  left_join(filter(years.criminalized.only, crimlevel == 2),
            by=c("year", "iso")) %>%
  group_by(iso) %>%
  mutate(crimlevel = na.locf(crimlevel, na.rm=FALSE),  # Forward fill value
         crimlevel = ifelse(is.na(crimlevel), FALSE, TRUE),  # Replace NAs with FALSE
         years.since.full = lag(cumsum(crimlevel))) %>%  # Shift everything back
  rename(crim.full = crimlevel)

# Calculate running tally of years since partial criminalization
criminalizations.partial <- all.country.years %>%
  left_join(filter(years.criminalized.only, crimlevel == 1),
            by=c("year", "iso")) %>%
  group_by(iso) %>%
  mutate(crimlevel = na.locf(crimlevel, na.rm=FALSE),  # Forward fill value
         crimlevel = ifelse(is.na(crimlevel), FALSE, TRUE),  # Replace NAs with FALSE
         years.since.partial = lag(cumsum(crimlevel))) %>%  # Shift everything back
  rename(crim.partial = crimlevel)

# Combine everything into one dataframe of complete criminalization data
criminalizations.all <- criminalizations.full %>%
  left_join(criminalizations.partial, by=c("year", "iso")) %>%
  left_join(years.criminalized.only.wide, by="iso") %>%
  mutate(years.since.partial.reset = ifelse(years.since.full > 0, 
                                            as.integer(0), years.since.partial))


# Variables to make:
#   time.in.report = years present in report
#   change.since.entering = p_t - p_{entered report}
#   change/time
# TODO: Take missing values into account (like Afghanistan)
p.index <- cho.orig %>% left_join(year.joined, by="cow") %>%
  left_join(year.criminalized.all, by=c("iso", "year")) %>%
  left_join(criminalizations.all, by=c("iso", "year")) %>%
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
  mutate(criminalized = na.locf(crimlevel, na.rm=FALSE),
         criminalized = ifelse(is.na(criminalized), as.integer(0), criminalized),
         criminalization.type = factor(criminalized, levels=c(0, 1, 2),
                                       labels=c("None    ", "Partial    ", "Full"),
                                       ordered=TRUE)) %>%
  mutate(years.since.full.alt = year - year.full,
         years.since.partial.alt = year - year.partial) %>%
  ungroup()
saveRDS(p.index, "data/policy_index.rds")


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

year.criminalized.subset <- read_stata("original_files/Criminalization Data UpdatedJK.dta") %>%
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

crim.years.cases <- read_csv("data/criminalization_cases.csv") %>%
  gather(criminalization.type, year, -countryname) %>%
  filter(!is.na(year)) %>%
  mutate(criminalization.type = factor(criminalization.type,
                                       levels=c("Partial", "Full", "Strengthened"),
                                       ordered=TRUE))

# Cho Chang!  (⌐○Ϟ○)
fig.cho.changes <- ggplot(plot.data, aes(x=year)) + 
  geom_hline(data=plot.baselines, aes(yintercept=p.while.in.tip),
             size=0.25, color="grey50", linetype="dashed") + 
  geom_vline(data=crim.years.cases, 
             aes(xintercept=year, linetype=criminalization.type),
             size=0.5, color="grey50") +
  geom_line(aes(y=p), size=0.75) +
  geom_line(aes(y=p.while.in.tip), size=0.75) + 
  labs(x=NULL, y="Anti-TIP policy index") + 
  scale_y_continuous(limits=c(0, 15)) + 
  scale_x_continuous(limits=c(2000, 2015)) + 
  scale_linetype_manual(values=c("dotted", "solid", "longdash"), guide=FALSE) +
  facet_wrap(~ countryname, scales="free", ncol=3) + 
  theme_clean(10) + 
  theme(panel.grid.minor=element_blank(), strip.text=element_text(size=rel(0.8)))
fig.cho.changes

# Plot average changes for 15 case study countries vs. all countries (without 
# 15 case study countries)
all.countries <- p.index %>% 
  filter(year != 2014, year != 1999) %>%
  filter(!(iso %in% countries.to.plot)) %>%
  group_by(year) %>% summarise(avg.all = mean(p, na.rm=TRUE))

all.countries.not.always.tier1 <- p.index %>% 
  filter(year != 2014, year != 1999) %>%
  filter(!(iso %in% countries.to.plot)) %>%
  group_by(cow) %>% mutate(highest.tier = max(tier, na.rm=TRUE)) %>%
  ungroup() %>% filter(highest.tier > 1) %>%
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

plot.data.tier1.out <- all.countries.not.always.tier1 %>% 
  left_join(just.cases, by="year") %>%
  gather(Variable, Value, -year) %>%
  mutate(Variable = factor(Variable, levels=c("avg.cases", "avg.all"),
                           labels=c("Selected case countries    ", "All other countries"),
                           ordered=TRUE))

fig.cho.all.vs.cases.tier1.out <- ggplot(plot.data.tier1.out, 
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
  summarise(avg.score = mean(p, na.rm=TRUE),
            std.dev = sd(p, na.rm=TRUE),
            se = std.dev / sqrt(n()),
            upper = avg.score + (qnorm(0.975) * se),
            lower = avg.score + (qnorm(0.025) * se))

dodge <- position_dodge(width=0.9)
plot.cho.crim <- ggplot(cho.crim, aes(x=criminalization.type, y=avg.score)) +
  geom_bar(stat="identity", position=dodge) +
  geom_errorbar(aes(ymax=upper, ymin=lower), position=dodge, 
                width=0.25, colour="grey60", size=0.5) +
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
  summarise(avg.score = mean(subcomponent.value, na.rm=TRUE),
            std.dev = sd(subcomponent.value, na.rm=TRUE),
            se = std.dev / sqrt(n()),
            upper = avg.score + (qnorm(0.975) * se),
            lower = avg.score + (qnorm(0.025) * se))

dodge <- position_dodge(width=0.9)
plot.cho.crim.sub <- ggplot(cho.crim.sub, aes(x=subcomponent.type, y=avg.score, 
                                              fill=criminalization.type)) +
  geom_bar(stat="identity", position=dodge) +
  geom_errorbar(aes(ymax=upper, ymin=lower), position=dodge, 
                width=0.25, colour="grey60", size=0.5) +
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


# See the average change in the Cho index X years after criminalization
# Calculate the change in index since full criminalization
change.full <- p.index %>%
  filter(year > 1999, year.full > 1999) %>%
  mutate(change.since.full = p - p[crim.full & year == year.full],
         change.since.full = as.numeric(ifelse(crim.full, change.since.full, NA))) %>%
  select(year, cow, change.since.full)

# Calculate the change in index since partial criminalization
change.partial <- p.index %>%
  filter(year > 1999, year.partial > 1999) %>%
  mutate(change.since.partial = p - p[crim.partial & year == year.partial],
         change.since.partial = as.numeric(ifelse(crim.partial, change.since.partial, NA))) %>%
  select(year, cow, change.since.partial)

# Add changes to full dataframe
p.index.changes <- p.index %>%
  left_join(change.full, by=c("year", "cow")) %>%
  left_join(change.partial, by=c("year", "cow"))

# Summarize for plotting
df.years.since <- p.index.changes %>%
  group_by(years.since.full) %>%
  summarise(avg.change = mean(change.since.full, na.rm=TRUE),
            std.dev = sd(change.since.full, na.rm=TRUE),
            num = n(),
            se = std.dev / sqrt(num),
            upper = avg.change + (qnorm(0.975) * se),
            lower = avg.change + (qnorm(0.025) * se),
            lower.censored = ifelse(lower < 0, 0, lower)) %>%
  filter(years.since.full < 11, years.since.full > 0) %>%
  mutate(years.since.full = factor(years.since.full))

# Plot
dodge <- position_dodge(width=0.9)
plot.years.since <- ggplot(df.years.since, aes(x=years.since.full, y=avg.change)) +
  geom_bar(stat="identity", position=dodge) +
  geom_errorbar(aes(ymax=upper, ymin=lower.censored), position=dodge, 
                width=0.25, colour="grey60", size=0.5) +
  labs(x="Years since full TIP criminalization",
       y="Change in policy index since criminalization") + 
  theme_clean(10) + theme(panel.grid.minor=element_blank())
plot.years.since

filename <- "cho_years_since_full_crim"
width <- 4.5
height <- 3
ggsave(plot.years.since, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(plot.years.since, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# Remove criminalization part from Cho index
#
# According to the Cho codebook 
# (http://www.economics-human-trafficking.net/mediapool/99/998280/data/Coding_Guideline.pdf),
# legislation only influences the proseuction elemnt of the index; protection 
# reflects how well countries implement legislation for assisting victims, 
# while prevention reflects how well countries implement legislation for 
# preventing trafficking. Prosecution also includes non-legislative elements,
# but it's impossible to parse those out without recoding all the TIP reports
# for all countries in all years.
#
# So as a rough attempt to remove criminalization from the Cho index, we just
# remove the prosecution element and use a 0-10 scale instead of 0-15.

p.index.adjusted <- p.index %>%
  mutate(p.adj = prevention + protection)

# See the average change in the modified Cho index X years after criminalization
# Calculate the change in index since full criminalization
change.full <- p.index.adjusted %>%
  filter(year > 1999, year.full > 1999) %>%
  mutate(change.since.full = p.adj - p.adj[crim.full & year == year.full],
         change.since.full = as.numeric(ifelse(crim.full, change.since.full, NA))) %>%
  select(year, cow, change.since.full)

# Calculate the change in index since partial criminalization
change.partial <- p.index.adjusted %>%
  filter(year > 1999, year.partial > 1999) %>%
  mutate(change.since.partial = p.adj - p.adj[crim.partial & year == year.partial],
         change.since.partial = as.numeric(ifelse(crim.partial, change.since.partial, NA))) %>%
  select(year, cow, change.since.partial)

# Add changes to full dataframe
p.index.changes.adj <- p.index.adjusted %>%
  left_join(change.full, by=c("year", "cow")) %>%
  left_join(change.partial, by=c("year", "cow"))

# Summarize for plotting
df.years.since.adj <- p.index.changes.adj %>%
  group_by(years.since.full) %>%
  summarise(avg.change = mean(change.since.full, na.rm=TRUE),
            std.dev = sd(change.since.full, na.rm=TRUE),
            num = n(),
            se = std.dev / sqrt(num),
            upper = avg.change + (qnorm(0.975) * se),
            lower = avg.change + (qnorm(0.025) * se),
            lower.censored = ifelse(lower < 0, 0, lower)) %>%
  filter(years.since.full < 11, years.since.full > 0) %>%
  mutate(years.since.full = factor(years.since.full))

# Plot
dodge <- position_dodge(width=0.9)
plot.years.since.adj <- ggplot(df.years.since.adj, aes(x=years.since.full, y=avg.change)) +
  geom_bar(stat="identity", position=dodge) +
  geom_errorbar(aes(ymax=upper, ymin=lower.censored), position=dodge, 
                width=0.25, colour="grey60", size=0.5) +
  labs(x="Years since full TIP criminalization",
       y="Change in policy index since criminalization") + 
  theme_clean(10) + theme(panel.grid.minor=element_blank())
plot.years.since.adj

filename <- "cho_years_since_full_crim_adj"
width <- 4.5
height <- 3
ggsave(plot.years.since.adj, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(plot.years.since.adj, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# Avearge Cho scores for Full, partial, and no criminalization
countries.crim.categorized <- p.index.adjusted %>%
  group_by(cow) %>%
  mutate(crim.category = case_when(
    max(criminalized, na.rm=TRUE) == 2 ~ "Full",
    max(criminalized, na.rm=TRUE) == 1 ~ "Partial",
    max(criminalized, na.rm=TRUE) == 0 ~ "No"
  ))

countries.full.partial <- countries.crim.categorized %>%
  filter(crim.category == "Full") %>%
  # gather(since.type, years, c(years.since.full.alt, years.since.partial.alt)) %>%
  # filter(!(crim.category == "Full" & since.type == "years.since.partial.alt")) %>%
  # filter(!(crim.category == "Partial" & since.type == "years.since.full.alt")) %>%
  group_by(crim.category, years.since.full.alt) %>%
  summarise(avg.score = mean(p.adj, na.rm=TRUE),
            std.dev = sd(p.adj, na.rm=TRUE),
            num = n(),
            se = std.dev / sqrt(num),
            upper = avg.score + (qnorm(0.975) * se),
            lower = avg.score + (qnorm(0.025) * se),
            lower.censored = ifelse(lower < 0, 0, lower)) %>%
  filter(years.since.full.alt < 11, years.since.full.alt > -11) %>%
  ungroup() %>%
  mutate(years = years.since.full.alt,
         crim.category = "Countries with full criminalization")

# Get the average Cho score in the year of criminalization
# countries.crim.year0 <- countries.crim.categorized %>%
#   filter(crim.category != "No") %>%
#   filter(year == year.full | year == year.partial) %>%
#   gather(since.type, years, c(years.since.full, years.since.partial)) %>%
#   filter(!(crim.category == "Full" & since.type == "years.since.partial")) %>%
#   filter(!(crim.category == "Partial" & since.type == "years.since.full")) %>%
#   group_by(crim.category, since.type, years) %>%
#   summarise(avg.score = mean(p.adj, na.rm=TRUE),
#             std.dev = sd(p.adj, na.rm=TRUE),
#             num = n(),
#             se = std.dev / sqrt(num),
#             upper = avg.score + (qnorm(0.975) * se),
#             lower = avg.score + (qnorm(0.025) * se),
#             lower.censored = ifelse(lower < 0, 0, lower))

plot.years.since.full <- ggplot(countries.full.partial, 
                                aes(x=years, y=avg.score, 
                                    ymax=upper, ymin=lower)) +
  geom_rect(data=filter(countries.full.partial, years==0),
            aes(xmin=0, xmax=10, ymin=lower, ymax=upper),
            fill="grey50", alpha=0.3) +
  geom_segment(data=filter(countries.full.partial, years==0),
               aes(x=0, xend=10, y=avg.score, yend=avg.score),
               size=0.25) +
  geom_pointrange(size=0.25) +
  labs(x="Years since full TIP criminalization",
       y="Prevention and protection indexes") + 
  coord_cartesian(ylim=c(3, 8)) +
  theme_clean(8) + theme(panel.grid.minor=element_blank()) +
  facet_wrap(~ crim.category)
plot.years.since.full

plot.years.since.partial <- ggplot(filter(countries.full.partial, 
                                          crim.category=="Partial"), 
                                   aes(x=years, y=avg.score, 
                                       ymax=upper, ymin=lower)) +
  geom_rect(data=filter(countries.full.partial, 
                        crim.category=="Partial", years==0),
            aes(xmin=0, xmax=10, ymin=lower, ymax=upper),
            fill="grey50", alpha=0.3) +
  geom_segment(data=filter(countries.full.partial, 
                           crim.category=="Partial", years==0),
               aes(x=0, xend=10, y=avg.score, yend=avg.score),
               size=0.25) +
  geom_pointrange(size=0.25) +
  labs(x="Years since partial TIP criminalization",
       y="Prevention and protection indexes") + 
  coord_cartesian(ylim=c(3, 8)) +
  theme_clean(8) + theme(panel.grid.minor=element_blank()) +
  facet_wrap(~ crim.category)
plot.years.since.partial

countries.no.crim <- countries.crim.categorized %>%
  filter(crim.category != "Full") %>%
  group_by(year) %>%
  summarise(avg.score = mean(p.adj, na.rm=TRUE),
            std.dev = sd(p.adj, na.rm=TRUE),
            num = n(),
            se = std.dev / sqrt(num),
            upper = avg.score + (qnorm(0.975) * se),
            lower = avg.score + (qnorm(0.025) * se),
            lower.censored = ifelse(lower < 0, 0, lower)) %>%
  filter(year > 1999, year < 2014) %>%
  mutate(year = ymd(paste0(year, "-01-01")),
         crim.category = "Countries without full criminalization")

plot.countries.no.crim <- ggplot(countries.no.crim,
                                 aes(x=year, y=avg.score)) +
  geom_rect(data=slice(countries.no.crim, 1),
            aes(xmin=ymd("2000-01-01"), xmax=ymd("2013-01-01"),
                ymin=lower, ymax=upper),
            fill="grey50", alpha=0.3) +
  geom_segment(data=slice(countries.no.crim, 1),
               aes(x=ymd("2000-01-01"), xend=ymd("2013-01-01"),
                   y=avg.score, yend=avg.score),
               size=0.25) +
  geom_pointrange(aes(ymax=upper, ymin=lower.censored), size=0.25) +
  labs(x="Year", y="Prevention and protection indexes") + 
  coord_cartesian(ylim=c(3, 8)) +
  theme_clean(8) + theme(panel.grid.minor=element_blank()) + 
  facet_wrap(~ crim.category)
plot.countries.no.crim

# Blank plot for spacing things in arrangeGrob()
blank <- rectGrob(gp=gpar(col="white"))

plot.all.countries <- arrangeGrob(plot.years.since.full, blank,
                                  plot.countries.no.crim,
                                  ncol=1, heights=c(0.425, 0.05, 0.425))
grid::grid.draw(plot.all.countries)

cho.crim.all.countries <- plot.all.countries

filename <- "cho_all_countries_crim_type"
width <- 5.5
height <- 4.5
ggsave(plot.all.countries, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(plot.all.countries, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)
