library(dplyr)
library(tidyr)
library(haven)
library(readr)
library(readxl)
library(countrycode)

# Load and clean original Stata file
df.orig <- read_dta("../original_files/kelley_simmons_ajps_2014_replication.dta") %>%
  arrange(cowcode, year) %>%
  filter(year >= 1991) %>%
  mutate(year.factor = as.factor(year)) %>%
  # Deal with missing human trafficking data
  mutate(ht_incidence_origin = ifelse(is.na(ht_incidence_origin) & 
                                        (!is.na(ht_incidence_transit) | 
                                           !is.na(ht_incidence_destination)), 
                                      0, ht_incidence_origin),
         ht_incidence_transit = ifelse(is.na(ht_incidence_transit) & 
                                         (!is.na(ht_incidence_origin) | 
                                            !is.na(ht_incidence_destination)), 
                                       0, ht_incidence_transit),
         ht_incidence_destination = ifelse(is.na(ht_incidence_destination) & 
                                             (!is.na(ht_incidence_origin) | 
                                                !is.na(ht_incidence_transit)), 
                                           0, ht_incidence_destination)) %>%
  # Create alternative missing information variable that doesn't use incidence data
  mutate(missinfo82 = is.na(fh_pr) + is.na(data8) + is.na(data7) + 
           is.na(data6) + is.na(data3) + is.na(agree3un) + is.na(corrupt)) %>%
  # Create miscellaneous variables
  mutate(tierX = ifelse(tier > 3, NA, tier),
         fullwaiver = ifelse(fullwaiver != 1 | is.na(fullwaiver), 0, 1),
         notinreport = ifelse(tier != 555 | is.na(tier), 0, 1),
         special = ifelse(tier != 666 | is.na(tier), 0, 1),
         dostier = tier,
         # notier is different in Tables 1/2 and 3/4; in 3/4, both 555 and 666
         # are coded as 1; in 1/2 just 555 is.
         # notier = ifelse(tier != 555 | is.na(tier), 0, 1),  # For 1/2
         # HOWEVER, it doesn't matter---Tables 1 and 2 replicate just fine 
         # using the notier variable for 3 and 4.
         notier = ifelse(!(tier %in% c(555, 666)) | is.na(tier), 0, 1),  # For 3/4
         tier = ifelse(tier %in% c(555, 666) | is.na(tier), 0, tier),
         logpop = log(data9),
         crim1 = ifelse(adjbicrimlevel == 0, 0, 1),
         uspressure = ifelse(tier %in% c(2.5, 3), 1, 0),
         loggdp = log(data2),
         econasst0 = ifelse(is.na(econasst), 0, econasst),
         econasstP = econasst * 1000000,
         econasstP = ifelse(econasstP == 0 | is.na(econasstP), 1, econasstP),
         logeconasstP = log(econasstP),
         totalfreedom = fh_pr + fh_cl)

# Calculate grouped summary statistics
# Generate the mean tier rating for each country and mark when it's always 1 or 3
mean.tiers <- df.orig %>%
  group_by(cowcode) %>%
  summarise(meantierX = mean(tierX, na.rm=TRUE)) %>%
  mutate(always1 = ifelse(meantierX == 1, 1, NA),
         always3 = ifelse(meantierX == 3, 1, NA))

# Generate regional density variable (first incorrectly, including the ith country)
regional.density <- df.orig %>%
  group_by(subregion, year) %>%
  summarise(regcrim = mean(crim1, na.rm=TRUE),
            num_states_in_subreg = n()) %>%
  mutate(num_crim_1 = num_states_in_subreg * regcrim)

# Average aid per country
econ.asst <- df.orig %>%
  filter(year > 2000 & year < 2011) %>%
  group_by(name) %>%
  summarise(totalaidave = mean(econasst0))


# Load new variables
funding.raw <- read_csv("../data/funding_clean.csv") %>%
  mutate(cowcode = ifelse(country == "Serbia", 555, cowcode),
         countryname = countrycode(cowcode, "cown", "country.name"),
         countryname = ifelse(cowcode == 555, "Serbia", countryname)) %>%
  filter(!is.na(countryname)) 

funding.all <- funding.raw %>%
  group_by(cowcode, grant_year) %>%
  summarise(total.funding = sum(amount, na.rm=TRUE),
            avg.funding = mean(amount, na.rm=TRUE)) %>%
  group_by(cowcode) %>%
  mutate(cum.funding = cumsum(total.funding))

funding.ngos <- funding.raw %>%
  filter(recipient_type %in% c("NGO", "NPO")) %>%
  group_by(cowcode, grant_year) %>%
  summarise(total.funding.ngos = sum(amount, na.rm=TRUE),
            avg.funding.ngos = mean(amount, na.rm=TRUE)) 

# TODO: Grant purpose?

cables.raw <- read_dta("../data/cables_panel.dta") %>%
  mutate(cow = as.numeric(cow)) %>%
  select(cow, year, prop_tip_wl, prop_tip_estimated)

ngo.count <- read_csv("../data/ngo_count.csv")


# Merge in grouped summary statistics and new variables
df.complete <- df.orig %>%
  left_join(funding.all, by=c("cowcode", "year"="grant_year")) %>%
  left_join(funding.ngos, by=c("cowcode", "year"="grant_year")) %>%
  left_join(cables.raw, by=c("cowcode"="cow", "year")) %>%
  left_join(ngo.count, by="cowcode") %>%
  left_join(mean.tiers, by="cowcode") %>%
  left_join(regional.density, by=c("subregion", "year")) %>%
  left_join(econ.asst, by=c("name")) %>%
  mutate(totalaidave = ifelse(year > 2000 & year < 2011, totalaidave, NA)) %>%
  # Correct the regional density variable
  mutate(corrected_regcrim = NA %>%
           ifelse(crim1 == 0, num_crim_1 / (num_states_in_subreg - 1), .) %>%
           ifelse(crim1 == 1, (num_crim_1 - 1) / (num_states_in_subreg - 1), .),
         corrected_regcrim100 = corrected_regcrim * 100) %>%
  select(-num_crim_1)

# Lag a bunch of stuff
df.complete.with.lags.orig <- df.complete %>%
  # The lagged variables weren't done by country in original analysis, 
  # so values bleed into other countries. 
  # group_by(un_ccode) %>%
  mutate(missinfo82_1 = lag(missinfo82, 1),
         missinfo82_2 = lag(missinfo82, 2),
         fullwaiver1 = lag(fullwaiver),
         notier1 = lag(notier),
         inreport1 = ifelse(notier1 == 0, 1, 0),
         notinreport1 = lag(notinreport),
         special1 = lag(special),
         dostier1 = lag(dostier),
         tier1 = lag(tier),
         logpop_1 = lag(logpop),
         missinfo8_1 = lag(missinfo8, 1),
         missinfo8_2 = lag(missinfo8, 2),
         fh_cl1 = lag(fh_cl),
         totalfreedom1 = lag(totalfreedom),
         ratproto2000_1 = lag(ratproto2000),
         rule_of_law_1 = lag(rule_of_law),
         corruption_1 = lag(corruption),
         loggdp_1 = lag(loggdp),
         econasst1 = lag(econasst),
         logeconasstP_1 = lag(logeconasstP),
         loggdppercap_1 = log(lag(data4, 1)),
         loggdppercap_2 = log(lag(data4, 2)),
         women1 = lag(women_par),
         fh_cl1 = lag(fh_cl),
         totalfreedom1 = lag(totalfreedom),
         protection_1 = lag(protection)) %>%
  # Deal with windowed tier variables
  mutate(tier1_1 = ifelse(tier1 != 1 | is.na(tier1), 0, 1),
         tier1_2 = ifelse(tier1 != 2 | is.na(tier1), 0, 1),
         tier1_25 = ifelse(tier1 != 2.5 | is.na(tier1), 0, 1),
         tier1_3 = ifelse(tier1 != 3 | is.na(tier1), 0, 1),
         waivers1_tier1_3 = tier1_3 * fullwaiver1) %>%
  # Deal with windowed criminalization variables
  mutate(crim1_minus1 = lag(crim1),
         crim1_plus1 = lead(crim1),
         corrected_regcrim1_1 = lag(corrected_regcrim, 1),
         corrected_regcrim1_2 = lag(corrected_regcrim, 2),
         # Convert 0/1 data to logical, negate it, and convert back to numeric
         nocrimyrs = as.numeric(!as.logical(crim1_plus1))) %>%
  # Fix miscellaneous lagged variables
  mutate(econasst1 = ifelse(econasst1 == 0 | is.na(econasst1), 0.001, econasst1),
         logeconasst1 = log(econasst1)) %>%
  # Create some more miscellaneous variables 
  mutate(chtier = tier - tier1,
         improve_tier = ifelse(chtier >= 0, 0, 1),
         worsen_tier = ifelse(chtier > 0, 1, 0),
         low_tier = ifelse(tier == 3, 1, 0),
         econasstPgdp = econasstP / data2,
         logeconasstPgdp = log(econasstPgdp)) %>%
  # More lagging...
  # group_by(un_ccode) %>%
  mutate(chtier1 = lag(chtier),
         improve_tier1 = lag(improve_tier),
         worsen_tier1 = lag(worsen_tier),
         low_tier1 = lag(low_tier),
         new_watch1 = lag(new_watch, 1),
         new_watch2 = lag(new_watch, 2),
         new_watch3 = lag(new_watch, 3),
         econasstPgdp_1 = lag(econasstPgdp),
         logeconasstPgdp_1 = lag(logeconasstPgdp)) %>%
  # ungroup()
  mutate(new_watch1 = ifelse(new_watch1 != 1 | is.na(new_watch1), 0, new_watch1),
         new_watch2 = ifelse(new_watch2 != 1 | is.na(new_watch2), 0, new_watch2),
         new_watch3 = ifelse(new_watch3 != 1 | is.na(new_watch3), 0, new_watch3))

# Lag a bunch of stuff
df.complete.with.lags.correct <- df.complete %>%
  # The lagged variables weren't done by country in original analysis, 
  # so values bleed into other countries. 
  group_by(un_ccode) %>%
  mutate(missinfo82_1 = lag(missinfo82, 1),
         missinfo82_2 = lag(missinfo82, 2),
         fullwaiver1 = lag(fullwaiver),
         notier1 = lag(notier),
         inreport1 = ifelse(notier1 == 0, 1, 0),
         notinreport1 = lag(notinreport),
         special1 = lag(special),
         dostier1 = lag(dostier),
         tier1 = lag(tier),
         logpop_1 = lag(logpop),
         missinfo8_1 = lag(missinfo8, 1),
         missinfo8_2 = lag(missinfo8, 2),
         fh_cl1 = lag(fh_cl),
         totalfreedom1 = lag(totalfreedom),
         ratproto2000_1 = lag(ratproto2000),
         rule_of_law_1 = lag(rule_of_law),
         corruption_1 = lag(corruption),
         loggdp_1 = lag(loggdp),
         econasst1 = lag(econasst),
         logeconasstP_1 = lag(logeconasstP),
         loggdppercap_1 = log(lag(data4, 1)),
         loggdppercap_2 = log(lag(data4, 2)),
         women1 = lag(women_par),
         fh_cl1 = lag(fh_cl),
         totalfreedom1 = lag(totalfreedom),
         protection_1 = lag(protection),
         totalaidave1 = lag(totalaidave),
         newus_tradeshare_gdp1 = lag(newus_tradeshare_gdp),
         ht_news_country1 = lag(ht_news_country)) %>%
  # Deal with new variables
  mutate(total.funding1 = lag(total.funding),
         log.total.funding1 = log1p(total.funding1),
         avg.funding1 = lag(avg.funding),
         cum.funding1 = lag(cum.funding),
         total.funding.ngos1 = lag(total.funding.ngos),
         avg.funding.ngos1 = lag(avg.funding.ngos),
         prop_tip_wl1 = lag(prop_tip_wl),
         prop_tip_estimated1 = lag(prop_tip_estimated)) %>%
  # Deal with windowed tier variables
  mutate(tier1_1 = ifelse(tier1 != 1 | is.na(tier1), 0, 1),
         tier1_2 = ifelse(tier1 != 2 | is.na(tier1), 0, 1),
         tier1_25 = ifelse(tier1 != 2.5 | is.na(tier1), 0, 1),
         tier1_3 = ifelse(tier1 != 3 | is.na(tier1), 0, 1),
         waivers1_tier1_3 = tier1_3 * fullwaiver1) %>%
  # Deal with windowed criminalization variables
  mutate(crim1_minus1 = lag(crim1),
         crim1_plus1 = lead(crim1),
         corrected_regcrim1_1 = lag(corrected_regcrim, 1),
         corrected_regcrim1_2 = lag(corrected_regcrim, 2),
         # Convert 0/1 data to logical, negate it, and convert back to numeric
         nocrimyrs = as.numeric(!as.logical(crim1_plus1))) %>%
  # Fix miscellaneous lagged variables
  mutate(econasst1 = ifelse(econasst1 == 0 | is.na(econasst1), 0.001, econasst1),
         logeconasst1 = log(econasst1)) %>%
  # Create some more miscellaneous variables 
  mutate(chtier = tier - tier1,
         improve_tier = ifelse(chtier >= 0, 0, 1),
         worsen_tier = ifelse(chtier > 0, 1, 0),
         low_tier = ifelse(tier %in% c(2.5, 3), 1, 0),
         econasstPgdp = econasstP / data2,
         logeconasstPgdp = log(econasstPgdp)) %>%
  # More lagging...
  mutate(chtier1 = lag(chtier),
         improve_tier1 = lag(improve_tier),
         worsen_tier1 = lag(worsen_tier),
         low_tier1 = lag(low_tier),
         new_watch1 = lag(new_watch, 1),
         new_watch2 = lag(new_watch, 2),
         new_watch3 = lag(new_watch, 3),
         econasstPgdp_1 = lag(econasstPgdp),
         logeconasstPgdp_1 = lag(logeconasstPgdp)) %>%
  mutate(new_watch1 = ifelse(new_watch1 != 1 | is.na(new_watch1), 0, new_watch1),
         new_watch2 = ifelse(new_watch2 != 1 | is.na(new_watch2), 0, new_watch2),
         new_watch3 = ifelse(new_watch3 != 1 | is.na(new_watch3), 0, new_watch3))

# library(testthat)
# print(expect_equal(df.complete.with.lags.orig$logpop_1,
#                    df.complete.with.lags.correct$logpop_1, tolerance=0.001,
#                    check.attributes = FALSE))

# --------------------------------------------------
# Generate speical variables for the hazard models
# --------------------------------------------------
# Given a sequence of years for a country, return 0 for all years before
# entering the report, 1 for the year added to the report, and NA for all
# years after.
#
# Example:
#   name  year yr2fail2  fail
#   Cuba  1997       NA     0
#   Cuba  1998       NA     0
#   Cuba  1999        4     0
#   Cuba  2000        3     0
#   Cuba  2001        2     0
#   Cuba  2002        1     0
#   Cuba  2003        0     1
#   Cuba  2004       -1    NA
#   Cuba  2005       -2    NA
#
generate.failure <- function(years.to.report) {
  started.index <- which(years.to.report == 0)
  if (length(started.index) > 0) {
    output <- c(rep(0, started.index - 1),  # A bunch of 0s
                1,  # Added to report here
                rep(NA, length(years.to.report) - started.index))  # A bunch of NAs
  } else {
    output <- rep(0, length(years.to.report))  # All 0s
  }
}

df.hazardized.report <- df.complete.with.lags.orig %>%
  mutate(yr2fail2 = tier_date - year,  # Years before "failing" (entering report)
         yrfromj2 = year - 2000) %>%  # Years since possible to be in report (post 2000)
  group_by(name) %>%
  mutate(endstate_inreport = mean(notier, na.rm=TRUE),
         endstate_inreport = ifelse(endstate_inreport > 0, 1, 0),
         fail = generate.failure(yr2fail2)) %>%
  filter(yr2fail2 >= 0 | is.na(yr2fail2)) %>%
  filter(yrfromj2 >= 0) %>%
  ungroup() %>%
  arrange(cowcode, year)

df.hazardized.crim <- df.complete.with.lags.orig %>%
  mutate(yr2fail2 = crim1date - year,  # Years before "failing" (criminalization)
         yrfromj2 = year - 1991) %>%  # Years since possible to be in report (post 1991)
  group_by(name) %>%
  mutate(endstate1 = mean(crim1, na.rm=TRUE),
         endstate1 = as.numeric(ifelse(endstate1 > 0, 1, 0)),
         fail = generate.failure(yr2fail2)) %>% 
  filter(yr2fail2 >= 0 | is.na(yr2fail2)) %>%
  filter(yrfromj2 >= 0) %>%
  ungroup() %>%
  arrange(cowcode, year)

# Generate start time variable.
# Stata does this behind the scenes when running stset ..., id(name)
# coxph needs an explicit column
# See Q&A here: http://stats.stackexchange.com/q/177560/3025
df.survivalized.report <- df.hazardized.report %>%
  filter(!is.na(yrfromj2)) %>%
  group_by(name) %>%
  mutate(start_time = lag(yrfromj2, default=0)) %>%
  filter(year > 2000)

df.survivalized.crim <- df.hazardized.crim %>%
  filter(!is.na(yrfromj2)) %>%
  mutate(inreport1 = as.numeric(!as.logical(notier1)),
         test00 = inreport1 * logeconasstP_1,
         econasstPgdp_1_1000 = econasstPgdp_1 * 1000,
         test_gdp_1_1000 = econasstPgdp_1_1000 * inreport1) %>%
  group_by(name) %>%
  mutate(start_time = lag(yrfromj2, default=0)) %>% 
  filter(year > 1999)


# Use correct lags
df.hazardized.report.correct <- df.complete.with.lags.correct %>%
  mutate(yr2fail2 = tier_date - year,  # Years before "failing" (entering report)
         yrfromj2 = year - 2000) %>%  # Years since possible to be in report (post 2000)
  group_by(name) %>%
  mutate(endstate_inreport = mean(notier, na.rm=TRUE),
         endstate_inreport = ifelse(endstate_inreport > 0, 1, 0),
         fail = generate.failure(yr2fail2)) %>%
  filter(yr2fail2 >= 0 | is.na(yr2fail2)) %>%
  filter(yrfromj2 >= 0) %>%
  ungroup() %>%
  arrange(cowcode, year)

df.hazardized.crim.correct <- df.complete.with.lags.correct %>%
  mutate(yr2fail2 = crim1date - year,  # Years before "failing" (criminalization)
         yrfromj2 = year - 1991) %>%  # Years since possible to be in report (post 1991)
  group_by(name) %>%
  mutate(endstate1 = mean(crim1, na.rm=TRUE),
         endstate1 = as.numeric(ifelse(endstate1 > 0, 1, 0)),
         fail = generate.failure(yr2fail2)) %>% 
  filter(yr2fail2 >= 0 | is.na(yr2fail2)) %>%
  filter(yrfromj2 >= 0) %>%
  ungroup() %>%
  arrange(cowcode, year)

calc.year <- function(chunk) {
  if (all(chunk$ratproto2000 == 0, na.rm=TRUE)) {
    rat_year <- NA
  } else {
    rat_year <- min(filter(chunk, ratproto2000 == 1)$year)
  }
  return(chunk %>% mutate(rat_year = rat_year))
}

df.ratproto <- df.complete.with.lags.correct %>%
  select(name, cowcode, year, ratproto2000) %>%
  group_by(cowcode) %>%
  do(calc.year(.)) %>%
  select(cowcode, year, rat_year)

df.hazardized.rat <- df.complete.with.lags.correct %>%
  left_join(df.ratproto, by=c("cowcode", "year")) %>%
  mutate(yr2fail2 = rat_year - year,  # Years before "failing" (criminalization)
         yrfromj2 = year - 2000) %>%  # Years since possible to ratify protocol (post 2000)
  group_by(name) %>%
  mutate(endstate1 = mean(crim1, na.rm=TRUE),
         endstate1 = as.numeric(ifelse(endstate1 > 0, 1, 0)),
         fail = generate.failure(yr2fail2)) %>% 
  filter(yr2fail2 >= 0 | is.na(yr2fail2)) %>%
  filter(yrfromj2 >= 0) %>%
  ungroup() %>%
  arrange(cowcode, year)

# Generate start time variable.
# Stata does this behind the scenes when running stset ..., id(name)
# coxph needs an explicit column
# See Q&A here: http://stats.stackexchange.com/q/177560/3025
df.survivalized.report.correct <- df.hazardized.report.correct %>%
  filter(!is.na(yrfromj2)) %>%
  group_by(name) %>%
  mutate(start_time = lag(yrfromj2, default=0)) %>%
  filter(year > 2000)

df.survivalized.crim.correct <- df.hazardized.crim.correct %>%
  filter(!is.na(yrfromj2)) %>%
  mutate(inreport1 = as.numeric(!as.logical(notier1)),
         test00 = inreport1 * logeconasstP_1,
         econasstPgdp_1_1000 = econasstPgdp_1 * 1000,
         test_gdp_1_1000 = econasstPgdp_1_1000 * inreport1) %>%
  group_by(name) %>%
  mutate(start_time = lag(yrfromj2, default=0)) %>% 
  filter(year > 1999)

df.survivalized.rat <- df.hazardized.rat %>%
  filter(!is.na(yrfromj2)) %>%
  group_by(name) %>%
  mutate(start_time = lag(yrfromj2, default=0)) %>%
  filter(year > 2000)

# Deal with reactions data
reactions <- read_dta("../original_files/mergedreaction8_new.dta") %>%
  left_join(select(df.complete, year, cowcode, tier=dostier, crim1, 
                   adjbicrimlevel, corrected_regcrim, corrected_regcrim100,
                   total.funding, avg.funding, total.funding.ngos, 
                   avg.funding.ngos, prop_tip_wl, prop_tip_estimated, ht_ngos), 
            by=c("year", "cowcode")) %>%
  # Clean up variables to account for missing tiers
  mutate(howimprove = ifelse(tier == 555, NA, howimprove),
         negative = ifelse(tier == 555, NA, negative),
         image = ifelse(tier == 555, NA, image),
         usarrogance = ifelse(tier == 555, NA, usarrogance),
         movinggoal = ifelse(tier == 555, NA, movinggoal),
         comparisonsratingnotfair = ifelse(tier == 555, NA, comparisonsratingnotfair),
         anger = ifelse(tier == 555, NA, anger),
         publicfacesaving = ifelse(tier == 555, NA, publicfacesaving),
         disappointment = ifelse(tier == 555, NA, disappointment),
         embassment = ifelse(tier == 555, NA, embassment),
         funding = ifelse(tier == 555, NA, funding),
         mediabacklash = ifelse(tier == 555, NA, mediabacklash),
         objection = ifelse(tier == 555, NA, objection),
         appreciation = ifelse(tier == 555, NA, appreciation),
         bragging = ifelse(tier == 555, NA, bragging),
         cooperative = ifelse(tier == 555, NA, cooperative),
         ignoring = ifelse(tier == 555, NA, ignoring),
         harmingrelations = ifelse(tier == 555, NA, harmingrelations)) %>%
  # Create new variable that doesn't use media reactions
  mutate(totalreactionnomedia = harmingrelations + cooperative + bragging +
           appreciation + objection + funding + howimprove + embassment + 
           disappointment + publicfacesaving + anger + comparisonsratingnotfair +
           movinggoal + usarrogance + negative,
         totalreactionnomedia = ifelse(is.na(totalreactionnomedia) & 
                                         year > 2000 & year < 2011, 
                                       0, totalreactionnomedia),
         totalreactionnomedia = ifelse(tier == 555 | is.na(tier), 
                                       NA, totalreactionnomedia),
         totalreactionnomedia = ifelse(year< 2001 | year > 2010, 
                                       NA, totalreactionnomedia)) %>%
  # New reaction without media
  mutate(reactionnomedia = ifelse(totalreactionnomedia > 0, 1, NA),
         reactionnomedia = ifelse(is.na(reactionnomedia) & !is.na(tier) & 
                                    tier != 555 & !is.na(totalreactionnomedia), 
                                  0, reactionnomedia),
         reactionnomedia = ifelse(year == 2011, NA, reactionnomedia)) %>%
  # Positive variable
  mutate(positive11 = ifelse(cooperative == 1 | appreciation == 1 | 
                               howimprove == 1, 1, 0),
         positive11 = ifelse(tier < 4 & is.na(positive11), 0, positive11)) %>%
  # Image variable
  mutate(image11 = ifelse(embassment == 1 | comparisonsratingnotfair == 1 | 
                            publicfacesaving == 1, 1, 0),
         image11 = ifelse(tier < 4 & is.na(image11), 0, image11)) %>%
  mutate(totalfreedom = fh_pr + fh_cl)

reactions.small <- reactions %>% 
  select(year, cowcode, totalreactionnomedia, reactionnomedia, aid, bigaid)
df.correct <- df.complete.with.lags.correct %>%
  left_join(reactions.small, by=c("year", "cowcode")) %>%
  group_by(cowcode) %>%
  mutate(totalreactionnomedia1 = lag(totalreactionnomedia),
         reactionnomedia1 = lag(reactionnomedia),
         bigaid1 = lag(bigaid))

df.table5 <- reactions %>%
  filter(lag(adjbicrimlevel) == 0 & year > 2001 & year < 2011) %>%
  group_by(cowcode) %>%
  mutate(L.reactionnomedia = lag(reactionnomedia),
         L.totalreactionnomedia = lag(totalreactionnomedia),
         L.women_par = lag(women_par),
         L.totalfreedom = lag(totalfreedom),
         L.adj_ratproto2000 = lag(adj_ratproto2000),
         L.bigaid = lag(bigaid),
         L.corrected_regcrim = lag(corrected_regcrim)) %>%
  mutate(total.funding1 = lag(total.funding),
         avg.funding1 = lag(avg.funding),
         total.funding.ngos1 = lag(total.funding.ngos),
         avg.funding.ngos1 = lag(avg.funding.ngos),
         prop_tip_wl1 = lag(prop_tip_wl),
         prop_tip_estimated1 = lag(prop_tip_estimated))
# # Democracy data
# polity.url <- "http://www.systemicpeace.org/inscr/p4v2014.xls"
# polity.tmp <- paste0(tempdir(), basename(polity.url))
# download.file(polity.url, polity.tmp)
# 
# uds.url <- "http://www.unified-democracy-scores.org/files/20140312/z/uds_summary.csv.gz"
# uds.tmp <- paste0(tempdir(), basename(uds.url))
# download.file(uds.url, uds.tmp, method="internal")
# 
# polity <- read_excel(polity.tmp) %>%
#   filter(year > 1989) %>%
#   select(cowcode = ccode, year, democ, autoc, polity2, durable)
# 
# uds <- read_csv(uds.tmp) %>%
#   filter(year > 1989)
# 
# colnames(reactions)
