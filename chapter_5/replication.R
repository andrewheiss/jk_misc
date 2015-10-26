library(dplyr)
library(haven)
library(survival)
library(stargazer)

df.orig <- read_dta("original_files/kelley_simmons_ajps_2014_replication.dta") %>%
  arrange(cowcode, year) %>%
  filter(year >= 1991) %>%
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
         logpop = log(data9),
         crim1 = ifelse(adjbicrimlevel == 0, 0, 1),
         uspressure = ifelse(tier %in% c(2.5, 3), 1, 0),
         loggdp = log(data2),
         econasstP = econasst * 1000000,
         econasstP = ifelse(econasstP == 0 | is.na(econasstP), 1, econasstP),
         logeconasstP = log(econasstP),
         notinreport = ifelse(tier != 555 | is.na(tier), 0, 1),
         special = ifelse(tier != 666 | is.na(tier), 0, 1),
         dostier = tier,
         # TODO: This notier variable is different in tables 1 and 3/4; in 1 it's just == 555, but in 3/4 it's == 555 and 666
         # notier = ifelse(tier != 555 | is.na(tier), 0, 1),
         notier = ifelse(!(tier %in% c(555, 666)) | is.na(tier), 0, 1),
         tier = ifelse(tier %in% c(555, 666) | is.na(tier), 0, tier))

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

# Average aid between 2001 and 2010
mean.aid <- df.orig %>%
  filter(year > 2000 & year < 2011) %>%
  group_by(name) %>%
  summarise(totalaidave = mean(econasst, na.rm=TRUE))

# Merge in grouped summary statistics
df.complete <- df.orig %>%
  left_join(mean.tiers, by="cowcode") %>%
  left_join(mean.aid, by="name") %>%
  left_join(regional.density, by=c("subregion", "year")) %>%
  # Set average aid to missing for years where it wasn't calculated
  mutate(totalaidave = ifelse(year <= 2000 | year > 2010, NA, totalaidave)) %>%
  # Correct the regional density variable
  mutate(corrected_regcrim = NA %>%
           ifelse(crim1 == 0, num_crim_1 / (num_states_in_subreg - 1), .) %>%
           ifelse(crim1 == 1, (num_crim_1 - 1) / (num_states_in_subreg - 1), .),
         corrected_regcrim100 = corrected_regcrim * 100) %>%
  select(-num_crim_1)

# Lag a bunch of stuff
df.complete.with.lags <- df.complete %>%
  arrange(name, year) %>%  # Necessary to make the wrong lagged variables
  # The lagged variables weren't done by country in original analysis, 
  # so values bleed into other countries. 
  # group_by(cowcode) %>%
  mutate(missinfo82_1 = lag(missinfo82, 1),
         missinfo82_2 = lag(missinfo82, 2),
         fullwaiver1 = lag(fullwaiver),
         notier1 = lag(notier),
         inreport1 = ifelse(notier1 == 0, 1, 0),
         logpop_1 = lag(logpop),
         missinfo8_1 = lag(missinfo8, 1),
         missinfo8_2 = lag(missinfo8, 2),
         fh_cl1 = lag(fh_cl),
         ratproto2000_1 = lag(ratproto2000),
         rule_of_law_1 = lag(rule_of_law),
         loggdp_1 = lag(loggdp),
         logeconasstP_1 = lag(logeconasstP),
         notinreport1 = lag(notinreport),
         special1 = lag(special),
         dostier1 = lag(dostier),
         tier1 = lag(tier),
         econasst1 = lag(econasst),
         loggdppercap_1 = log(lag(data4, 1)),
         loggdppercap_2 = log(lag(data4, 2)),
         #
         women1 = lag(women_par),
         protection1 = lag(protection),
         corruption_1 = lag(corruption)) %>%
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
         nocrimyrs = NA %>%
           as.numeric(ifelse(crim1_plus1 == 1, 0, .)) %>%
           as.numeric(ifelse(crim1_plus1 == 0, 1, .))) %>%
  # Fix miscellaneous lagged variables
  mutate(econasst1 = ifelse(econasst1 == 0 | is.na(econasst1), 0.001, econasst1),
         logeconasst1 = log(econasst1)) %>%
  # ungroup()
  # Create some more miscellaneous variables 
  mutate(chtier = tier - tier1,
         improve_tier = ifelse(chtier >= 0, 0, 1),
         worsen_tier = ifelse(chtier > 0, 1, 0),
         econasstPgdp = econasstP / data2,
         logeconasstPgdp = log(econasstPgdp)) %>%
  # More lagging...
  # group_by(cowcode) %>%
  mutate(chtier1 = lag(chtier),
         improve_tier1 = lag(improve_tier),
         worsen_tier1 = lag(worsen_tier),
         new_watch1 = lag(new_watch, 1),
         new_watch2 = lag(new_watch, 2),
         new_watch3 = lag(new_watch, 3),
         econasstPgdp_1 = lag(econasstPgdp),
         logeconasstPgdp_1 = lag(logeconasstPgdp)) %>%
  # ungroup()
  mutate(new_watch1 = ifelse(new_watch1 != 1 | is.na(new_watch1), 0, new_watch1),
         new_watch2 = ifelse(new_watch2 != 1 | is.na(new_watch2), 0, new_watch2),
         new_watch3 = ifelse(new_watch3 != 1 | is.na(new_watch3), 0, new_watch3))

# library(testthat)
# df.test <- readr::read_csv("~/Desktop/df_temp.csv") %>% 
#   arrange(cowcode, year)
# df.real <- df.complete.with.lags %>%
#   arrange(cowcode, year)
# View(df.real %>% select(name, year, contains("tier")))
# print(expect_equal(df.real$tier1_3, df.test$tier1_3))
# print(expect_equal(df.real$chtier1, df.test$chtier1))
# print(expect_equal(df.real$new_watch1, df.test$new_watch1))
# print(expect_equal(df.real$cowcode, df.test$cowcode))

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

df.hazardized <- df.complete.with.lags %>%
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

df.hazardized1 <- df.complete.with.lags %>%
  mutate(yr2fail2 = crim1date - year,
         yrfromj2 = year - 1991) %>%
  group_by(name) %>%
  mutate(endstate1 = mean(crim1, na.rm=TRUE),
         endstate1 = as.numeric(ifelse(endstate1 > 0, 1, 0)),
         fail = generate.failure(yr2fail2)) %>%
  filter(yr2fail2 >= 0 + is.na(yr2fail2)) %>%
  filter(yrfromj2 >= 0) %>%
  ungroup() %>%
  arrange(cowcode, year)

# ----------
# --------
# Models
# --------
# ----------
#
# ---------
# Table 1
# ---------
# Stata Cox proportional hazards models
# stset time, failure(fail)
# stcox var1 var2
# Stata uses Breslow for ties
#
# R Cox proportional hazards models
# library(survival)
# coxph(Surv(time, fail) ~ var1 + var2, ties="breslow")
#
# Useful documentation for the survival package: 
# http://socserv.socsci.mcmaster.ca/jfox/Books/Companion/appendix/Appendix-Cox-Regression.pdf
# Visualize things with:
# http://www.r-bloggers.com/showing-results-from-cox-proportional-hazard-models-in-r-with-simph/
#
# Robust SEs will be slightly off because of differences between R and Stata

# Prepare data for modeling
df.survivalized <- df.hazardized %>%
  filter(!is.na(yrfromj2)) %>%
  filter(year > 2000) %>%
  # Generate start time variable. 
  # Stata does this behind the scenes when running stset ..., id(name)
  # coxph needs an explicit column
  # See Q&A here: http://stats.stackexchange.com/q/177560/3025
  group_by(name) %>%
  mutate(start_time = 0:(n()-1))

# Models
model1.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 + 
                  cluster(name), 
                data=df.survivalized, ties="breslow") 
summary(model1.1)

model1.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 + 
                  ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                  cluster(name), 
                data=df.survivalized, ties="breslow")
summary(model1.2)

model1.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 + 
                  ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                  ht_incidence_origin + ht_incidence_transit + 
                  ht_incidence_destination + cluster(name), 
                data=df.survivalized, ties="breslow")
summary(model1.3)

# Pretty table
ses <- list(sqrt(diag(model1.1$var)), sqrt(diag(model1.2$var)), sqrt(diag(model1.3$var)))
stargazer(model1.1, model1.2, model1.3, type="text", apply.coef=exp, se=ses)
stargazer(model1.1, model1.2, model1.3, type="text")
# TODO: Make sure the *s, coefs, and SEs are all displayed correctly


# ---------
# Table 2
# ---------
model2.1 <- glm(uspressure ~ fh_cl1 + logeconasstP_1 + loggdp_1 + logpop + 
                  ratproto2000_1 + ngos_ave + corruption_1,
                data=df.complete.with.lags, 
                family=binomial(link="logit"))
summary(model2.1)

model2.2 <- glm(uspressure ~ fh_cl1 + logeconasstP_1 + loggdp_1 + logpop + 
                  ratproto2000_1 + ngos_ave + rule_of_law_1,
                data=df.complete.with.lags, 
                family=binomial(link="logit"))
summary(model2.2)

stargazer(model2.1, model2.2, apply.coef=exp, type="text")
# TODO: Make sure the *s, coefs, and SEs are all displayed correctly


# ---------
# Table 3
# ---------
# Prepare data for modeling
df.survivalized1 <- df.hazardized1 %>%
  filter(!is.na(yrfromj2)) %>%
  filter(year > 1999) %>%
  # Generate start time variable. 
  # Stata does this behind the scenes when running stset ..., id(name)
  # coxph needs an explicit column
  # See Q&A here: http://stats.stackexchange.com/q/177560/3025
  group_by(name) %>%
  mutate(start_time = 0:(n()-1),
         inreport1 = ifelse(notier1 == 0, 1, ifelse(notier1 == 1, 0, NA)))

# Models
model3.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + fh_cl1 + 
                    corrected_regcrim1_1 + ratproto2000_1 + missinfo8_2 + 
                    ht_incidence_origin + ht_incidence_transit + 
                    ht_incidence_destination + cluster(name) , 
                  data=df.survivalized1, ties="breslow") 
summary(model3.1)

# TODO: Check that new df vars are equal to Stata
# TODO: Figure out why models aren't replicating
# TODO: Check that tables 1 and 2 still work right
