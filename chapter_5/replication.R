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
         notier = ifelse(tier != 555 | is.na(tier), 0, 1),
         logpop = log(data9),
         crim1 = ifelse(adjbicrimlevel == 0, 0, 1),
         uspressure = ifelse(tier %in% c(2.5, 3), 1, 0),
         loggdp = log(data2),
         econasstP = econasst * 1000000,
         econasstP = ifelse(econasstP == 0 | is.na(econasstP), 1, econasstP),
         logeconasstP = log(econasstP))

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

# Merge in grouped summary statistics
df.complete <- df.orig %>%
  left_join(mean.tiers, by="cowcode") %>%
  left_join(regional.density, by=c("subregion", "year")) %>%
  # Correct the regional density variable
  mutate(corrected_regcrim = NA %>%
           ifelse(crim1 == 0, num_crim_1 / (num_states_in_subreg - 1), .) %>%
           ifelse(crim1 == 1, (num_crim_1 - 1) / (num_states_in_subreg - 1), .),
         corrected_regcrim100 = corrected_regcrim * 100) %>%
  select(-num_crim_1)

# Lag a bunch of stuff
df.complete.with.lags <- df.complete %>%
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
         corruption_1 = lag(corruption),
         loggdp_1 = lag(loggdp),
         logeconasstP_1 = lag(logeconasstP)) %>%
  # Deal with windowed criminalization variables
  mutate(crim1_minus1 = lag(crim1),
         crim1_plus1 = lead(crim1),
         corrected_regcrim1_1 = lag(corrected_regcrim, 1),
         corrected_regcrim1_2 = lag(corrected_regcrim, 2),
         nocrimyrs = NA %>%
           as.numeric(ifelse(crim1_plus1 == 1, 0, .)) %>%
           as.numeric(ifelse(crim1_plus1 == 0, 1, .))) #%>%
  # ungroup()

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
