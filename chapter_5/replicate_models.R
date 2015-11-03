library(dplyr)
library(broom)
library(survival)
# library(stargazer)

source("clean_data.R")

# Calculate a pseudo R-squared measure using
# 1-\frac{\text{Residual Deviance}}{\text{Null Deviance}}
calc.pseudo.r.squared <- function(x.model) {
  return(round(1 - (x.model$deviance / x.model$null.deviance), 4))
}

# Format pretty in-text parenthetical statistics and p-values
# Example:
#   get.parenthetical.stats(model5.8, "L.totalfreedom", "z")
#   # [1] "z = -0.778, p = 0.437"
#
# Example in R Markdown:
#   Lorem ipsum dolor sit amet (`r I(get.parenthetical.stats(model, "x1", "t"))`).
#
#   Lorem ipsum dolor sit amet (t = 5.34, p < 0.001).
#
get.parenthetical.stats <- function(model, variable, statistic) {
  df <- model %>% tidy() %>%
    filter(term == variable) %>%
    select(statistic, p.value)
  
  if (df$p.value > 0.001) {
    out <- sprintf("%s = %.3f, p = %.3f", statistic, df$statistic, df$p.value)
  } else {
    out <- sprintf("%s = %.3f, p < 0.001", statistic, df$statistic)
  }
  
  return(out)
}

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


# Models
model1.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    cluster(name),
                  data=df.survivalized.report, ties="breslow")
# summary(model1.1)
model1.1.fit <- summary(survfit(model1.1))$table

model1.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                    cluster(name),
                  data=df.survivalized.report, ties="breslow")
# summary(model1.2)
model1.2.fit <- summary(survfit(model1.2))$table

model1.2.good <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                    cluster(name),
                  data=df.survivalized.report, ties="efron")
# summary(model1.2.better)
model1.2.good.fit <- summary(survfit(model1.2.good))$table

model1.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                    ht_incidence_origin + ht_incidence_transit +
                    ht_incidence_destination + cluster(name),
                  data=df.survivalized.report, ties="breslow")
# summary(model1.3)
model1.3.fit <- summary(survfit(model1.3))$table

model1.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                    ht_incidence_origin + ht_incidence_transit +
                    ht_incidence_destination + total.funding1 + cluster(name),
                  data=df.survivalized.report.correct, ties="efron")
summary(model1.4)
model1.4.fit <- summary(survfit(model1.4))$table

model1.5 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                    ht_incidence_origin + ht_incidence_transit +
                    ht_incidence_destination + prop_tip_wl1 + cluster(name),
                  data=df.survivalized.report.correct, ties="efron")
summary(model1.5)
model1.5.fit <- summary(survfit(model1.5))$table

model1.6 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                    ht_incidence_origin + ht_incidence_transit +
                    ht_incidence_destination + prop_tip_estimated1 + cluster(name),
                  data=df.survivalized.report.correct, ties="efron")
summary(model1.6)
model1.6.fit <- summary(survfit(model1.6))$table

model1.7 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                    ht_incidence_origin + ht_incidence_transit +
                    ht_incidence_destination + ht_ngos + cluster(name),
                  data=df.survivalized.report.correct, ties="efron")
summary(model1.7)
model1.7.fit <- summary(survfit(model1.7))$table


# ---------
# Table 2
# ---------
model2.1 <- glm(uspressure ~ fh_cl1 + logeconasstP_1 + loggdp_1 + logpop +
                  ratproto2000_1 + ngos_ave + corruption_1,
                data=df.complete.with.lags.orig,
                family=binomial(link="logit"))
# summary(model2.1)

model2.2 <- glm(uspressure ~ fh_cl1 + logeconasstP_1 + loggdp_1 + logpop +
                  ratproto2000_1 + ngos_ave + rule_of_law_1,
                data=df.complete.with.lags.orig,
                family=binomial(link="logit"))
# summary(model2.2)

model2.1.good <- glm(uspressure ~ fh_cl1 + logeconasstP_1 + loggdp_1 + logpop +
                  ratproto2000_1 + ngos_ave + corruption_1,
                data=df.complete.with.lags.correct,
                family=binomial(link="logit"))
# summary(model2.1.good)

model2.2.good <- glm(uspressure ~ fh_cl1 + logeconasstP_1 + loggdp_1 + logpop +
                  ratproto2000_1 + ngos_ave + rule_of_law_1,
                data=df.complete.with.lags.correct,
                family=binomial(link="logit"))
# summary(model2.2.good)

# Pretty table
# stargazer(model2.1, model2.2, apply.coef=exp, type="text")
# TODO: Make sure the *s, coefs, and SEs are all displayed correctly


# ---------
# Table 3
# ---------
model3.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + ht_incidence_origin + 
                    ht_incidence_transit + ht_incidence_destination + 
                    cluster(name),
                  data=df.survivalized.crim, ties="breslow")
# summary(model3.1)
model3.1.fit <- summary(survfit(model3.1))$table

model3.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logpop_1 + ngos_ave + loggdppercap_1 + 
                    corruption_1 + cluster(name),
                  data=df.survivalized.crim, ties="breslow")
# summary(model3.2)
model3.2.fit <- summary(survfit(model3.2))$table

model3.2.good <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logpop_1 + ngos_ave + loggdppercap_1 + 
                    corruption_1 + cluster(name),
                  data=df.survivalized.crim.correct, ties="breslow")
# summary(model3.2.good)
model3.2.good.fit <- summary(survfit(model3.2.good))$table

model3.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logeconasstP_1 + cluster(name),
                  data=df.survivalized.crim, ties="breslow")
# summary(model3.3)
model3.3.fit <- summary(survfit(model3.3))$table

model3.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logeconasstP_1 + test00 + cluster(name),
                  data=df.survivalized.crim, ties="breslow")
# summary(model3.4)
model3.4.fit <- summary(survfit(model3.4))$table

model3.5 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + econasstPgdp_1_1000 + test_gdp_1_1000 + 
                    cluster(name),
                  data=df.survivalized.crim, ties="breslow")
# summary(model3.5)
model3.5.fit <- summary(survfit(model3.5))$table

model3.5.good <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + econasstPgdp_1_1000 + test_gdp_1_1000 + 
                    cluster(name),
                  data=df.survivalized.crim.correct, ties="breslow")
# summary(model3.5)
model3.5.good.fit <- summary(survfit(model3.5.good))$table

model3.6 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logpop_1 + ngos_ave + loggdppercap_1 + 
                    corruption_1 + total.funding1 + cluster(name),
                  data=df.survivalized.crim.correct, ties="efron")
# summary(model3.6)
model3.6.fit <- summary(survfit(model3.6))$table

model3.7 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logpop_1 + ngos_ave + loggdppercap_1 + 
                    corruption_1 + prop_tip_wl1 + cluster(name),
                  data=df.survivalized.crim.correct, ties="efron")
# summary(model3.7)
model3.7.fit <- summary(survfit(model3.7))$table

model3.8 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logpop_1 + ngos_ave + loggdppercap_1 + 
                    corruption_1 + prop_tip_estimated1 + cluster(name),
                  data=df.survivalized.crim.correct, ties="efron")
# summary(model3.8)
model3.8.fit <- summary(survfit(model3.8))$table

model3.9 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logpop_1 + loggdppercap_1 + 
                    corruption_1 + ht_ngos + cluster(name),
                  data=df.survivalized.crim.correct, ties="efron")
# summary(model3.9)
model3.9.fit <- summary(survfit(model3.9))$table


# ---------
# Table 4
# ---------
model4.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ tier1_1 + tier1_2 + 
                    tier1_25 + tier1_3 + women1 + fh_cl1 + corrected_regcrim1_1 +
                    ratproto2000_1 + missinfo8_2 + ht_incidence_origin + 
                    ht_incidence_transit + ht_incidence_destination + 
                    cluster(name),
                  data=df.survivalized.crim, ties="breslow")
# summary(model4.1)
model4.1.fit <- summary(survfit(model4.1))$table

model4.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ tier1_1 + tier1_2 + 
                    tier1_25 + tier1_3 + women1 + fh_cl1 + corrected_regcrim1_1 +
                    ratproto2000_1 + missinfo8_2 + logpop_1 + ngos_ave + 
                    logeconasstP_1 + loggdppercap_1 + corruption_1 + 
                    cluster(name),
                  data=df.survivalized.crim, ties="breslow")
# summary(model4.2)
model4.2.fit <- summary(survfit(model4.2))$table

model4.2.good <- coxph(Surv(start_time, yrfromj2, fail) ~ tier1_1 + tier1_2 + 
                    tier1_25 + tier1_3 + women1 + fh_cl1 + corrected_regcrim1_1 +
                    ratproto2000_1 + missinfo8_2 + logpop_1 + ngos_ave + 
                    logeconasstP_1 + loggdppercap_1 + corruption_1 + 
                    cluster(name),
                  data=df.survivalized.crim.correct, ties="breslow")
# summary(model4.2)
model4.2.good.fit <- summary(survfit(model4.2.good))$table

model4.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ tier1_1 + tier1_2 + 
                    tier1_25 + tier1_3 + women1 + fh_cl1 + corrected_regcrim1_1 +
                    ratproto2000_1 + missinfo8_2 + cluster(name),
                  data=df.survivalized.crim, ties="breslow")
# summary(model4.3)
model4.3.fit <- summary(survfit(model4.3))$table

model4.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + new_watch3 + 
                    new_watch2 + new_watch1 + women1 + fh_cl1 + 
                    corrected_regcrim1_1 + ratproto2000_1 + missinfo8_2 + 
                    cluster(name),
                  data=df.survivalized.crim, ties="breslow")
# summary(model4.4)
model4.4.fit <- summary(survfit(model4.4))$table

model4.4.good <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + new_watch3 + 
                    new_watch2 + new_watch1 + women1 + fh_cl1 + 
                    corrected_regcrim1_1 + ratproto2000_1 + missinfo8_2 + 
                    cluster(name),
                  data=df.survivalized.crim.correct, ties="breslow")
# summary(model4.4)
model4.4.good.fit <- summary(survfit(model4.4.good))$table

# Pretty table
# stargazer(model4.1, model4.2, model4.3, model4.4, 
#           apply.coef=exp, type="text")
# TODO: Make sure the *s, coefs, and SEs are all displayed correctly


# ------------------
# Check everything
# ------------------
# Verify that all the models replicate correctly
# source("chapter_5/tests/test-data.R")
# source("chapter_5/tests/test-models.R")


# Table 5
# Models of criminalization
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

model5.1 <- glm(crim1 ~ L.reactionnomedia + L.women_par + L.totalfreedom + 
                  L.adj_ratproto2000 + L.bigaid + as.factor(year),
                data=df.table5,
                family=binomial(link="logit"))
# summary(model5.1)

model5.2 <- glm(crim1 ~ L.reactionnomedia + L.women_par + L.totalfreedom + 
                  L.adj_ratproto2000 + L.bigaid + L.corrected_regcrim + as.factor(year),
                data=df.table5,
                family=binomial(link="logit"))
# summary(model5.2)

model5.3 <- glm(crim1 ~ L.totalreactionnomedia + L.women_par + L.totalfreedom + 
                  L.adj_ratproto2000 + L.bigaid + as.factor(year),
                data=df.table5,
                family=binomial(link="logit"))
# summary(model5.3)

model5.4 <- glm(crim1 ~ L.totalreactionnomedia + L.women_par + L.totalfreedom + 
                  L.adj_ratproto2000 + L.bigaid + L.corrected_regcrim + as.factor(year),
                data=df.table5,
                family=binomial(link="logit"))
# summary(model5.4)

model5.5 <- glm(crim1 ~ L.totalreactionnomedia + L.women_par + L.totalfreedom + 
                  L.adj_ratproto2000 + L.bigaid + L.corrected_regcrim + 
                  total.funding1 + as.factor(year),
                data=df.table5,
                family=binomial(link="logit"))
# summary(model5.5)

model5.6 <- glm(crim1 ~ L.totalreactionnomedia + L.women_par + L.totalfreedom + 
                  L.adj_ratproto2000 + L.bigaid + L.corrected_regcrim + 
                  prop_tip_wl1 + as.factor(year),
                data=df.table5,
                family=binomial(link="logit"))
# summary(model5.6)

model5.7 <- glm(crim1 ~ L.totalreactionnomedia + L.women_par + L.totalfreedom + 
                  L.adj_ratproto2000 + L.bigaid + L.corrected_regcrim + 
                  prop_tip_estimated1 + as.factor(year),
                data=df.table5,
                family=binomial(link="logit"))
# summary(model5.7)

model5.8 <- glm(crim1 ~ L.totalreactionnomedia + L.women_par + L.totalfreedom + 
                  L.adj_ratproto2000 + L.bigaid + L.corrected_regcrim + 
                  ht_ngos + as.factor(year),
                data=df.table5,
                family=binomial(link="logit"))
# summary(model5.8)
