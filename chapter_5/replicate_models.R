library(dplyr)
library(survival)
library(stargazer)

source("chapter_5/clean_data.R")

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
summary(model1.1)

model1.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                    cluster(name),
                  data=df.survivalized.report, ties="breslow")
summary(model1.2)

model1.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                    ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                    ht_incidence_origin + ht_incidence_transit +
                    ht_incidence_destination + cluster(name),
                  data=df.survivalized.report, ties="breslow")
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

# Pretty table
stargazer(model2.1, model2.2, apply.coef=exp, type="text")
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
summary(model3.1)

model3.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logpop_1 + ngos_ave + loggdppercap_1 + 
                    corruption_1 + cluster(name),
                  data=df.survivalized.crim, ties="breslow")
summary(model3.2)

model3.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logeconasstP_1 + cluster(name),
                  data=df.survivalized.crim, ties="breslow")
summary(model3.3)

model3.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logeconasstP_1 + test00 + cluster(name),
                  data=df.survivalized.crim, ties="breslow")
summary(model3.4)

model3.5 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + econasstPgdp_1_1000 + test_gdp_1_1000 + 
                    cluster(name),
                  data=df.survivalized.crim, ties="breslow")
summary(model3.5)

# Pretty table
stargazer(model3.1, model3.2, model3.3, model3.4, model3.5, 
          apply.coef=exp, type="text")
# TODO: Make sure the *s, coefs, and SEs are all displayed correctly


# ---------
# Table 4
# ---------
model4.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ tier1_1 + tier1_2 + 
                    tier1_25 + tier1_3 + women1 + fh_cl1 + corrected_regcrim1_1 +
                    ratproto2000_1 + missinfo8_2 + ht_incidence_origin + 
                    ht_incidence_transit + ht_incidence_destination + 
                    cluster(name),
                  data=df.survivalized.crim, ties="breslow")
summary(model4.1)

model4.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ tier1_1 + tier1_2 + 
                    tier1_25 + tier1_3 + women1 + fh_cl1 + corrected_regcrim1_1 +
                    ratproto2000_1 + missinfo8_2 + logpop_1 + ngos_ave + 
                    logeconasstP_1 + loggdppercap_1 + corruption_1 + 
                    cluster(name),
                  data=df.survivalized.crim, ties="breslow")
summary(model4.2)

model4.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ tier1_1 + tier1_2 + 
                    tier1_25 + tier1_3 + women1 + fh_cl1 + corrected_regcrim1_1 +
                    ratproto2000_1 + missinfo8_2 + cluster(name),
                  data=df.survivalized.crim, ties="breslow")
summary(model4.3)

model4.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + new_watch3 + 
                    new_watch2 + new_watch1 + women1 + fh_cl1 + 
                    corrected_regcrim1_1 + ratproto2000_1 + missinfo8_2 + 
                    cluster(name),
                  data=df.survivalized.crim, ties="breslow")
summary(model4.4)

# Pretty table
stargazer(model4.1, model4.2, model4.3, model4.4, 
          apply.coef=exp, type="text")
# TODO: Make sure the *s, coefs, and SEs are all displayed correctly


# ------------------
# Check everything
# ------------------
# Verify that all the models replicate correctly
source("chapter_5/tests/test-data.R")
source("chapter_5/tests/test-models.R")
