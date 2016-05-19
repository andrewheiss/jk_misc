# Libraries
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(broom)
library(pander)
library(stargazer)
library(survival)
library(countrycode)
library(feather)


# Pandoc options
panderOptions('pandoc.binary', '/Users/andrew/.cabal/bin/pandoc')
panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', 50)
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

# Remove "non-robust" from logit notes, since there's no need to be robust anyway:
# http://davegiles.blogspot.com/2013/05/robust-standard-errors-for-nonlinear.html
# https://stat.ethz.ch/pipermail/r-help/2006-July/108722.html

# TODO: Write script to remove all rows with a colspan + add notes row?
# Or
# # /Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to odt --infilter="HTML" testing.html
# /Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to odt --infilter="HTML (StarWriter)" testing.html


# Locations
base.folder <- "final_tables"


# ------------------
# Useful functions
# ------------------

# Generate special variables for the hazard models
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

# Calculate a pseudo R-squared measure using
# 1-\frac{\text{Residual Deviance}}{\text{Null Deviance}}
calc.pseudo.r.squared <- function(x.model) {
  return(round(1 - (x.model$deviance / x.model$null.deviance), 4))
}

# Calculating odds ratios for logit coefficients is trivial: 
#  exp(coef(model))
#
# But calcuating the standard errors for those coefficients is tricker; just
# running exp() on the standard errors doesn't work. Stata automatically
# applies the delta method to odds ratio standard errors, but R doesn't.
# See also: https://www.stata.com/support/faqs/statistics/delta-rule/
#
# It's easy to use the delta method manually in R, though. Multiply the odds
# ratio (or gradient, technically) by the diagonal of the variance-covariance
# matrix by the gradient (again), or `or^2 * se.diag`
# See also: http://www.ats.ucla.edu/stat/r/faq/deltamethod.htm
#
get.or.se <- function(model) {
  tidy(model) %>% 
    mutate(or = exp(estimate),
           se.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * se.diag)) %>%
    select(or.se) %>% unlist %>% unname
}


# -----------------------
# Load and reshape data
# -----------------------
# Full, clean, properly lagged data
df.complete <- readRDS("final_tables/df_complete.rds") %>% ungroup()
df.robustness <- read_feather("data/robustness_df.feather")

df.complete <- df.complete %>% 
  left_join(df.robustness, by=c("year", "cowcode")) %>%
  mutate(tier_1 = ifelse(tier == 1, 1, 0),
         tier_2 = ifelse(tier == 2, 1, 0),
         tier_25 = ifelse(tier == 2.5, 1, 0),
         tier_3 = ifelse(tier == 3, 1, 0),
         pressure = ifelse(tier_25 == 1 | tier_3 == 1, 1, 0)) %>%
  group_by(cowcode) %>%
  mutate(pressure_lag = lag(pressure)) %>%
  ungroup()

ever.dac.eligible <- df.complete %>%
  filter(dac_eligible) %>%
  select(cowcode) %>% unique %>% unlist

df.dac.only <- df.complete %>%
  filter(cowcode %in% ever.dac.eligible)
  

# Generate variables for hazard models
df.hazardized.crim <- df.complete %>%
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

df.hazardized.crim.dac <- df.dac.only %>%
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

# Generate start time variables
df.survivalized.crim <- df.hazardized.crim %>%
  filter(!is.na(yrfromj2)) %>%
  mutate(inreport1 = as.numeric(!as.logical(notier1)),
         test00 = inreport1 * logeconasstP_1,
         econasstPgdp_1_1000 = econasstPgdp_1 * 1000,
         test_gdp_1_1000 = econasstPgdp_1_1000 * inreport1) %>%
  group_by(name) %>%
  mutate(start_time = lag(yrfromj2, default=0)) %>% 
  filter(year > 1999)

df.survivalized.crim.dac <- df.hazardized.crim.dac %>%
  filter(!is.na(yrfromj2)) %>%
  mutate(inreport1 = as.numeric(!as.logical(notier1)),
         test00 = inreport1 * logeconasstP_1,
         econasstPgdp_1_1000 = econasstPgdp_1 * 1000,
         test_gdp_1_1000 = econasstPgdp_1_1000 * inreport1) %>%
  group_by(name) %>%
  mutate(start_time = lag(yrfromj2, default=0)) %>% 
  filter(year > 1999)


# Table A6.1: Time to TIP criminalization
# (Model 3.4 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model6.1.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + logeconasstP_1 +
                      inreport1 * logeconasstP_1 + cluster(name),
                    data=df.survivalized.crim.dac, ties="efron")
model6.1.1.fit <- summary(survfit(model6.1.1))$table

# (Model 3.5 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model6.1.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + econasstPgdp_1_1000 +
                      econasstPgdp_1_1000 * inreport1 + cluster(name),
                    data=df.survivalized.crim.dac, ties="efron")
model6.1.2.fit <- summary(survfit(model6.1.2))$table

model6.1.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + newus_tradeshare_gdp1 +
                      newus_tradeshare_gdp1 * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.1.3.fit <- summary(survfit(model6.1.3))$table

model6.1.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + has.bit.sig.with.us_lag +
                      has.bit.sig.with.us_lag * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.1.4.fit <- summary(survfit(model6.1.4))$table

model6.1.5 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + us.military.aid.log_lag +
                      us.military.aid.log_lag * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.1.5.fit <- summary(survfit(model6.1.5))$table

model6.1.6 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + fdi.from.us.log_lag +
                      fdi.from.us.log_lag * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.1.6.fit <- summary(survfit(model6.1.6))$table

model6.1.7 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + trade.to.us.log_lag +
                      trade.to.us.log_lag * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.1.7.fit <- summary(survfit(model6.1.7))$table

model6.1.8 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + aid.us.total.perc_lag +
                      aid.us.total.perc_lag * inreport1 + cluster(name),
                    data=df.survivalized.crim.dac, ties="efron")
model6.1.8.fit <- summary(survfit(model6.1.8))$table


# Save table
var.labs <- c("In report", "Share of women in parliament", "Worse civil liberties",
              "Regional density of criminalization", "2000 TIP protocol ratification",
              "Missing information (t−2)",
              "US aid (logged)", "US aid × In report",
              "US aid as share of GDP (logged)",
              "US aid as share of GDP × In report",
              "US trade as share of GDP (logged)",
              "US trade as share of GDP (logged) × In report",
              "Has BIT with US",
              "Has BIT with US × In report",
              "US military aid (logged)",
              "US military aid (logged) × In report",
              "FDI from US (logged)",
              "FDI from US (logged) × In report",
              "Imports to US (logged)",
              "Imports to US (logged) × In report",
              "US aid as share of total aid (logged)",
              "US aid as share of total aid (logged) × In report")
col.labs <- c("Model 6.1.1", "Model 6.1.2", "Model 6.1.3", "Model 6.1.4",
              "Model 6.1.5", "Model 6.1.6", "Model 6.1.7", "Model 6.1.8")

ses <- list(get.or.se(model6.1.1), get.or.se(model6.1.2),
            get.or.se(model6.1.3), get.or.se(model6.1.4),
            get.or.se(model6.1.5), get.or.se(model6.1.6),
            get.or.se(model6.1.7), get.or.se(model6.1.8))

extra.lines <- list(c("Number of countries",
                      c(model6.1.1.fit["n.max"], model6.1.2.fit["n.max"],
                        model6.1.3.fit["n.max"], model6.1.4.fit["n.max"],
                        model6.1.5.fit["n.max"], model6.1.6.fit["n.max"],
                        model6.1.7.fit["n.max"], model6.1.8.fit["n.max"])),
                    c("Number of criminalizations",
                      c(model6.1.1.fit["events"], model6.1.2.fit["events"],
                        model6.1.3.fit["events"], model6.1.4.fit["events"],
                        model6.1.5.fit["events"], model6.1.6.fit["events"],
                        model6.1.7.fit["events"], model6.1.8.fit["events"])),
                    c("OECD DAC eligible countries only",
                      c(rep("Yes", 2), rep("No", 5), "Yes")))

title <- "Table A6.1: Time to TIP criminalization"
notes <- "Robust standard errors in parentheses; values differ from published article because of differences in the robustness algorithms Stata and R use. All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a6_1_new.html")

stargazer(model6.1.1, model6.1.2, model6.1.3, model6.1.4,
          model6.1.5, model6.1.6, model6.1.7, model6.1.8,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, se=ses, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs,
          dep.var.caption="Time to TIP criminalization",
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)



model6.1.1.pres <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + logeconasstP_1 +
                           pressure_lag * logeconasstP_1 + cluster(name),
                         data=df.survivalized.crim, ties="efron")
model6.1.1.pres.fit <- summary(survfit(model6.1.1.pres))$table

# (Model 3.5 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model6.1.2.pres <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + econasstPgdp_1_1000 +
                           econasstPgdp_1_1000 * pressure_lag + cluster(name),
                         data=df.survivalized.crim, ties="efron")
model6.1.2.pres.fit <- summary(survfit(model6.1.2.pres))$table

model6.1.3.pres <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + newus_tradeshare_gdp1 +
                           newus_tradeshare_gdp1 * pressure_lag + cluster(name),
                         data=df.survivalized.crim, ties="efron")
model6.1.3.pres.fit <- summary(survfit(model6.1.3.pres))$table

model6.1.4.pres <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + has.bit.sig.with.us_lag +
                           has.bit.sig.with.us_lag * pressure_lag + cluster(name),
                         data=df.survivalized.crim, ties="efron")
model6.1.4.pres.fit <- summary(survfit(model6.1.4.pres))$table

model6.1.5.pres <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + us.military.aid.log_lag +
                           us.military.aid.log_lag * pressure_lag + cluster(name),
                         data=df.survivalized.crim, ties="efron")
model6.1.5.pres.fit <- summary(survfit(model6.1.5.pres))$table

model6.1.6.pres <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + fdi.from.us.log_lag +
                           fdi.from.us.log_lag * pressure_lag + cluster(name),
                         data=df.survivalized.crim, ties="efron")
model6.1.6.pres.fit <- summary(survfit(model6.1.6.pres))$table

model6.1.7.pres <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + trade.to.us.log_lag +
                           trade.to.us.log_lag * pressure_lag + cluster(name),
                         data=df.survivalized.crim, ties="efron")
model6.1.7.pres.fit <- summary(survfit(model6.1.7.pres))$table

asdf <- select(df.survivalized.crim, start_time, yrfromj2, fail, pressure_lag, women1, 
            fh_cl1, corrected_regcrim1_1, ratproto2000_1, 
            missinfo8_2, trade.to.us.log_lag)

qwer <- complete.cases(asdf)

sum(is.na(imports.us$amount))

model6.1.8.pres <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + aid.us.total.perc_lag +
                           aid.us.total.perc_lag * pressure_lag + cluster(name),
                         data=df.survivalized.crim, ties="efron")
model6.1.8.pres.fit <- summary(survfit(model6.1.8.pres))$table


# Save table
var.labs <- c("US pressure", "Share of women in parliament", "Worse civil liberties",
              "Regional density of criminalization", "2000 TIP protocol ratification",
              "Missing information (t−2)",
              "US aid (logged)", "US aid × US pressure",
              "US aid as share of GDP (logged)",
              "US aid as share of GDP × US pressure",
              "US trade as share of GDP (logged)",
              "US trade as share of GDP (logged) × US pressure",
              "Has BIT with US",
              "Has BIT with US × US pressure",
              "US military aid (logged)",
              "US military aid (logged) × US pressure",
              "FDI from US (logged)",
              "FDI from US (logged) × US pressure",
              "Imports to US (logged)",
              "Imports to US (logged) × US pressure",
              "US aid as share of total aid (logged)",
              "US aid as share of total aid (logged) × US pressure")
col.labs <- c("Model 6.1.1", "Model 6.1.2", "Model 6.1.3", "Model 6.1.4")

ses <- list(get.or.se(model6.1.1.pres), get.or.se(model6.1.2.pres),
            get.or.se(model6.1.3.pres), get.or.se(model6.1.4.pres),
            get.or.se(model6.1.5.pres), get.or.se(model6.1.6.pres),
            get.or.se(model6.1.7.pres), get.or.se(model6.1.8.pres))

extra.lines <- list(c("Number of countries",
                      c(model6.1.1.pres.fit["n.max"], model6.1.2.pres.fit["n.max"],
                        model6.1.3.pres.fit["n.max"], model6.1.4.pres.fit["n.max"],
                        model6.1.5.pres.fit["n.max"], model6.1.6.pres.fit["n.max"],
                        model6.1.7.pres.fit["n.max"], model6.1.8.pres.fit["n.max"])),
                    c("Number of criminalizations",
                      c(model6.1.1.pres.fit["events"], model6.1.2.pres.fit["events"],
                        model6.1.3.pres.fit["events"], model6.1.4.pres.fit["events"],
                        model6.1.5.pres.fit["events"], model6.1.6.pres.fit["events"],
                        model6.1.7.pres.fit["events"], model6.1.8.pres.fit["events"])),
                    c("OECD DAC eligible countries only",
                      c(rep("Yes", 2), rep("No", 5), "Yes")))

title <- "Table A6.1: Time to TIP criminalization"
notes <- "Robust standard errors in parentheses; values differ from published article because of differences in the robustness algorithms Stata and R use. All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a6_1_new_pressure.html")

stargazer(model6.1.1.pres, model6.1.2.pres, model6.1.3.pres, model6.1.4.pres,
          model6.1.5.pres, model6.1.6.pres, model6.1.7.pres, model6.1.8.pres,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, se=ses, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs,
          dep.var.caption="Time to TIP criminalization",
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)
