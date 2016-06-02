# Libraries
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(feather)
library(broom)
library(pander)
library(stargazer)
library(survival)
library(countrycode)
library(ggplot2)
library(scales)

source("shared_functions.R")


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

cases <- c("ARM", "IDN", "ECU", "MOZ", "KAZ", "ARG", "ISR", 
           "ARE", "NGA", "OMN", "HND", "JPN", "TCD", "ZWE", "MYS")

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
df.complete.orig <- readRDS("final_tables/df_complete.rds")
df.robustness <- read_feather("data/robustness_df.feather")

df.complete <- df.complete.orig %>% 
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

df.reactions.small <- readRDS("final_tables/df_reactions_small.rds")

df.reactions <- df.complete %>%
  left_join(df.reactions.small, by=c("year", "cowcode")) %>%
  group_by(cowcode) %>%
  mutate(totalreactionnomedia1 = lag(totalreactionnomedia),
         reactionnomedia1 = lag(reactionnomedia),
         bigaid1 = lag(bigaid),
         loght_news_country1 = lag(loght_news_country),
         adj_ratproto2000_1 = lag(adj_ratproto2000)) %>%
  filter(cowcode != 2)

# Generate variables for hazard models
df.hazardized.report <- df.complete %>%
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

df.survivalized.crim.dac <- df.hazardized.crim.dac %>%
  filter(!is.na(yrfromj2)) %>%
  mutate(inreport1 = as.numeric(!as.logical(notier1)),
         test00 = inreport1 * logeconasstP_1,
         econasstPgdp_1_1000 = econasstPgdp_1 * 1000,
         test_gdp_1_1000 = econasstPgdp_1_1000 * inreport1) %>%
  group_by(name) %>%
  mutate(start_time = lag(yrfromj2, default=0)) %>% 
  filter(year > 1999)


# ------------------
# Methods appendix
# ------------------
df.methods.summary <- df.complete %>%
  filter(year >= 2000) %>%
  mutate(tier = ifelse(tier == 0, NA, tier),
         percapeconasst0 = econasst0 / data9,
         case = cowcode %in% countrycode(cases, "iso3c", "cown")) %>%
  filter(tier < 4 & !is.na(tier)) %>%
  group_by(cowcode) %>% mutate(highest.tier = max(tier, na.rm=TRUE)) %>%
  ungroup()


run.t.test <- function(variable, title, single.year=NA, tier1.exclude=FALSE) {
  if (!is.na(single.year)) {
    df <- df.methods.summary %>% filter(year == single.year)
  } else {
    df <- df.methods.summary
  }
  
  if (tier1.exclude) {
    df <- df %>% filter(highest.tier != 1)
  }
  
  result <- t.test(as.formula(paste0(variable, " ~ case")),
                   data=df, var.equal=TRUE)
  data_frame(Statistic = title,
             `Case study countries` = comma(result$estimate[2], digits=3),
             `Other countries` = comma(result$estimate[1], digits=3),
             `Difference` = comma(result$estimate[2] - 
                                        result$estimate[1], digits=3),
             `Significant difference at p = 0.05` = 
               ifelse(result$p.value < 0.05, "Yes", "No"))
}


final <- bind_rows(
  run.t.test("tier", title="Tier"),
  run.t.test("ngos_ave", title="Count of NGOs", 
             single.year=2011),
  run.t.test("igos_ave", title="Count of IGOs", 
             single.year=2011),
  run.t.test("ht_incidence_transit", title="Incidence (transit)", 
             single.year=2011),
  run.t.test("ht_incidence_origin", title="Incidence (origin)", 
             single.year=2011),
  run.t.test("ht_incidence_destination", title="Incidence (destination)", 
             single.year=2011),
  run.t.test("ht_news_country", title="TIP media coverage"),
  run.t.test("data4", title="GDP per capita (constant 2000 dollars)"),
  run.t.test("data9", title="Population"),
  run.t.test("corrupt", title="Corruption"),
  run.t.test("fh_pr", title="Political rights"),
  run.t.test("data8", title="Aid"),
  run.t.test("percapeconasst0", title="Aid per capita"),
  run.t.test("ratproto2000", title="Ratification of 2000 TIP protocol", 
             single.year=2011),
  run.t.test("prop_tip_wl", title="US TIP effort (proportion of Wikileaks cables mentioning TIP", 
             single.year=2007)
)

cat(pandoc.table.return(final),
    file=file.path(base.folder, paste0("table_methods_a4.txt")))

Pandoc.convert(file.path(getwd(), base.folder, paste0("table_methods_a4.txt")),
               format="html", footer=FALSE, proc.time=FALSE, 
               options = "-s", open=FALSE)


final.tier1.out <- bind_rows(
  run.t.test("tier", title="Tier", 
             tier1.exclude=TRUE),
  run.t.test("ngos_ave", title="Count of NGOs", 
             single.year=2011, tier1.exclude=TRUE),
  run.t.test("igos_ave", title="Count of IGOs", 
             single.year=2011, tier1.exclude=TRUE),
  run.t.test("ht_incidence_transit", title="Incidence (transit)", 
             single.year=2011, tier1.exclude=TRUE),
  run.t.test("ht_incidence_origin", title="Incidence (origin)", 
             single.year=2011, tier1.exclude=TRUE),
  run.t.test("ht_incidence_destination", title="Incidence (destination)", 
             single.year=2011, tier1.exclude=TRUE),
  run.t.test("ht_news_country", title="TIP media coverage", 
             tier1.exclude=TRUE),
  run.t.test("data4", title="GDP per capita (constant 2000 dollars)", 
             tier1.exclude=TRUE),
  run.t.test("data9", title="Population", 
             tier1.exclude=TRUE),
  run.t.test("corrupt", title="Corruption", 
             tier1.exclude=TRUE),
  run.t.test("fh_pr", title="Political rights", 
             tier1.exclude=TRUE),
  run.t.test("data8", title="Aid", 
             tier1.exclude=TRUE),
  run.t.test("percapeconasst0", title="Aid per capita", 
             tier1.exclude=TRUE),
  run.t.test("ratproto2000", title="Ratification of 2000 TIP protocol", 
             single.year=2011, tier1.exclude=TRUE),
  run.t.test("prop_tip_wl", title="US TIP effort (proportion of Wikileaks cables mentioning TIP", 
             single.year=2007, tier1.exclude=TRUE)
)

cat(pandoc.table.return(final.tier1.out),
    file=file.path(base.folder, paste0("table_methods_a4_no_tier1.txt")))

Pandoc.convert(file.path(getwd(), base.folder, paste0("table_methods_a4_no_tier1.txt")),
               format="html", footer=FALSE, proc.time=FALSE, 
               options = "-s", open=FALSE)


# -----------
# Chapter 2
# -----------
# Table A2.1: Time to inclusion in report
# (Model 1.3 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model.2.1.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ logpop_1 + missinfo8_1 +
                       ngos_ave + fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                       ht_incidence_origin + ht_incidence_transit +
                       ht_incidence_destination + cluster(name),
                     data=df.survivalized.report, ties="efron")
model.2.1.1.fit <- summary(survfit(model.2.1.1))$table


# Save table
var.labs <- c("Total population (logged)", "Missing information", 
              "NGO density", "Worse civil liberties", 
              "Regional density of criminalization", 
              "2000 TIP protocol ratification",
              "Trafficking intensity in countries of origin", 
              "Trafficking intensity in transit countries", 
              "Trafficking intensity in destination countries")
col.labs <- c("Model 2.1.1")

ses <- list(get.or.se(model.2.1.1))

extra.lines <- list(c("Number of countries", c(model.2.1.1.fit["n.max"])),
                    c("Number of inclusions", c(model.2.1.1.fit["events"])))

title <- "Table A2.1: Time to a country’s inclusion in the annual <em>U.S. Trafficking in Persons Report</em>"
notes <- "Robust standard errors in parentheses. All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a2_1.html")

stargazer(model.2.1.1,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, se=ses, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="Time to Inclusion in Report", 
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# Table A2.2: Correlates of shaming
# (Model 2.1 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model2.2.1 <- glm(uspressure ~ fh_cl1 + logeconasstP_1 + loggdp_1 + logpop +
                    ratproto2000_1 + ngos_ave + corruption_1,
                  data=df.complete,
                  family=binomial(link="logit"))

# (Model 2.2 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model2.2.2 <- glm(uspressure ~ fh_cl1 + logeconasstP_1 + loggdp_1 + logpop +
                    ratproto2000_1 + ngos_ave + rule_of_law_1,
                  data=df.complete,
                  family=binomial(link="logit"))


# Save table
var.labs <- c("Worse civil liberties", "US aid (logged)", "GDP (logged)", 
              "Total population (logged)", "2000 TIP protocol ratification", 
              "NGO density", "Corruption", "Rule of law", "Constant")
col.labs <- c("Model 2.2.1", "Model 2.2.2")

ses <- list(get.or.se(model2.2.1), get.or.se(model2.2.2))

extra.lines <- list(c("Pseudo R-squared",
                      sapply(list(model2.2.1, model2.2.2), 
                             calc.pseudo.r.squared)))

title <- "Table A2.2: Correlates of shaming in the annual <em>U.S. Trafficking in Persons Report</em>"
notes <- "Logit model; odds ratios reported. Standard errors in parentheses. All explanatory variables are lagged one period."

out.file <- file.path(base.folder, "table_a2_2.html")

stargazer(model2.2.1, model2.2.2,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="US pressure", se=ses,
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# -----------
# Chapter 3
# -----------
# Table A3.1: Determinants of media coverage
df.media <- df.complete %>% filter(year > 1998) %>%
  mutate(cowcode_factor = factor(cowcode),
         cowcode_relevel = relevel(cowcode_factor, ref="92"))

# ("Media coverage 3" at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model3.1.1 <- lm(logstory ~ inreport + inreport_diff + logstory1 + 
                   fh_cl1 + loggdppercap_1 + ratproto2000_1 + logpop_1 + 
                   year.factor + cowcode_factor, 
                 data=df.media)

# ("Media coverage and incidence" at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model3.1.2 <- lm(logstory ~ inreport + inreport_diff + logstory1 + 
                   fh_cl1 + loggdppercap_1 + ratproto2000_1 + logpop_1 + 
                   ht_incidence_origin + ht_incidence_transit +
                   ht_incidence_destination +
                   year.factor + cowcode_factor, 
                 data=df.media)


# Save table
var.labs <- c("In report", "First year in report", "Coverage (lagged)",
              "Worse civil liberties", "GDP per capita (logged)",
              "2000 TIP protocol ratification", "Population (logged)",
              "Trafficking intensity in countries of origin", 
              "Trafficking intensity in transit countries", 
              "Trafficking intensity in destination countries")

col.labs <- c("Model 3.1.1", "Model 3.1.2")

ses <- list(get.or.se(model3.1.1), get.or.se(model3.1.2))

extra.lines <- list(c("Year fixed effects", rep("Yes", 2)),
                    c("Country fixed effects", rep("Yes", 2)))

title <- "Table A3.1: Determinants of receiving increased coverage of TIP issues"
notes <- "All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a3_1.html")

stargazer(model3.1.1, model3.1.2,
          type="html", out=out.file, out.header=TRUE,
          p.auto=FALSE, no.space=TRUE, omit="cowcode|year\\.",
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="Logged coverage", se=ses,
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# Export prediction data
# Determine which country coefficient is the most average/median
# Just the COW coefficients
coefs.cow <- tidy(model3.1.1) %>%
  filter(grepl("cowcode", term))

# Mean and median COW coefficients
coefs.cow.avg <- coefs.cow %>%
  summarise(coef.mean = mean(estimate),
            coef.median = median(estimate))

# Find closest actual coefficient
possible.coefs <- coefs.cow %>%
  arrange(estimate) %>% select(estimate) %>% c %>% unlist

closest.cow <- coefs.cow %>% 
  filter(estimate == possible.coefs[findInterval(coefs.cow.avg$coef.median, 
                                                 possible.coefs)]) %>%
  mutate(cow = gsub("\\D+", "", term))

# Manually find closest coefficient (to get a more "normal" country)
coefs.cow %>%
  mutate(country = countrycode(gsub("\\D+", "", term), "cown", "country.name")) %>%
  filter(estimate < coefs.cow.avg$coef.median + 0.1,
         estimate > coefs.cow.avg$coef.median - 0.1)

# Predict number of TIP-related stories
# new.data.covars <- model3.1.1$model %>%
#   summarise_each(funs(mean), -c(year.factor, cowcode_factor)) %>%
#   mutate(year.factor = factor(2005),
#          cowcode_factor = factor(92),
#          index = 1) %>%
#   select(-inreport)
# 
# new.data <- data_frame(inreport = c(0, 1), index = 1) %>% 
#   left_join(new.data.covars, by="index") %>%
#   select(-index)
# 
# filter(tidy(model3.1.1), term == "inreport") %>%
#   mutate(adjusted = exp(estimate))

coverage.predicted <- augment(model3.1.1) %>%
  group_by(inreport) %>%
  summarise(avg.logstory = mean(.fitted),
            se.logstory = mean(.se.fit),
            upper = avg.logstory + (qnorm(0.975) * se.logstory),
            lower = avg.logstory + (qnorm(0.025) * se.logstory)) %>%
  mutate_each(funs(exp = exp)) %>%
  mutate(inreport = factor(inreport, levels=0:1, labels=c("Not in report", "In report")))

write_csv(coverage.predicted, path="final_figures/data_figureA_3_media_predict.csv")


# -----------
# Chapter 4
# -----------
# Table A4.1: Determinants of documented reactions in cables
# ("Reaction 4" at http://stats.andrewheiss.com/judith/chapter_5/report.html),
# without interaction term
model4.1.1 <- glm(reactionnomedia ~ tier_2 + pressure +
                    logeconasstP_1, 
                  data=df.reactions,
                  family=binomial(link="logit"))

# ("Reaction 4" at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model4.1.2 <- glm(reactionnomedia ~ tier_2 + pressure +
                    logeconasstP_1 + pressure * logeconasstP_1, 
                  data=df.reactions,
                  family=binomial(link="logit"))

# ("Reaction 5" at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model4.1.3 <- glm(reactionnomedia ~ tier_2 + tier_25 + tier_3 +
                    logeconasstP_1 + loggdppercap_1 + logpop_1 + 
                    newus_share_tot_trade1 + totalfreedom1 + 
                    ratproto2000_1 + loght_news_country, 
                  data=df.reactions,
                  family=binomial(link="logit"))


# Save table
var.labs <- c("Tier 2", "US pressure (Watchlist or Tier 3)", "Watchlist", "Tier 3",
              "US aid (logged)", "US aid (logged) × US pressure",
              "GDP per capita (logged)", "Population (logged)", 
              "Share of total trade with US",
              "Worse total freedom",
              "2000 TIP protocol ratification",
              "Human trafficking news (logged)")
col.labs <- c("Model 4.1.1", "Model 4.1.2", "Model 4.1.3")

ses <- list(get.or.se(model4.1.1), get.or.se(model4.1.2),
            get.or.se(model4.1.3))

extra.lines <- list(c("Pseudo R-squared",
                      sapply(list(model4.1.1, model4.1.2, model4.1.3), 
                             calc.pseudo.r.squared)))

title <- "Table A4.1: Determinants of observing a reaction to the TIP report in Wikileaks cables"
notes <- "Logit model; odds ratios reported. Standard errors in parentheses. All explanatory variables are lagged one period."

out.file <- file.path(base.folder, "table_a4_1.html")

stargazer(model4.1.1, model4.1.2, model4.1.3,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="Reaction in cables", se=ses,
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# -----------
# Chapter 5
# -----------
# Table A5.1: Time to TIP criminalization
# (Model 3.1 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model5.1.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                      missinfo8_2 + ht_incidence_origin + 
                      ht_incidence_transit + ht_incidence_destination + 
                      cluster(name),
                    data=df.survivalized.crim, ties="efron")
model5.1.1.fit <- summary(survfit(model5.1.1))$table

# (Model 3.2 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model5.1.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logpop_1 + ngos_ave + loggdppercap_1 + 
                    corruption_1 + cluster(name),
                  data=df.survivalized.crim, ties="efron")
model5.1.2.fit <- summary(survfit(model5.1.2))$table

# (Model 3.3 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model5.1.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 + 
                    fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 + 
                    missinfo8_2 + logeconasstP_1 + cluster(name),
                  data=df.survivalized.crim, ties="efron")
model5.1.3.fit <- summary(survfit(model5.1.3))$table


# Save table
var.labs <- c("In report", "Share of women in parliament", "Worse civil liberties",
              "Regional density of criminalization", "2000 TIP protocol ratification",
              "Missing information (t−2)", 
              "Trafficking intensity in countries of origin", 
              "Trafficking intensity in transit countries", 
              "Trafficking intensity in destination countries",
              "Total population (logged)",
              "NGO density", "GDP per capita (logged)", "Corruption", 
              "US aid (logged)")
col.labs <- c("Model 5.1.1", "Model 5.1.2", "Model 5.1.3")

ses <- list(get.or.se(model5.1.1), get.or.se(model5.1.2),
            get.or.se(model5.1.3))

extra.lines <- list(c("Number of countries", 
                      c(model5.1.1.fit["n.max"], model5.1.2.fit["n.max"],
                        model5.1.3.fit["n.max"])),
                    c("Number of criminalizations", 
                      c(model5.1.1.fit["events"], model5.1.2.fit["events"],
                        model5.1.3.fit["events"])))

title <- "Table A5.1: Time to TIP criminalization"
notes <- "Robust standard errors in parentheses. All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a5_1.html")

stargazer(model5.1.1, model5.1.2, model5.1.3,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, se=ses, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="Time to TIP criminalization", 
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# Table A5.2: Time to TIP criminalization
# (Model 4.1 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model5.2.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ tier1_1 + tier1_2 + 
                      tier1_25 + tier1_3 + women1 + fh_cl1 + corrected_regcrim1_1 +
                      ratproto2000_1 + missinfo8_2 + ht_incidence_origin + 
                      ht_incidence_transit + ht_incidence_destination + 
                      cluster(name),
                    data=df.survivalized.crim, ties="efron")
model5.2.1.fit <- summary(survfit(model5.2.1))$table

# (Model 4.2 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model5.2.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ tier1_1 + tier1_2 + 
                      tier1_25 + tier1_3 + women1 + fh_cl1 + corrected_regcrim1_1 +
                      ratproto2000_1 + missinfo8_2 + logpop_1 + ngos_ave + 
                      logeconasstP_1 + loggdppercap_1 + corruption_1 + 
                      cluster(name),
                    data=df.survivalized.crim, ties="efron")
model5.2.2.fit <- summary(survfit(model5.2.2))$table

# (Model 4.4 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model5.2.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + new_watch3 + 
                      new_watch2 + new_watch1 + women1 + fh_cl1 + 
                      corrected_regcrim1_1 + ratproto2000_1 + missinfo8_2 + 
                      cluster(name),
                    data=df.survivalized.crim, ties="efron")
model5.2.3.fit <- summary(survfit(model5.2.3))$table


# Save table
var.labs <- c("Tier 1", "Tier 2", "Watch list", "Tier 3", "In report", 
              "First demotion (t−3)", "First demotion (t−2)", 
              "First demotion (t−1)", "Share of women in parliament", 
              "Worse civil liberties", "Regional density of criminalization", 
              "2000 TIP protocol ratification", "Missing information",
              "Trafficking intensity in countries of origin", 
              "Trafficking intensity in transit countries", 
              "Trafficking intensity in destination countries",
              "Total population (logged)", "NGO density", "US aid (logged)", 
              "GDP per capita (logged)", "Corruption")
col.labs <- c("Model 5.2.1", "Model 5.2.2", "Model 5.2.3")

ses <- list(get.or.se(model5.2.1), get.or.se(model5.2.2),
            get.or.se(model5.2.3))

extra.lines <- list(c("Number of countries", 
                      c(model5.2.1.fit["n.max"], model5.2.2.fit["n.max"],
                        model5.2.3.fit["n.max"])),
                    c("Number of criminalizations", 
                      c(model5.2.1.fit["events"], model5.2.2.fit["events"],
                        model5.2.3.fit["events"])))

title <- "Table A5.2: Time to TIP criminalization"
notes <- "Robust standard errors in parentheses. All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a5_2.html")

stargazer(model5.2.1, model5.2.2, model5.2.3,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, se=ses, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="Time to TIP criminalization", 
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# A5.3: Determinants of TIP criminalization
df.model5.3 <- df.reactions %>%
  filter(lag(adjbicrimlevel) == 0 & year > 2001 & year < 2011)
# (Model 5.2 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model5.3.1 <- glm(crim1 ~ reactionnomedia1 + women1 + totalfreedom1 + 
                    adj_ratproto2000_1 + bigaid1 + corrected_regcrim1_1 + as.factor(year),
                  data=df.model5.3,
                  family=binomial(link="logit"))

# (Model 5.4 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model5.3.2 <- glm(crim1 ~ totalreactionnomedia1 + women1 + totalfreedom1 + 
                    adj_ratproto2000_1 + bigaid1 + corrected_regcrim1_1 + as.factor(year),
                  data=df.model5.3,
                  family=binomial(link="logit"))


# Save table
var.labs <- c("Reactions (no media)", "Total reactions (no media)", 
              "Share of women in parliament", 
              "Worse total freedom (political rights + civil liberties)", 
              "2000 TIP protocol ratification", "Big aid", 
              "Regional density of criminalization")
col.labs <- c("Model 5.3.1", "Model 5.3.2")

ses <- list(get.or.se(model5.3.1), get.or.se(model5.3.2))

extra.lines <- list(c("Year fixed effects",
                      rep("Yes", 2)),
                    c("Pseudo R-squared",
                      sapply(list(model5.3.1, model5.3.2), 
                             calc.pseudo.r.squared)))

title <- "Table A5.3: Determinants of TIP criminalization"
notes <- "Logit model; odds ratios reported. Robust standard errors in parentheses. All explanatory variables are lagged one period."

out.file <- file.path(base.folder, "table_a5_3.html")

stargazer(model5.3.1, model5.3.2,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, p.auto=FALSE, no.space=TRUE, omit="factor\\(year",
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="Criminalization", se=ses,
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# -----------
# Chapter 6
# -----------
# Table A6.1: Time to TIP criminalization (with presence in report, aid)
# (Model 3.4 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model6.1.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + logeconasstP_1 +
                      inreport1 * logeconasstP_1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.1.1.fit <- summary(survfit(model6.1.1))$table

# (Model 3.5 at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model6.1.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + econasstPgdp_1_1000 +
                      econasstPgdp_1_1000 * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.1.2.fit <- summary(survfit(model6.1.2))$table

model6.1.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                       fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                       missinfo8_2 + aid.us.total.perc_lag +
                       aid.us.total.perc_lag * inreport1 + cluster(name),
                     data=df.survivalized.crim, ties="efron")
model6.1.3.fit <- summary(survfit(model6.1.3))$table

model6.1.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + logeconasstP_1 +
                      inreport1 * logeconasstP_1 + cluster(name),
                    data=df.survivalized.crim.dac, ties="efron", model=TRUE)
model6.1.4.fit <- summary(survfit(model6.1.4))$table

model6.1.5 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + econasstPgdp_1_1000 +
                      econasstPgdp_1_1000 * inreport1 + cluster(name),
                    data=df.survivalized.crim.dac, ties="efron")
model6.1.5.fit <- summary(survfit(model6.1.5))$table

model6.1.6 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                       fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                       missinfo8_2 + aid.us.total.perc_lag +
                       aid.us.total.perc_lag * inreport1 + cluster(name),
                     data=df.survivalized.crim.dac, ties="efron")
model6.1.6.fit <- summary(survfit(model6.1.6))$table

# Save table
var.labs <- c("In report", "Share of women in parliament", "Worse civil liberties",
              "Regional density of criminalization", "2000 TIP protocol ratification",
              "Missing information (t−2)",
              "US aid (logged)", "US aid × In report",
              "US aid as share of GDP (logged)",
              "US aid as share of GDP × In report",
              "US aid as share of total aid (logged)",
              "US aid as share of total aid (logged) × In report")
col.labs <- c("Model 6.1.1", "Model 6.1.2", "Model 6.1.3", "Model 6.1.4",
              "Model 6.1.5", "Model 6.1.6")

ses <- list(get.or.se(model6.1.1), get.or.se(model6.1.2),
            get.or.se(model6.1.3), get.or.se(model6.1.4),
            get.or.se(model6.1.5), get.or.se(model6.1.6))

extra.lines <- list(c("Number of countries",
                      c(model6.1.1.fit["n.max"], model6.1.2.fit["n.max"],
                        model6.1.3.fit["n.max"], model6.1.4.fit["n.max"],
                        model6.1.5.fit["n.max"], model6.1.6.fit["n.max"])),
                    c("Number of criminalizations",
                      c(model6.1.1.fit["events"], model6.1.2.fit["events"],
                        model6.1.3.fit["events"], model6.1.4.fit["events"],
                        model6.1.5.fit["events"], model6.1.6.fit["events"])),
                    c("OECD DAC eligible countries only",
                      c(rep("No", 3), rep("Yes", 3))))

title <- "Table A6.1: Time to TIP criminalization: presence in report (aid)"
notes <- "Robust standard errors in parentheses. All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a6_1.html")

stargazer(model6.1.1, model6.1.2, model6.1.3, model6.1.4,
          model6.1.5, model6.1.6,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, se=ses, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs,
          dep.var.caption="Time to TIP criminalization",
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# Table A6.2: Time to TIP criminalization (with presence in report, trade)
model6.2.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + newus_tradeshare_gdp1 +
                      newus_tradeshare_gdp1 * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.2.1.fit <- summary(survfit(model6.2.1))$table

model6.2.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + has.bit.sig.with.us_lag +
                      has.bit.sig.with.us_lag * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.2.2.fit <- summary(survfit(model6.2.2))$table

model6.2.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + us.military.aid.log_lag +
                      us.military.aid.log_lag * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.2.3.fit <- summary(survfit(model6.2.3))$table

model6.2.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + fdi.from.us.log_lag +
                      fdi.from.us.log_lag * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.2.4.fit <- summary(survfit(model6.2.4))$table

model6.2.5 <- coxph(Surv(start_time, yrfromj2, fail) ~ inreport1 + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + trade.to.us.log_lag +
                      trade.to.us.log_lag * inreport1 + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.2.5.fit <- summary(survfit(model6.2.5))$table

# Save table
var.labs <- c("In report", "Share of women in parliament", "Worse civil liberties",
              "Regional density of criminalization", "2000 TIP protocol ratification",
              "Missing information (t−2)",
              "US trade as share of GDP (logged)",
              "US trade as share of GDP (logged) × US pressure",
              "Has BIT with US",
              "Has BIT with US × In report",
              "US military aid (logged)",
              "US military aid (logged) × In report",
              "FDI from US (logged)",
              "FDI from US (logged) × In report",
              "Imports to US (logged)",
              "Imports to US (logged) × In report")
col.labs <- c("Model 6.2.1", "Model 6.2.2", "Model 6.2.3", "Model 6.2.4",
              "Model 6.2.5")

ses <- list(get.or.se(model6.2.1), get.or.se(model6.2.2),
            get.or.se(model6.2.3), get.or.se(model6.2.4),
            get.or.se(model6.2.5))

extra.lines <- list(c("Number of countries",
                      c(model6.2.1.fit["n.max"], model6.2.2.fit["n.max"],
                        model6.2.3.fit["n.max"], model6.2.4.fit["n.max"],
                        model6.2.5.fit["n.max"])),
                    c("Number of criminalizations",
                      c(model6.2.1.fit["events"], model6.2.2.fit["events"],
                        model6.2.3.fit["events"], model6.2.4.fit["events"],
                        model6.2.5.fit["events"])))

title <- "Table A6.2: Time to TIP criminalization: presence in report (trade)"
notes <- "Robust standard errors in parentheses. All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a6_2.html")

stargazer(model6.2.1, model6.2.2, model6.2.3, model6.2.4, model6.2.5,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, se=ses, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs,
          dep.var.caption="Time to TIP criminalization",
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# Table A6.3: Time to TIP criminalization (with US pressure (2.5 or 3 rating), aid)
model6.3.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + logeconasstP_1 +
                           pressure_lag * logeconasstP_1 + cluster(name),
                         data=df.survivalized.crim, ties="efron", model=TRUE)
model6.3.1.fit <- summary(survfit(model6.3.1))$table

model6.3.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + econasstPgdp_1_1000 +
                           econasstPgdp_1_1000 * pressure_lag + cluster(name),
                         data=df.survivalized.crim, ties="efron")
model6.3.2.fit <- summary(survfit(model6.3.2))$table

model6.3.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                       fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                       missinfo8_2 + aid.us.total.perc_lag +
                       aid.us.total.perc_lag * pressure_lag + cluster(name),
                     data=df.survivalized.crim, ties="efron")
model6.3.3.fit <- summary(survfit(model6.3.3))$table

model6.3.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + logeconasstP_1 +
                           pressure_lag * logeconasstP_1 + cluster(name),
                         data=df.survivalized.crim.dac, ties="efron", model=TRUE)
model6.3.4.fit <- summary(survfit(model6.3.4))$table

model6.3.5 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                           fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                           missinfo8_2 + econasstPgdp_1_1000 +
                           econasstPgdp_1_1000 * pressure_lag + cluster(name),
                         data=df.survivalized.crim.dac, ties="efron")
model6.3.5.fit <- summary(survfit(model6.3.5))$table

model6.3.6 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                       fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                       missinfo8_2 + aid.us.total.perc_lag +
                       aid.us.total.perc_lag * pressure_lag + cluster(name),
                     data=df.survivalized.crim.dac, ties="efron")
model6.3.6.fit <- summary(survfit(model6.3.6))$table


# Save table
var.labs <- c("US pressure", "Share of women in parliament", "Worse civil liberties",
              "Regional density of criminalization", "2000 TIP protocol ratification",
              "Missing information (t−2)",
              "US aid (logged)", "US aid × US pressure",
              "US aid as share of GDP (logged)",
              "US aid as share of GDP × US pressure",
              "US aid as share of total aid (logged)",
              "US aid as share of total aid (logged) × US pressure")
col.labs <- c("Model 6.3.1", "Model 6.3.2", "Model 6.3.3", "Model 6.3.4",
              "Model 6.3.5", "Model 6.3.6")

ses <- list(get.or.se(model6.3.1), get.or.se(model6.3.2),
            get.or.se(model6.3.3), get.or.se(model6.3.4),
            get.or.se(model6.3.5), get.or.se(model6.3.6))

extra.lines <- list(c("Number of countries",
                      c(model6.3.1.fit["n.max"], model6.3.2.fit["n.max"],
                        model6.3.3.fit["n.max"], model6.3.4.fit["n.max"],
                        model6.3.5.fit["n.max"], model6.3.6.fit["n.max"])),
                    c("Number of criminalizations",
                      c(model6.3.1.fit["events"], model6.3.2.fit["events"],
                        model6.3.3.fit["events"], model6.3.4.fit["events"],
                        model6.3.5.fit["events"], model6.3.6.fit["events"])),
                    c("OECD DAC eligible countries only",
                      c(rep("No", 3), rep("Yes", 3))))

title <- "Table A6.3: Time to TIP criminalization: US pressure (aid)"
notes <- "Robust standard errors in parentheses. All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a6_3.html")

stargazer(model6.3.1, model6.3.2, model6.3.3, model6.3.4,
          model6.3.5, model6.3.6, 
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, se=ses, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs,
          dep.var.caption="Time to TIP criminalization",
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# Table A6.4: Time to TIP criminalization (with US pressure (2.5 or 3 rating), trade)
model6.4.1 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + newus_tradeshare_gdp1 +
                      newus_tradeshare_gdp1 * pressure_lag + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.4.1.fit <- summary(survfit(model6.4.1))$table

model6.4.2 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + has.bit.sig.with.us_lag +
                      has.bit.sig.with.us_lag * pressure_lag + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.4.2.fit <- summary(survfit(model6.4.2))$table

model6.4.3 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + us.military.aid.log_lag +
                      us.military.aid.log_lag * pressure_lag + cluster(name),
                    data=df.survivalized.crim, ties="efron", model=TRUE)
model6.4.3.fit <- summary(survfit(model6.4.3))$table

model6.4.4 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + fdi.from.us.log_lag +
                      fdi.from.us.log_lag * pressure_lag + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.4.4.fit <- summary(survfit(model6.4.4))$table

model6.4.5 <- coxph(Surv(start_time, yrfromj2, fail) ~ pressure_lag + women1 +
                      fh_cl1 + corrected_regcrim1_1 + ratproto2000_1 +
                      missinfo8_2 + trade.to.us.log_lag +
                      trade.to.us.log_lag * pressure_lag + cluster(name),
                    data=df.survivalized.crim, ties="efron")
model6.4.5.fit <- summary(survfit(model6.4.5))$table


# Save table
var.labs <- c("US pressure", "Share of women in parliament", "Worse civil liberties",
              "Regional density of criminalization", "2000 TIP protocol ratification",
              "Missing information (t−2)",
              "US trade as share of GDP (logged)",
              "US trade as share of GDP (logged) × US pressure",
              "Has BIT with US",
              "Has BIT with US × US pressure",
              "US military aid (logged)",
              "US military aid (logged) × US pressure",
              "FDI from US (logged)",
              "FDI from US (logged) × US pressure",
              "Imports to US (logged)",
              "Imports to US (logged) × US pressure")
col.labs <- c("Model 6.4.1", "Model 6.4.2", "Model 6.4.3", "Model 6.4.4",
              "Model 6.4.5")

ses <- list(get.or.se(model6.4.1), get.or.se(model6.4.2),
            get.or.se(model6.4.3), get.or.se(model6.4.4),
            get.or.se(model6.4.5))

extra.lines <- list(c("Number of countries",
                      c(model6.4.1.fit["n.max"], model6.4.2.fit["n.max"],
                        model6.4.3.fit["n.max"], model6.4.4.fit["n.max"],
                        model6.4.5.fit["n.max"])),
                    c("Number of criminalizations",
                      c(model6.4.1.fit["events"], model6.4.2.fit["events"],
                        model6.4.3.fit["events"], model6.4.4.fit["events"],
                        model6.4.5.fit["events"])))

title <- "Table A6.4: Time to TIP criminalization: US pressure (trade)"
notes <- "Robust standard errors in parentheses. All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a6_4.html")

stargazer(model6.4.1, model6.4.2, model6.4.3, model6.4.4, model6.4.5, 
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, se=ses, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs,
          dep.var.caption="Time to TIP criminalization",
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# Table A6.5: Determinants of criminalization (democracy × treatments)
df.interaction.models <- df.complete %>% 
  filter(lag(adjbicrimlevel) == 0 & year > 2001 & year < 2011)

# Model 1.1.2(2) at http://stats.andrewheiss.com/judith/chapter_5/interactions.html
model6.5.1 <- glm(crim1 ~ fh_cl1 + inreport1 + fh_cl1 * inreport1 + 
                    women1 + ratproto2000_1 + 
                    corrected_regcrim1_1 + missinfo8_2 + year.factor,
                  data=df.interaction.models,
                  family=binomial(link="logit"))

# Model 2.1.2(2) at http://stats.andrewheiss.com/judith/chapter_5/interactions.html
model6.5.2 <- glm(crim1 ~ fh_cl1 + low_tier1 + fh_cl1 * low_tier1 + 
                    women1 + ratproto2000_1 + 
                    corrected_regcrim1_1 + missinfo8_2 + year.factor,
                  data=df.interaction.models,
                  family=binomial(link="logit"))

# Model 3.1.2(2) at http://stats.andrewheiss.com/judith/chapter_5/interactions.html
model6.5.3 <- glm(crim1 ~ fh_cl1 + 
                    new_watch3 + new_watch2 + new_watch1 + 
                    fh_cl1 * new_watch3 + 
                    fh_cl1 * new_watch2 + 
                    fh_cl1 * new_watch1 + 
                    women1 + ratproto2000_1 + 
                    corrected_regcrim1_1 + missinfo8_2 + year.factor,
                  data=df.interaction.models,
                  family=binomial(link="logit"))


# Save table
var.labs <- c("Worse democracy (Freedom House civil liberties)", 
              "In TIP report", "Lowest tier",
              "First demotion (t−3)", "First demotion (t−2)", 
              "First demotion (t−1)",
              "Share of women in parliament", 
              "2000 TIP protocol ratification", 
              "Regional density of criminalization",
              "Missing info",
              "Worse democracy × In TIP report", "Worse democracy × Lowest tier",
              "Worse democracy × First demotion (t−3)", 
              "Worse democracy × First demotion (t−2)", 
              "Worse democracy × First demotion (t−1)")

col.labs <- c("Model 6.5.1<br>Presence in TIP report", 
              "Model 6.5.2<br>Lower tier ratings", 
              "Model 6.5.3<br>Downgrading")

ses <- list(get.or.se(model6.5.1), get.or.se(model6.5.2),
            get.or.se(model6.5.3))

extra.lines <- list(c("Year fixed effects",
                      rep("Yes", 3)),
                    c("Pseudo R-squared",
                      sapply(list(model6.5.1, model6.5.2, model6.5.3), 
                             calc.pseudo.r.squared)))

title <- "Table A6.5: Determinants of criminalization—effects of democracy interacted with scorecard diplomacy treatments"
notes <- "Logit models; odds ratios reported. Standard errors in parentheses. All explanatory variables are lagged one period."

out.file <- file.path(base.folder, "table_a6_5.html")

stargazer(model6.5.1, model6.5.2, model6.5.3,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, p.auto=FALSE, no.space=TRUE, omit="year\\.factor",
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="Criminalization", se=ses,
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# Export prediction data for model 6.5.1
model <- model6.5.1

new.data <- model$model %>%
  select(-fh_cl1) %>%
  group_by(inreport1) %>%
  summarise_each(funs(mean), -(year.factor)) %>%
  mutate(year.factor = factor(2005)) %>%
  right_join(expand.grid(fh_cl1 = seq(1, 7, 0.1), 
                         inreport1 = c(0, 1)), by="inreport1")

plot.predict <- augment(model, newdata=new.data) %>%
  mutate(prob = model$family$linkinv(.fitted),
         prob.lower = model$family$linkinv(.fitted - (qnorm(0.95 / 2 + 0.5) * .se.fit)),
         prob.upper = model$family$linkinv(.fitted + (qnorm(0.95 / 2 + 0.5) * .se.fit)),
         inreport1 = factor(inreport1, labels=c("Not in report", "In report"),
                            ordered=TRUE))

saveRDS(plot.predict, file="final_figures/data_figureA_6_report_predict.rds")


# Export prediction data for model 6.5.2
model <- model6.5.2

new.data <- model$model %>%
  select(-fh_cl1) %>%
  group_by(low_tier1) %>%
  summarise_each(funs(mean), -(year.factor)) %>%
  mutate(year.factor = factor(2005)) %>%
  right_join(expand.grid(fh_cl1 = seq(1, 7, 0.1), 
                         low_tier1 = c(0, 1)), by="low_tier1")

plot.predict <- augment(model, newdata=new.data) %>%
  mutate(prob = model$family$linkinv(.fitted),
         prob.lower = model$family$linkinv(.fitted - (qnorm(0.95 / 2 + 0.5) * .se.fit)),
         prob.upper = model$family$linkinv(.fitted + (qnorm(0.95 / 2 + 0.5) * .se.fit)),
         low_tier1 = factor(low_tier1, labels=c("1 or 2", "Watchlist or 3"),
                            ordered=TRUE))

# Save data for book
saveRDS(plot.predict, file="final_figures/data_figureA_6_lowest_tier_predict.rds")


# Export prediction data for model 6.5.3
model <- model6.5.3

covars <- model$model %>%
  select(-fh_cl1, -starts_with("new_watch")) %>%
  summarise_each(funs(mean), -(year.factor)) %>%
  mutate(year.factor = factor(2005),
         id = 1)  # For joining into fully expanded dataframe later

new.combinations <- expand.grid(fh_cl1 = seq(1, 7, 0.1),
                                new_watch = c("1", "2", "3"))

baseline.combinations <- data_frame(fh_cl1 = seq(1, 7, 0.1)) %>%
  mutate(new_watch = NA, new_watch1 = 0, new_watch2 = 0, new_watch3 = 0)

new.data <- new.combinations %>%
  bind_cols(data.frame(model.matrix(fh_cl1 ~ new_watch + 0, 
                                    data=new.combinations))) %>%
  bind_rows(baseline.combinations) %>%
  mutate(id = 1) %>% left_join(covars, by="id") %>%
  select(-c(id, new_watch))

plot.predict <- augment(model, newdata=new.data) %>%
  mutate(prob = model$family$linkinv(.fitted),
         prob.lower = model$family$linkinv(.fitted - (qnorm(0.95 / 2 + 0.5) * .se.fit)),
         prob.upper = model$family$linkinv(.fitted + (qnorm(0.95 / 2 + 0.5) * .se.fit))) %>%
  mutate(demote_type = ifelse(new_watch1 == 1, "1 year", NA),
         demote_type = ifelse(new_watch2 == 1, "2 years", demote_type),
         demote_type = ifelse(new_watch3 == 1, "3 years", demote_type),
         demote_type = ifelse(new_watch1 + new_watch2 + new_watch3 == 0, 
                              "No demotion", demote_type))

saveRDS(plot.predict, file="final_figures/data_figureA_6_downgrade_predict.rds")
