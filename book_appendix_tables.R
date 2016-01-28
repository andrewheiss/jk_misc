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


# Pandoc options
panderOptions('pandoc.binary', '/Users/andrew/.cabal/bin/pandoc')
panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', 50)
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

# TODO: Remove "non-robust" from logit notes, since there's no need to be robust anyway:
# http://davegiles.blogspot.com/2013/05/robust-standard-errors-for-nonlinear.html
# https://stat.ethz.ch/pipermail/r-help/2006-July/108722.html

# TODO: Write script to remove all rows with a colspan + add notes row?
# Or
# # /Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to odt --infilter="HTML" testing.html
# /Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to odt --infilter="HTML (StarWriter)" testing.html


# Locations
base.folder <- "final_tables"

# TODO: Make sure all models use correctly lagged data

# ------------------
# Useful functions
# ------------------

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

# Calculate a pseudo R-squared measure using
# 1-\frac{\text{Residual Deviance}}{\text{Null Deviance}}
calc.pseudo.r.squared <- function(x.model) {
  return(round(1 - (x.model$deviance / x.model$null.deviance), 4))
}


# -----------------------
# Load and reshape data
# -----------------------
# Full, clean, properly lagged data
df.complete <- readRDS("final_tables/df_complete.rds")

df.reactions.small <- readRDS("final_tables/df_reactions_small.rds")

df.reactions <- df.complete %>%
  left_join(df.reactions.small, by=c("year", "cowcode")) %>%
  group_by(cowcode) %>%
  mutate(totalreactionnomedia1 = lag(totalreactionnomedia),
         reactionnomedia1 = lag(reactionnomedia),
         bigaid1 = lag(bigaid),
         loght_news_country1 = lag(loght_news_country)) %>%
  mutate(tier_1 = ifelse(tier == 1, 1, 0),
         tier_2 = ifelse(tier == 2, 1, 0),
         tier_25 = ifelse(tier == 2.5, 1, 0),
         tier_3 = ifelse(tier == 3, 1, 0),
         pressure = ifelse(tier_25 == 1 | tier_3 == 1, 1, 0)) %>%
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


# Generate start time variables
# Stata does this behind the scenes when running stset ..., id(name)
# coxph needs an explicit column
# See Q&A here: http://stats.stackexchange.com/q/177560/3025
df.survivalized.report <- df.hazardized.report %>%
  filter(!is.na(yrfromj2)) %>%
  group_by(name) %>%
  mutate(start_time = lag(yrfromj2, default=0)) %>%
  filter(year > 2000)


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

ses <- list(sqrt(diag(model.2.1.1$var)))

extra.lines <- list(c("Number of countries", c(model.2.1.1.fit["n.max"])),
                    c("Number of inclusions", c(model.2.1.1.fit["events"])))

title <- "Table A2.1: Time to a country’s inclusion in the annual <em>U.S. Trafficking in Persons Report</em>"
notes <- "Robust standard errors in parentheses; values differ from published article because of differences in the robustness algorithms Stata and R use. All explanatory variables are lagged one period unless otherwise noted."

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

extra.lines <- list(c("Pseudo R-squared",
                      sapply(list(model2.2.1, model2.2.2), 
                             calc.pseudo.r.squared)))

title <- "Table A2.2: Correlates of shaming in the annual <em>U.S. Trafficking in Persons Report</em>"
notes <- "Logit model; odds ratios reported. Non-robust standard errors in parentheses. All explanatory variables are lagged one period."

out.file <- file.path(base.folder, "table_a2_2.html")

stargazer(model2.2.1, model2.2.2,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="US pressure", 
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# -----------
# Chapter 3
# -----------
# Table A3.1: Determinants of media coverage
df.media <- df.complete %>% filter(year > 1998)

# ("Media coverage 3" at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model3.1.1 <- lm(logstory ~ inreport + inreport_diff + logstory1 + 
                   fh_cl1 + loggdppercap_1 + ratproto2000_1 + logpop_1 + 
                   year.factor + as.factor(cowcode), 
                 data=df.media)

# ("Media coverage and incidence" at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model3.1.2 <- lm(logstory ~ inreport + inreport_diff + logstory1 + 
                   fh_cl1 + loggdppercap_1 + ratproto2000_1 + logpop_1 + 
                   ht_incidence_origin + ht_incidence_transit +
                   ht_incidence_destination +
                   year.factor + as.factor(cowcode), 
                 data=df.media)


# Save table
var.labs <- c("In report", "First year in report", "Coverage (lagged)",
              "Worse civil liberties", "GDP per capita (logged)",
              "2000 TIP protocol ratification", "Population (logged)",
              "Trafficking intensity in countries of origin", 
              "Trafficking intensity in transit countries", 
              "Trafficking intensity in destination countries")

col.labs <- c("Model 3.1.1", "Model 3.1.2")

extra.lines <- list(c("Year fixed effects", rep("Yes", 2)),
                    c("Country fixed effects", rep("Yes", 2)))

title <- "Table A3.1: Determinants of receiving increased coverage of TIP issues"
notes <- "All explanatory variables are lagged one period unless otherwise noted."

out.file <- file.path(base.folder, "table_a3_1.html")

stargazer(model3.1.1, model3.1.2,
          type="html", out=out.file, out.header=TRUE,
          p.auto=FALSE, no.space=TRUE, omit="\\.factor",
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="Logged coverage", 
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
new.data.covars <- model3.1.1$model %>%
  summarise_each(funs(mean), -c(year.factor, `as.factor(cowcode)`)) %>%
  mutate(year.factor = factor(2005),
         cowcode = factor(92),
         index = 1) %>%
  select(-inreport)

new.data <- data_frame(inreport = c(0, 1), index = 1) %>% 
  left_join(new.data.covars, by="index") %>%
  select(-index)

plot.predict <- augment(model3.1.1, newdata=new.data) %>%
  mutate(pred = exp(.fitted),
         pred.lower = exp(.fitted - (qnorm(0.95 / 2 + 0.5) * .se.fit)),
         pred.upper = exp(.fitted + (qnorm(0.95 / 2 + 0.5) * .se.fit)),
         inreport = factor(inreport, labels=c("Not in report", "In report"),
                           ordered=TRUE))

write_csv(plot.predict, path="final_figures/data_figureA_3_media_predict.csv")


# -----------
# Chapter 4
# -----------
# Table A4.1: Determinants of documented reactions in cables
# ("Reaction 4" at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model4.1.1 <- glm(reactionnomedia ~ tier_2 + pressure +
                    logeconasstP_1 + pressure * logeconasstP_1, 
                  data=df.reactions,
                  family=binomial(link="logit"))

# ("Reaction 5" at http://stats.andrewheiss.com/judith/chapter_5/report.html)
model4.1.2 <- glm(reactionnomedia ~ tier_2 + tier_25 + tier_3 +
                    logeconasstP_1 + loggdppercap_1 + logpop_1 + 
                    newus_share_tot_trade1 + totalfreedom1 + 
                    ratproto2000_1 + loght_news_country, 
                  data=df.reactions,
                  family=binomial(link="logit"))


# Save table
var.labs <- c("Tier 2", "US pressure (Watchlist or Tier 3)", "Watchlist", "Tier 3",
              "US aid (logged)", "US aid (logged) × US pressure",
              "GDP per capita (logged)", "Population (logged)", 
              "Share of total trade with US (lagged)",
              "Worse total freedom (lagged)",
              "2000 TIP protocol ratification (lagged)",
              "Human trafficking news (logged)")
col.labs <- c("Model 4.1.1", "Model 4.1.2")

extra.lines <- list(c("Pseudo R-squared",
                      sapply(list(model4.1.1, model4.1.2), 
                             calc.pseudo.r.squared)))

title <- "Table A4.1: Determinants of observing a reaction to the TIP report in Wikileaks cables"
notes <- "Logit model; odds ratios reported. Non-robust standard errors in parentheses. All explanatory variables are lagged one period."

out.file <- file.path(base.folder, "table_a4_1.html")

stargazer(model4.1.1, model4.1.2,
          type="html", out=out.file, out.header=TRUE,
          apply.coef=exp, p.auto=FALSE, no.space=TRUE,
          covariate.labels=var.labs, column.labels=col.labs, 
          dep.var.caption="Reaction in cables", 
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          notes.align="l", add.lines=extra.lines, keep.stat=c("n"),
          notes.label="Notes:", notes=notes, title=title)


# -----------
# Chapter 5
# -----------
# A5.1
# 5.1.1-3
# Models 3.1-3

# A5.2
# 5.2.1, 5.2.2, 5.2.3 (Models 4.1, 4.2, 4.4)

# A5.3
# Model 5.2, 5.4
# 5.3.1, 5.3.2


# -----------
# Chapter 6
# -----------
# A6.1
# Model 3.4	Model 3.5
# Add trade and interaction (or just mention it)

# A6.2
# 1.1.2, 2.1.2, 3.1.2, second model for each
# Predicted probabilties for all three
