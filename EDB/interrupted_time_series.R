#' ---
#' title: "EDB and interrupted time series models"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     css: html/fixes.css
#'     code_folding: hide
#'     toc: yes
#'     toc_float: 
#'       collapsed: false
#'     toc_depth: 4
#'     highlight: pygments
#'     self_contained: no
#'     theme: flatly
#'     fig_height: 3
#'     fig_width: 4.5
#' ---

#+ message=FALSE
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(readr)
library(ggplot2)
library(ggstance)
library(haven)
library(stargazer)
library(lubridate)
library(lazyeval)
library(sandwich)
library(lmtest)
library(broom)

knitr::opts_chunk$set(cache=FALSE, fig.retina=2,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120),  # For output
                      warning=FALSE)

countries.with.edb.bureau <- read_csv(file.path(PROJHOME, "EDB/data/countries_with_edb_bureau.csv"))
edb.its <- read_dta("~/Dropbox/Andrew/EDB/MasterWBMarch16_15.dta") %>%
  filter(year > 1999) %>%
  rename(p_edb_rank = p_ebd_rank) %>%
  select(ccode, year, 
         sb_proced, sb_days, sb_capital, sb_cost, con_proced, con_days, 
         gdp, gdpcap, pop, gdpgrowth, polity = polity2, ibrd,
         p_edb_rank) %>%
  mutate_each(funs(ln = log1p(.)), 
              starts_with("sb"), starts_with("con"), gdp, gdpcap, pop) %>%
  mutate(year.centered.2005 = year - 2005,
         year.centered.2006 = year - 2006,
         ranked.2005 = year.centered.2005 >= 0,
         ranked.2006 = year.centered.2006 >= 0) %>%
  group_by(ccode) %>%
  mutate(loan_ln = log1p(sum(ibrd, na.rm=TRUE))) %>%
  mutate_each(funs(lag = lag(.))) %>%
  ungroup()

edb.its.constrained.countries <- edb.its %>%
  mutate(in.report.in.2004 = year == 2004 & !is.na(sb_days),
         in.report.in.2001 = year == 2001 & !is.na(sb_days)) %>%
  group_by(ccode) %>%
  summarise(in.2004 = sum(in.report.in.2004),
            in.2001 = sum(in.report.in.2001))

edb.its <- edb.its %>%
  left_join(edb.its.constrained.countries, by="ccode") %>%
  filter(in.2004 == 1)

edb.its.2001 <- edb.its %>%
  filter(in.2001 == 1)

edb.its.committee <- edb.its %>%
  filter(ccode %in% countries.with.edb.bureau$cowcode)

edb.its.cap.constrained <- filter(edb.its, year >= 2003)

edb.its.2001.cap.constrained <- filter(edb.its.2001, year >= 2003)

edb.its.committee.cap.constrained <- filter(edb.its.committee, year >= 2003)


theme_edb <- function(base_size=9, base_family="Clear Sans Light") {
  update_geom_defaults("label", list(family="Clear Sans Light"))
  update_geom_defaults("text", list(family="Clear Sans Light"))
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          title=element_text(vjust=1.2, family="Clear Sans", face="bold"),
          plot.subtitle=element_text(family="Clear Sans Light"),
          plot.caption=element_text(family="Clear Sans Light",
                                    size=rel(0.8), colour="grey70"),
          panel.border = element_blank(), 
          axis.line=element_line(colour="grey50", size=0.2),
          #panel.grid=element_blank(), 
          axis.ticks=element_blank(),
          legend.position="bottom", 
          legend.title=element_text(size=rel(0.8)),
          axis.title=element_text(size=rel(0.8), family="Clear Sans", face="bold"),
          strip.text=element_text(size=rel(1), family="Clear Sans", face="bold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin.y=unit(1.5, "lines"),
          legend.key=element_blank(),
          legend.margin=unit(0.2, "lines"))
  
  ret
}

plot.its <- function(model, var.name, var.title, y.title, plot.year = 2005) {
  # summary_dots <- list(
  #   n = ~ n(),
  #   variable = interp(~ mean(val, na.rm = T), val=as.name(var.name)),
  #   stdev = interp(~ sd(val, na.rm = T), val = as.name(var.name))
  # )
  
  if (plot.year == 2005) {
    plot.data <- edb.its %>%
      group_by(year.centered.2005) %>%
      summarise_(variable = interp(~mean(var, na.rm=TRUE), var=as.name(var.name)))

    newdata <- data_frame(year.centered.2005 = seq(min(edb.its$year.centered.2005),
                                              max(edb.its$year.centered.2005), by=1),
                          ranked.2005 = year.centered.2005 >= 0,
                          gdpcap_ln_lag = mean(edb.its$gdpcap_ln_lag, na.rm=TRUE),
                          gdpgrowth_lag = mean(edb.its$gdpgrowth_lag, na.rm=TRUE),
                          pop_ln_lag = mean(edb.its$pop_ln_lag, na.rm=TRUE))

    plot.predict <- augment(model, newdata=newdata) %>% 
      rename(variable = .fitted,
             year.centered = year.centered.2005,
             ranked = ranked.2005)
  } else {
    plot.data <- edb.its %>%
      group_by(year.centered.2006) %>%
      summarise_(variable = interp(~mean(var, na.rm=TRUE), var=as.name(var.name)))
    
    newdata <- data_frame(year.centered.2006 = seq(min(edb.its$year.centered.2006),
                                              max(edb.its$year.centered.2006), by=1),
                          ranked.2006 = year.centered.2006 >= 0,
                          gdpcap_ln_lag = mean(edb.its$gdpcap_ln_lag, na.rm=TRUE),
                          gdpgrowth_lag = mean(edb.its$gdpgrowth_lag, na.rm=TRUE),
                          pop_ln_lag = mean(edb.its$pop_ln_lag, na.rm=TRUE))

    plot.predict <- augment(model, newdata=newdata) %>% 
      rename(variable = .fitted,
             year.centered = year.centered.2006,
             ranked = ranked.2006)
  }

  ggplot(plot.predict, aes(x=year.centered, y=variable)) +
    geom_line() +
    geom_line(data=plot.predict, aes(colour=ranked), size=0.75) +
    geom_vline(xintercept=0) +
    scale_colour_manual(values=c("#0073D9", "#CC3340"),
                        labels=c("Not ranked    ", "Ranked"),
                        name=NULL) +
    labs(title=var.title, y=y.title, x=paste("Years since", plot.year)) +
    theme_edb()
}

# Calculate clustered robust standard errors
robust.clusterify <- function(model, dat, cluster) {
  attach(dat, warn.conflicts = F)
  not <- attr(model$model,"na.action")
  
  if( ! is.null(not)) {  # only drop the NA values if there are any left
    cluster <- cluster[-not]
    dat <- dat[-not,]
  }
  
  with(dat, {
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj <- apply(estfun(model),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
    coefs <- coeftest(model, vcovCL, type="HC1")  # HC1 or HC0 are close to Stata
    return(list(clcov=vcovCL, coefs=coefs))
  })
}

#' ## Starting a business variables over time
#' 
#' ### Countries in 2004 report
plot.edb <- edb.its %>%
  select(year, sb_days, sb_proced, sb_cost, sb_capital, con_days, con_proced) %>%
  gather(variable, value, -year) %>%
  group_by(year, variable) %>%
  summarise(avg = mean(value, na.rm=TRUE)) %>%
  filter(!is.nan(avg))

plot.interventions <- data_frame(year = 2005:2006,
                                 intervention = c("2005", "2006"))

#+ fig.width=6, fig.height=3.5
ggplot(plot.edb, aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  scale_color_manual(values=c("red", "blue"), name=NULL) +
  labs(x=NULL, y=NULL, title="Average values of sb variables over time",
       subtitle="Only countries included in 2004 report") +
  facet_wrap(~ variable, scales="free_y") + 
  theme_edb()

#' ### Countries in 2001 report
plot.edb.2001 <- edb.its.2001 %>%
  select(year, sb_days, sb_proced, sb_cost, sb_capital, con_days, con_proced) %>%
  gather(variable, value, -year) %>%
  group_by(year, variable) %>%
  summarise(avg = mean(value, na.rm=TRUE)) %>%
  filter(!is.nan(avg))

plot.interventions <- data_frame(year = 2005:2006,
                                 intervention = c("2005", "2006"))

#+ fig.width=6, fig.height=3.5
ggplot(plot.edb.2001, aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  scale_color_manual(values=c("red", "blue"), name=NULL) +
  labs(x=NULL, y=NULL, title="Average values of sb variables over time",
       subtitle="Only countries included in 2001 report") +
  facet_wrap(~ variable, scales="free_y") + 
  theme_edb()

#' ### Countries with reform committees in 2015
plot.edb.committee <- edb.its.committee %>%
  select(year, sb_days, sb_proced, sb_cost, sb_capital, con_days, con_proced) %>%
  gather(variable, value, -year) %>%
  group_by(year, variable) %>%
  summarise(avg = mean(value, na.rm=TRUE)) %>%
  filter(!is.nan(avg))

plot.interventions <- data_frame(year = 2005:2006,
                                 intervention = c("2005", "2006"))

#+ fig.width=6, fig.height=3.5
ggplot(plot.edb.committee, aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  scale_color_manual(values=c("red", "blue"), name=NULL) +
  labs(x=NULL, y=NULL, title="Average values of sb variables over time",
       subtitle="Only countries that have EDB reform committees by 2015") +
  facet_wrap(~ variable, scales="free_y") + 
  theme_edb()


#' ## Model results
#' 
#' ### Original models from spring version of paper
#' 
#' These are the models from the first column of the paper from April/May, just
#' for the sake of comparison. There are only tiny differences in coefficients,
#' since data from 1999 is now omitted (it wasn't in the original models).
#' 
#' <img src="img/original_stata_models.png" class="img-responsive"/>
#' 
model.proced.orig <- lm(sb_proced ~ sb_proced_lag + ranked.2005_lag + gdpcap_ln_lag + 
                          gdpgrowth_lag + polity_lag + pop_ln_lag, 
                        data=edb.its)
model.proced.orig.se <- robust.clusterify(model.proced.orig, edb.its, edb.its$ccode)

model.days.orig <- lm(sb_days ~ sb_days_lag + ranked.2005_lag + gdpcap_ln_lag + 
                          gdpgrowth_lag + polity_lag + pop_ln_lag, 
                        data=edb.its)
model.days.orig.se <- robust.clusterify(model.days.orig, edb.its, edb.its$ccode)

model.days_ln.orig <- lm(sb_days_ln ~ sb_days_ln_lag + ranked.2005_lag + gdpcap_ln_lag + 
                          gdpgrowth_lag + polity_lag + pop_ln_lag, 
                        data=edb.its)
model.days_ln.orig.se <- robust.clusterify(model.days_ln.orig, edb.its, edb.its$ccode)

model.cost_ln.orig <- lm(sb_cost_ln ~ sb_cost_ln_lag + ranked.2005_lag + gdpcap_ln_lag + 
                          gdpgrowth_lag + polity_lag + pop_ln_lag, 
                        data=edb.its)
model.cost_ln.orig.se <- robust.clusterify(model.cost_ln.orig, edb.its, edb.its$ccode)

model.capital_ln.orig <- lm(sb_capital_ln ~ sb_capital_ln_lag + ranked.2005_lag + gdpcap_ln_lag + 
                          gdpgrowth_lag + polity_lag + pop_ln_lag, 
                        data=edb.its)
model.capital_ln.orig.se <- robust.clusterify(model.capital_ln.orig, edb.its, edb.its$ccode)


model.proced.orig.nolag <- lm(sb_proced ~ ranked.2005_lag + gdpcap_ln_lag + 
                          gdpgrowth_lag + polity_lag + pop_ln_lag, 
                        data=edb.its)
model.proced.orig.nolag.se <- robust.clusterify(model.proced.orig.nolag, edb.its, edb.its$ccode)

model.days.orig.nolag <- lm(sb_days ~ ranked.2005_lag + gdpcap_ln_lag + 
                        gdpgrowth_lag + polity_lag + pop_ln_lag, 
                      data=edb.its)
model.days.orig.nolag.se <- robust.clusterify(model.days.orig.nolag, edb.its, edb.its$ccode)

model.days_ln.orig.nolag <- lm(sb_days_ln ~ ranked.2005_lag + gdpcap_ln_lag + 
                           gdpgrowth_lag + polity_lag + pop_ln_lag, 
                         data=edb.its)
model.days_ln.orig.nolag.se <- robust.clusterify(model.days_ln.orig.nolag, edb.its, edb.its$ccode)

model.cost_ln.orig.nolag <- lm(sb_cost_ln ~ ranked.2005_lag + gdpcap_ln_lag + 
                           gdpgrowth_lag + polity_lag + pop_ln_lag, 
                         data=edb.its)
model.cost_ln.orig.nolag.se <- robust.clusterify(model.cost_ln.orig.nolag, edb.its, edb.its$ccode)

model.capital_ln.orig.nolag <- lm(sb_capital_ln ~ ranked.2005_lag + gdpcap_ln_lag + 
                              gdpgrowth_lag + polity_lag + pop_ln_lag, 
                            data=edb.its)
model.capital_ln.orig.nolag.se <- robust.clusterify(model.capital_ln.orig.nolag, edb.its, edb.its$ccode)

#+ results="asis"
stargazer(model.proced.orig, model.proced.orig.nolag,
          model.days.orig, model.days.orig.nolag,
          model.days_ln.orig, model.days_ln.orig.nolag,
          model.cost_ln.orig, model.cost_ln.orig.nolag,
          model.capital_ln.orig, model.capital_ln.orig.nolag,
          se=list(model.proced.orig.se$coefs[,2], model.proced.orig.nolag.se$coefs[,2], 
                  model.days.orig.se$coefs[,2], model.days.orig.nolag.se$coefs[,2],
                  model.days_ln.orig.se$coefs[,2], model.days_ln.orig.nolag.se$coefs[,2],
                  model.cost_ln.orig.se$coefs[,2], model.cost_ln.orig.nolag.se$coefs[,2], 
                  model.capital_ln.orig.se$coefs[,2], model.capital_ln.orig.nolag.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 10))),
          notes="Robust standard errors clustered by country")


#' ### Simple interrupted time series
#' 
#' How to interpret coefficients:
#' 
#' - β~0~ = Constant: pre-period intercept - baseline pre intervention
#' - β~1~ = `year.centered`: pre-period slope - baseline time trend - level of increase prior to intervention
#' - β~2~ = `ranked`: immediate effect of the event - change in intercept at point of experiment
#' - β~3~ = `year.centered:ranked`: change in slope after the experiment - what happens after
#'
#' 
model.proced.2005 <- lm(sb_proced ~ 
                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                   data=edb.its)
model.proced.2005.se <- robust.clusterify(model.proced.2005, edb.its, edb.its$ccode)

model.days_ln.2005 <- lm(sb_days_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its)
model.days_ln.2005.se <- robust.clusterify(model.days_ln.2005, edb.its, edb.its$ccode)

model.cost_ln.2005 <- lm(sb_cost_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its)
model.cost_ln.2005.se <- robust.clusterify(model.cost_ln.2005, edb.its, edb.its$ccode)

model.capital_ln.2005 <- lm(sb_capital_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.cap.constrained)
model.capital_ln.2005.se <- robust.clusterify(model.capital_ln.2005, edb.its.cap.constrained, edb.its.cap.constrained$ccode)


model.proced.2006 <- lm(sb_proced ~ 
                     year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                   data=edb.its)
model.proced.2006.se <- robust.clusterify(model.proced.2006, edb.its, edb.its$ccode)

model.days_ln.2006 <- lm(sb_days_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its)
model.days_ln.2006.se <- robust.clusterify(model.days_ln.2006, edb.its, edb.its$ccode)

model.cost_ln.2006 <- lm(sb_cost_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its)
model.cost_ln.2006.se <- robust.clusterify(model.cost_ln.2006, edb.its, edb.its$ccode)

model.capital_ln.2006 <- lm(sb_capital_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.cap.constrained)
model.capital_ln.2006.se <- robust.clusterify(model.capital_ln.2006, edb.its.cap.constrained, edb.its.cap.constrained$ccode)


#+ results='asis'
stargazer(model.proced.2005, model.days_ln.2005,
          model.cost_ln.2005, model.capital_ln.2005,
          model.proced.2006, model.days_ln.2006,
          model.cost_ln.2006, model.capital_ln.2006,
          se=list(model.proced.2005.se$coefs[,2], model.days_ln.2005.se$coefs[,2],
                  model.cost_ln.2005.se$coefs[,2], model.capital_ln.2005.se$coefs[,2],
                  model.proced.2006.se$coefs[,2], model.days_ln.2006.se$coefs[,2],
                  model.cost_ln.2006.se$coefs[,2], model.capital_ln.2006.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 8))),
          notes="Robust standard errors clustered by country")

#' ### Simple ITS controlling for lags
model.proced.2005_lag <- lm(sb_proced ~ sb_proced_lag +
                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                   data=edb.its)
model.proced.2005_lag.se <- robust.clusterify(model.proced.2005_lag, edb.its, edb.its$ccode)

model.days_ln.2005_lag <- lm(sb_days_ln ~ sb_days_ln_lag +
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its)
model.days_ln.2005_lag.se <- robust.clusterify(model.days_ln.2005_lag, edb.its, edb.its$ccode)

model.cost_ln.2005_lag <- lm(sb_cost_ln ~ sb_cost_ln_lag +
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its)
model.cost_ln.2005_lag.se <- robust.clusterify(model.cost_ln.2005_lag, edb.its, edb.its$ccode)

model.capital_ln.2005_lag <- lm(sb_capital_ln ~ sb_capital_ln_lag +
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.cap.constrained)
model.capital_ln.2005_lag.se <- robust.clusterify(model.capital_ln.2005_lag, edb.its.cap.constrained, edb.its.cap.constrained$ccode)

#' Simple ITS models included as models 1, 3, 5, and 7 for comparison with
#' versions that control for lagged DV.
#' 
#+ results='asis'
stargazer(model.proced.2005, model.proced.2005_lag,
          model.days_ln.2005, model.days_ln.2005_lag,
          model.cost_ln.2005, model.cost_ln.2005_lag,
          model.capital_ln.2005, model.capital_ln.2005_lag,
          se=list(model.proced.2005.se$coefs[,2], model.proced.2005_lag.se$coefs[,2],
                  model.days_ln.2005.se$coefs[,2], model.days_ln.2005_lag.se$coefs[,2],
                  model.cost_ln.2005.se$coefs[,2], model.cost_ln.2005_lag.se$coefs[,2],
                  model.capital_ln.2005.se$coefs[,2], model.capital_ln.2005_lag.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes with lagged DV as control",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 8))),
          notes="Robust standard errors clustered by country")


#' ### Simple ITS, constrained to original 2001 countries
model.proced.2005.2001 <- lm(sb_proced ~ 
                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                   data=edb.its.2001)
model.proced.2005.2001.se <- robust.clusterify(model.proced.2005.2001, edb.its.2001, edb.its.2001$ccode)

model.days_ln.2005.2001 <- lm(sb_days_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.2001)
model.days_ln.2005.2001.se <- robust.clusterify(model.days_ln.2005.2001, edb.its.2001, edb.its.2001$ccode)

model.cost_ln.2005.2001 <- lm(sb_cost_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.2001)
model.cost_ln.2005.2001.se <- robust.clusterify(model.cost_ln.2005.2001, edb.its.2001, edb.its.2001$ccode)

model.capital_ln.2005.2001 <- lm(sb_capital_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.2001.cap.constrained)
model.capital_ln.2005.2001.se <- robust.clusterify(model.capital_ln.2005.2001, edb.its.2001.cap.constrained, edb.its.2001.cap.constrained$ccode)

model.capital_ln_controls.2005.2001 <- lm(sb_capital_ln ~ 
                                            year.centered.2005 + ranked.2005 + 
                                            year.centered.2005 * ranked.2005 + 
                                            gdpcap_ln_lag + gdpgrowth_lag + 
                                            pop_ln_lag + polity_lag +
                                            as.factor(ccode), 
                                          data=edb.its.2001.cap.constrained)
model.capital_ln_controls.2005.2001.se <- robust.clusterify(model.capital_ln_controls.2005.2001, edb.its.2001.cap.constrained, edb.its.2001.cap.constrained$ccode)

#' Models 2, 4, 6, and 8 are constrained to countries that appeared in the
#' original 2001 EDB report.
#' 
#+ results='asis'
stargazer(model.proced.2005, model.proced.2005.2001,
          model.days_ln.2005, model.days_ln.2005.2001,
          model.cost_ln.2005, model.cost_ln.2005.2001,
          model.capital_ln.2005, model.capital_ln.2005.2001,
          model.capital_ln_controls.2005.2001,
          se=list(model.proced.2005.se$coefs[,2], model.proced.2005.2001.se$coefs[,2],
                  model.days_ln.2005.se$coefs[,2], model.days_ln.2005.2001.se$coefs[,2],
                  model.cost_ln.2005.se$coefs[,2], model.cost_ln.2005.2001.se$coefs[,2],
                  model.capital_ln.2005.se$coefs[,2], model.capital_ln.2005.2001.se$coefs[,2],
                  model.capital_ln_controls.2005.2001.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes, limited to countries in 2001 report",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", c(rep("No", 8), "Yes"))),
          notes="Robust standard errors clustered by country")


#' ### Simple ITS in countries with reform committees by 2015
model.proced.committee <- lm(sb_proced ~ 
                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                   data=edb.its.committee)
model.proced.committee.se <- robust.clusterify(model.proced.committee, edb.its.committee, edb.its.committee$ccode)

model.days_ln.committee <- lm(sb_days_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.committee)
model.days_ln.committee.se <- robust.clusterify(model.days_ln.committee, edb.its.committee, edb.its.committee$ccode)

model.cost_ln.committee <- lm(sb_cost_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.committee)
model.cost_ln.committee.se <- robust.clusterify(model.cost_ln.committee, edb.its.committee, edb.its.committee$ccode)

model.capital_ln.committee <- lm(sb_capital_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.committee.cap.constrained)
model.capital_ln.committee.se <- robust.clusterify(model.capital_ln.committee, edb.its.committee.cap.constrained, edb.its.committee.cap.constrained$ccode)

model.capital_ln_controls.committee <- lm(sb_capital_ln ~ 
                                            year.centered.2005 + ranked.2005 + 
                                            year.centered.2005 * ranked.2005 + 
                                            gdpcap_ln_lag + gdpgrowth_lag + 
                                            pop_ln_lag + polity_lag +
                                            as.factor(ccode), 
                                          data=edb.its.committee.cap.constrained)
model.capital_ln_controls.committee.se <- robust.clusterify(model.capital_ln_controls.committee, edb.its.committee.cap.constrained, edb.its.committee.cap.constrained$ccode)

#+ results='asis'
stargazer(model.proced.committee,
          model.days_ln.committee,
          model.cost_ln.committee,
          model.capital_ln.committee,
          model.capital_ln_controls.committee,
          se=list(model.proced.committee.se$coefs[,2],
                  model.days_ln.committee.se$coefs[,2],
                  model.cost_ln.committee.se$coefs[,2],
                  model.capital_ln.committee.se$coefs[,2],
                  model.capital_ln_controls.committee.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes, limited to countries with EDB reform committees",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", c(rep("No", 4), "Yes"))),
          notes="Robust standard errors clustered by country")


#' ### Simple ITS + loans + country fixed effects
model.proced_loan_fe.2005 <- lm(sb_proced ~ 
                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005 +
                     loan_ln + as.factor(ccode), 
                   data=edb.its)
model.proced_loan_fe.2005.se <- robust.clusterify(model.proced_loan_fe.2005, edb.its, edb.its$ccode)

model.days_ln_loan_fe.2005 <- lm(sb_days_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005 +
                      loan_ln + as.factor(ccode), 
                    data=edb.its)
model.days_ln_loan_fe.2005.se <- robust.clusterify(model.days_ln_loan_fe.2005, edb.its, edb.its$ccode)

model.cost_ln_loan_fe.2005 <- lm(sb_cost_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005 +
                      loan_ln + as.factor(ccode), 
                    data=edb.its)
model.cost_ln_loan_fe.2005.se <- robust.clusterify(model.cost_ln_loan_fe.2005, edb.its, edb.its$ccode)

model.capital_ln_loan_fe.2005<- lm(sb_capital_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005 +
                      loan_ln + as.factor(ccode), 
                    data=edb.its.cap.constrained)
model.capital_ln_loan_fe.2005.se <- robust.clusterify(model.capital_ln_loan_fe.2005, edb.its.cap.constrained, edb.its.cap.constrained$ccode)


model.proced_loan_fe.2006 <- lm(sb_proced ~ 
                     year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006 +
                     loan_ln + as.factor(ccode), 
                   data=edb.its)
model.proced_loan_fe.2006.se <- robust.clusterify(model.proced_loan_fe.2006, edb.its, edb.its$ccode)

model.days_ln_loan_fe.2006 <- lm(sb_days_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006 +
                      loan_ln + as.factor(ccode), 
                    data=edb.its)
model.days_ln_loan_fe.2006.se <- robust.clusterify(model.days_ln_loan_fe.2006, edb.its, edb.its$ccode)

model.cost_ln_loan_fe.2006 <- lm(sb_cost_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006 +
                      loan_ln + as.factor(ccode), 
                    data=edb.its)
model.cost_ln_loan_fe.2006.se <- robust.clusterify(model.cost_ln_loan_fe.2006, edb.its, edb.its$ccode)

model.capital_ln_loan_fe.2006 <- lm(sb_capital_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006 +
                      loan_ln + as.factor(ccode), 
                    data=edb.its.cap.constrained)
model.capital_ln_loan_fe.2006.se <- robust.clusterify(model.capital_ln_loan_fe.2006, edb.its.cap.constrained, edb.its.cap.constrained$ccode)


#+ results='asis'
stargazer(model.proced_loan_fe.2005, model.days_ln_loan_fe.2005,
          model.cost_ln_loan_fe.2005, model.capital_ln_loan_fe.2005,
          model.proced_loan_fe.2006, model.days_ln_loan_fe.2006,
          model.cost_ln_loan_fe.2006, model.capital_ln_loan_fe.2006,
          se=list(model.proced_loan_fe.2005.se$coefs[,2], model.days_ln_loan_fe.2005.se$coefs[,2],
                  model.cost_ln_loan_fe.2005.se$coefs[,2], model.capital_ln_loan_fe.2005.se$coefs[,2],
                  model.proced_loan_fe.2006.se$coefs[,2], model.days_ln_loan_fe.2006.se$coefs[,2],
                  model.cost_ln_loan_fe.2006.se$coefs[,2], model.capital_ln_loan_fe.2006.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes (with loans and country fixed effects)",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("Yes", 8))),
          notes="Robust standard errors clustered by country")


#' ### ITS models + basic controls + country fixed effects
model.proced_controls_fe.2005 <- lm(sb_proced ~ 
                                 year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005 +
                                 gdpcap_ln_lag + gdpgrowth_lag + pop_ln_lag + polity_lag +
                                 as.factor(ccode), 
                               data=edb.its)
model.proced_controls_fe.2005.se <- robust.clusterify(model.proced_controls_fe.2005, edb.its, edb.its$ccode)

model.days_ln_controls_fe.2005 <- lm(sb_days_ln ~ 
                                  year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005 +
                                  gdpcap_ln_lag + gdpgrowth_lag + pop_ln_lag + polity_lag +
                                  as.factor(ccode), 
                                data=edb.its)
model.days_ln_controls_fe.2005.se <- robust.clusterify(model.days_ln_controls_fe.2005, edb.its, edb.its$ccode)

model.cost_ln_controls_fe.2005 <- lm(sb_cost_ln ~ 
                                  year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005 +
                                  gdpcap_ln_lag + gdpgrowth_lag + pop_ln_lag + polity_lag +
                                  as.factor(ccode), 
                                data=edb.its)
model.cost_ln_controls_fe.2005.se <- robust.clusterify(model.cost_ln_controls_fe.2005, edb.its, edb.its$ccode)

model.capital_ln_controls_fe.2005 <- lm(sb_capital_ln ~ 
                                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005 +
                                     gdpcap_ln_lag + gdpgrowth_lag + pop_ln_lag + polity_lag +
                                     as.factor(ccode), 
                                   data=edb.its.cap.constrained)
model.capital_ln_controls_fe.2005.se <- robust.clusterify(model.capital_ln_controls_fe.2005, edb.its.cap.constrained, edb.its.cap.constrained$ccode)


model.proced_controls_fe.2006 <- lm(sb_proced ~ 
                                 year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006 +
                                 gdpcap_ln_lag + gdpgrowth_lag + pop_ln_lag + polity_lag +
                                 as.factor(ccode), 
                               data=edb.its)
model.proced_controls_fe.2006.se <- robust.clusterify(model.proced_controls_fe.2006, edb.its, edb.its$ccode)

model.days_ln_controls_fe.2006 <- lm(sb_days_ln ~ 
                                  year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006 +
                                  gdpcap_ln_lag + gdpgrowth_lag + pop_ln_lag + polity_lag +
                                  as.factor(ccode), 
                                data=edb.its)
model.days_ln_controls_fe.2006.se <- robust.clusterify(model.days_ln_controls_fe.2006, edb.its, edb.its$ccode)

model.cost_ln_controls_fe.2006 <- lm(sb_cost_ln ~ 
                                  year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006 +
                                  gdpcap_ln_lag + gdpgrowth_lag + pop_ln_lag + polity_lag +
                                  as.factor(ccode), 
                                data=edb.its)
model.cost_ln_controls_fe.2006.se <- robust.clusterify(model.cost_ln_controls_fe.2006, edb.its, edb.its$ccode)

model.capital_ln_controls_fe.2006 <- lm(sb_capital_ln ~ 
                                     year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006 +
                                     gdpcap_ln_lag + gdpgrowth_lag + pop_ln_lag + polity_lag +
                                     as.factor(ccode), 
                                   data=edb.its.cap.constrained)
model.capital_ln_controls_fe.2006.se <- robust.clusterify(model.capital_ln_controls_fe.2006, edb.its.cap.constrained, edb.its.cap.constrained$ccode)


#+ results='asis'
stargazer(model.proced_controls_fe.2005, model.days_ln_controls_fe.2005,
          model.cost_ln_controls_fe.2005, model.capital_ln_controls_fe.2005,
          model.proced_controls_fe.2006, model.days_ln_controls_fe.2006,
          model.cost_ln_controls_fe.2006, model.capital_ln_controls_fe.2006,
          se=list(model.proced_controls_fe.2005.se$coefs[,2], model.days_ln_controls_fe.2005.se$coefs[,2],
                  model.cost_ln_controls_fe.2005.se$coefs[,2], model.capital_ln_controls_fe.2005.se$coefs[,2],
                  model.proced_controls_fe.2006.se$coefs[,2], model.days_ln_controls_fe.2006.se$coefs[,2],
                  model.cost_ln_controls_fe.2006.se$coefs[,2], model.capital_ln_controls_fe.2006.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes (with controls and country fixed effects)",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("Yes", 8))),
          notes="Robust standard errors clustered by country")


#' ## Figures
#' 
# #' ### Coefficient plots
# all.coefs <- bind_rows(mutate(tidy(model.proced, conf.int=TRUE), model="Simple", variable="sb_proced"),
#                        mutate(tidy(model.days, conf.int=TRUE), model="Simple", variable="sb_days"),
#                        mutate(tidy(model.days_ln, conf.int=TRUE), model="Simple", variable="sb_days_ln"),
#                        mutate(tidy(model.cost_ln, conf.int=TRUE), model="Simple", variable="sb_cost_ln"),
#                        mutate(tidy(model.capital_ln, conf.int=TRUE), model="Simple", variable="sb_capital_ln"),
#                        mutate(tidy(model.proced_con, conf.int=TRUE), model="Simple", variable="con_proced"),
#                        mutate(tidy(model.days_con, conf.int=TRUE), model="Simple", variable="con_days"),
#                        mutate(tidy(model.proced_con_ln, conf.int=TRUE), model="Simple", variable="con_proced_ln"),
#                        mutate(tidy(model.days_con_ln, conf.int=TRUE), model="Simple", variable="con_days_ln"),
#                        mutate(tidy(model.proced_fe, conf.int=TRUE), model="Simple + FE", variable="sb_proced"),
#                        mutate(tidy(model.days_fe, conf.int=TRUE), model="Simple + FE", variable="sb_days"),
#                        mutate(tidy(model.days_ln_fe, conf.int=TRUE), model="Simple + FE", variable="sb_days_ln"),
#                        mutate(tidy(model.cost_ln_fe, conf.int=TRUE), model="Simple + FE", variable="sb_cost_ln"),
#                        mutate(tidy(model.capital_ln_fe, conf.int=TRUE), model="Simple + FE", variable="sb_capital_ln"),
#                        mutate(tidy(model.proced_con_fe, conf.int=TRUE), model="Simple + FE", variable="con_proced"),
#                        mutate(tidy(model.days_con_fe, conf.int=TRUE), model="Simple + FE", variable="con_days"),
#                        mutate(tidy(model.proced_con_ln_fe, conf.int=TRUE), model="Simple + FE", variable="con_proced_ln"),
#                        mutate(tidy(model.days_con_ln_fe, conf.int=TRUE), model="Simple + FE", variable="con_days_ln"),
#                        mutate(tidy(model.proced_loan, conf.int=TRUE), model="Simple + loans", variable="sb_proced"),
#                        mutate(tidy(model.days_loan, conf.int=TRUE), model="Simple + loans", variable="sb_days"),
#                        mutate(tidy(model.days_ln_loan, conf.int=TRUE), model="Simple + loans", variable="sb_days_ln"),
#                        mutate(tidy(model.cost_ln_loan, conf.int=TRUE), model="Simple + loans", variable="sb_cost_ln"),
#                        mutate(tidy(model.capital_ln_loan, conf.int=TRUE), model="Simple + loans", variable="sb_capital_ln"),
#                        mutate(tidy(model.proced_con_loan, conf.int=TRUE), model="Simple + loans", variable="con_proced"),
#                        mutate(tidy(model.days_con_loan, conf.int=TRUE), model="Simple + loans", variable="con_days"),
#                        mutate(tidy(model.proced_con_ln_loan, conf.int=TRUE), model="Simple + loans", variable="con_proced_ln"),
#                        mutate(tidy(model.days_con_ln_loan, conf.int=TRUE), model="Simple + loans", variable="con_days_ln"),
#                        mutate(tidy(model.proced_controls, conf.int=TRUE), model="Controls", variable="sb_proced"),
#                        mutate(tidy(model.days_controls, conf.int=TRUE), model="Controls", variable="sb_days"),
#                        mutate(tidy(model.days_ln_controls, conf.int=TRUE), model="Controls", variable="sb_days_ln"),
#                        mutate(tidy(model.cost_ln_controls, conf.int=TRUE), model="Controls", variable="sb_cost_ln"),
#                        mutate(tidy(model.capital_ln_controls, conf.int=TRUE), model="Controls", variable="sb_capital_ln"),
#                        mutate(tidy(model.proced_con_controls, conf.int=TRUE), model="Controls", variable="con_proced"),
#                        mutate(tidy(model.days_con_controls, conf.int=TRUE), model="Controls", variable="con_days"),
#                        mutate(tidy(model.proced_con_ln_controls, conf.int=TRUE), model="Controls", variable="con_proced_ln"),
#                        mutate(tidy(model.days_con_ln_controls, conf.int=TRUE), model="Controls", variable="con_days_ln"),
#                        mutate(tidy(model.proced_controls_fe, conf.int=TRUE), model="Controls + FE", variable="sb_proced"),
#                        mutate(tidy(model.days_controls_fe, conf.int=TRUE), model="Controls + FE", variable="sb_days"),
#                        mutate(tidy(model.days_ln_controls_fe, conf.int=TRUE), model="Controls + FE", variable="sb_days_ln"),
#                        mutate(tidy(model.cost_ln_controls_fe, conf.int=TRUE), model="Controls + FE", variable="sb_cost_ln"),
#                        mutate(tidy(model.capital_ln_controls_fe, conf.int=TRUE), model="Controls + FE", variable="sb_capital_ln"),
#                        mutate(tidy(model.proced_con_controls_fe, conf.int=TRUE), model="Controls + FE", variable="con_proced"),
#                        mutate(tidy(model.days_con_controls_fe, conf.int=TRUE), model="Controls + FE", variable="con_days"),
#                        mutate(tidy(model.proced_con_ln_controls_fe, conf.int=TRUE), model="Controls + FE", variable="con_proced_ln"),
#                        mutate(tidy(model.days_con_ln_controls_fe, conf.int=TRUE), model="Controls + FE", variable="con_days_ln")) %>%
#   mutate(low = estimate - std.error,
#          high = estimate + std.error) %>%
#   mutate(model = factor(model, levels=rev(c("Simple", "Simple + FE", "Simple + loans", "Controls", "Controls + FE")), ordered=TRUE))

# plot.data <- all.coefs %>%
#   filter(term == "year.centered:rankedTRUE")

# #+ fig.width=6.5, fig.height=5
# ggplot(plot.data, aes(x=estimate, y=model, xmin=low, xmax=high, colour=model)) +
#   geom_vline(xintercept=0, colour="#8C2318", alpha=0.6, size=1) + 
#   geom_pointrangeh(position=position_dodge(width=.7)) + 
#   # scale_color_manual(values=c("#FF851C", "#85144A", "#001F40", "#2ECC40", "#ABABAB")) +
#   guides(colour="none") +
#   facet_wrap(~ variable, scales="free_x") + 
#   theme_edb() + theme(panel.background = element_rect(fill="grey95", colour=NA))


#' ### Simple models
plot.its(model=model.proced.2005, var.name="sb_proced",
         var.title="Procedures to open a business (2005)",
         y.title="Number of procedures", plot.year=2005)

plot.its(model=model.proced.2006, var.name="sb_proced",
         var.title="Procedures to open a business (2006)",
         y.title="Number of procedures", plot.year=2006)

plot.its(model=model.days_ln.2005, var.name="sb_days_ln",
         var.title="Days to open a business (logged) (2005)",
         y.title="Days (logged)", plot.year=2005)

plot.its(model=model.days_ln.2006, var.name="sb_days_ln",
         var.title="Days to open a business (logged) (2006)",
         y.title="Days (logged)", plot.year=2006)

plot.its(model=model.cost_ln.2005, var.name="sb_cost_ln",
         var.title="Cost to open a business (logged) (2005)",
         y.title="Dollars (logged)", plot.year=2005)

plot.its(model=model.cost_ln.2006, var.name="sb_cost_ln",
         var.title="Cost to open a business (logged) (2006)",
         y.title="Dollars (logged)", plot.year=2006)

plot.its(model=model.capital_ln.2005, var.name="sb_capital_ln",
         var.title="Capital to open a business (logged) (2005)",
         y.title="Dollars (logged)", plot.year=2005)

plot.its(model=model.capital_ln.2006, var.name="sb_capital_ln",
         var.title="Capital to open a business (logged) (2006)",
         y.title="Dollars (logged)", plot.year=2006)

