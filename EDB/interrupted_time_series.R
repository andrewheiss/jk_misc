#' ---
#' title: "EDB and interrupted time series models"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     css: html/fixes.css
#'     code_folding: hide
#'     toc: yes
#'     toc_float: true
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
library(gridExtra)
library(haven)
library(stargazer)
library(lubridate)
library(lazyeval)
library(sandwich)
library(lmtest)
library(broom)
library(pander)
library(countrycode)

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

edb.its.2001.committee <- edb.its %>%
  filter(in.2001 == 1) %>%
  filter(ccode %in% countries.with.edb.bureau$cowcode)

edb.its.2001.nocommittee <- edb.its %>%
  filter(in.2001 == 1) %>%
  filter(!(ccode %in% countries.with.edb.bureau$cowcode))

edb.its.cap.constrained <- filter(edb.its, year >= 2003)

edb.its.2001.cap.constrained <- filter(edb.its.2001, year >= 2003)

edb.its.committee.cap.constrained <- filter(edb.its.committee, year >= 2003)

edb.its.2001.committee.cap.constrained <- filter(edb.its.2001.committee, year >= 2003)
edb.its.2001.nocommittee.cap.constrained <- filter(edb.its.2001.nocommittee, year >= 2003)


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

#+ fig.width=9, fig.height=5.5
ggplot(plot.edb, aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  scale_color_manual(values=c("red", "blue"), name=NULL) +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  coord_cartesian(xlim=c(2000, 2015)) +
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

#+ fig.width=9, fig.height=5.5
ggplot(plot.edb.2001, aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  scale_color_manual(values=c("red", "blue"), name=NULL) +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  coord_cartesian(xlim=c(2000, 2015)) +
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

#+ fig.width=9, fig.height=5.5
ggplot(plot.edb.committee, aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  scale_color_manual(values=c("red", "blue"), name=NULL) +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  coord_cartesian(xlim=c(2000, 2015)) +
  labs(x=NULL, y=NULL, title="Average values of sb variables over time",
       subtitle="Only countries that have EDB reform committees by 2015") +
  facet_wrap(~ variable, scales="free_y") + 
  theme_edb()

#' ### Countries in report in 2001, with/without reform committees
plot.edb.committee.2001 <- edb.its.2001 %>%
  mutate(has.committee = factor(ccode %in% countries.with.edb.bureau$cowcode,
                                levels=c(FALSE, TRUE),
                                labels=c("No committee", "Committee"),
                                ordered=TRUE)) %>%
  select(year, has.committee, sb_days, sb_proced, sb_cost, sb_capital) %>%
  # select(year, has.committee, sb_days, sb_proced, sb_cost, sb_capital, con_days, con_proced) %>%
  gather(variable, value, -year, -has.committee) %>%
  group_by(year, variable, has.committee) %>%
  summarise(avg = mean(value, na.rm=TRUE)) %>%
  filter(!is.nan(avg)) %>%
  ungroup() %>%
  mutate(variable = factor(variable, levels=c("sb_proced", "sb_days",
                                              "sb_cost", "sb_capital"),
                           labels=c("Procedures", "Days", "Cost", "Capital"),
                           ordered=TRUE))

plot.interventions <- data_frame(year = 2005:2006,
                                 intervention = c("2005", "2006"))

# plot.con_days <- ggplot(filter(plot.edb.committee.2001, variable=="con_days"), 
#                         aes(x=year, y=avg)) +
#   geom_vline(data=plot.interventions, aes(xintercept=year,
#                                           colour=intervention),
#              linetype="dashed", size=0.5) +
#   geom_line() + 
#   labs(x=NULL, y="Days") +
#   scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
#   coord_cartesian(xlim=c(2000, 2015)) +
#   scale_color_manual(values=c("red", "blue"), name=NULL, guide=FALSE) +
#   facet_wrap(~ variable + has.committee) + 
#   theme_edb()
# 
# plot.con_proced <- ggplot(filter(plot.edb.committee.2001, variable=="con_proced"), 
#                         aes(x=year, y=avg)) +
#   geom_vline(data=plot.interventions, aes(xintercept=year,
#                                           colour=intervention),
#              linetype="dashed", size=0.5) +
#   geom_line() + 
#   labs(x=NULL, y="Procedures") +
#   scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
#   coord_cartesian(xlim=c(2000, 2015)) +
#   scale_color_manual(values=c("red", "blue"), name=NULL, guide=FALSE) +
#   facet_wrap(~ variable + has.committee) + 
#   theme_edb()

plot.sb_capital <- ggplot(filter(plot.edb.committee.2001, variable=="Capital"), 
                        aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  labs(x=NULL, y="Percent") +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  coord_cartesian(xlim=c(2000, 2015)) +
  scale_color_manual(values=c("red", "blue"), name=NULL, guide=FALSE) +
  facet_wrap(~ variable + has.committee) + 
  theme_edb() + theme(axis.text.x = element_text(angle=45, hjust=1))

plot.sb_cost <- ggplot(filter(plot.edb.committee.2001, variable=="Cost"), 
                        aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  labs(x=NULL, y="Percent") +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  coord_cartesian(xlim=c(2000, 2015)) +
  scale_color_manual(values=c("red", "blue"), name=NULL, guide=FALSE) +
  facet_wrap(~ variable + has.committee) + 
  theme_edb() + theme(axis.text.x = element_text(angle=45, hjust=1))

plot.sb_days <- ggplot(filter(plot.edb.committee.2001, variable=="Days"), 
                        aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  labs(x=NULL, y="Days") +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  coord_cartesian(xlim=c(2000, 2015)) +
  scale_color_manual(values=c("red", "blue"), name=NULL, guide=FALSE) +
  facet_wrap(~ variable + has.committee) + 
  theme_edb() + theme(axis.text.x = element_text(angle=45, hjust=1))

plot.sb_proced <- ggplot(filter(plot.edb.committee.2001, variable=="Procedures"), 
                        aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  labs(x=NULL, y="Procedures") +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  coord_cartesian(xlim=c(2000, 2015)) +
  scale_color_manual(values=c("red", "blue"), name=NULL, guide=FALSE) +
  facet_wrap(~ variable + has.committee) + 
  theme_edb() + theme(axis.text.x = element_text(angle=45, hjust=1))

plot.with.committees <- ggplot(filter(plot.edb.committee.2001, has.committee=="Committee"), 
                         aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  labs(x=NULL, y=NULL, title="Countries with reform committees") +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  coord_cartesian(xlim=c(2000, 2015)) +
  scale_color_manual(values=c("red", "blue"), name=NULL, guide=FALSE) +
  facet_wrap(~ variable, scales="free_y") + 
  theme_edb()# + theme(axis.text.x = element_text(angle=45, hjust=1))

plot.without.committees <- ggplot(filter(plot.edb.committee.2001, has.committee=="No committee"), 
                               aes(x=year, y=avg)) +
  geom_vline(data=plot.interventions, aes(xintercept=year,
                                          colour=intervention),
             linetype="dashed", size=0.5) +
  geom_line() + 
  labs(x=NULL, y=NULL, title="Countries without reform committees") +
  scale_x_continuous(limits=c(2000, 2015), breaks=seq(2000, 2015, 5)) +
  coord_cartesian(xlim=c(2000, 2015)) +
  scale_color_manual(values=c("red", "blue"), name=NULL, guide=FALSE) +
  facet_wrap(~ variable, scales="free_y") + 
  theme_edb()# + theme(axis.text.x = element_text(angle=45, hjust=1))

#+ fig.width=5, fig.height=4
plot.without.committees

#+ fig.width=5, fig.height=4
plot.with.committees

#+ fig.width=10, fig.height=4
grid.arrange(plot.without.committees, plot.with.committees, ncol=2)



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


#' ### Simple: In since 2001
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


model.proced.2006.2001 <- lm(sb_proced ~ 
                     year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                   data=edb.its.2001)
model.proced.2006.2001.se <- robust.clusterify(model.proced.2006.2001, edb.its.2001, edb.its.2001$ccode)

model.days_ln.2006.2001 <- lm(sb_days_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.2001)
model.days_ln.2006.2001.se <- robust.clusterify(model.days_ln.2006.2001, edb.its.2001, edb.its.2001$ccode)

model.cost_ln.2006.2001 <- lm(sb_cost_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.2001)
model.cost_ln.2006.2001.se <- robust.clusterify(model.cost_ln.2006.2001, edb.its.2001, edb.its.2001$ccode)

model.capital_ln.2006.2001 <- lm(sb_capital_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.2001.cap.constrained)
model.capital_ln.2006.2001.se <- robust.clusterify(model.capital_ln.2006.2001, edb.its.2001.cap.constrained, edb.its.2001.cap.constrained$ccode)


#+ results='asis'
stargazer(model.proced.2005.2001, model.days_ln.2005.2001,
          model.cost_ln.2005.2001, model.capital_ln.2005.2001,
          model.proced.2006.2001, model.days_ln.2006.2001,
          model.cost_ln.2006.2001, model.capital_ln.2006.2001,
          se=list(model.proced.2005.2001.se$coefs[,2], model.days_ln.2005.2001.se$coefs[,2],
                  model.cost_ln.2005.2001.se$coefs[,2], model.capital_ln.2005.2001.se$coefs[,2],
                  model.proced.2006.2001.se$coefs[,2], model.days_ln.2006.2001.se$coefs[,2],
                  model.cost_ln.2006.2001.se$coefs[,2], model.capital_ln.2006.2001.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes, only countries in 2001 report",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 8))),
          notes="Robust standard errors clustered by country")


#' ### Simple: Only those with reform committees
model.proced.2005.committee <- lm(sb_proced ~ 
                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                   data=edb.its.committee)
model.proced.2005.committee.se <- robust.clusterify(model.proced.2005.committee, edb.its.committee, edb.its.committee$ccode)

model.days_ln.2005.committee <- lm(sb_days_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.committee)
model.days_ln.2005.committee.se <- robust.clusterify(model.days_ln.2005.committee, edb.its.committee, edb.its.committee$ccode)

model.cost_ln.2005.committee <- lm(sb_cost_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.committee)
model.cost_ln.2005.committee.se <- robust.clusterify(model.cost_ln.2005.committee, edb.its.committee, edb.its.committee$ccode)

model.capital_ln.2005.committee <- lm(sb_capital_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.committee.cap.constrained)
model.capital_ln.2005.committee.se <- robust.clusterify(model.capital_ln.2005.committee, edb.its.committee.cap.constrained, edb.its.committee.cap.constrained$ccode)


model.proced.2006.committee <- lm(sb_proced ~ 
                     year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                   data=edb.its.committee)
model.proced.2006.committee.se <- robust.clusterify(model.proced.2006.committee, edb.its.committee, edb.its.committee$ccode)

model.days_ln.2006.committee <- lm(sb_days_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.committee)
model.days_ln.2006.committee.se <- robust.clusterify(model.days_ln.2006.committee, edb.its.committee, edb.its.committee$ccode)

model.cost_ln.2006.committee <- lm(sb_cost_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.committee)
model.cost_ln.2006.committee.se <- robust.clusterify(model.cost_ln.2006.committee, edb.its.committee, edb.its.committee$ccode)

model.capital_ln.2006.committee <- lm(sb_capital_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.committee.cap.constrained)
model.capital_ln.2006.committee.se <- robust.clusterify(model.capital_ln.2006.committee, edb.its.committee.cap.constrained, edb.its.committee.cap.constrained$ccode)


#+ results='asis'
stargazer(model.proced.2005.committee, model.days_ln.2005.committee,
          model.cost_ln.2005.committee, model.capital_ln.2005.committee,
          model.proced.2006.committee, model.days_ln.2006.committee,
          model.cost_ln.2006.committee, model.capital_ln.2006.committee,
          se=list(model.proced.2005.committee.se$coefs[,2], model.days_ln.2005.committee.se$coefs[,2],
                  model.cost_ln.2005.committee.se$coefs[,2], model.capital_ln.2005.committee.se$coefs[,2],
                  model.proced.2006.committee.se$coefs[,2], model.days_ln.2006.committee.se$coefs[,2],
                  model.cost_ln.2006.committee.se$coefs[,2], model.capital_ln.2006.committee.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes, only countries with EDB committees",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 8))),
          notes="Robust standard errors clustered by country")


#' ### Simple: In since 2001 and with reform committees
model.proced.2005.2001.committee <- lm(sb_proced ~ 
                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                   data=edb.its.2001.committee)
model.proced.2005.2001.committee.se <- robust.clusterify(model.proced.2005.2001.committee, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.days_ln.2005.2001.committee <- lm(sb_days_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.2001.committee)
model.days_ln.2005.2001.committee.se <- robust.clusterify(model.days_ln.2005.2001.committee, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.cost_ln.2005.2001.committee <- lm(sb_cost_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.2001.committee)
model.cost_ln.2005.2001.committee.se <- robust.clusterify(model.cost_ln.2005.2001.committee, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.capital_ln.2005.2001.committee <- lm(sb_capital_ln ~ 
                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005, 
                    data=edb.its.2001.committee.cap.constrained)
model.capital_ln.2005.2001.committee.se <- robust.clusterify(model.capital_ln.2005.2001.committee, edb.its.2001.committee.cap.constrained, edb.its.2001.committee.cap.constrained$ccode)


model.proced.2006.2001.committee <- lm(sb_proced ~ 
                     year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                   data=edb.its.2001.committee)
model.proced.2006.2001.committee.se <- robust.clusterify(model.proced.2006.2001.committee, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.days_ln.2006.2001.committee <- lm(sb_days_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.2001.committee)
model.days_ln.2006.2001.committee.se <- robust.clusterify(model.days_ln.2006.2001.committee, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.cost_ln.2006.2001.committee <- lm(sb_cost_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.2001.committee)
model.cost_ln.2006.2001.committee.se <- robust.clusterify(model.cost_ln.2006.2001.committee, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.capital_ln.2006.2001.committee <- lm(sb_capital_ln ~ 
                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006, 
                    data=edb.its.2001.committee.cap.constrained)
model.capital_ln.2006.2001.committee.se <- robust.clusterify(model.capital_ln.2006.2001.committee, edb.its.2001.committee.cap.constrained, edb.its.2001.committee.cap.constrained$ccode)


#+ results='asis'
stargazer(model.proced.2005.2001.committee, model.days_ln.2005.2001.committee,
          model.cost_ln.2005.2001.committee, model.capital_ln.2005.2001.committee,
          model.proced.2006.2001.committee, model.days_ln.2006.2001.committee,
          model.cost_ln.2006.2001.committee, model.capital_ln.2006.2001.committee,
          se=list(model.proced.2005.2001.committee.se$coefs[,2], model.days_ln.2005.2001.committee.se$coefs[,2],
                  model.cost_ln.2005.2001.committee.se$coefs[,2], model.capital_ln.2005.2001.committee.se$coefs[,2],
                  model.proced.2006.2001.committee.se$coefs[,2], model.days_ln.2006.2001.committee.se$coefs[,2],
                  model.cost_ln.2006.2001.committee.se$coefs[,2], model.capital_ln.2006.2001.committee.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes, only countries in 2001 report with EDB committees",
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


#' ## Seth's ideas: difference-ish models
#' 
#' $$
#' y_t = \beta_0 + \beta_1 y_{t-1} + \beta_2 T + \epsilon
#' $$
#' 
#' ### Models for all countries in 2001 report
model.proced.lag.2005.all <- lm(sb_proced ~ sb_proced_lag + ranked.2005, 
                                data=edb.its.2001)
model.proced.lag.2005.all.se <- robust.clusterify(model.proced.lag.2005.all, edb.its.2001, edb.its.2001$ccode)

model.days_ln.lag.2005.all <- lm(sb_days_ln ~ sb_days_ln_lag + ranked.2005, 
                                 data=edb.its.2001)
model.days_ln.lag.2005.all.se <- robust.clusterify(model.days_ln.lag.2005.all, edb.its.2001, edb.its.2001$ccode)

model.cost_ln.lag.2005.all <- lm(sb_cost_ln ~ sb_cost_ln_lag + ranked.2005, 
                                 data=edb.its.2001)
model.cost_ln.lag.2005.all.se <- robust.clusterify(model.cost_ln.lag.2005.all, edb.its.2001, edb.its.2001$ccode)

model.capital_ln.lag.2005.all <- lm(sb_capital_ln ~ sb_capital_ln_lag + ranked.2005, 
                                    data=edb.its.2001.cap.constrained)
model.capital_ln.lag.2005.all.se <- robust.clusterify(model.capital_ln.lag.2005.all, edb.its.2001.cap.constrained, edb.its.2001.cap.constrained$ccode)


model.proced.lag.2006.all <- lm(sb_proced ~ sb_proced_lag + ranked.2006, 
                                data=edb.its.2001)
model.proced.lag.2006.all.se <- robust.clusterify(model.proced.lag.2006.all, edb.its.2001, edb.its.2001$ccode)

model.days_ln.lag.2006.all <- lm(sb_days_ln ~ sb_days_ln_lag + ranked.2006, 
                                 data=edb.its.2001)
model.days_ln.lag.2006.all.se <- robust.clusterify(model.days_ln.lag.2006.all, edb.its.2001, edb.its.2001$ccode)

model.cost_ln.lag.2006.all <- lm(sb_cost_ln ~ sb_cost_ln_lag + ranked.2006, 
                                 data=edb.its.2001)
model.cost_ln.lag.2006.all.se <- robust.clusterify(model.cost_ln.lag.2006.all, edb.its.2001, edb.its.2001$ccode)

model.capital_ln.lag.2006.all <- lm(sb_capital_ln ~ sb_capital_ln_lag + ranked.2006, 
                                    data=edb.its.2001.cap.constrained)
model.capital_ln.lag.2006.all.se <- robust.clusterify(model.capital_ln.lag.2006.all, edb.its.2001.cap.constrained, edb.its.2001.cap.constrained$ccode)


#
#+ results='asis'
stargazer(model.proced.lag.2005.all, model.days_ln.lag.2005.all,
          model.cost_ln.lag.2005.all, model.capital_ln.lag.2005.all,
          model.proced.lag.2006.all, model.days_ln.lag.2006.all,
          model.cost_ln.lag.2006.all, model.capital_ln.lag.2006.all,
          se=list(model.proced.lag.2005.all.se$coefs[,2], model.days_ln.lag.2005.all.se$coefs[,2],
                  model.cost_ln.lag.2005.all.se$coefs[,2], model.capital_ln.lag.2005.all.se$coefs[,2],
                  model.proced.lag.2006.all.se$coefs[,2], model.days_ln.lag.2006.all.se$coefs[,2],
                  model.cost_ln.lag.2006.all.se$coefs[,2], model.capital_ln.lag.2006.all.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes (all countries from 2001 report)",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 8))),
          notes="Robust standard errors clustered by country")


#' ### Models for countries with/without reform committees
model.proced.lag.2005.nocom <- lm(sb_proced ~ sb_proced_lag + ranked.2005, 
                        data=edb.its.2001.nocommittee)
model.proced.lag.2005.nocom.se <- robust.clusterify(model.proced.lag.2005.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.days_ln.lag.2005.nocom <- lm(sb_days_ln ~ sb_days_ln_lag + ranked.2005, 
                         data=edb.its.2001.nocommittee)
model.days_ln.lag.2005.nocom.se <- robust.clusterify(model.days_ln.lag.2005.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.cost_ln.lag.2005.nocom <- lm(sb_cost_ln ~ sb_cost_ln_lag + ranked.2005, 
                         data=edb.its.2001.nocommittee)
model.cost_ln.lag.2005.nocom.se <- robust.clusterify(model.cost_ln.lag.2005.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.capital_ln.lag.2005.nocom <- lm(sb_capital_ln ~ sb_capital_ln_lag + ranked.2005, 
                            data=edb.its.2001.nocommittee.cap.constrained)
model.capital_ln.lag.2005.nocom.se <- robust.clusterify(model.capital_ln.lag.2005.nocom, edb.its.2001.nocommittee.cap.constrained, edb.its.2001.nocommittee.cap.constrained$ccode)


model.proced.lag.2006.nocom <- lm(sb_proced ~ sb_proced_lag + ranked.2006, 
                        data=edb.its.2001.nocommittee)
model.proced.lag.2006.nocom.se <- robust.clusterify(model.proced.lag.2006.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.days_ln.lag.2006.nocom <- lm(sb_days_ln ~ sb_days_ln_lag + ranked.2006, 
                         data=edb.its.2001.nocommittee)
model.days_ln.lag.2006.nocom.se <- robust.clusterify(model.days_ln.lag.2006.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.cost_ln.lag.2006.nocom <- lm(sb_cost_ln ~ sb_cost_ln_lag + ranked.2006, 
                         data=edb.its.2001.nocommittee)
model.cost_ln.lag.2006.nocom.se <- robust.clusterify(model.cost_ln.lag.2006.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.capital_ln.lag.2006.nocom <- lm(sb_capital_ln ~ sb_capital_ln_lag + ranked.2006, 
                            data=edb.its.2001.nocommittee.cap.constrained)
model.capital_ln.lag.2006.nocom.se <- robust.clusterify(model.capital_ln.lag.2006.nocom, edb.its.2001.nocommittee.cap.constrained, edb.its.2001.nocommittee.cap.constrained$ccode)


model.proced.lag.2005.com <- lm(sb_proced ~ sb_proced_lag + ranked.2005, 
                        data=edb.its.2001.committee)
model.proced.lag.2005.com.se <- robust.clusterify(model.proced.lag.2005.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.days_ln.lag.2005.com <- lm(sb_days_ln ~ sb_days_ln_lag + ranked.2005, 
                         data=edb.its.2001.committee)
model.days_ln.lag.2005.com.se <- robust.clusterify(model.days_ln.lag.2005.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.cost_ln.lag.2005.com <- lm(sb_cost_ln ~ sb_cost_ln_lag + ranked.2005, 
                         data=edb.its.2001.committee)
model.cost_ln.lag.2005.com.se <- robust.clusterify(model.cost_ln.lag.2005.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.capital_ln.lag.2005.com <- lm(sb_capital_ln ~ sb_capital_ln_lag + ranked.2005, 
                            data=edb.its.2001.committee.cap.constrained)
model.capital_ln.lag.2005.com.se <- robust.clusterify(model.capital_ln.lag.2005.com, edb.its.2001.committee.cap.constrained, edb.its.2001.committee.cap.constrained$ccode)


model.proced.lag.2006.com <- lm(sb_proced ~ sb_proced_lag + ranked.2006, 
                        data=edb.its.2001.committee)
model.proced.lag.2006.com.se <- robust.clusterify(model.proced.lag.2006.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.days_ln.lag.2006.com <- lm(sb_days_ln ~ sb_days_ln_lag + ranked.2006, 
                         data=edb.its.2001.committee)
model.days_ln.lag.2006.com.se <- robust.clusterify(model.days_ln.lag.2006.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.cost_ln.lag.2006.com <- lm(sb_cost_ln ~ sb_cost_ln_lag + ranked.2006, 
                         data=edb.its.2001.committee)
model.cost_ln.lag.2006.com.se <- robust.clusterify(model.cost_ln.lag.2006.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.capital_ln.lag.2006.com <- lm(sb_capital_ln ~ sb_capital_ln_lag + ranked.2006, 
                            data=edb.its.2001.committee.cap.constrained)
model.capital_ln.lag.2006.com.se <- robust.clusterify(model.capital_ln.lag.2006.com, edb.its.2001.committee.cap.constrained, edb.its.2001.committee.cap.constrained$ccode)


#+ results='asis'
stargazer(model.proced.lag.2005.nocom, model.proced.lag.2005.com,
          model.days_ln.lag.2005.nocom, model.days_ln.lag.2005.com,
          model.proced.lag.2006.nocom, model.proced.lag.2006.com,
          model.days_ln.lag.2006.nocom, model.days_ln.lag.2006.com,
          se=list(model.proced.lag.2005.nocom$coefs[,2], model.proced.lag.2005.com$coefs[,2],
                  model.days_ln.lag.2005.nocom$coefs[,2], model.days_ln.lag.2005.com$coefs[,2],
                  model.proced.lag.2006.nocom$coefs[,2], model.proced.lag.2006.com$coefs[,2],
                  model.days_ln.lag.2006.nocom$coefs[,2], model.days_ln.lag.2006.com$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 8)),
                         c("EDB reform committee", rep(c("No", "Yes"), 4))),
          notes="Robust standard errors clustered by country")

#+ results='asis'
stargazer(model.cost_ln.lag.2005.nocom, model.cost_ln.lag.2005.com,
          model.capital_ln.lag.2005.nocom, model.capital_ln.lag.2005.com,
          model.cost_ln.lag.2006.nocom, model.cost_ln.lag.2006.com,
          model.capital_ln.lag.2006.nocom, model.capital_ln.lag.2006.com,
          se=list(model.cost_ln.lag.2005.nocom$coefs[,2], model.cost_ln.lag.2005.com$coefs[,2],
                  model.capital_ln.lag.2005.nocom$coefs[,2], model.capital_ln.lag.2005.com$coefs[,2],
                  model.cost_ln.lag.2006.nocom$coefs[,2], model.cost_ln.lag.2006.com$coefs[,2],
                  model.capital_ln.lag.2006.nocom$coefs[,2], model.capital_ln.lag.2006.com$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 8)),
                         c("EDB reform committee", rep(c("No", "Yes"), 4))),
          notes="Robust standard errors clustered by country")


#' ### ITS for countries in 2001 report, with and without reform committees
model.proced.lag.2005.its.nocom <- lm(sb_proced ~
                                      year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005,
                        data=edb.its.2001.nocommittee)
model.proced.lag.2005.its.nocom.se <- robust.clusterify(model.proced.lag.2005.its.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.days_ln.lag.2005.its.nocom <- lm(sb_days_ln ~
                                       year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005,
                         data=edb.its.2001.nocommittee)
model.days_ln.lag.2005.its.nocom.se <- robust.clusterify(model.days_ln.lag.2005.its.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.cost_ln.lag.2005.its.nocom <- lm(sb_cost_ln ~
                                       year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005,
                         data=edb.its.2001.nocommittee)
model.cost_ln.lag.2005.its.nocom.se <- robust.clusterify(model.cost_ln.lag.2005.its.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.capital_ln.lag.2005.its.nocom <- lm(sb_capital_ln ~
                                          year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005,
                            data=edb.its.2001.nocommittee.cap.constrained)
model.capital_ln.lag.2005.its.nocom.se <- robust.clusterify(model.capital_ln.lag.2005.its.nocom, edb.its.2001.nocommittee.cap.constrained, edb.its.2001.nocommittee.cap.constrained$ccode)


model.proced.lag.2006.its.nocom <- lm(sb_proced ~
                                      year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006,
                        data=edb.its.2001.nocommittee)
model.proced.lag.2006.its.nocom.se <- robust.clusterify(model.proced.lag.2006.its.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.days_ln.lag.2006.its.nocom <- lm(sb_days_ln ~
                                       year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006,
                         data=edb.its.2001.nocommittee)
model.days_ln.lag.2006.its.nocom.se <- robust.clusterify(model.days_ln.lag.2006.its.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.cost_ln.lag.2006.its.nocom <- lm(sb_cost_ln ~
                                       year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006,
                         data=edb.its.2001.nocommittee)
model.cost_ln.lag.2006.its.nocom.se <- robust.clusterify(model.cost_ln.lag.2006.its.nocom, edb.its.2001.nocommittee, edb.its.2001.nocommittee$ccode)

model.capital_ln.lag.2006.its.nocom <- lm(sb_capital_ln ~
                                          year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006,
                            data=edb.its.2001.nocommittee.cap.constrained)
model.capital_ln.lag.2006.its.nocom.se <- robust.clusterify(model.capital_ln.lag.2006.its.nocom, edb.its.2001.nocommittee.cap.constrained, edb.its.2001.nocommittee.cap.constrained$ccode)


model.proced.lag.2005.its.com <- lm(sb_proced ~
                                    year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005,
                        data=edb.its.2001.committee)
model.proced.lag.2005.its.com.se <- robust.clusterify(model.proced.lag.2005.its.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.days_ln.lag.2005.its.com <- lm(sb_days_ln ~
                                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005,
                         data=edb.its.2001.committee)
model.days_ln.lag.2005.its.com.se <- robust.clusterify(model.days_ln.lag.2005.its.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.cost_ln.lag.2005.its.com <- lm(sb_cost_ln ~
                                     year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005,
                         data=edb.its.2001.committee)
model.cost_ln.lag.2005.its.com.se <- robust.clusterify(model.cost_ln.lag.2005.its.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.capital_ln.lag.2005.its.com <- lm(sb_capital_ln ~
                                        year.centered.2005 + ranked.2005 + year.centered.2005 * ranked.2005,
                            data=edb.its.2001.committee.cap.constrained)
model.capital_ln.lag.2005.its.com.se <- robust.clusterify(model.capital_ln.lag.2005.its.com, edb.its.2001.committee.cap.constrained, edb.its.2001.committee.cap.constrained$ccode)


model.proced.lag.2006.its.com <- lm(sb_proced ~ 
                                    year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006,
                        data=edb.its.2001.committee)
model.proced.lag.2006.its.com.se <- robust.clusterify(model.proced.lag.2006.its.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.days_ln.lag.2006.its.com <- lm(sb_days_ln ~ 
                                     year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006,
                         data=edb.its.2001.committee)
model.days_ln.lag.2006.its.com.se <- robust.clusterify(model.days_ln.lag.2006.its.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.cost_ln.lag.2006.its.com <- lm(sb_cost_ln ~ 
                                     year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006,
                         data=edb.its.2001.committee)
model.cost_ln.lag.2006.its.com.se <- robust.clusterify(model.cost_ln.lag.2006.its.com, edb.its.2001.committee, edb.its.2001.committee$ccode)

model.capital_ln.lag.2006.its.com <- lm(sb_capital_ln ~ 
                                        year.centered.2006 + ranked.2006 + year.centered.2006 * ranked.2006,
                            data=edb.its.2001.committee.cap.constrained)
model.capital_ln.lag.2006.its.com.se <- robust.clusterify(model.capital_ln.lag.2006.its.com, edb.its.2001.committee.cap.constrained, edb.its.2001.committee.cap.constrained$ccode)


#+ results='asis'
stargazer(model.proced.lag.2005.its.nocom, model.proced.lag.2005.its.com,
          model.days_ln.lag.2005.its.nocom, model.days_ln.lag.2005.its.com,
          model.proced.lag.2006.its.nocom, model.proced.lag.2006.its.com,
          model.days_ln.lag.2006.its.nocom, model.days_ln.lag.2006.its.com,
          se=list(model.proced.lag.2005.its.nocom.se$coefs[,2], model.proced.lag.2005.its.com.se$coefs[,2],
                  model.days_ln.lag.2005.its.nocom.se$coefs[,2], model.days_ln.lag.2005.its.com.se$coefs[,2],
                  model.proced.lag.2006.its.nocom.se$coefs[,2], model.proced.lag.2006.its.com.se$coefs[,2],
                  model.days_ln.lag.2006.its.nocom.se$coefs[,2], model.days_ln.lag.2006.its.com.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes (ITS, with/without reform committees)",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 8)),
                         c("EDB reform committee", rep(c("No", "Yes"), 4))),
          notes="Robust standard errors clustered by country")

#+ results='asis'
stargazer(model.cost_ln.lag.2005.its.nocom, model.cost_ln.lag.2005.its.com,
          model.capital_ln.lag.2005.its.nocom, model.capital_ln.lag.2005.its.com,
          model.cost_ln.lag.2006.its.nocom, model.cost_ln.lag.2006.its.com,
          model.capital_ln.lag.2006.its.nocom, model.capital_ln.lag.2006.its.com,
          se=list(model.cost_ln.lag.2005.its.nocom.se$coefs[,2], model.cost_ln.lag.2005.its.com.se$coefs[,2],
                  model.capital_ln.lag.2005.its.nocom.se$coefs[,2], model.capital_ln.lag.2005.its.com.se$coefs[,2],
                  model.cost_ln.lag.2006.its.nocom.se$coefs[,2], model.cost_ln.lag.2006.its.com.se$coefs[,2],
                  model.capital_ln.lag.2006.its.nocom.se$coefs[,2], model.capital_ln.lag.2006.its.com.se$coefs[,2]),
          type="html", dep.var.caption="EDB outcomes (ITS, with/without reform committees)",
          intercept.bottom=FALSE,
          omit="\\.factor",
          add.lines=list(c("Year fixed effects", rep("No", 8)),
                         c("EDB reform committee", rep(c("No", "Yes"), 4))),
          notes="Robust standard errors clustered by country")


#' ### Flexible ITS (with year²)
# make t more flexible in ITS by squaring it

#' ## List of countries in initial 2001 report
#' 
#' \* indicates country has an EDB reform committee by 2015
country.names <- edb.its.2001 %>%
  group_by(ccode) %>%
  summarise(cow = unique(ccode)) %>%
  ungroup() %>%
  mutate(has.committee = ifelse(ccode %in% countries.with.edb.bureau$cowcode, "\\*", ""),
         Country = countrycode(cow, "cown", "country.name")) %>%
  mutate(Country = case_when(
           .$cow == 1001 ~ "Serbia",
           .$cow == 1005 ~ "Hong Kong",
           TRUE ~ .$Country
         )) %>%
  arrange(Country) %>%
  mutate(Country = paste0(Country, has.committee)) %>%
  select(Country)

#+ results="asis"
pandoc.table(matrix(c(country.names$Country, rep("", 2)), ncol=4),
             split.tables=Inf)


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

