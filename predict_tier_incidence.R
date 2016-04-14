#' ---
#' title: "Predicting tier ratings with trafficking incidence"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document:
#'     css: fixes.css
#'     toc: yes
#'     highlight: pygments
#'     theme: flatly
#'     self_contained: no
#' ---

#' The seriousness of trafficking in a country influences its tier rating.
#' Countries are more likely to receive worse tier ratings when they act as
#' either an origin or a destination country. This likely shows that the TIP
#' report picks up on increases in the sending and receiving of trafficked
#' people. Conversely, the report does not appear to condemn countries that see
#' increases in transitory trafficking—these countries are more likely to
#' receive better tier ratings as trafficking through their borders worsens.

#+ include=FALSE
library(dplyr)
library(broom)
library(ggplot2)
library(stargazer)

source("shared_functions.R")
base.folder <- "final_figures"

# By default, R uses polynomial contrasts for ordered factors in linear models
# options("contrasts") 
# So make ordered factors use treatment contrasts instead
options(contrasts=rep("contr.treatment", 2))
# Or do it on a single variable:
# contrasts(df.incidence$ht_incidence_origin) <- "contr.treatment"

# Load clean data
df.complete <- readRDS(file="final_tables/df_complete.rds")

incidence.levels <- c("Very low", "Low", "Medium", "High", "Very high")

df.incidence <- df.complete %>%
  filter(year > 2000) %>%
  # dplyr 0.4.3.9000 and above creates new named columns when using mutate_each
  # https://github.com/hadley/dplyr/commit/a7d92b4cbf0e77744e60e02d2be07949dff18700
  mutate_each(funs(factor = factor(., levels=1:5, labels=incidence.levels, ordered=TRUE)),
              starts_with("ht_incidence"))

df.summary <- df.incidence %>%
  group_by(tier) %>%
  summarise(asdf = n())

# Models
model.factor <- lm(tier ~ ht_incidence_origin_factor + ht_incidence_transit_factor + 
                     ht_incidence_destination_factor, data=df.incidence)
# summary(model.factor)

model.numeric <- lm(tier ~ ht_incidence_origin + ht_incidence_transit + 
                      ht_incidence_destination, data=df.incidence)
# summary(model.numeric)

model.full <- lm(tier ~ ht_incidence_origin + ht_incidence_transit + 
                   ht_incidence_destination + 
                   inreport + logpop + ngos_ave + totalfreedom + 
                   corrected_regcrim + ratproto2000, data=df.incidence)
# summary(model.full)

#' ### Predict tier using simple model
#+ echo=FALSE
new.data.covars <- model.numeric$model %>%
  summarise_each(funs(mean), -c(tier)) %>%
  mutate(index = 1)

new.data.origin <- data_frame(ht_incidence_origin = 1:5, index = 1) %>%
  left_join(select(new.data.covars, -ht_incidence_origin), by="index") %>%
  select(-index) %>%
  augment(model.numeric, newdata=.) %>%
  mutate(model = "Origin")

new.data.transit <- data_frame(ht_incidence_transit = 1:5, index = 1) %>%
  left_join(select(new.data.covars, -ht_incidence_transit), by="index") %>%
  select(-index) %>%
  augment(model.numeric, newdata=.) %>%
  mutate(model = "Transit")

new.data.destination <- data_frame(ht_incidence_destination = 1:5, index = 1) %>%
  left_join(select(new.data.covars, -ht_incidence_destination), by="index") %>%
  select(-index) %>%
  augment(model.numeric, newdata=.) %>%
  mutate(model = "Destination")

plot.predict <- bind_rows(new.data.origin, new.data.transit, new.data.destination) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         model = factor(model, levels=c("Origin", "Transit", "Destination"),
                        ordered=TRUE),
         incidence = rep(1:5, 3))

plot.incidence.pred <- ggplot(plot.predict, 
                              aes(x=incidence, 
                                  y=pred, colour=model)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=model), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Severity of trafficking incidence", 
       y="Predicted tier") + 
  scale_colour_manual(values=c("#004259", "#FC7300", "#BFDB3B"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#FC7300", "#BFDB3B"), name=NULL, guide=FALSE) +
  scale_x_continuous(labels=incidence.levels) +
  scale_y_continuous(breaks=c(1, 2, 2.5, 3), 
                     labels=c("Tier 1", "Tier 2", "Watch List", "Tier 3")) +
  coord_cartesian(ylim=c(1, 3)) +
  theme_clean() + 
  theme(panel.grid.minor=element_blank(), legend.key=element_blank())
plot.incidence.pred

# Save plot
filename <- "figure_incidence_predicted"
width <- 4.5
height <- 3
ggsave(plot.incidence.pred, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(plot.incidence.pred, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


#' ### Predict tier using full model
#+ echo=FALSE
new.data.covars <- model.full$model %>%
  summarise_each(funs(mean), -c(tier)) %>%
  mutate(index = 1, inreport=1)

new.data.origin <- data_frame(ht_incidence_origin = 1:5, index = 1) %>%
  left_join(select(new.data.covars, -ht_incidence_origin), by="index") %>%
  select(-index) %>%
  augment(model.full, newdata=.) %>%
  mutate(model = "Origin")

new.data.transit <- data_frame(ht_incidence_transit = 1:5, index = 1) %>%
  left_join(select(new.data.covars, -ht_incidence_transit), by="index") %>%
  select(-index) %>%
  augment(model.full, newdata=.) %>%
  mutate(model = "Transit")

new.data.destination <- data_frame(ht_incidence_destination = 1:5, index = 1) %>%
  left_join(select(new.data.covars, -ht_incidence_destination), by="index") %>%
  select(-index) %>%
  augment(model.full, newdata=.) %>%
  mutate(model = "Destination")

plot.predict <- bind_rows(new.data.origin, new.data.transit, new.data.destination) %>%
  mutate(pred = .fitted,
         pred.lower = pred + (qnorm(0.025) * .se.fit),
         pred.upper = pred + (qnorm(0.975) * .se.fit),
         model = factor(model, levels=c("Origin", "Transit", "Destination"),
                        ordered=TRUE),
         incidence = rep(1:5, 3))

plot.incidence.pred.full <- ggplot(plot.predict, 
                                   aes(x=incidence, 
                                       y=pred, colour=model)) + 
  geom_ribbon(aes(ymin=pred.lower, ymax=pred.upper, fill=model), 
              alpha=0.3, colour=NA) +
  geom_line(size=1.5) + 
  labs(x="Severity of trafficking incidence", 
       y="Predicted tier") + 
  scale_colour_manual(values=c("#004259", "#FC7300", "#BFDB3B"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#FC7300", "#BFDB3B"), name=NULL, guide=FALSE) +
  scale_x_continuous(labels=incidence.levels) +
  scale_y_continuous(breaks=c(1, 2, 2.5, 3), 
                     labels=c("Tier 1", "Tier 2", "Watch List", "Tier 3")) +
  coord_cartesian(ylim=c(1, 3)) +
  theme_clean() + 
  theme(panel.grid.minor=element_blank(), legend.key=element_blank())
plot.incidence.pred.full

# Save plot
filename <- "figure_incidence_predicted_full"
width <- 4.5
height <- 3
ggsave(plot.incidence.pred.full, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(plot.incidence.pred.full, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


var.labs <- c("Trafficking intensity in countries of origin", 
              "Trafficking intensity in transit countries", 
              "Trafficking intensity in destination countries",
              "In TIP report", "Total population (logged)",
              "NGO density", "Worse civil liberties", 
              "Regional density of criminalization", 
              "2000 TIP protocol ratification",
              "Origin intensity (low)",
              "Origin intensity (medium)",
              "Origin intensity (high)",
              "Origin intensity (very high)",
              "Transit intensity (low)",
              "Transit intensity (medium)",
              "Transit intensity (high)",
              "Transit intensity (very high)",
              "Destination intensity (low)",
              "Destination intensity (medium)",
              "Destination intensity (high)",
              "Destination intensity (very high)")
col.labs <- c("Simple model", "Full model", "Simple with categorical intensity")

#' ### Full results from all models
#+ results='asis', echo=FALSE
stargazer(model.numeric, model.full, model.factor, 
          covariate.labels=var.labs, column.labels=col.labs, 
          model.numbers=FALSE, dep.var.labels.include=FALSE,
          dep.var.caption="Tier rating", 
          type="html")
