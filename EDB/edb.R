library(dplyr)
library(readr)
library(tidyr)
library(haven)
library(readxl)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(gtable)
library(Cairo)

# df.what <- read_dta("/Users/andrew/Dropbox/Andrew/EDB/MasterWBMarch16_15_WHAT.dta")
df <- read_dta("/Users/andrew/Dropbox/Andrew/EDB/MasterWBMarch16_15.dta")


rankings <- c("-0.242***", "-0.261", "-0.033*", "-0.088***", "0.032", "-1.339***", "-67.590***", "-0.133***", "-0.087*", "-0.256***", "-0.249", "-0.031", "-0.092***", "0.048", "-1.293***", "-67.840***", "-0.135***", "-0.077*", "-0.260***", "-0.313", "-0.033", "-0.093***", "0.047", "-1.279***", "-67.358***", "-0.134***", "-0.079*", "-0.237**", "-2.634**", "-0.065**", "-0.075**", "0.040", "-0.045", "-9.040*", "-0.017*", "-0.063", "-0.237**", "-2.634**", "-0.065**", "-0.075**", "0.040", "-0.045", "-9.040*", "-0.017*", "-0.063", "-0.225**", "-2.279**", "-0.061**", "-0.057*", "0.040", "-0.204***", "-14.114**", "-0.017*", "-0.063")
data.frame(matrix(rankings, nrow=9))

write_csv(data.frame(matrix(rankings, nrow=9)), path="~/Desktop/table_3.csv")


# Clean messy Stata outreg2 output
# year.coefs <- read_csv("~/Dropbox/Andrew/EDB/years_coefs.csv") %>%
#   separate(VARIABLES, c("year", "param"), sep="\\.") %>%
#   mutate(param = ifelse(param == "year", "coef", param),
#          year = ymd(paste0(year, "-01-01"))) %>%
#   gather(model, value, -c(year, param)) %>%
#   mutate(value = as.numeric(gsub("\\*|\\(|\\)", "", value))) %>%
#   spread(param, value) %>%
#   mutate(upper = coef + (qnorm(0.975) * se),
#          lower = coef + (qnorm(0.025) * se))

year.coefs <- read_csv("~/Dropbox/Andrew/EDB/years_coefs_controls.csv") %>%
  separate(VARIABLES, c("year", "param"), sep="\\.") %>%
  mutate(param = ifelse(param == "year", "coef", param),
         year = ymd(paste0(year, "-01-01"))) %>%
  gather(model, value, -c(year, param)) %>%
  mutate(value = as.numeric(gsub("\\*|\\(|\\)", "", value))) %>%
  spread(param, value) %>%
  mutate(upper = coef + (qnorm(0.975) * se),
         lower = coef + (qnorm(0.025) * se))

# Plots
p.procedures <- ggplot(filter(year.coefs, model == "sb_proced"), 
                       aes(x=year, y=coef)) + 
  geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
  geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
  labs(x=NULL, y="Procedures", title="Procedures to start a business") +
  theme_light(8, base_family="Clear Sans") +
  theme(panel.grid.minor=element_blank())
# 
# p.days <- ggplot(filter(year.coefs, model == "sb_days"), 
#                  aes(x=year, y=coef)) + 
#   geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
#   geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
#   labs(x=NULL, y="Days", title="Days to start a business") +
#   theme_light(8, base_family="Clear Sans") +
#   theme(panel.grid.minor=element_blank())
# 
# p.days.clean <- ggplot(filter(year.coefs, model == "sb_days_clean"), 
#                        aes(x=year, y=coef)) + 
#   geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
#   geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
#   labs(x=NULL, y="Days", title="Days to start a business (no outliers)") +
#   theme_light(8, base_family="Clear Sans") +
#   theme(panel.grid.minor=element_blank())

p.days.log <- ggplot(filter(year.coefs, model == "lnsb_days"), 
                     aes(x=year, y=coef)) + 
  geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
  geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
  labs(x=NULL, y="Log days", title="Days to start a business (log)") +
  theme_light(8, base_family="Clear Sans") +
  theme(panel.grid.minor=element_blank())

# p.days.clean.log <- ggplot(filter(year.coefs, model == "lnsb_days_clean"), 
#                            aes(x=year, y=coef)) + 
#   geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
#   geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
#   labs(x=NULL, y="Log days", title="Days to start a business (log; no outliers)") +
#   theme_light(8, base_family="Clear Sans") +
#   theme(panel.grid.minor=element_blank())

p.cost.log <- ggplot(filter(year.coefs, model == "lnsb_cost"), 
                       aes(x=year, y=coef)) + 
  geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
  geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
  labs(x=NULL, y="Log $", title="Cost to start a business (log)") +
  theme_light(8, base_family="Clear Sans") +
  theme(panel.grid.minor=element_blank())

p.capital.log <- ggplot(filter(year.coefs, model == "lnsb_capital"), 
                          aes(x=year, y=coef)) + 
  geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
  geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
  labs(x=NULL, y="Log $", title="Capital to start a business (log)") +
  theme_light(8, base_family="Clear Sans") +
  theme(panel.grid.minor=element_blank())
# 
# p.con.proced <- ggplot(filter(year.coefs, model == "con_proced"), 
#                        aes(x=year, y=coef)) + 
#   geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
#   geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
#   labs(x=NULL, y="Procedures", title="Construction procedures") +
#   theme_light(8, base_family="Clear Sans") +
#   theme(panel.grid.minor=element_blank())
# 
# p.con.days <- ggplot(filter(year.coefs, model == "con_days"), 
#                      aes(x=year, y=coef)) + 
#   geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
#   geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
#   labs(x=NULL, y="Days", title="Construction days") +
#   theme_light(8, base_family="Clear Sans") +
#   theme(panel.grid.minor=element_blank())
# 
# p.con.cost.log <- ggplot(filter(year.coefs, model == "lncon_cost"), 
#                          aes(x=year, y=coef)) + 
#   geom_hline(yintercept=0, colour="#FF4136", size=0.5) +
#   geom_pointrange(aes(ymax=upper, ymin=lower), size=0.3) + 
#   labs(x=NULL, y="Log $", title="Construction cost (log)") +
#   theme_light(8, base_family="Clear Sans") +
#   theme(panel.grid.minor=element_blank())

p.all <- cbind(rbind(ggplotGrob(p.days.log), ggplotGrob(p.procedures)),
               rbind(ggplotGrob(p.capital.log), ggplotGrob(p.cost.log)))
grid::grid.draw(p.all)
ggsave(p.all, 
       filename="~/Dropbox/Andrew/EDB/figures/year_changes_controls.pdf",
       width=6, height=6, units="in", device=cairo_pdf)
ggsave(p.all, 
       filename="~/Dropbox/Andrew/EDB/figures/year_changes_controls.png",
       width=6, height=6, units="in", type="cairo", dpi=300)


# -----------------
# Gap in rankings
# -----------------
gap.df <- read_csv("~/Dropbox/Andrew/EDB/ranking_gap.csv") %>%
  select(Variable, Mean, Std_Dev, Gap) %>%
  mutate(Gap = factor(Gap)) %>%
  mutate(se = Std_Dev / sqrt(n()),
         upper = Mean + (qnorm(0.975) * se),
         lower = Mean + (qnorm(0.025) * se),
         lower_censored = ifelse(lower < 0, 0, lower))

dodge <- position_dodge(width=0.9)
plot.ranking.gap <- ggplot(gap.df, aes(x=Variable, y=Mean, fill=Gap)) + 
  geom_bar(stat="identity", position=dodge) + 
  labs(x=NULL, y="Average value") +
  scale_fill_manual(values=c("#b3cde3", "#8c96c6", "#8856a7", "#810f7c"),
                    guide=guide_legend(title="Gap in rankings", 
                                       title.position="left",
                                       keywidth=0.65, keyheight=0.65)) +
  facet_wrap(~ Variable, scales="free") + 
  theme_light(10, base_family="Clear Sans") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom")
plot.ranking.gap
ggsave(plot.ranking.gap, filename="~/Dropbox/Andrew/EDB/figures/ranking_gap.pdf",
       width=5, height=4, units="in", device=cairo_pdf)
ggsave(plot.ranking.gap, filename="~/Dropbox/Andrew/EDB/figures/ranking_gap.png",
       width=5, height=4, units="in", type="cairo", dpi=300)

plot.ranking.gap.bars <- plot.ranking.gap + 
  geom_errorbar(aes(ymax=upper, ymin=lower_censored), position=dodge, 
                width=0.25, colour="grey60", size=0.5)

ggsave(plot.ranking.gap.bars, 
       filename="~/Dropbox/Andrew/EDB/figures/ranking_gap_bars.pdf",
       width=5, height=4, units="in", device=cairo_pdf)
ggsave(plot.ranking.gap.bars, 
       filename="~/Dropbox/Andrew/EDB/figures/ranking_gap_bars.png",
       width=5, height=4, units="in", type="cairo", dpi=300)
