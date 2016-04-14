library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(lubridate)
library(ggplot2)
library(Cairo)


theme_edb <- function(base_size=9, base_family="Clear Sans Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          title=element_text(vjust=1.2, family="Clear Sans", face="bold"),
          plot.subtitle=element_text(family="Clear Sans Light"),
          plot.caption=element_text(family="Clear Sans Light", size=rel(0.8)),
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
          legend.key=element_blank())
  
  ret
}


# ------------
# Munge data
# ------------

# Original raw data
edb.raw <- read_dta("~/Dropbox (Personal)/Andrew/EDB/MasterWBMarch16_15.dta")
countries.with.edb.bureau <- read_csv("EDB/data/countries_with_edb_bureau.csv")



# Only select data with reform counts and make a variable indicating if a
# country has a dedicated EDB bureaucracy
edb.reforms <- edb.raw %>%
  mutate(has.bureau = ccode %in% countries.with.edb.bureau$cowcode) %>%
  select(ccode, year, contains("_reform"), has.bureau, sb_rank) %>%
  gather(reform.type, reform.num, contains("_reform")) %>%
  mutate(has.bureau = factor(has.bureau, 
                             labels=c("No EDB bureaucracy    ", 
                                      "Special EDB bureaucracy")),
         reform.num.no.na = ifelse(is.na(reform.num), 0, reform.num),
         reform.positive = ifelse(reform.num.no.na > 0, reform.num.no.na, 0))

cor(edb.reforms, p_edb_rank)

# Summarize reform counts by year and presence of bureaucracy
edb.reforms.year.bureau <- edb.reforms %>%
  group_by(year, has.bureau) %>%
  summarise(total.reforms = sum(reform.num, na.rm=TRUE),
            total.reforms.no.na = sum(reform.num.no.na),
            total.reforms.positive = sum(reform.positive),
            n = n(),
            avg.reforms = mean(reform.positive, na.rm=TRUE),
            stdev = sd(reform.positive, na.rm=TRUE),
            std.error = stdev / sqrt(n),
            lower = avg.reforms + (qnorm(0.025) * std.error),
            upper = avg.reforms + (qnorm(0.975) * std.error)) %>%
  filter(total.reforms > 0) %>% 
  ungroup() %>%
  mutate(year.date = ymd(paste0(year, "-01-01")))


# -----------------------------------------
# Plot average, total, and total positive
# -----------------------------------------
plot.reforms.avg <- ggplot(edb.reforms.year.bureau, 
       aes(x=year.date, y=avg.reforms, colour=has.bureau)) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=has.bureau), 
              colour=NA, alpha=0.2) +
  geom_line(size=1) +
  scale_colour_manual(values=c("#004259", "#FC7300"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#FC7300"), name=NULL) + 
  labs(x=NULL, y="Average reforms", title="Average number of EDB reforms, 2007–2014",
       subtitle="Separated by countries with special EDB bureaucracies",
       caption=paste("Ease of Doing Business Index reports, 2007–2014", 
                     "List of countries with special bureacracies from 2015 report",
                     "Shaded region shows 95% confidence interval",
                     sep="\n")) +
  theme_edb()
plot.reforms.avg

ggsave(plot.reforms.avg, filename="EDB/figures/bureau_reforms_average.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.reforms.avg, filename="EDB/figures/bureau_reforms_average.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)


plot.reforms.total <- ggplot(edb.reforms.year.bureau, 
                           aes(x=year.date, y=total.reforms.no.na, 
                               colour=has.bureau)) + 
  geom_line(size=1) +
  scale_colour_manual(values=c("#004259", "#FC7300"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#FC7300"), name=NULL) + 
  coord_cartesian(ylim=c(100, 250)) +
  labs(x=NULL, y="Reforms", title="Total number of EDB reforms, 2007–2014",
       subtitle="Separated by countries with special EDB bureaucracies",
       caption=paste("Ease of Doing Business Index reports, 2007–2014", 
                     "List of countries with special bureacracies from 2015 report",
                     sep="\n")) +
  theme_edb()
plot.reforms.total

ggsave(plot.reforms.total, filename="EDB/figures/bureau_reforms_total.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.reforms.total, filename="EDB/figures/bureau_reforms_total.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)


plot.reforms.positive <- ggplot(edb.reforms.year.bureau, 
                             aes(x=year.date, y=total.reforms.positive, 
                                 colour=has.bureau)) + 
  geom_line(size=1) +
  scale_colour_manual(values=c("#004259", "#FC7300"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#FC7300"), name=NULL) + 
  coord_cartesian(ylim=c(100, 250)) +
  labs(x=NULL, y="Reforms", title="Total number of positive EDB reforms, 2007–2014",
       subtitle="Separated by countries with special EDB bureaucracies. Negative reforms ignored.",
       caption=paste("Ease of Doing Business Index reports, 2007–2014", 
                     "List of countries with special bureacracies from 2015 report",
                     sep="\n")) +
  theme_edb()
plot.reforms.positive

ggsave(plot.reforms.positive, filename="EDB/figures/bureau_reforms_positive.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.reforms.positive, filename="EDB/figures/bureau_reforms_positive.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)
