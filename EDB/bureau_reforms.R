library(dplyr)
library(tidyr)
library(broom)
library(readr)
library(haven)
library(lubridate)
library(countrycode)
library(ggplot2)
library(ggrepel)
library(ggstance)
library(scales)
library(gridExtra)
library(Cairo)


theme_edb <- function(base_size=9, base_family="Clear Sans Light") {
  update_geom_defaults("label", list(family="Clear Sans Light"))
  update_geom_defaults("label_repel", list(family="Clear Sans Light"))
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

# Blank plot for spacing things in arrangeGrob()
blank <- grid::rectGrob(gp=grid::gpar(col="white"))


# ------------
# Munge data
# ------------
# Original raw data
countries.with.edb.bureau <- read_csv("EDB/data/countries_with_edb_bureau.csv")
edb.raw <- read_dta("~/Dropbox (Personal)/Andrew/EDB/MasterWBMarch16_15.dta") %>%
  rename(p_edb_rank = p_ebd_rank) %>%
  mutate(has.bureau = ccode %in% countries.with.edb.bureau$cowcode,
         has.bureau = factor(has.bureau, 
                             labels=c("No EDB reform committee    ", 
                                      "Special EDB reform committee")))

# Only select data with reform counts
reform.types <- data_frame(var.name = c("sb_reform", "cp_reform", 
                                        "el_reform", "rp_reform", 
                                        "cred_reform", "pmi_reform", 
                                        "tx_reform", "trade_reform", 
                                        "con_reform", "insolv_reform"),
                           clean.name = c("Starting a business", "Construction permits", 
                                          "Getting electricity", "Registering property", 
                                          "Getting credit", "Protecting minority investors",
                                          "Paying taxes", "Trading across borders", 
                                          "Enforcing contracts", "Resolving insolvency"))

edb.reforms <- edb.raw %>%
  select(ccode, year, contains("_reform"), has.bureau, sb_rank, p_edb_rank) %>%
  gather(reform.type, reform.num, contains("_reform")) %>%
  mutate(reform.num.no.na = ifelse(is.na(reform.num), 0, reform.num),
         reform.positive = ifelse(reform.num.no.na > 0, reform.num.no.na, 0),
         reform.type.clean = factor(reform.type, levels=reform.types$var.name,
                                    labels=reform.types$clean.name, ordered=TRUE))

# edb.reforms %>% group_by(year) %>% summarise(total = sum(reform.num.no.na))

# Summarize reform counts by year and presence of a reform committee
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
       subtitle="Separated by countries with special EDB reform committees",
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
       subtitle="Separated by countries with special EDB reform committees",
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
       subtitle="Separated by countries with special EDB reform committees. Negative reforms ignored.",
       caption=paste("Ease of Doing Business Index reports, 2007–2014", 
                     "List of countries with special bureacracies from 2015 report",
                     sep="\n")) +
  theme_edb()
plot.reforms.positive

ggsave(plot.reforms.positive, filename="EDB/figures/bureau_reforms_positive.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.reforms.positive, filename="EDB/figures/bureau_reforms_positive.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)


# Rankings in 2005 and 2014
edb.rankings <- edb.raw %>%
  select(ccode, year, p_edb_rank, has.bureau) %>%
  filter(year %in% c(2005, 2014)) %>%
  spread(year, p_edb_rank) %>%
  mutate(countryname = countrycode(ccode, "cown", "country.name"),
         countryname = ifelse(countryname == "Lao People's Democratic Republic", 
                              "Laos", countryname),
         add.label = countryname %in% c("Georgia", "Rwanda", 
                                        "Bangladesh", "Nigeria", "Pakistan",
                                        "Norway", "New Zealand",
                                        "Laos", "Niger"),
         label = ifelse(add.label, countryname, NA))

annotations <- data_frame(x = c(max(edb.rankings$`2014`, na.rm=TRUE), 0),
                          y = c(0, max(edb.rankings$`2005`, na.rm=TRUE)),
                          text = c("Outliers\nworsening", "Outliers\nimproving"),
                          hjust = c("right", "left"),
                          vjust = c("bottom", "top"))

reference.line <- data_frame(x = c(0, max(edb.rankings$`2014`, na.rm=TRUE)),
                             y = c(0, max(edb.rankings$`2005`, na.rm=TRUE)))

set.seed(1234)
plot.rankings.cor <- ggplot(edb.rankings, aes(x=`2014`, y=`2005`, 
                                              colour=has.bureau, label=label)) + 
  geom_line(data=reference.line, aes(x=x, y=y, label=NULL), colour="grey70", size=0.5) +
  geom_point(size=1) +
  geom_label_repel(aes(fill=has.bureau), colour="white", 
                   size=2, alpha=0.9, segment.color="grey40") +
  geom_text(data=annotations, aes(x=x, y=y, label=text, hjust=hjust, vjust=vjust),
            colour="grey40", size=2, lineheight=1) +
  scale_color_manual(values=c("#004259", "#FC7300"), name=NULL) +
  scale_fill_manual(values=c("#004259", "#FC7300"), name=NULL, guide=FALSE) +
  labs(x="Rank in 2014", y="Rank in 2005", 
       title="Ease of Doing Business Index, 2005 vs. 2014",
       subtitle="Countries with EDB reform committees highlighted",
       caption=paste("Ease of Doing Business Index reports, 2005 and 2014", 
                     "List of countries with special bureacracies from 2015 report",
                     sep="\n")) +
  theme_edb()
set.seed(1234); plot.rankings.cor

set.seed(1234)
ggsave(plot.rankings.cor, filename="EDB/figures/bureau_rankings.pdf",
       width=5, height=4, units="in", device=cairo_pdf)
set.seed(1234)
ggsave(plot.rankings.cor, filename="EDB/figures/bureau_rankings.png",
       width=5, height=4, units="in", type="cairo", dpi=300)

set.seed(1234)
plot.rankings.cor.notitle <- plot.rankings.cor + 
  labs(title=NULL, subtitle=NULL, caption=NULL)

set.seed(1234)
ggsave(plot.rankings.cor.notitle, filename="EDB/figures/bureau_rankings_notitle.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
set.seed(1234)
ggsave(plot.rankings.cor.notitle, filename="EDB/figures/bureau_rankings_notitle.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)

# Do the number of reforms predict absolute gains in EDB rankings?
# If bureaucracies are more strategic, correlation between number of reforms
# and change in rankings should be stronger in countries with bureaucracies
edb.reforms.rankings <- edb.reforms %>%
  group_by(ccode, year) %>%
  summarise(num.reforms = sum(reform.num.no.na)) %>%
  left_join(select(edb.raw, ccode, year, p_edb_rank, has.bureau), by=c("ccode", "year")) %>%
  mutate(rank0 = p_edb_rank,
         rank1 = lag(p_edb_rank, 1),
         rank2 = lag(p_edb_rank, 2),
         change1 = rank1 - rank0,
         change2 = rank2 - rank0) %>%
  filter(year >= 2005)

# Are the number of reforms and changes in rankings correlated?
cor(edb.reforms.rankings$num.reforms, edb.reforms.rankings$change1, 
    use="complete.obs")

plot.reforms.rankings <- ggplot(edb.reforms.rankings, 
                                aes(x=num.reforms, y=change1, colour=has.bureau)) + 
  stat_smooth(aes(fill=has.bureau), method="lm", size=0.5) +
  geom_point(size=1, alpha=0.8) +
  scale_color_manual(values=c("#004259", "#FC7300"), name=NULL) + 
  scale_fill_manual(values=c("#004259", "#FC7300"), name=NULL, guide=FALSE) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  labs(x="Number of reforms", y="Change in rankings in following year",
       title="Relationship between reforms and EDB rankings",
       subtitle="Change in absolute EDB ranking one year after reforms",
       caption="Shaded region shows 95% confidence interval") +
  theme_edb()
plot.reforms.rankings

ggsave(plot.reforms.rankings, filename="EDB/figures/bureau_reforms_rank.pdf",
       width=5, height=4, units="in", device=cairo_pdf)
ggsave(plot.reforms.rankings, filename="EDB/figures/bureau_reforms_rank.png",
       width=5, height=4, units="in", type="cairo", dpi=300)


summary.corr <- function(x, y=NULL) {
  out <- data_frame(avg = mean(x, na.rm=TRUE), 
                    stdev = sd(x, na.rm=TRUE), 
                    std.error = stdev / sqrt(length(x)), 
                    lower = avg + (qnorm(0.025) * std.error), 
                    upper = avg + (qnorm(0.975) * std.error))
  
  if(length(y) > 0) {
    out$correlation <- cor(x, y, use="complete.obs")
  }
  
  out
}

# Multiple named arguments in do() creates columns with nested lists
# Use tidyr::unnest() to unpack those lists as actual columns
#
# There's no way to automatically add a prefix, though, so unpacking all nested
# list columns at the same time creates duplicate columns. Unnest columns
# individually as needed when plotting
reform.rankings.summary <- edb.reforms.rankings %>%
  group_by(has.bureau) %>%
  do(change = summary.corr(.$change1, .$num.reforms),
     reforms = summary.corr(.$num.reforms))

plot.avg.reforms <- ggplot(unnest(reform.rankings.summary, reforms), 
                           aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Reforms",
       title="Reform committees and EDB rankings",
       subtitle="Average number of EDB reforms") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0))

plot.avg.rank.change <- ggplot(unnest(reform.rankings.summary, change), 
       aes(y=has.bureau, x=avg, colour=has.bureau)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) +
  scale_color_manual(values=c("#004259", "#FC7300"), guide=FALSE) + 
  labs(y=NULL, x="Change in absolute ranking",
       subtitle="Average change in EDB rankings in the following year") +
  theme_edb() + theme(axis.text.y=element_text(hjust=0))

plot.avg.reforms.rank <- arrangeGrob(plot.avg.reforms, blank, 
                                     plot.avg.rank.change, 
                                     heights=c(0.52, 0.04, 0.44), ncol=1)
grid::grid.draw(plot.avg.reforms.rank)

ggsave(plot.avg.reforms.rank, filename="EDB/figures/bureau_avg_reforms_rank.pdf",
       width=5, height=2.5, units="in", device=cairo_pdf)
ggsave(plot.avg.reforms.rank, filename="EDB/figures/bureau_avg_reforms_rank.png",
       width=5, height=2.5, units="in", type="cairo", dpi=300)


# Types of reforms
reform.rankings.type <- edb.reforms %>%
  # mutate(reform.num.no.na = ifelse(reform.num.no.na < 0, 0, reform.num.no.na)) %>%
  select(ccode, year, has.bureau, reform.type, reform.type.clean, reform.num.no.na) %>%
  left_join(edb.reforms.rankings, c("ccode", "year", "has.bureau")) %>%
  filter(year >= 2005)

reform.rankings.type.summary <- reform.rankings.type %>%
  group_by(reform.type.clean, has.bureau) %>%
  do(change = summary.corr(.$change1, .$reform.num.no.na),
     reforms = summary.corr(.$reform.num.no.na))

reform.rankings.type.cor <- reform.rankings.type.summary %>%
  unnest(change) %>%
  arrange(desc(has.bureau), correlation) %>%
  mutate(reform.type.clean = factor(reform.type.clean, 
                                    levels=unique(reform.type.clean), 
                                    ordered=TRUE))

plot.ranking.cor <- ggplot(reform.rankings.type.cor, 
                           aes(x=correlation, y=reform.type.clean, colour=has.bureau)) +
  geom_point() +
  scale_color_manual(values=c("#004259", "#FC7300"), name=NULL) +
  labs(x="Correlation between reform and movement in rankings", y="Type of reform",
       title="Does the correlation between ranking\ngains and number of reforms vary\nby committee status?") +
  theme_edb()
plot.ranking.cor

ggsave(plot.ranking.cor, filename="EDB/figures/bureau_ranking_cor.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.ranking.cor, filename="EDB/figures/bureau_ranking_cor.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)

plot.ranking.cor.notitle <- plot.ranking.cor + labs(title=NULL)
ggsave(plot.ranking.cor.notitle, filename="EDB/figures/bureau_ranking_cor_notitle.pdf",
       width=5, height=3, units="in", device=cairo_pdf)
ggsave(plot.ranking.cor.notitle, filename="EDB/figures/bureau_ranking_cor_notitle.png",
       width=5, height=3, units="in", type="cairo", dpi=300)


reform.rankings.type.wide <- reform.rankings.type %>%
  select(-reform.type.clean) %>%
  spread(reform.type, reform.num.no.na)

model.rankings.type <- lm(change1 ~ has.bureau +
                            sb_reform + cp_reform + el_reform + rp_reform + 
                            cred_reform + pmi_reform + tx_reform + 
                            trade_reform + con_reform + insolv_reform, 
                          data=reform.rankings.type.wide)
summary(model.rankings.type)

rankings.type.tidy <- tidy(model.rankings.type, conf.int=TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = factor(term, levels=rev(term), 
                       labels=rev(c("Has EDB committee", reform.types$clean.name)),
                       ordered=TRUE))

plot.model.rankings.type <- ggplot(rankings.type.tidy, aes(x=estimate, y=term)) +
  geom_vline(xintercept=0, colour="#FF4036") +
  geom_pointrangeh(aes(xmin=conf.low, xmax=conf.high)) + 
  labs(x="Coefficient\n(ranking change in following year)", y=NULL,
       title="Predicting ranking changes with reforms",
       subtitle="Coefficients from OLS regression") +
  theme_edb()
plot.model.rankings.type

ggsave(plot.model.rankings.type, filename="EDB/figures/bureau_model_rankings.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.model.rankings.type, filename="EDB/figures/bureau_model_rankings.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)


# Mix of reforms in committee vs. non-committee countries
reform.mix <- reform.rankings.type %>%
  group_by(has.bureau) %>%
  mutate(group.total = sum(reform.num.no.na)) %>%
  group_by(has.bureau, reform.type.clean) %>%
  summarise(reforms = sum(reform.num.no.na),
            reforms.prop = reforms / max(group.total))

plot.mix.reforms <- ggplot(reform.mix, 
                           aes(x=reforms.prop, y=reform.type.clean, fill=has.bureau)) + 
  geom_barh(stat="identity", position="dodge") +
  scale_fill_manual(values=c("#004259", "#FC7300"), name=NULL) + 
  scale_x_continuous(labels=percent, expand=c(0.025, 0)) +
  labs(x="Proportion", y=NULL, title="Mix of EDB reforms",
       subtitle="Proportion of reforms in countries with and without reform committees") +
  theme_edb() + theme(legend.key.size=unit(0.75, "lines"))
plot.mix.reforms

ggsave(plot.mix.reforms, filename="EDB/figures/bureau_reforms.mix.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.mix.reforms, filename="EDB/figures/bureau_reforms.mix.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)


# ----------------------------------------
# Predict presence of a reform committee
# ----------------------------------------
# What explains which countries create EDB reform committees?
model.committee = glm(has.bureau ~ p_edb_rank + icrg_index + 
                        gdpcap + gdpgrowth + fdi_inper + trade + log1p(ibrd) +
                        polity2 + yrsoffc,
                      data=filter(edb.raw, year == 2008),
                      family=binomial(link="logit"))
summary(model.committee)

committee.tidy <- tidy(model.committee, conf.int=TRUE, exponentiate=TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = factor(term, levels=rev(term), ordered=TRUE))

plot.model.committee <- ggplot(committee.tidy, aes(x=estimate, y=term)) +
  geom_vline(xintercept=1, colour="#FF4036") +
  geom_pointrangeh(aes(xmin=conf.low, xmax=conf.high)) + 
  labs(x="Odds ratio", y=NULL,
       title="Predicting the formation of an EDB committee",
       subtitle="Coefficients from logistic regression; data limited to 2008") +
  theme_edb()
plot.model.committee

ggsave(plot.model.committee, filename="EDB/figures/bureau_model_presence.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.model.committee, filename="EDB/figures/bureau_model_presence.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)


# ----------
# Bunching
# ----------
gap.sb <- edb.raw %>%
  select(ccode, year, sb_reform, p_sb_rank, sb_days, has.bureau) %>%
  filter(year == 2008, !is.na(p_sb_rank)) %>%
  # TODO: Why not p_sb_rank? Because it includes other variables?
  arrange(sb_days) %>%
  mutate(distance.15 = sb_days - lag(sb_days, 15),
         distance.10 = sb_days - lag(sb_days, 10),
         distance.5 = sb_days - lag(sb_days, 5),
         distance.1 = sb_days - lag(sb_days, 1),
         fake.index = 1:n())

gap.plots <- gap.sb %>%
  gather(distance, gap, starts_with("distance")) %>%
  mutate(distance = factor(as.numeric(gsub("distance\\.", "", distance))))

plot.gaps.lots <- ggplot(gap.plots, aes(x=sb_days, y=gap, colour=distance)) + 
  geom_point(size=0.5) + 
  # geom_smooth(se=FALSE) + 
  scale_color_manual(values=c("#BD144F", "#E88000", "#F7C900", "#8A9C0F"),
                     name="Positions ahead") +
  coord_cartesian(xlim=c(0, 175), ylim=c(0, 100)) + 
  labs(x="Days to start a business",
       y="Distance to country X positions ahead",
       title="Effect of different gaps",
       subtitle="Data from 2008; values greater than 200 removed") +
  theme_edb()
plot.gaps.lots
# The size of the gap doesn't matter much until there are 50+ days; no easy
# gains in the 0-50 range.

ggsave(plot.gaps.lots, filename="EDB/figures/bureau_gap_range.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.gaps.lots, filename="EDB/figures/bureau_gap_range.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)


gap.dists <- gap.sb %>%
  gather(distance, gap, c(sb_days, starts_with("distance"))) %>%
  filter(distance %in% c("sb_days", "distance.10")) %>%
  mutate(distance = factor(distance, levels=c("sb_days", "distance.10"),
                           labels=c("Days to start a business    ",
                                    "Distance to country 10 positions ahead"),
                           ordered=TRUE))

plot.gaps.dist <- ggplot(gap.dists, aes(x=gap, fill=distance)) + 
  geom_density(aes(y=..count..), colour="white", size=0.25) +
  geom_rug(data=gap.sb, mapping=aes(x=sb_days), sides="b", 
           colour="#BD144F", size=0.5, inherit.aes=FALSE) +
  geom_rug(data=gap.sb, mapping=aes(x=distance.10), sides="t", 
           colour="#F7C900", size=0.5, inherit.aes=FALSE) +
  scale_fill_manual(values=c("#BD144F", "#F7C900"), name=NULL) +
  coord_cartesian(xlim=c(0, 200)) + 
  labs(x="Days to start a business", y="Count", 
       title="Distribution of days to start a business",
       subtitle="Data from 2008; marginal rug plots indicate frequency of data;\nvalues greater than 200 removed\n") +
  theme_edb() + theme(legend.key.size=unit(0.75, "lines"))
plot.gaps.dist
# Because the number of days to start a business are relatively distributed (at
# least under 50), the gap is tiny and constant.

ggsave(plot.gaps.dist, filename="EDB/figures/bureau_gap_dists.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.gaps.dist, filename="EDB/figures/bureau_gap_dists.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)


plot.gaps.bureau <- ggplot(gap.sb, aes(x=sb_days, y=distance.10, colour=has.bureau)) + 
  geom_point(size=0.5) + 
  scale_colour_manual(values=c("#004259", "#FC7300"), name=NULL) + 
  coord_cartesian(xlim=c(0, 175), ylim=c(0, 100)) + 
  labs(x="Days to start a business",
       y="Distance to country 10 positions ahead",
       title="Distance to country 10 positions ahead",
       subtitle="Separated by presence of reform committee; data from 2008;\nvalues greater than 200 removed") +
  theme_edb()
plot.gaps.bureau

ggsave(plot.gaps.bureau, filename="EDB/figures/bureau_gap_bureau.pdf",
       width=5, height=3.5, units="in", device=cairo_pdf)
ggsave(plot.gaps.bureau, filename="EDB/figures/bureau_gap_bureau.png",
       width=5, height=3.5, units="in", type="cairo", dpi=300)

# TODO: Difference in bunching between reform and no reform (Today)
