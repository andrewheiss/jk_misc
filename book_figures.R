# --------------------
# Simple style guide
# --------------------
# ggplot theme: theme_clean(), defined in `shared_functions.R`
# Font: Source Sans Pro Light + Semibold (https://github.com/adobe-fonts/source-sans-pro)
# Font size: 10pt (smaller is okay if needed)
# Line thickness: 0.75
# Colors:
#   All single bars and lines (default set in theme_clean()): 
#     * grey30 / #4D4D4D
#   Two colors: 
#     * grey30 / #4D4D4D
#     * grey80 / #CCCCCC
#   Three colors: 
#     * black  / #000000
#     * grey30 / #4D4D4D
#     * grey80 / #CCCCCC
#   Four colors (with black): 
#     * black  / #000000
#     * grey30 / #4D4D4D
#     * grey60 / #999999
#     * grey90 / #E5E5E5
#   Four colors (with white): 
#     * grey30 / #4D4D4D
#     * grey60 / #999999
#     * grey90 / #E5E5E5
#     * white  / #FFFFFF
#   Four colors (with black, alternate): 
#     * black  / #000000
#     * grey30 / #4D4D4D
#     * grey50 / #808080
#     * grey80 / #CCCCCC

source("shared_functions.R")
library(dplyr)
library(tidyr)
library(readr)
library(pander)
panderOptions('pandoc.binary', '/Users/andrew/.cabal/bin/pandoc')

# Location for output
base.folder <- "final_figures"

# Figures and data from other files
source("criminalization.R")
source("tier_placements.R")
source("year_joined.R")
source("funding.R")
source("reactions.R")
source("policy_index.R")


# -----------
# Chapter 1
# -----------
# Figure 1.1: The spread of criminalization around the world
filename <- "figure1_1_crim_map"
width <- 4.5
height <- 5
ggsave(crim.map, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(crim.map, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 1.2: Cycle of scorecard diplomacy


# -----------
# Chapter 2
# -----------
# Figure 2.1: Trafficking patterns 2010-2012
# UNODC map

# Figure 2.2: Trafficking statistics over time 
df.fig2.2 <- read_csv("final_figures/data_figure2_2.csv") %>%
  select(Year, Prosecutions, Convictions) %>%
  gather(Variable, Num, -Year) %>%
  mutate(Year = ymd(paste0(Year, "-01-01")))

fig2.2 <- ggplot(df.fig2.2, aes(x=Year, y=Num, colour=Variable)) + 
  geom_line(size=0.75) + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels=comma) + 
  scale_colour_manual(values=c("grey30", "grey80"), name=NULL,
                      labels=c("Prosecutions    ", "Convictions")) +
  theme_clean(10) + theme(legend.key.size=unit(0.65, "lines"),
                          legend.key = element_blank(),
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))

filename <- "figure2_2_tip_convictions"
width <- 4.5
height <- 2.5
ggsave(fig2.2, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig2.2, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# Figure 2.3: Tier ratings over time
filename <- "figure2_3_tier_ratings_time"
width <- 4.5
height <- 2.5
ggsave(tier.plot, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(tier.plot, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

filename <- "figure2_3_tier_ratings_time_2001"
width <- 4.5
height <- 2.5
ggsave(tier.plot.2001, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(tier.plot.2001, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

filename <- "figure2_3_tier_ratings_time_2005"
width <- 4.5
height <- 2.5
ggsave(tier.plot.2005, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(tier.plot.2005, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 2.4: The Timing of inclusion of countries in the report
filename <- "figure2_4_map_year_joined"
width <- 4.5
height <- 3.5
ggsave(report.map, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(report.map, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 2.5: Number of meetings documented between 2001-2009
df.fig2.5 <- read_csv("final_figures/data_figure2_5.csv") %>%
  mutate(Meeting = factor(Meeting, levels=rev(Meeting), ordered=TRUE))

fig2.5 <- ggplot(df.fig2.5, aes(x=Meeting, y=Count)) + 
  geom_bar(stat="identity") + 
  labs(x=NULL, y="Meetings") + 
  coord_flip() + 
  theme_clean(10)

filename <- "figure2_5_meetings"
width <- 4.5
height <- 2
ggsave(fig2.5, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig2.5, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 2.x: Embassies or foreign governments NGOs reported as active partners in the fight against human trafficking
df.fig2.x.active <- read_csv(file.path(base.folder, 
                                       "data_figure2_x_active_embassies_plot.csv")) %>%
  mutate(country = factor(country, levels=country, ordered=TRUE),
         prop.nice = ifelse(num > 0, percent(prop.nice / 100), prop.nice))

df.fig2.x.most.active <- read_csv(file.path(base.folder, 
                                            "data_figure2_x_most_active_embassies_plot.csv")) %>%
  mutate(country = factor(country, levels=levels(df.fig2.x.active$country), ordered=TRUE),
         prop.nice = percent(prop.nice / 100))

fig.active <- ggplot(df.fig2.x.active, aes(x=country, y=num)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop.nice), size=1.5, hjust=1.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="Number of times country was mentioned\nas a partner in anti-TIP work") + 
  scale_y_continuous(breaks=seq(0, max(df.fig2.x.active$num), by=50), 
                     trans="reverse", expand = c(.15, .15)) + 
  coord_flip() + 
  theme_clean(6) + 
  theme(axis.text.y = element_blank(), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.25, 0.75), "lines"))

fig.most.active <- ggplot(df.fig2.x.most.active, aes(x=country, y=total)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop.nice), size=1.5, hjust=-0.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="Number of times country was mentioned\nas the most active partner in anti-TIP work") + 
  scale_y_continuous(expand = c(.2, .2)) + 
  coord_flip() + 
  theme_clean(6) + 
  theme(axis.text.y = element_text(hjust=0.5), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(0.5, 1.25, 0.25, 0), "lines"))

fig.embassies <- arrangeGrob(fig.active, fig.most.active, nrow=1)

filename <- "figure2_x_tip_partner_embassies"
width <- 4.5
height <- 2
ggsave(fig.embassies, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig.embassies, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 2.6: Distribution of US grants across purposes
filename <- "figure2_6_grants_purpose"
width <- 4.5
height <- 1.5
ggsave(grants.purpose, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(grants.purpose, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 2.7: Distribution of US TIP funding across countries, 2001-2014
filename <- "figure2_7_map_tip_funding"
width <- 4.5
height <- 3.5
ggsave(map.funding, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(map.funding, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 2.8: Distribution of waivers for sanctions in eligible countries, 2003-2013
df.fig2.8 <- read_csv("final_figures/data_figure2_8.csv") %>%
  mutate(Waiver = ifelse(Waiver != "Full waiver*", paste0(Waiver, "    "), Waiver),
         Waiver = factor(Waiver, levels=rev(Waiver), ordered=TRUE)) %>%
  gather(year, num, -Waiver) %>%
  group_by(year) %>%
  mutate(year.total = sum(num)) %>%
  group_by(year, Waiver) %>%
  summarise(prop = num / year.total,
            label = paste0(year, "\n", "(N = ", year.total, ")"))

fig2.8 <- ggplot(df.fig2.8, aes(x=label, y=prop, fill=Waiver)) + 
  geom_bar(stat="identity", position="stack") + 
  labs(x=NULL, y="Waivers and sanctions") + 
  scale_fill_manual(values=c("black", "grey80", "grey30"), name=NULL) + 
  scale_y_continuous(labels=percent) + 
  theme_clean(10) + theme(legend.key.size=unit(0.65, "lines"),
                          legend.key = element_blank(),
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))

filename <- "figure2_8_waivers"
width <- 4.5
height <- 3
ggsave(fig2.8, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig2.8, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# -----------
# Chapter 3
# -----------
# Figure 3.1: Cycle of scorecard diplomacy

# Figure 3.2: Distribution of US TIP funding across sectors, 2001-2014
filename <- "figure3_2_grants_sectors"
width <- 4.5
height <- 1.5
ggsave(grants.to.all.sectors, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(grants.to.all.sectors, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 3.3: Distribution of US TIP funding across IGOs and other agencies
filename <- "figure3_3_grants_igos"
width <- 4.5
height <- 1.75
ggsave(grants.to.igos, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(grants.to.igos, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 3.x: Q3.18 from survey
df.book.plots <- read_csv(file.path(base.folder, "data_q3_18_21.csv"))

df.Q3.18.lookup <- data_frame(q.num = c("Q3.18_1", "Q3.18_2", 
                                        "Q3.18_3", "Q3.18_4",
                                        "Q3.18_5", "Q3.18_6"),
                              q.label = c("Direct contact (meetings)", 
                                          "Direct cooperation", 
                                          "Our organization\nreceived funding", 
                                          "Other", 
                                          "We have not had any contact\nor funding from the US", 
                                          "Don't know"))

denom.Q3.18 <- df.book.plots %>%
  select(starts_with("Q3.18")) %>%
  mutate(num.answered = rowSums(., na.rm=TRUE)) %>%
  filter(num.answered > 0) %>%
  nrow()

df.Q3.18 <- df.book.plots %>%
  select(clean.id, starts_with("Q3.18")) %>%
  gather(question, response, -clean.id) %>%
  group_by(question) %>%
  summarize(num = sum(response, na.rm=TRUE),
            prop = num / denom.Q3.18) %>%
  left_join(df.Q3.18.lookup, by=c("question" = "q.num")) %>%
  mutate(q.label = factor(q.label, levels=rev(q.label), ordered=TRUE))

fig3.x_3_18 <- ggplot(df.Q3.18, aes(x=q.label, y=prop)) +
  geom_bar(stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels=percent) +
  coord_flip() + 
  theme_clean(10)

filename <- "figure3_x_3_18_contact_with_us"
width <- 4.5
height <- 2
ggsave(fig3.x_3_18, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig3.x_3_18, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 3.4: Stakeholders with whom NGOs discuss the TIP report
df.Q3.21.lookup <- data_frame(q.num = c("Q3.21_1", "Q3.21_2", 
                                        "Q3.21_3", "Q3.21_4"),
                              q.label = c("National government", 
                                          "Another government",
                                          "Other NGOs", "Other"))

denom.Q3.21 <- df.book.plots %>%
  select(starts_with("Q3.21")) %>%
  mutate(num.answered = rowSums(., na.rm=TRUE)) %>%
  filter(num.answered > 0) %>%
  nrow()

df.Q3.21 <- df.book.plots %>%
  select(clean.id, starts_with("Q3.21")) %>%
  gather(question, response, -clean.id) %>%
  group_by(question) %>%
  summarize(num = sum(response, na.rm=TRUE),
            prop = num / denom.Q3.21) %>%
  left_join(df.Q3.21.lookup, by=c("question" = "q.num")) %>%
  mutate(q.label = factor(q.label, levels=rev(q.label), ordered=TRUE))

fig3.4 <- ggplot(df.Q3.21, aes(x=q.label, y=prop)) +
  geom_bar(stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels=percent) +
  coord_flip() + 
  theme_clean(10)

filename <- "figure3_4_discuss_tip_report"
width <- 4.5
height <- 1.75
ggsave(fig3.4, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig3.4, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 3.x: Coverage in Oman
df.fig3.x <- read_csv("final_figures/data_figure3_x.csv") %>%
  mutate(Month = factor(Month, levels=Month, ordered=TRUE))

fig3.x <- ggplot(df.fig3.x, aes(x=Month, y=Number)) + 
  geom_bar(stat="identity") +
  labs(x=NULL, y="Media coverage of TIP issues") + 
  theme_clean(10) + theme(axis.text.x = element_text(angle=45, hjust=0.5, vjust=0.5))

filename <- "figure3_x_oman_coverage"
width <- 4.5
height <- 2
ggsave(fig3.x, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig3.x, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# -----------
# Chapter 4
# -----------
# Figure 4.1: Cycle of scorecard diplomacy

# Figure 4.x: Image from the Bangkok Post, 2014-12-29

# Figure 4.2: Percent of TIP reports with a reported reaction
# TODO: Second axis for wikileaks figure (figure 4_2) (or something else
# because using two y axes is bad)
df.fig4.raw <- read_csv("final_figures/data_figure4_2.csv") 

df.num.reports <- df.fig4.raw %>%
  filter(grepl("Number", Variable)) %>%
  gather(year, num, -Variable)

df.perc.reports <- df.fig4.raw %>%
  filter(grepl("Percent", Variable)) %>%
  gather(year, num, -Variable)

fig4.2 <- ggplot() + 
  geom_bar(data=df.num.reports, aes(x=year, y=num, fill=Variable), 
           stat="identity") + 
  geom_line(data=df.perc.reports, aes(x=year, y=num, colour=Variable, 
                                      group=Variable), size=1) + 
  scale_fill_manual(values=c("grey30"), name=NULL) +
  scale_colour_manual(values=c("grey80"), name=NULL) + 
  guides(fill=guide_legend(order=1)) + 
  labs(x=NULL, y=NULL) +
  theme_clean(10) + theme(legend.position="bottom", legend.box="horizontal",
                          legend.key.size=unit(0.65, "lines"),
                          legend.key = element_blank(),
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))

filename <- "figure4_2_wikileaks_coverage"
width <- 4.5
height <- 3
ggsave(fig4.2, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig4.2, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 4.3: Coding categories

# Figure 4.4: Distribution of reactions
filename <- "figure4_4_distribution_reactions"
width <- 4.5
height <- 4
ggsave(p.reactions, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(p.reactions, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 4.x: Co-occurrence of reactions
df.fig4.x <- read_csv("final_figures/data_figure4_x.csv") %>%
  mutate(Reaction = factor(Reaction, levels=rev(Reaction), ordered=TRUE)) %>%
  gather(Reaction2, num, -Reaction, na.rm=TRUE) %>%
  mutate(num.shade = ifelse(as.character(Reaction) != as.character(Reaction2),
                            num, NA),
         num.shade = rescale(num.shade),
         text.color = ifelse(num.shade < 0.5 | is.na(num.shade), "black", "white"))

fig4.x <- ggplot(df.fig4.x, aes(x=Reaction2, y=Reaction, fill=num.shade)) + 
  geom_tile(colour="white") +
  geom_text(aes(label=num, colour=text.color), size=2.5, hjust=0.5, 
            family="Source Sans Pro Semibold") + 
  labs(x=NULL, y=NULL) +
  scale_fill_gradient(low="white", high="grey30", na.value="white", guide=FALSE) +
  scale_color_identity() + 
  coord_fixed() +
  theme_clean(10) + theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
                          panel.grid=element_blank())

filename <- "figure4_x_co_occurrence"
width <- 4.5
height <- 4
ggsave(fig4.x, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig4.x, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 4.5: Distribution of public versus private reactions, as percent of reports with a reported reaction
df.fig4.5 <- read_csv("final_figures/data_figure4_5.csv") %>%
  gather(scope, num, -Reaction) %>%
  mutate(Reaction = factor(Reaction, levels=rev(unique(Reaction)), ordered=TRUE))

fig4.5 <- ggplot(df.fig4.5, aes(x=Reaction, y=num, fill=scope)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels=percent, breaks=seq(0, 0.5, 0.1)) + 
  scale_fill_manual(values=c("grey80", "grey30"), name=NULL, 
                    breaks=c("Public", "Private"), 
                    labels=c("Public reaction    ", "Private reaction")) + 
  coord_flip() + 
  theme_clean(10) + theme(legend.key.size=unit(0.65, "lines"),
                          legend.key = element_blank(),
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))

filename <- "figure4_5_reaction_scope"
width <- 4.5
height <- 3.5
ggsave(fig4.5, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig4.5, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# -----------
# Chapter 5
# -----------
# Figure 5.1: Cycle of scorecard diplomacy

# Figure 5.1: Criminalization and TIP report inclusion over time
df.fig5.1 <- read_csv("final_figures/data_figure5_1.csv") %>%
  gather(Variable, num, -Year) %>%
  filter(!is.na(num))

fig5.1 <- ggplot(df.fig5.1, aes(x=Year, y=num, colour=Variable)) +
  geom_line(size=0.75) + 
  labs(x=NULL, y="Number of countries") + 
  scale_colour_manual(values=c("grey30", "grey80"), name=NULL,
                      labels=c("Included in the US TIP report    ",
                               "Criminalized human trafficking")) +
  scale_y_continuous(breaks=seq(0, 200, 25)) + 
  scale_x_continuous(breaks=seq(2000, 2014, 2)) + 
  theme_clean(10) + theme(legend.key.size=unit(0.65, "lines"),
                          legend.key = element_blank(),
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))

filename <- "figure5_1_crim_report"
width <- 4.5
height <- 2.5
ggsave(fig5.1, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig5.1, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Figure 5.2: Number of anti-TIP laws passing, by month, 2001-2014
df.fig5.2 <- read_stata("original_files/Criminalization Data UpdatedJK.dta") %>%
  filter(crimlevel == 2) %>%
  group_by(month) %>%
  summarise(num = n()) %>%
  filter(!is.na(month)) %>%
  mutate(month = month.name[month],
         month = factor(month, levels=month, ordered=TRUE))
  
fig5.2 <- ggplot(df.fig5.2, aes(x=month, y=num)) + 
  geom_bar(stat="identity") +
  labs(x=NULL, y="Anti-TIP laws passed") + 
  theme_clean(10) + theme(axis.text.x = element_text(angle=45, hjust=0.5, vjust=0.5))

filename <- "figure5_2_laws_passed"
width <- 4.5
height <- 2
ggsave(fig5.2, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig5.2, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# Figure 5.3: Probability of criminalizing fully in a given year if a country had not already done so, 2001-2010
ajps.raw <- read_dta("original_files/kelley_simmons_ajps_2014_replication.dta") %>%
  group_by(cowcode) %>%
  mutate(adjbicrimlevel.lag = lag(adjbicrimlevel),
         tier.lag = lag(tier),
         tier.lag2 = lag(tier, 2))

# Summarize probability of criminalization based on presence in the report
report.presence <- ajps.raw %>%
  filter(year > 2000) %>%
  mutate(tier.lag.bin = ifelse(tier.lag < 4, "In the report", 
                               ifelse(tier.lag == 555, "Not in the report", NA))) %>%
  group_by(tier.lag.bin, adjbicrimlevel.lag) %>%
  summarise(total = n(),
            crim.no = sum(adjbicrimlevel == 0),
            crim.yes = sum(adjbicrimlevel == 2),
            prob.yes = crim.yes / total) %>%
  filter(adjbicrimlevel.lag == 0, !is.na(tier.lag.bin)) %>%
  select(label = tier.lag.bin, num = crim.yes, total, prob.yes)

# Summarize probability of criminalization based on assignment to a tier
# Temporarily prefix an order number to get the tiers in the right order
# *without* using an ordered factor (since conversion to factor happens at the
# end after the dataframes are all merged)
tiers.clean <- data_frame(tier.lag = c(1, 2, 2.5, 3, 666),
                          label = c("4:Tier 1", "3:Tier 2", "2:Watchlist", 
                                    "1:Tier 3", "5:Special case"))
tier.assignments <- ajps.raw %>%
  group_by(tier.lag, adjbicrimlevel.lag) %>%
  summarise(total = n(),
            crim.no = sum(adjbicrimlevel == 0),
            crim.yes = sum(adjbicrimlevel == 2),
            prob.yes = crim.yes / total) %>%
  filter(adjbicrimlevel.lag == 0, !is.na(tier.lag), tier.lag != 555) %>%
  ungroup() %>%
  left_join(tiers.clean, by="tier.lag") %>%
  select(label, num = crim.yes, total, prob.yes) %>%
  # Sort and get rid of temporary prefix
  arrange(label) %>% mutate(label = gsub("\\d:", "", label))

# Summarize probability of criminalization based on changes in tier assignment
no.change <- ajps.raw %>%
  filter(adjbicrimlevel.lag == 0, tier.lag2 == tier.lag, tier.lag2 < 5, tier.lag < 5) %>%
  group_by(adjbicrimlevel) %>%
  summarise(num = n()) %>%
  mutate(total = sum(num),
         prob.yes = num / total) %>%
  filter(adjbicrimlevel == 2) %>%
  mutate(label = "No change") %>%
  select(label, num, total, prob.yes)

drop.to.3 <- ajps.raw %>%
  filter(adjbicrimlevel.lag == 0, tier.lag == 3, tier.lag2 < 3) %>%
  group_by(adjbicrimlevel) %>%
  summarise(num = n()) %>%
  mutate(total = sum(num),
         prob.yes = num / total) %>%
  filter(adjbicrimlevel == 2) %>%
  mutate(label = "Drop to 3") %>%
  select(label, num, total, prob.yes)

tier.drop <- ajps.raw %>%
  filter(adjbicrimlevel.lag == 0, tier.lag2 < tier.lag, tier.lag2 < 5, tier.lag < 5) %>%
  group_by(adjbicrimlevel) %>%
  summarise(num = n()) %>%
  mutate(total = sum(num),
         prob.yes = num / total) %>%
  filter(adjbicrimlevel == 2) %>%
  mutate(label = "Tier drop") %>%
  select(label, num, total, prob.yes)


tier.changes <- bind_rows(no.change, tier.drop, drop.to.3)

# Combine all those dataframes and add spacer rows
blank.row1 <- data_frame(label=" ", num=NA, total=NA, prob.yes=0)
blank.row2 <- data_frame(label="  ", num=NA, total=NA, prob.yes=0)

full.table <- bind_rows(report.presence, blank.row1,
                        tier.assignments, blank.row2, 
                        tier.changes) %>%
  mutate(label.n = ifelse(!is.na(num), paste0(label, " (N = ", total, ")"), label),
         label.n = factor(label.n, levels=rev(label.n), ordered=TRUE),
         label = factor(label, levels=rev(label), ordered=TRUE))

# Finally plot it all
fig5.3 <- ggplot(full.table, aes(x=label.n, y=prob.yes)) + 
  geom_bar(stat="identity") + 
  labs(x=NULL, y="Probability of criminalizing") + 
  scale_y_continuous(labels=percent) + 
  coord_flip() + 
  theme_clean(10)

filename <- "figure5_3_prob_criminalize"
width <- 4.5
height <- 3
ggsave(fig5.3, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig5.3, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# Figure 5.2 (again): Incidence of criminalization in the following year, 2001-2010 for all countries included in the TIP report that had not yet criminalized in the year of the report
# Load reaction data from Stata file
reaction.vars <- c("appreciation", "funding", "anger", "cooperative", 
                   "objection", "comparisonsratingnotfair", "publicfacesaving",
                   "embassment", "disappointment", "howimprove")

reaction.names <- c("Appreciation", "Funding", "Anger", 
                    "Cooperative", "Objection", 
                    "Comparisons", "Public face", 
                    "Embarassment", "Disappointment", "How improve")

reaction.types <- data_frame(var.name = reaction.vars, 
                             nice.name = reaction.names)

reactions.crim <- read_stata("original_files/mergedreaction8_new.dta") %>%
  select(name, year, cowcode, crim1date, totalreact,
         one_of(reaction.types$var.name)) %>%
  mutate_each(funs(as.numeric), one_of(reaction.types$var.name)) %>%
  mutate(criminalized = ifelse(year >= crim1date, TRUE, FALSE),
         reaction = ifelse(totalreact > 0, "Documented reaction",
                           "No documented reaction")) %>%
  group_by(cowcode) %>%
  mutate(criminalized.lead = lead(criminalized))

# Summarize probability of criminalization given a documented reaction
df.reactions <- reactions.crim %>%
  filter(!criminalized) %>%
  group_by(reaction) %>%
  summarise(total = n(),
            crim.no = sum(!criminalized.lead, na.rm=TRUE),
            crim.yes = sum(criminalized.lead, na.rm=TRUE),
            prob.yes = crim.yes / total) %>%
  filter(!is.na(reaction)) %>%
  arrange(prob.yes) %>%
  select(reaction, num = crim.yes, total, prob.yes)

# Summarize probability of criminalization given the type of reaction
reactions.crim.long <- reactions.crim %>%
  gather(var.name, value, one_of(reaction.vars), convert=TRUE) %>%
  left_join(reaction.types, by="var.name")

df.reaction.types <- reactions.crim.long %>% 
  filter(value > 0, totalreact > 0) %>%
  group_by(nice.name, criminalized) %>%
  summarise(total = n(),
            crim.no = sum(!criminalized.lead),
            crim.yes = sum(criminalized.lead),
            prob.yes = crim.yes / total)  %>%
  filter(!criminalized) %>%
  ungroup() %>%
  arrange(prob.yes) %>%
  select(reaction = nice.name, num = crim.yes, total, prob.yes)

# Combine the two dataframes, separated by a blank row
df.fig5.2.2 <- bind_rows(df.reaction.types, 
                         data_frame(reaction=" ", num=NA, total=NA, prob.yes=0), 
                         df.reactions) %>%
  mutate(label.n = ifelse(reaction != " ", 
                          paste0(reaction, " (N = ", 
                                 format(total, big.mark=",", trim=TRUE), 
                                 ")"), " "),
         label.n = factor(label.n, levels=label.n, ordered=TRUE))

# Plot, finally
fig5.2.2 <- ggplot(df.fig5.2.2, aes(x=label.n, y=prob.yes)) + 
  geom_bar(stat="identity") + 
  labs(x=NULL, y="Probability of criminalization") + 
  scale_y_continuous(labels=percent) + 
  coord_flip() + 
  theme_clean(10)

filename <- "figure5_2_2_react_criminalization"
width <- 4.5
height <- 3
ggsave(fig5.2.2, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig5.2.2, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# -----------
# Chapter 6
# -----------
# Figure 6.x: Predicted probability of criminalization, democracy × in report
plot.predict <- read_csv("final_figures/data_figure6_x_predict.csv")
plot.data <- read_csv("final_figures/data_figure6_x_plot.csv")

set.seed(1234)
fig6.x <- ggplot(plot.predict, aes(x=fh_cl1, y=prob)) +  
  geom_ribbon(aes(ymax=prob.upper, ymin=prob.lower, fill=inreport1), 
              alpha=0.2) + 
  geom_line(aes(colour=inreport1), size=0.75) +
  geom_rug(data=plot.data, alpha=0.3, size=0.1, sides="b", position="jitter") + 
  labs(x="Worse democracy (Freedom House civil liberties)", 
       y="Predicted probability of criminalization") + 
  scale_y_continuous(labels=percent, limits=c(0, max(plot.predict$prob.upper))) + 
  scale_x_continuous(breaks=seq(1, 7, 1)) +
  scale_fill_manual(values=c("grey80", "grey30"), name=NULL, guide=FALSE) + 
  scale_colour_manual(values=c("grey80", "grey30"), name=NULL,
                      labels=c("In report    ", "Not in report")) +
  theme_clean(10) + theme(legend.key.size=unit(0.65, "lines"),
                          legend.key = element_blank(),
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))

filename <- "figure6_x_interactions_democracy_report"
width <- 4.5
height <- 3
ggsave(fig6.x, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig6.x, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# Figure 6.x: Predicted probability of criminalization, democracy × tier
plot.predict <- read_csv("final_figures/data_figure6_x1_predict.csv")
plot.data <- read_csv("final_figures/data_figure6_x1_plot.csv")

set.seed(1234)
fig6.x1 <- ggplot(plot.predict, aes(x=fh_cl1, y=prob)) +  
  geom_ribbon(aes(ymax=prob.upper, ymin=prob.lower, fill=low_tier1), 
              alpha=0.2) + 
  geom_line(aes(colour=low_tier1), size=0.75) +
  geom_rug(data=plot.data, alpha=0.3, size=0.1, sides="b", position="jitter") + 
  labs(x="Worse democracy (Freedom House civil liberties)", 
       y="Predicted probability of criminalization") + 
  scale_y_continuous(labels=percent, limits=c(0, max(plot.predict$prob.upper))) + 
  scale_x_continuous(breaks=seq(1, 7, 1)) +
  scale_fill_manual(values=c("grey80", "grey30"), name=NULL, guide=FALSE) + 
  scale_colour_manual(values=c("grey80", "grey30"), name=NULL,
                      labels=c("Tier 1 or 2    ", "Watchlist or Tier 3")) +
  theme_clean(10) + theme(legend.key.size=unit(0.65, "lines"),
                          legend.key = element_blank(),
                          legend.margin = unit(0.25, "lines"),
                          plot.margin = unit(c(1, 0.25, 0, 0.25), "lines"))

filename <- "figure6_x_interactions_democracy_tiers"
width <- 4.5
height <- 3
ggsave(fig6.x1, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig6.x1, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# -----------
# Chapter 7
# -----------
# Figure 7.1: Summary of claims


# --------------------
# Unassigned figures
# --------------------
# Cho changes for case studies
filename <- "figurex_1_cho_changes_case_studies"
width <- 4.5
height <- 6
ggsave(fig.cho.changes, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig.cho.changes, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

# Average Cho scores for all countries vs. case countries
filename <- "figurex_2_avg_all_vs_cases"
width <- 4.5
height <- 2.5
ggsave(fig.cho.all.vs.cases, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig.cho.all.vs.cases, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)


# ------------------
# Appendix figures
# ------------------
# Impact of report on media coverage
plot.predict <- read_csv("final_figures/data_figureA_3_media_predict.csv") %>%
  mutate(inreport = factor(inreport, levels=c("Not in report", "In report"), 
                           ordered=TRUE))

fig.media.report <- ggplot(plot.predict, aes(x=inreport, y=pred)) + 
  geom_pointrange(aes(ymax=pred.upper, ymin=pred.lower), size=.75,
                  position=position_dodge(width=0.25)) + 
  labs(x=NULL, y="Predicted number of TIP-related stories") + 
  theme_clean(10)

filename <- "figureA_3_media_inreport_predict"
width <- 4.5
height <- 3
ggsave(fig.media.report, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(fig.media.report, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)
