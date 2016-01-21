# TODO: Same super dark grey for all bars
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
# TODO: Make more step-ful


# -----------
# Chapter 2
# -----------
# Figure 2.1: Trafficking patterns 2010-2012
# UNODC map

# Figure 2.2: Trafficking statistics over time 
# TODO: Convert to a figure
df.fig2.2 <- read_csv("final_figures/data_figure2_2.csv")
# "Meanwhile, despite an increase in prosecutions and convictions, compared to the estimated incidence, human trafficking goes virtually unpunished"

# Figure 2.3: Tier ratings over time
filename <- "figure2_3_tier_ratings_time"
width <- 4.5
height <- 2.5
ggsave(tier.plot, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(tier.plot, filename=file.path(base.folder, paste0(filename, ".png")),
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
# Side-by-side bar chart from survey
# TODO: Check dimensions and font size

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
  scale_fill_manual(values=c("black", "grey70", "grey40"), name=NULL) + 
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
# Bar chart about contact with the US embassy from survey
# TODO: Make this match style
# TODO: Remove "Q3.18"

# Figure 3.x: Table row for Q3.21
# TODO: Make this

# Figure 3.4: Stakeholders with whom NGOs discuss the TIP report
# Bar chart about who NGOs discuss the TIP report
# TODO: Make this match style
# TODO: Check dimensions and font size

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
  scale_fill_manual(values=c("grey50"), name=NULL) +
  scale_colour_manual(values=c("grey10"), name=NULL) + 
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
# Diagram
# TODO: Make this in Illustrator, save as open file

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
  scale_fill_gradient(low="white", high="grey20", na.value="white", guide=FALSE) +
  scale_color_identity() + 
  coord_fixed() +
  theme_clean(10) + theme(axis.text.x=element_text(angle=90, vjust=1, hjust=1),
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
  scale_fill_manual(values=c("grey70", "grey20"), name=NULL, 
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
# Line graph
# TODO: Make this in R

# Figure 5.2: Number of anti-TIP laws passing, by month, 2001-2014
# Bar chart
# TODO: Get data for this
# TODO: Make this in R

# Figure 5.3: Probability of criminalizing fully in a given year if a country had not already done so, 2001-2010
# Bar chart
# TODO: Make this in R

# Figure 5.2: Incidence of criminalization in the following year, 2001-2010 for all countries included in the TIP report that had not yet criminalized in the year of the report
# Bar chart
# TODO: Make this in R
# Use Figure 4.2 data


# -----------
# Chapter 6
# -----------
# Figure 6.x: Predicted probability of criminalization with worse democracy as X, colour=1/2 vs watchlist/3
# TODO: Make this match style, colors

# Figure 6.x: Predicted probability of criminalization with worse democracy as X, colour=in report
# TODO: Make this match style, colors
