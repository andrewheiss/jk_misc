library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggrepel)

source("shared_functions.R")

cases <- read_csv("data/cases_favorability_influence.csv") %>%
  gather(country, score, -Countries) %>%
  rename(variable = Countries) %>%
  mutate(score = gsub("−", "-", score),
         score = as.numeric(score)) %>%
  mutate(variable = recode(variable,`Favorability of conditioning factors` = "Favorability")) %>%
  spread(variable, score) %>%
  mutate(nudge_x = case_when(
    .$country %in% c("Chad", "Japan", "Nigeria", "Israel", "Kazakhstan", "Mozambique") ~ -0.5,
    .$country == "United Arab Emirates" ~ 1.5,
    TRUE ~ 0.5
  )) %>%
  mutate(nudge_y = case_when(
    .$country == "United Arab Emirates" ~ -0.5,
    .$country == "Ecuador" ~ -0.4,
    TRUE ~ 0
  ))

p.cases.favor.influence <- ggplot(cases, 
                                  aes(x=Influence, y=Favorability,
                                      label=country)) + 
  stat_smooth(size=0.5, method="lm", fullrange=TRUE, colour="grey30", alpha=0.1) +
  geom_text_repel(size=2.5, family="Source Sans Pro Light",
                  nudge_x = cases$nudge_x, nudge_y = cases$nudge_y,
                  segment.color = "grey30", segment.size = 0.25,
                  box.padding = unit(0.2, "lines")) +
  geom_point(size=1) + 
  coord_cartesian(ylim=c(-4, 4)) + xlim(0, 15) + 
  theme_clean()
p.cases.favor.influence

filename <- "figureX_X_cases_favor_influence"
width <- 4.5
height <- 3
ggsave(p.cases.favor.influence, filename=file.path(base.folder, paste0(filename, ".pdf")), 
       width=width, height=height, device=cairo_pdf)
ggsave(p.cases.favor.influence, filename=file.path(base.folder, paste0(filename, ".png")),
       width=width, height=height, type="cairo", dpi=300)

