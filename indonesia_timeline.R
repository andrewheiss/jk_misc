# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(lubridate)
library(ggplot2)
library(grid)


# ---------------------
# Load and clean data
# ---------------------
timeline <- read_csv("original_files/indonesia_timeline.csv") %>%
  assign("raw.timeline", value=., pos=1) %>%
  gather(year, progress, -Recommendations) %>%
  mutate(progress = ifelse(progress == "", NA, progress),
         Recommendations = factor(Recommendations, 
                                  levels=raw.timeline$Recommendations, 
                                  ordered=TRUE)) %>%
  arrange(Recommendations, year)

in.report <- read_csv("original_files/indonesia_timeline_in_report.csv") %>%
  gather(year, included, -Recommendations) %>%
  mutate(included = ifelse(included == "X", TRUE, NA),
         Recommendations = factor(Recommendations, 
                                  levels=raw.timeline$Recommendations, 
                                  ordered=TRUE)) %>%
  filter(!is.na(included)) %>%
  arrange(Recommendations, year)

timeline.full <- timeline %>% 
  left_join(in.report, by=c("Recommendations", "year"))


# ----------------
# Plot timelines
# ----------------
progress.num <- data_frame(progress = c("none", "some", "a lot", 
                                        "complete", "none mentioned", "?"),
                           progress.num = c(0, 1/3, 2/3, 1, 0, 0))

timeline.plot <- timeline.full %>% left_join(progress.num, by="progress") %>%
  mutate(progress.num = ifelse(is.na(progress.num), 0, progress.num),
         year.actual = ymd(paste0(as.character(year), "-01-01")),
         included.num = ifelse(included, 0.5, NA))

timelines <- ggplot(timeline.plot, aes(x=year.actual, y=progress.num)) + 
  geom_area(fill="grey50") + geom_point(aes(x=year.actual, y=included.num)) + 
  facet_wrap(~ Recommendations, ncol=3) + 
  theme(panel.background=element_rect(fill="transparent", colour = NA),
        panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

ggsave(timelines, filename="figures/indonesia_raw.pdf", 
       width=16, height=10, units="in")


# ------------------------------------
# Export underlying plot data to CSV
# ------------------------------------
timeline.to.csv <- timeline.plot %>%
  mutate(included_in_report = as.numeric(included)) %>%
  select(recommendation = Recommendations, year, progress, included_in_report)

write_csv(timeline.to.csv, path="data/timeline_recommendations_indonesia.csv")
