library(dplyr)
library(tidyr)
library(readr)
library(scales)

flows <- read_csv("data/trafficking_flows_fig_23.csv") %>%
  gather(Destination, Flow, -Victims) %>%
  filter(Flow >= 0.05) %>%
  arrange(desc(Flow)) %>%
  mutate(stroke.width = round(rescale(Flow, to=c(0.75, 5)), 2)) 
