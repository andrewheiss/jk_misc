# ----------------
# Load libraries
# ----------------
library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
library(haven)
library(ggplot2)


# --------------------------------------------
# Create lookup table for reaction variables
# --------------------------------------------
reaction.vars <- c("funding", "comparisonsratingnotfair", "publicfacesaving", 
                   "embassment", "objection", "disappointment", "anger", 
                   "negative", "usarrogance", "movinggoal", "harmingrelations", 
                   "cooperative", "bragging", "howimprove")

reaction.names <- c("Funding", "Comparisons", "Public face saving", 
                    "Embarrassment", "Objection", "Disappointment", 
                    "Anger or Frustration", "Other negative reaction", 
                    "US arrogance", "Moving goal", "Harming relations", 
                    "Cooperative", "Appreciation/Bragging", "How improve")

reaction.groups <- c("Funding", rep("Image", 3), 
                     rep("Negative", 7), rep("Positive", 3))

reaction.types <- data_frame(var.name = reaction.vars, 
                             type.name = reaction.names, 
                             type.group = reaction.groups) %>%
  mutate(type.name = factor(type.name, levels=reaction.names, ordered=TRUE),
         type.group = factor(type.group, levels=unique(reaction.groups), ordered=TRUE))


# -----------------------
# Load and wrangle data
# -----------------------
# Load original Stata file and select just the reaction columns.
# "appreciation" needs to be merged with "bragging", so it's not 
#   included in the lookup table.
reactions.raw <- read_stata("original_files/mergedreaction8_new.dta") %>%
  select(name, year, cowcode, totalreact, 
         one_of(reaction.types$var.name), appreciation) %>%
  mutate_each(funs(as.numeric), 
              one_of(reaction.types$var.name, "appreciation")) %>%
  mutate(bragging = bragging + appreciation,
         bragging = ifelse(bragging < 1, 0, 1)) %>%
  select(-appreciation)

# Get the number of country-years (or reports) with reactions
n.reports.reactions <- nrow(filter(reactions.raw, totalreact > 0))

# Convert to long based on reaction type
reactions.long <- reactions.raw %>%
  gather(var.name, value, one_of(reaction.types$var.name), convert=TRUE) %>%
  left_join(reaction.types, by="var.name")

# Summarize reaction types
reactions.type <- reactions.long %>%
  group_by(type.name) %>%
  summarise(total = sum(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(prop = total / sum(total),
         prop.all.reactions = total / n.reports.reactions)

# Summarize groups of reaction types
reactions.group <- reactions.long %>%
  # Group each observation by general type and check if at least one reaction is present
  group_by(type.group, name, year) %>%
  summarise(total = sum(value)) %>%
  # If there was a reaction in the reaction group, mark true
  mutate(present.in.group = ifelse(total > 0, TRUE, FALSE)) %>%
  # Leave observation level; summarize just the groups
  group_by(type.group) %>%
  summarise(at.least.one = sum(present.in.group, na.rm=TRUE)) %>%
  mutate(prop.all.reactions.group = at.least.one / n.reports.reactions) %>%
  select(-at.least.one)

# Combine types and groups into one table
reactions <- reactions.type %>% 
  left_join(select(reaction.types, -var.name), by="type.name") %>%
  left_join(reactions.group, by="type.group") %>%
  select(type.group, everything())

reactions.print <- reactions %>%
  mutate_each(funs(sprintf("%.1f%%", . * 100)), starts_with("prop")) %>%
  mutate(type.name = as.character(type.name)) %>%
  group_by(type.group) %>%
  mutate(num.row = 1:n()) %>%
  ungroup() %>%
  mutate(type.group = ifelse(num.row == 1, as.character(type.group), "—"),
         prop.all.reactions.group = ifelse(num.row == 1, 
                                           prop.all.reactions.group, "—")) %>%
  select(-num.row) %>%
  set_colnames(c("Reaction type", "Reaction", "Total reactions", 
                 "Percent of all reactions", 
                 "Percent of reports with this reaction", 
                 "Percent of reports with at least one reaction of this type"))

total.row <- c("—", "Total", sum(reactions.print$`Total reactions`), 
               "100%", "Not mutually exclusive", "Not mutually exclusive")

reactions.print <- rbind(reactions.print, total.row)


# ------------
# Plot stuff
# ------------
reactions.plot <- reactions %>%
  mutate(type.name = factor(type.name, 
                            levels=rev(levels(type.name)), ordered=TRUE))

ggplot(reactions.plot, aes(x=type.name, y=total, fill=type.group)) + 
  geom_bar(stat="identity") + coord_flip()

p1 <- ggplot(reactions.plot, aes(x=type.name, y=prop, fill=type.group)) + 
  geom_bar(stat="identity") + coord_flip()

p2 <- ggplot(reactions.plot, aes(x=type.name, y=prop.all.reactions, fill=type.group)) + 
  geom_bar(stat="identity") + coord_flip()

p1
p2
