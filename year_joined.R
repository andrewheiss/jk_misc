# ----------------
# Load libraries
# ----------------
library(dplyr)
library(readr)
library(countrycode)
library(rgdal)
library(ggplot2)
library(grid)
library(Cairo)


# ------------------
# Useful functions
# ------------------
theme_blank_map <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border=element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          axis.title=element_blank(), axis.text=element_blank(),
          legend.text=element_text(size=rel(0.7), family="Source Sans Pro Light"),
          legend.title=element_text(size=rel(0.9), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"))
  ret
}


# -----------
# Load data
# -----------
# Clean data first
# cleaned <- read_csv("data/year_joined_original.csv") %>%
#   mutate(actual.name = countrycode(country_name, "country.name", "country.name"),
#          cow = countrycode(actual.name, "country.name", "cown")) %>%
#   select(country_name = actual.name, cow, start_year)
# write_csv(cleaned, "data/year_joined.csv")

# Load map information
countries.map <- readOGR("map_data", "ne_110m_admin_0_countries")
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99)))  # Get rid of Antarctica and NAs

# All possible countries (to fix the South Sudan issue)
possible.countries <- data_frame(id = unique(as.character(countries.ggmap$id)))

# Year bins
year.levels <- data_frame(start_year = 2001, year.level = 1) %>%
  bind_rows(data_frame(start_year = 2002:2004, year.level = 2)) %>%
  bind_rows(data_frame(start_year = 2005:2007, year.level = 3)) %>%
  bind_rows(data_frame(start_year = 2007:2010, year.level = 4)) %>%
  bind_rows(data_frame(start_year = NA, year.level = 5))

joined <- read_csv("data/year_joined.csv") %>%
  mutate(id = countrycode(cow, "cown", "iso3c")) %>%
  select(id, start_year)

year.labels <- c("Initial 2001 report    ", "2002-2004    ", "2005-2007    ",
                 "2007-2010    ", "Not in report")

joined.full <- possible.countries %>% 
  left_join(joined, by="id") %>%
  left_join(year.levels, by="start_year") %>%
  mutate(year.level = factor(year.level, labels = year.labels, ordered = TRUE))


# -----------
# Plot data
# -----------
report.map <- ggplot(joined.full, aes(fill=year.level, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show_guide=FALSE) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_manual(values=c("grey85", "grey65", "grey30", "grey5", "white"), name="") +
  theme_blank_map() + 
  theme(legend.position="top", legend.key.size=unit(0.5, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))
report.map
ggsave(report.map, filename="figures/map_joined_report.pdf", device=cairo_pdf)
