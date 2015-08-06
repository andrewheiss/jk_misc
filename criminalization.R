# ----------------
# Load libraries
# ----------------
library(dplyr)
library(haven)
library(countrycode)
library(ggplot2)
library(grid)
library(Cairo)
library(rgdal)


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
crim <- read_stata("original_files/kelley_simmons_ajps_2014_replication.dta") %>%
  select(name, year, cowcode, adjbicrimlevel) %>%
  mutate(id = countrycode(cowcode, "cown", "iso3c")) %>%
  filter(year %in% c(2001, 2011))

# Load map information
countries.map <- readOGR("map_data", "ne_110m_admin_0_countries")
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99)))  # Get rid of Antarctica and NAs

# All possible countries (to fix the South Sudan issue)
possible.countries <- expand.grid(id = unique(as.character(countries.ggmap$id)),
                                  year = c(2001, 2011), stringsAsFactors=FALSE)

all.countries <- possible.countries %>% 
  left_join(crim, by=c("id", "year")) %>%
  mutate(crim.level.num = ifelse(is.na(adjbicrimlevel), -1, adjbicrimlevel),
         crim.level = factor(crim.level.num,
                             levels=c(2, 0, -1),
                             labels=c("Full criminalization    ",
                                      "No criminalization    ", 
                                      "No data"),
                             ordered=TRUE))


# -----------
# Plot data
# -----------
crim.map <- ggplot(all.countries, aes(fill=crim.level, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show_guide=FALSE) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  facet_wrap(~ year, ncol=1) + 
  scale_fill_manual(values=c("grey30", "white", "grey90"), name="") +
  theme_blank_map() +
  theme(legend.position="top", legend.key.size=unit(0.5, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))
crim.map
ggsave(crim.map, filename="figures/map_criminalization.pdf", device=cairo_pdf)
