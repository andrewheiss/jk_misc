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

# Determine a country's presence in the annual TIP report
# Fun logic to get this working:
#   1. Take the `joined` column, which just has a TRUE in the year a country 
#      joined, convert to numeric and calculate the cumulative sum (x.num)
#   2. Calculate the cumulative sum again to find how many years the country 
#      has been part of the report (x.final)
#   3. Convert to factor: 
#      Not present: x.final = 0, Newly added: x.final = 1, 
#      Already present: x.final > 1
#
# Example of all intermediate columns:
#
#     id report_year joined x.num x.final x.text
# 1  AFG        2001     NA     0       0     No
# 2  AFG        2002   TRUE     1       1    New
# 3  AFG        2003     NA     1       2    Yes
# 4  AFG        2004     NA     1       3    Yes
# 5  AFG        2005     NA     1       4    Yes
#
# Function only returns the final text
calc.tip.presence <- function(x) {
  possible.levels <- c("New to report    ", "In report    ", "Not in report")
  
  if (all(is.na(x))) {
    x.text <- factor(possible.levels[3], levels=possible.levels)
  } else{
    x.num <- cumsum(ifelse(is.na(x), FALSE, x))
    x.final <- cumsum(x.num)
    x.text <- ifelse(x.final == 0, possible.levels[3], 
                     ifelse(x.final == 1, possible.levels[1], 
                            possible.levels[2]))
#     x.text <- cut(x.final, breaks=c(0, 0.9, 1, Inf), include.lowest=TRUE, 
#                   labels=possible.levels)
  }
  return(factor(x.text, levels=possible.levels, ordered=TRUE))
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

joined <- read_csv("data/year_joined.csv") %>%
  mutate(id = countrycode(cow, "cown", "iso3c"),
         joined = TRUE) %>%
  select(id, start_year, joined)

all.possibilities <- expand.grid(id = countrycode_data$iso3c,
                                 report_year = 2001:2010, 
                                 stringsAsFactors=FALSE)

all.years <- all.possibilities %>%
  left_join(joined, by=c("id", "report_year" = "start_year")) %>%
  arrange(id, report_year)

# Collapse 2007-2010; if any countries were added in those years, mark 2007 
# as true; return just the 2007 row
yrs2007_10 <- all.years %>%
  filter(report_year >= 2007) %>%
  group_by(id) %>%
  mutate(joined = ifelse(all(is.na(joined)), NA, TRUE)) %>%
  filter(report_year == 2007)

# Remove 2007-2010 and add the collapsed 2007 row; make the year a factor and 
# add the 2007-2010 label to the 2007
full.data <- all.years %>%
  filter(report_year < 2007) %>%
  bind_rows(yrs2007_10) %>%
  arrange(id, report_year) %>%
  group_by(id) %>%
  mutate(in.report = calc.tip.presence(joined),
         year.factor = factor(report_year, 
                              labels=c(as.character(2001:2006), "2007-2010"),
                              ordered=TRUE))

# Load map information
countries.map <- readOGR("map_data", "ne_110m_admin_0_countries")
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99)))  # Get rid of Antarctica and NAs

# All possible countries (to fix the South Sudan issue)
possible.countries <- expand.grid(id = unique(as.character(countries.ggmap$id)),
                                  year = c(2001, 2010), stringsAsFactors=FALSE)


# -----------
# Plot data
# -----------
# TODO: See if we can collapse 2006-2010 instead so there's a better grid
report.map <- ggplot(full.data, aes(fill=in.report, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show_guide=FALSE) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  facet_wrap(~ year.factor, ncol=3) + 
  scale_fill_manual(values=c("grey10", "grey70", "white"), name="") +
  theme_blank_map() + 
  theme(legend.position="top", legend.key.size=unit(0.5, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))
report.map
ggsave(report.map, filename="figures/map_joined_report.pdf", device=cairo_pdf)
