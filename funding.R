# ----------------
# Load libraries
# ----------------
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(stringr)
library(lubridate)
library(foreign)
library(countrycode)
library(pander)
library(maptools)
library(rgdal)
library(gridExtra)

source("shared_functions.R")


# ------------------
# Useful functions
# ------------------
# Shrink long strings
truncate <- function(x) {
  if (nchar(x) > 80) {
    paste0(strtrim(x, 80), "...")
  } else {
    x
  }
}


# ------------------------
# Clean super messy data
# ------------------------
raw.years <- read.csv("original_files/funding_original.csv", 
                      stringsAsFactors=FALSE) %>%
  select(grant_year = fiscal.year)

funding.clean <- read.csv("original_files/funding_master.csv", 
                          stringsAsFactors=FALSE) %>%
  slice(-c(1000, 3157)) %>%  # Remove these two extra rows
  bind_cols(raw.years) %>%  # Bring in years from other CSV
  mutate(amount = as.numeric(gsub("\\D", "", amount)),
         grant = as.numeric(ifelse(!(grant %in% c(0, 1)), NA, grant)),
         Prevention = as.numeric(ifelse(!(Prevention %in% c(0, 1)), NA, Prevention)),
         protection = as.numeric(ifelse(!(protection %in% c(0, 1)), NA, protection)),
         prosecution = as.numeric(ifelse(!(prosecution %in% c(0, 1)), NA, prosecution)),
         research = as.numeric(gsub("\\D", "", gsub("^o$", "0", research))),
         year_actual = ymd(paste0(grant_year, "-01-01")),
         recipient.1 = factor(recipient.1),
         id = 1:nrow(.)) %>%
  select(id, country = Country, grant_year, year_actual, cowcode, grant, recipient, 
         subgrantee = Subgrantee, prevention = Prevention, protection, 
         prosecution, research, amount, region1 = Regional., 
         region2 = Regional..1, recipient_type = recipient.1) %>%
  mutate(recipient = ifelse(recipient == "", NA, recipient),
         subgrantee = ifelse(subgrantee == "", NA, subgrantee),
         region1 = ifelse(region1 == "", NA, region1),
         region2 = ifelse(region2 == "", NA, region2))


# ----------------
# Write to Stata
# ----------------
# Stata (or at least write.dta?) gets mad at long character columns
funding.clean.stata <- funding.clean %>%
  select(-year_actual) %>%
  rowwise() %>%
  mutate(recipient = truncate(recipient),
         subgrantee = truncate(subgrantee)) %>%
  ungroup()
  
labs <- c("Row ID", "Country name", "Year of grant", 
          "COW code", "Grant", "Grant recipient", "Subgrantee", "Prevention", 
          "Protection", "Prosecution", "Research", "Amount given", "Region", 
          "Region (alternate)", "Type of recipient")
attr(funding.clean.stata, "var.labels") <- labs

write.dta(funding.clean.stata, "data/funding_clean.dta")
system("stata-se -b do funding_clean_stata.do")
system("rm funding_clean_stata.log")


# --------------------------------------------------------------------
# ---------------
# Summary plots
# ---------------
# --------------------------------------------------------------------
# Clean IGO names
igo.names <- funding.clean %>% 
  filter(recipient_type == "IGO") %>% select(recipient) %>%
  unique() %>%
  arrange(recipient) %T>%
  # Manually choose which IGO counts
  write_csv(path="data/igo_abbreviations_NEW.csv")

# Merge manual decicions into full dataset
funding.clean.abbrev <- funding.clean %>%
  left_join(read_csv("data/igo_abbreviations.csv"), by="recipient")


# --------------------------------------------------------------------
# Number of grants + total amount granted to IGOs, by individual IGO
# --------------------------------------------------------------------
funding.igos.indiv <- funding.clean.abbrev %>%
  filter(recipient_type == "IGO",
         recipient_clean != "NGO") %>%
  group_by(recipient_clean) %>%
  summarise(total = sum(amount, na.rm=TRUE),
            number = n()) %>%
  mutate(prop_n = sprintf("%.1f%%", number / sum(number) * 100),
         prop_grants = sprintf("%.1f%%", total / sum(total) * 100)) %>%
  arrange(desc(total)) %>%
  mutate(recipient_factor = factor(recipient_clean, 
                                   levels=rev(recipient_clean), ordered=TRUE)) %>%
  arrange(desc(number)) %>%
  mutate(recipient_factor_n = factor(recipient_clean, 
                                   levels=rev(recipient_clean), ordered=TRUE))

fig.total.to.igos <- ggplot(funding.igos.indiv, 
                            aes(x = recipient_factor_n, y = total)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop_grants), size=3.5, hjust=-0.3, 
            family="Source Sans Pro Light") + 
  scale_y_continuous(labels = dollar, expand = c(.1, .1)) + 
  labs(x = NULL, y = "Total grant amount") + 
  coord_flip() + 
  theme_clean() + 
  theme(axis.text.y = element_text(hjust=0.5), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,1,1,0), "lines"))
fig.total.to.igos

fig.n.to.igos <- ggplot(funding.igos.indiv, 
                        aes(x = recipient_factor_n, y = number)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop_n), size=3.5, hjust=1.3, 
            family="Source Sans Pro Light") + 
  scale_y_reverse(expand = c(.1, .1)) + 
  labs(x = NULL, y = "Total number of grants") + 
  coord_flip() + 
  theme_clean() + 
  theme(axis.text.y = element_blank(), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,0.5,1,1), "lines"))
fig.n.to.igos

grants.to.igos <- arrangeGrob(fig.n.to.igos, fig.total.to.igos, nrow=1)
ggsave(grants.to.igos, filename="figures/fig_grants_to_igos.pdf",
       width=5, height=2.5, units="in", device=cairo_pdf, scale=2.5)
ggsave(grants.to.igos, filename="figures/fig_grants_to_igos.png",
       width=5, height=2.5, units="in", scale=2.5)


# -------------------------------------
# Share of all grants awarded to IGOs
# -------------------------------------
funding.igos <- funding.clean.abbrev %>%
  group_by(recipient_type) %>%
  summarise(total = sum(amount, na.rm=TRUE),
            number = n()) %>%
  mutate(prop_n = sprintf("%.1f%%", number / sum(number) * 100),
         prop_grants = sprintf("%.1f%%", total / sum(total) * 100)) %>%
  arrange(total) %>%
  mutate(prop = total / sum(total)) %>%
  mutate(recipient_type = gsub("^$", "Not specified", recipient_type),
         recipient_type_factor = factor(recipient_type, 
                                        levels=recipient_type, ordered=TRUE)) %>%
  arrange(number) %>%
  mutate(recipient_type_factor_n = factor(recipient_type, 
                                          levels=recipient_type, ordered=TRUE))

fig.total.all.sectors <- ggplot(funding.igos, 
                                aes(x = recipient_type_factor_n, y = total)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop_grants), size=3.5, hjust=-0.3, 
            family="Source Sans Pro Light") + 
  scale_y_continuous(labels = dollar, expand = c(.15, .15)) + 
  labs(x = NULL, y = "Total grant amount") + 
  coord_flip() + 
  theme_clean() + 
  theme(axis.text.y = element_text(hjust=0.5), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,1,1,0), "lines"))

fig.n.all.sectors <- ggplot(funding.igos, 
                        aes(x = recipient_type_factor_n, y = number)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop_n), size=3.5, hjust=1.3, 
            family="Source Sans Pro Light") + 
  scale_y_reverse(expand = c(.1, .1)) + 
  labs(x = NULL, y = "Total number of grants") + 
  coord_flip() + 
  theme_clean() + 
  theme(axis.text.y = element_blank(), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,0.5,1,1), "lines"))

grants.to.all.sectors <- arrangeGrob(fig.n.all.sectors, fig.total.all.sectors, nrow=1)
ggsave(grants.to.all.sectors, filename="figures/fig_grants_to_all_sectors.pdf",
       width=5, height=2, units="in", device=cairo_pdf, scale=2.5)
ggsave(grants.to.all.sectors, filename="figures/fig_grants_to_all_sectors.png",
       width=5, height=2, units="in", scale=2.5)


# -----------------------------------
# Purpose of grants awarded to IGOs
# -----------------------------------
# TODO: Pie chart
# TODO: Amount too
library(magrittr)
bloop <- funding.clean.abbrev %>%
  filter(recipient_type == "IGO") %>%
  select(prevention, protection, prosecution, research) %>%
  na.omit()

asdf <- bloop %>% left_join(possible.combinations) %>% 
  group_by(id) %>% summarise(total = n()) %>%
  left_join(possible.combinations, by="id")

sum((asdf %>% filter(prevention == 1))$total)

possible.combinations <- expand.grid(rep(list(0:1), 4)) %>% 
  set_colnames(c("prevention", "protection", "prosecution", "research")) %>%
  mutate(id=1:n())


funding.purpose <- funding.clean.abbrev %>%
  filter(recipient_type == "IGO") %>%
  summarise_each(funs(sum(., na.rm=TRUE)), 
                 c(prevention, protection, prosecution, research)) %>%
  gather(grant_purpose, count) %>%
  arrange(count) %>%
  mutate(purpose_factor = factor(grant_purpose, 
                                 levels=grant_purpose, 
                                 labels=str_to_title(grant_purpose), ordered=TRUE))

fig.grant.purpose <- ggplot(funding.purpose, aes(x = purpose_factor, y=count)) + 
  geom_bar(stat="identity", position="dodge") + 
  labs(x=NULL, y="Number of grants awarded") + 
  coord_flip() + 
  theme_clean()
fig.grant.purpose
ggsave(fig.grant.purpose, filename="figures/fig_grant_purpose.pdf", 
       width=6, height=2.5, units="in", device=cairo_pdf)
ggsave(fig.grant.purpose, filename="figures/fig_grant_purpose.png",
       width=6, height=2.5, units="in")


# ------------------------
# IGO funding by country
# ------------------------
# TODO: All TIP funding
regions <- c("Africa", "East Asia and Pacific Islands", "Europe", "Global", 
             "Near East Asia", "South and Central Asia", "Western Hemisphere")
funding.igos.countries <- funding.clean.abbrev %>%
  mutate(id = countrycode(cowcode, "cown", "iso3c"),
         clean.name = countrycode(cowcode, "cown", "country.name"),
         clean.name = ifelse(is.na(clean.name), country, clean.name)) %>%
  filter(recipient_type == "IGO") %>%
  mutate(region = ifelse(clean.name %in% regions, TRUE, FALSE)) %>%
  filter(!region) %>%
  group_by(id) %>%
  summarise(total = sum(amount, na.rm=TRUE)) %>%
  arrange(desc(total))

funding.igos.countries.print <- funding.igos.countries %>%
  mutate(total = dollar(total)) %>%
  set_colnames(c("Country", "Total awarded"))

pandoc.table(head(funding.igos.countries.print, 20))

# Load map information
countries.map <- readOGR("map_data", "ne_110m_admin_0_countries")
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99))) %>%  # Get rid of Antarctica and NAs
  mutate(id = ifelse(id == "GRL", "DNK", id))  # Greenland is part of Denmark

# All possible countries (to fix the South Sudan issue)
possible.countries <- data_frame(id = unique(as.character(countries.ggmap$id)))

all.countries <- possible.countries %>% 
  left_join(funding.igos.countries, by=c("id"))

# Map of proportions with a gradient fill
map.igo.country <- ggplot(all.countries, aes(fill=total, map_id=id)) +
  geom_map(map=countries.ggmap) + 
  # Second layer to add borders and slash-less legend
  geom_map(map=countries.ggmap, size=0.15, colour="black", show.legend=FALSE) + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(high="black", low="white", na.value="white",
                      labels=dollar, name="", limits=c(0, max(all.countries$total)),
                      guide=guide_colorbar(draw.llim=TRUE, barwidth=15, barheight=0.5, ticks=FALSE)) +
  theme_blank_map() + 
  theme(legend.position="bottom")
map.igo.country
ggsave(map.igo.country, filename="figures/map_igo_funding.pdf", device=cairo_pdf)
ggsave(map.igo.country, filename="figures/map_igo_funding.png")
