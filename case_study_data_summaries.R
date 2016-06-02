library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)
library(WDI)
library(feather)
library(readr)
library(lubridate)
library(gtable)
library(gridExtra)
library(ggrepel)
library(scales)
library(pander)
panderOptions('pandoc.binary', '/Users/andrew/.cabal/bin/pandoc')
panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)
panderOptions('table.alignment.default', 'left')

source("shared_functions.R")

base.folder <- "/Users/andrew/Desktop/cases"

cases <- c("ARM", "IDN", "ECU", "MOZ", "KAZ", "ARG", "ISR", 
           "ARE", "NGA", "OMN", "HND", "JPN", "TCD", "ZWE", "MYS")


# ------------------------
# Load all sorts of data
# ------------------------
# TIP funding
funding <- read_csv("data/funding_clean.csv") %>%
  mutate(iso = countrycode(cowcode, "cown", "iso3c")) %>%
  filter(iso %in% cases) %>%
  group_by(iso, grant_year) %>%
  summarise(total.funding = sum(amount, na.rm=TRUE),
            avg.funding = mean(amount, na.rm=TRUE)) %>%
  group_by(iso) %>%
  mutate(cum.funding = cumsum(total.funding))


# World Bank data
wdi.indicators <- c("NY.GDP.PCAP.KD",  # GDP per capita (constant 2005 US$)
                    "NY.GDP.MKTP.KD",  # GDP (constant 2005 US$)
                    "SP.POP.TOTL",     # Population, total
                    "DT.ODA.ALLD.KD")  # Net ODA and official aid received (constant 2013 US$)
wdi.raw <- WDI(country=countrycode(cases, "iso3c", "iso2c"), wdi.indicators,
               extra=TRUE, start=2000, end=2014)

wdi.clean <- wdi.raw %>%
  rename(gdpcap = NY.GDP.PCAP.KD, gdp = NY.GDP.MKTP.KD, 
         population = SP.POP.TOTL, oda = DT.ODA.ALLD.KD) %>%
  mutate(oda.gdp = oda / gdp,
         gdp.millions = gdp / 1000000,
         oda.millions = oda / 1000000) %>%
  mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
         population.log = log(population)) %>%
  mutate(gdpcap.log = log(gdpcap), gdp.log = log(gdp),
         population.log = log(population)) %>%
  # Ignore negative values of oda
  mutate(oda.log = sapply(oda, FUN=function(x) ifelse(x < 0, NA, log1p(x))))


# Policy index
df.cho <- readRDS("data/policy_index.rds") %>%
  mutate(year.actual = ymd(paste0(year, "-01-01"))) %>%
  filter(iso %in% cases)

df.crim.orig <- df.cho %>%
  group_by(iso) %>%
  slice(1) %>%
  ungroup() %>%
  select(iso, year, year.actual, year.full, year.partial) %>%
  gather(crim.type, crim.year, year.full, year.partial) %>%
  mutate(crim.year = ymd(paste0(crim.year, "-01-01"), quiet=TRUE),
         crim.type = recode(crim.type, year.full = "Full criminalization",
                            year.partial = "Partial criminalization"))

# Export to CSV for hand refining
write_csv(df.crim.orig, "data/crim_cases_WILL_BE_OVERWRITTEN.csv")
df.crim <- read_csv("data/crim_cases.csv")


# Archigos database of political leaders
# http://privatewww.essex.ac.uk/~ksg/archigos.html
leaders <- read_tsv("http://privatewww.essex.ac.uk/~ksg/data/1March_Archigos_4.1.txt")
leaders.cases.orig <- leaders %>%
  filter(ccode %in% countrycode(cases, "iso3c", "cown")) %>%
  filter(enddate > ymd("2001-01-01")) %>%
  mutate(middate = startdate - floor((startdate - enddate) / 2),
         time.in.office = enddate - startdate,
         leader = iconv(leader, from="Windows-1252", to="UTF-8"))  # Convert to UTF-8

# Export to CSV for hand refining
write_csv(leaders.cases.orig, "data/leaders_WILL_BE_OVERWRITTEN.csv")
leaders.cases <- read_csv("data/leaders.csv")


# -------------------
# Summary functions
# -------------------
# Export Markdown+HTML tables of summary statistics
summarize_case <- function(iso3) {
  mean.gdp.cap <- mean(filter(wdi.clean, iso3c == iso3)$gdpcap, na.rm=TRUE)
  total.aid <- sum(filter(wdi.clean, iso3c == iso3)$oda.million, na.rm=TRUE)
  mean.oda.gdp <-  mean(filter(wdi.clean, iso3c == iso3)$oda.gdp, na.rm=TRUE)
  mean.oda.gdp <- ifelse(is.nan(mean.oda.gdp), 0, mean.oda.gdp)  # Hi Japan
  total.tip.grants <- sum(filter(funding, iso == iso3)$total.funding, na.rm=TRUE)
  
  rows <- c("Average GDP per capita", "Total aid",
            "Average aid as percent of GDP", "Total TIP grants")
  
  just.numbers <- data_frame(Statistic = rows,
                             Value = c(dollar(mean.gdp.cap),
                                       paste(dollar(total.aid), "million"),
                                       percent(mean.oda.gdp),
                                       dollar(total.tip.grants)))
  cat(pandoc.table.return(just.numbers, justify="ll"),
      file=file.path(base.folder, paste0("summary_", iso3, ".txt")))
  
  Pandoc.convert(file.path(base.folder, paste0("summary_", iso3, ".txt")),
                 format="html", footer=FALSE, proc.time=FALSE, 
                 options = "-s", open=FALSE)
}

# Export a fancy timeline of criminalization, TIP ratings, Cho scores, and
# heads of state
timeline_case <- function(iso3) {
  # Criminalization events for Kazakhstan are too close together and too close
  # the edge of the plot, so full criminalization needs to be moved over
  df.crim.plot <- filter(df.crim, iso == iso3) %>%
    mutate(crim.year.adj = crim.year) %>%
    mutate(crim.year.adj = ifelse(iso3 == "KAZ" & crim.type == "Full criminalization", 
                                  crim.year + years(2), crim.year))
  
  # Also, ifelse() needlessly destroys the class of objects it works with,
  # including dates, so we have to reassign the date class
  class(df.crim.plot$crim.year.adj) <- "Date"
  
  # Cho scores
  plot.cho <- ggplot(filter(df.cho, iso == iso3),
                     aes(x=year.actual, y=p)) + 
    geom_line() + 
    geom_vline(data=df.crim.plot, aes(xintercept=as.numeric(crim.year)),
               size=0.5, colour="grey50", linetype="dotted") +
    labs(x=NULL, y="Cho policy index") +
    scale_y_continuous(limits=c(0, 15)) +
    coord_cartesian(xlim=ymd(c("2000-01-01", "2014-12-31"))) +
    theme_clean(10) + theme(panel.grid.minor.x=element_blank(),
                            panel.grid.minor.y=element_blank())

  # Tier rankings
  plot.tier <- ggplot(filter(df.cho, iso == iso3),
                      aes(x=year.actual, y=tier)) + 
    geom_line() + 
    geom_vline(data=df.crim.plot, aes(xintercept=as.numeric(crim.year)),
               size=0.5, colour="grey50", linetype="dotted") +
    labs(x=NULL, y="TIP ranking") +
    scale_y_continuous(breaks=c(1, 2, 2.5, 3), 
                       labels=c("Tier 1", "Tier 2", "Watch List", "Tier 3")) +
    coord_cartesian(xlim=ymd(c("2000-01-01", "2014-12-31")),
                    ylim=c(1, 3)) +
    theme_clean(10) + theme(panel.grid.minor.x=element_blank(),
                            panel.grid.minor.y=element_blank())

  # Heads of state
  plot.leaders <- ggplot(data=filter(leaders.cases,
                                     ccode == countrycode(iso3, "iso3c", "cown"))) +
    geom_segment(aes(x=middate, xend=middate, y=pos.y, yend=0.5),
                 size=0.5, colour="grey30") +
    geom_segment(aes(x=startdate, xend=enddate,
                     y=0.5, yend=0.5, colour=leader), size=3) +
    geom_label(data=filter(leaders.cases,
                           ccode == countrycode(iso3, "iso3c", "cown")),
               aes(x=middate, y=pos.y, label=leader, hjust=hjust),
               family="Source Sans Pro Semibold", size=2.5,
               fill="grey30", colour="white", label.padding=unit(0.3, "lines")) +
    scale_colour_grey() +
    coord_cartesian(xlim=ymd(c("2000-01-01", "2014-12-31")),
                    ylim=c(-0.1, 0.55)) +
    labs(x=NULL, y=NULL) +
    guides(colour=FALSE) +
    theme_clean(10) +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text=element_blank())

  # Criminalization
  plot.crim <- ggplot(df.crim.plot,
                      aes(x=crim.year, y=0)) + 
    geom_segment(aes(x=crim.year, xend=crim.year.adj, y=0, yend=1), 
                 size=0.5, colour="grey30") +
    geom_label(aes(x=crim.year.adj, y=1, label=crim.type, hjust=hjust),
               family="Source Sans Pro Semibold", size=2.5,
               fill="grey30", colour="white", label.padding=unit(0.3, "lines")) +
    geom_point(size=1, colour="grey30") +
    coord_cartesian(xlim=ymd(c("2000-01-01", "2014-12-31")),
                    ylim=c(-0.15, 1.5)) +
    labs(x=NULL, y=NULL) +
    theme_clean(10) + 
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text=element_blank())
  
  # Combine all the plots
  plot.timeline <- rbind.gtable(ggplotGrob(plot.crim),
                                ggplotGrob(plot.cho),
                                ggplotGrob(plot.tier),
                                ggplotGrob(plot.leaders))
  
  # Adjust panel sizes
  # via http://stackoverflow.com/a/24333504/120898
  panels <- plot.timeline$layout$t[grep("panel", plot.timeline$layout$name)]
  plot.timeline$heights[panels] <- unit(c(0.6, 1, 1, 1), "null")
  
  # grid::grid.draw(plot.timeline)
  
  # Save plot
  filename <- paste0("timeline_", iso3)
  width <- 4.5
  height <- 3
  ggsave(plot.timeline, filename=file.path(base.folder, paste0(filename, ".pdf")), 
         width=width, height=height, device=cairo_pdf)
  ggsave(plot.timeline, filename=file.path(base.folder, paste0(filename, ".png")),
         width=width, height=height, type="cairo", dpi=300)
}


# -------------------------------------
# Summarize and timelineize all cases
# -------------------------------------
sapply(cases, FUN=summarize_case)
sapply(cases, FUN=timeline_case)
