#' ---
#' title: "Supporting data and numbers for Chapter 3"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document:
#'     css: fixes.css
#'     toc: yes
#'     highlight: pygments
#'     theme: flatly
#' ---

#+ include=FALSE
# Set up knitr
library(knitr)
knitr::opts_chunk$set(warning=FALSE, message=FALSE, echo=FALSE,
                      tidy.opts=list(width.cutoff=120),  # For code
                      options(width=120))  # For results

# Load libraries
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(pander)
library(stringr)
library(gridExtra)

source("shared_functions.R")

# Return a data frame of counts and proportions for multiple responses
separate.answers.summary <- function(df, cols, labels, total=FALSE) {
  cols.to.select <- which(colnames(df) %in% cols)
  
  denominator <- df %>%
    select(cols.to.select) %>%
    mutate(num.answered = rowSums(., na.rm=TRUE)) %>%
    filter(num.answered > 0) %>%
    nrow()
  
  df <- df %>%
    select(survey.id, cols.to.select) %>%
    gather(question, value, -survey.id) %>%
    mutate(question = factor(question, labels=labels, ordered=TRUE)) %>%
    group_by(question) %>%
    summarize(response = sum(value, na.rm=TRUE), 
              pct = round(response / denominator * 100, 2),
              plot.pct = response / denominator)
  
  colnames(df) <- c("Answer", "Responses", "%", "plot.pct")
  
  if (total) {
    df <- df %>% select(1:3)
    df <- rbind(as.matrix(df), c("Total responses", denominator, "—"))
  }
  
  return(list(df=df, denominator=denominator))
}

# Load pre-cleaned data from TIP NGO article
# TODO: Add anonymized data to this repository?
phone <- c("R_6LtFazScXoe9Ghn", "R_1zGgLHVDJ7zjMsR", "R_1RXP2n3Mfo1G72t", "R_8J71M4uvcEtul6d", "R_0xD0ZL0EtiunkIl", "R_6z2ml48L9alMliR", "R_b8CWsdk9NQ92TDD", "R_ai7LwL15oKE1yLz", "R_9YTUYHjbnVsc8DP", "R_cZVA0PUYhCfdH2l", "R_ehuLVSh6mlPO9HT", "R_3Cc7TZ8Q7REb9k1", "R_3wVReMnO6uS4Lat", "R_56zsFOZcq2ceNwN")

linkedin <- c("R_daIbVJuXdccyJU1", "R_02iOgW9hAxLGdJr", "R_1XqAXcms7A52Okl", "R_cVfWt7ooTCsCee9", "R_0oerAAwD33YRIPP", "R_bOtLb37U4uoPgfH", "R_07FPaO59PTZRPoh", "R_5vFLG1eQl7ZwtHD", "R_5z2t8Pn0LHN6nDD", "R_0r0UnzXUP3Ot8Pz", "R_3vXTjhBAzkgoa6p", "R_3VQPZS31kueRf01", "R_ctOVf4JqajviXUV", "R_aWP2loRHMh5VhBj", "R_9Ak81i9i2Ovyvzv", "R_9QtABKriYqPtnbD", "R_eKe6KPNjJZQMF2R", "R_0CxnQvaHUAJ4PYN", "R_9TPGVrLaXwLiIe1", "R_3gDsycZfTJSTsXP", "R_cLW165CtBFvAmqx", "R_80q9YgXimZBurqZ", "R_8e8oOEOnHXOzxbv", "R_6sdNfHkg9h8wKMZ", "R_eD5Zd8oOxHEg29v", "R_a93q5ITARCY2rLT", "R_9NabGsD2zZhEzMp", "R_3e0IxpHaWRj1YC9", "R_6JrMcyw1meIaTQh")

responses.full <- readRDS("/Users/andrew/Research/••Projects/Human trafficking/Anti-TIP NGOs and the US/data/responses_full.rds") %>%
  mutate(survey.method = ifelse(survey.id %in% phone, "Phone", 
                                ifelse(survey.id %in% linkedin, "LinkedIn", 
                                       "Online")))

# Load funding data
funding.clean <- read_csv("data/funding_clean.csv")
recipient.regions <- c("Africa", "East Asia and Pacific Islands", "Europe", 
                       "Global", "Near East Asia", "South and Central Asia", 
                       "Western Hemisphere")


#' # Countries receiving most aid through IGOs
igo.countries <- funding.clean %>%
  filter(country != "USA") %>%
  filter(recipient_type == "IGO") %>%
  filter(!(country %in% recipient.regions)) %>%
  group_by(country) %>%
  summarise(total = sum(amount, na.rm=TRUE)) %>%
  arrange(desc(total)) %T>%
  {pandoc.table(head(., 10), big.mark=",")}

# Top 5 countries, formatted nicely
top.countries.igo <- igo.countries %>% 
  head(5) %>%
  do(as.data.frame(paste(.$country, collapse=", "))) %>%
  mutate_each(funs(gsub("(.*),", "\\1, and", .)))  # Add "and" before last element

#' ### Actual text
#' > The countries that have received most US aid through IGOs are `r top.countries.igo`.

# 

#' # US funding to NGOs
ngo.projects <- funding.clean %>%
  filter(country != "USA") %>%
  mutate(type_collapsed = ifelse(recipient_type %in% c("NGO", "NPO"),
                                 "NGO", recipient_type)) %>%
  filter(type_collapsed == "NGO")

#' Grants given just to NGOs:
(num.ngo.projects <- nrow(ngo.projects))

#' Number of countries receiving grants to NGOs:
(num.ngo.countries <- length(unique(ngo.projects$country)))

#+ results='asis'
#' Countries receiving the most grant money for NGOs:
countries.with.ngo.grants <- ngo.projects %>%
  group_by(country) %>%
  summarise(total = sum(amount, na.rm=TRUE)) %>%
  arrange(desc(total)) %T>%
  {pandoc.table(head(., 10), big.mark=",")}

# Top 5 countries, formatted nicely
top.countries <- countries.with.ngo.grants %>% 
  head(5) %>%
  mutate(country = ifelse(country == "Philippines", "the Philippines", country)) %>%
  do(as.data.frame(paste(.$country, collapse=", "))) %>%
  mutate_each(funs(gsub("(.*),", "\\1, and", .)))  # Add "and" before last element

#' Surveyed NGOs that received funding from the US:
survey.received.funding <- responses.full %>%
  group_by(survey.id) %>%
  summarise(received.funding = any(received.funding)) %T>%
  {print(sum(.$received.funding))}

#' ### Actual text
#' > Although the US funding is modest, between 2001-2014, the US funded `r num.ngo.projects` NGO projects on TIP in `r num.ngo.countries` of countries. The biggest recipient countries of NGO grants have been `r top.countries[1,1]`. In the survey of NGOs worldwide, `r sum(survey.received.funding$received.funding)` reported having received some sort of funding from the US.
#

#' # Stakeholders with whom NGOs discuss the TIP report

#' ### Updated Figure 3.4
cols <- c("Q3.21_1", "Q3.21_2", "Q3.21_3", "Q3.21_4")
labels <- c("Government", "Another government", "Other NGOs", "Other")
discussed <- separate.answers.summary(responses.full, cols, labels)

plot.data <- discussed$df %>% 
  mutate(Answer=factor(Answer, levels=rev(labels), ordered=TRUE))

fig.discussed <- ggplot(plot.data, aes(x=Answer, y=Responses)) +
  geom_bar(aes(y=plot.pct), stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
  coord_flip() + theme_clean()

filename <- "figures/fig_stakeholders_discussed"
ggsave(fig.discussed, filename=paste0(filename, ".pdf"), device=cairo_pdf)
ggsave(fig.discussed, filename=paste0(filename, ".png"))

#' This image is saved in Dropbox as ``r filename`.pdf` (and `.png` too).
fig.discussed


#' # Interactions with the US
#' Number of respondents *that don't work exclusively in the US*:
(num.respondents <- responses.full$survey.id %>% unique %>% length)

#' Proportion of organizations that were involved with the US somehow:
us.involvement <- responses.full %>%
  group_by(survey.id) %>%
  summarise(us.involvement = any(us.involvement)) %T>%
  {print(sum(.$us.involvement) / nrow(.))}

# Nice graph of involvement with the US
cols <- c("Q3.18_1", "Q3.18_2", "Q3.18_3", "Q3.18_4", "Q3.18_5", "Q3.18_6")
labels <- c("Direct contact (meetings)", "Direct cooperation", 
            "Our organization received funding", "Other", 
            "We have not had any contact or funding from the US", "Don't know")
involvement <- separate.answers.summary(responses.full, cols, labels)

plot.data <- involvement$df %>% 
  mutate(Answer=factor(Answer, levels=rev(labels), ordered=TRUE))

fig.involvement <- ggplot(plot.data, aes(x=Answer, y=Responses)) +
  geom_bar(aes(y=plot.pct), stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
  coord_flip() + theme_clean()

filename <- "figures/fig_involvement_with_us"
ggsave(fig.involvement, filename=paste0(filename, ".pdf"), device=cairo_pdf)
ggsave(fig.involvement, filename=paste0(filename, ".png"))


#' ### Actual text
#' > The global survey found that many NGOs interact extensively with the US embassy or government. More than two thirds of the `r num.respondents` respondents said they’d engaged in some form with the US government over the last 10-15 years. About half said they had had direct contact with US officials, and about a fifth reported some form of direct cooperation. Another fifth reported receiving direct funding from the US government to facilitate their work.

involvement$df %>%
  arrange(desc(Responses)) %>%
  rename(`Type of involvement` = Answer) %>%
  select(-plot.pct) %>%
  pandoc.table

#' This image is saved in Dropbox as ``r filename`.pdf` (and `.png` too).
fig.involvement


#' # Awareness of TIP report
heard.of.TIP <- responses.full %>%
  group_by(survey.id) %>%
  slice(1) %>%  # Select each organization's first country
  ungroup() %>%
  do(as.data.frame(table(.$Q2.5, dnn="Heard"))) %>%
  mutate(Percent = round(Freq / sum(Freq) * 100, 2))

#' ### Actual text
#' > One set of questions pertained to the awareness of the TIP report as a measure of the penetration of its message. If the US TIP report is effective in gaining attention around the world, then most TIP NGOs should have at least heard of the report. Indeed, the survey found that `r filter(heard.of.TIP, Heard == "Yes")$Percent`% of respondents had heard of the annual report.
#

#' # TIP use by government officials
#' Proportion of organization-countries that heard the report used by
#' government officials:
TIP.used <- responses.full %>%
  do(as.data.frame(table(.$Q3.23, dnn="TIP used"))) %>%
  mutate(Percent = round(Freq / sum(Freq) * 100, 2)) %T>%
  {pandoc.table(.)}

# Reasons:
# responses.full %>% select(Q3.24.Text) %>% filter(!is.na(Q3.24.Text)) %>% View
cols <- c("Q3.24_Negative.AgainstTIP", "Q3.24_Negative.General", 
          "Q3.24_OfficalCommentsOnTIP.Gov.Context.", 
          "Q3.24_UnofficialCommentsOnTIP", "Q3.24_NGOs.CSOs", 
          "Q3.24_AssessmentPurposes.General", "Q3.24_Policy", 
          "Q3.24_Conferences.Meetings", "Q3.24_Other", 
          "Q3.24_CommentMadeInOtherContext", "Q3.24_Efforts", 
          "Q3.24_Media", "Q3.24_Awareness", "Q3.24_Research")

labels <- c("Negative, against TIP", "Negative, general", 
            "Official comments on TIP, government context", 
            "Unofficial comments on TIP", "NGOs, CSOs", 
            "Assessment purposes, general", "Policy", 
            "Conferences, meetings", "Other", 
            "Comment made in other context", "Efforts", 
            "Media", "Awareness", "Research")

govt.mentions <- responses.full %>% select(survey.id, starts_with("Q3.24")) %>%
  # Convert values to numeric
  mutate_each(funs(as.numeric(levels(.))[.]), -c(survey.id, Q3.24.Text))

reasons.govt.mentions <- separate.answers.summary(govt.mentions, cols, labels)

plot.data <- reasons.govt.mentions$df %>% 
  arrange(desc(Responses)) %>%
  mutate(Answer=factor(Answer, levels=rev(Answer), ordered=TRUE))

fig.reasons <- ggplot(plot.data, aes(x=Answer, y=Responses)) +
  geom_bar(aes(y=plot.pct), stat="identity") + 
  labs(x=NULL, y=NULL) + 
  scale_y_continuous(labels = percent, 
                     breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
  coord_flip() + theme_clean()

filename <- "figures/fig_reasons_for_tip_mention"
ggsave(fig.reasons, filename=paste0(filename, ".pdf"), device=cairo_pdf)
ggsave(fig.reasons, filename=paste0(filename, ".png"))

#' ### Actual text
#' > To explore this, NGOs were asked if they had ever heard government officials mention the report either publically or in private and were then offered a write-in question about the connection. The results showed that `r filter(TIP.used, TIP.used == "Yes")$Percent`% of respondents had heard government officials refer to the report in any of the countries they work in. {TODO: STUFF ABOUT REASONS MENTIONED.}
#

#' Reasons the government mentioned the report: 
reasons.govt.mentions$df %>%
  arrange(desc(Responses)) %>%
  rename(`Reason for mention` = Answer) %>%
  select(-plot.pct) %>%
  pandoc.table

#' This image is saved in Dropbox as ``r filename`.pdf` (and `.png` too).
fig.reasons


#' # US embassy activity
active.embassies <- readRDS("/Users/andrew/Research/••Projects/Human trafficking/Anti-TIP NGOs and the US/data/active_embassies.rds")
most.active.clean <- readRDS("/Users/andrew/Research/••Projects/Human trafficking/Anti-TIP NGOs and the US/data/most_active_embassies.rds")

#' Which countries or embassies have been the *most* active?
most.active.clean %>% arrange(desc(total))
nrow(most.active.clean) - 1  # Subtract one because of "None"s

#' Over the last 10–15 years, has the United States or its embassy been active in the fight against human trafficking in X?
responses.full$Q3.8 %>% table %>% print %>% prop.table

plot.data <- active.embassies %>%
  arrange(num) %>% select(-country.raw) %>%
  filter(num > 10) %>%
  mutate(country = factor(country, levels=country, ordered=TRUE)) %>%
  arrange(desc(num)) %>%
  bind_rows(data_frame(num=0, country=c("All", "None"), 
                       prop=0, prop.nice="")) %>%
  arrange(num) %>%
  mutate(country = factor(country, levels=country, ordered=TRUE))

plot.data.active <- most.active.clean %>%
  filter(clean %in% plot.data$country) %>%
  mutate(country = factor(clean, levels=levels(plot.data$country), ordered=TRUE))

fig.active <- ggplot(plot.data, aes(x=country, y=num)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop.nice), size=3.5, hjust=1.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="Number of times country was mentioned as a partner in anti-TIP work") + 
  scale_y_continuous(breaks=seq(0, max(active.embassies$num), by=25), 
                     trans="reverse", expand = c(.1, .1)) + 
  coord_flip() + 
  theme_clean() + 
  theme(axis.text.y = element_blank(), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,0.5,1,1), "lines"))

fig.most.active <- ggplot(plot.data.active, aes(x=country, y=total)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = prop.nice), size=3.5, hjust=-0.3, 
            family="Source Sans Pro Light") + 
  labs(x=NULL, y="Number of times country was mentioned as the most active partner in anti-TIP work") + 
  scale_y_continuous(expand = c(.15, .15)) + 
  coord_flip() + 
  theme_clean() + 
  theme(axis.text.y = element_text(hjust=0.5), 
        axis.line.y = element_blank(),
        plot.margin = unit(c(1,1,1,0), "lines"))

fig.embassies <- arrangeGrob(fig.active, fig.most.active, nrow=1)
filename <- "figures/fig_embassies_mentioned"

#' Side-by-side graph of active countries + most active countries (this image is saved in Dropbox as ``r filename`.pdf` (and `.png` too)).

grid.draw(fig.embassies)
ggsave(fig.embassies, filename=paste0(filename, ".pdf"),
       width=5, height=2, units="in", device=cairo_pdf, scale=2.5)
ggsave(fig.embassies, filename=paste0(filename, ".png"),
       width=5, height=2, units="in", scale=2.5)

#' ### Actual text
#' > The rate at which the US embassy was mentioned as active was far greater than that of any other embassy mentioned. (TODO: There's a paragraph about these exact numbers in the article…). 
#

#' # US embassy importance
#' Raw counts, percents, and sum of "Most important" and "Somewhat important" percents:
responses.full$Q3.19 %>% table %>% print %>% 
  prop.table %T>% print %>% sum(.[1:2]) - 1

#' Percent of countries where at least one NGO said the US was important:
importance.by.country <- responses.full %>%
  group_by(work.country) %>%
  summarise(any.important = any(Q3.19 %in% c("Most important actor", 
                                             "Somewhat important actor"))) %>%
  ungroup() %T>%
  {print(sum(.$any.important) / nrow(.))}

#' ### Actual text
#' > Furthermore, nearly two-thirds of NGOs said that the US had played a very important or a somewhat important role in their country. If the responses are instead broken down by country, the share of countries in which at least one NGOs attributed an important or a somewhat important role was `r round(sum(importance.by.country$any.important) / nrow(importance.by.country) * 100, 2)`%. (TODO: fill in other way of breaking down this data by country and whether US funding recipients (lots of this kind of stuff is in the article)).
#


#' # US embassy positivity
embassy.positivity <- responses.full %>%
  group_by(Q3.25) %>% 
  summarise(num = n()) %>%
  na.omit() %>%
  mutate(prop = num / sum(num),
         prop.nice = sprintf("%.1f%%", prop * 100)) %T>%
  {pandoc.table(.)}

#' ### Actual text
#' > Finally, if respondents indicated that the US had played an important role in their countires, the survey asked about whether the influence of the US had been positive, negative or mixed. What was most astounding was the extremely low frequency of negative replies. The vast majority was positive (`r filter(embassy.positivity, Q3.25 == "Positive")$num`) and some were mixed (`r filter(embassy.positivity, Q3.25 == "Mixed")$num`), but only (`r filter(embassy.positivity, Q3.25 == "Negative")$num`) said the US had played a negative role. (TODO: fill in other way of breaking down this data, by country and whether US funding recipients (lots of this kind of stuff is in the article)).
#


#' # General survey details
num.loops <- responses.full %>%
  group_by(survey.id) %>%
  summarise(loops = n()) %>%
  do(data.frame(table(.$loops, dnn="Countries"))) %T>%
  {pandoc.table(.)}

survey.method <- responses.full %>%
  group_by(survey.id) %>%
  slice(1) %>%
  group_by(survey.method) %>%
  summarise(num = n()) %T>%
  {pandoc.table(.)}

#' ### Actual text
#' > Most organizations (`r filter(num.loops, Countries==1)$Freq`) chose to fill out the survey for just one country, the primary country of their advocacy work. The survey was assembled in Qualtrics and can be obtained in its entirety from the author (or is available in online appendix). To minimize frustration that might lead respondents to quit prematurely, they were free to skip any question and could move back and forth in the survey. The survey was administered via email, with the option for respondents to have a phone survey in lieu of answering it online or having a conversation in addition to the survey. Most responses (`r filter(survey.method, survey.method == "Online")$num`) were obtained directly in response to the email inquiry. `r filter(survey.method, survey.method == "Phone")$num` were conducted via phone. An invitation to participate in the survey was also posted to a LinkedIn discussion group used by anti-trafficking NGOs, which yielded `r filter(survey.method, survey.method == "LinkedIn")$num` complete responses. Each NGO in the database received two reminder emails, including respondents who started but did not finish the survey, and were provided with a link to resume their response. Additional efforts were made to reach non-responding NGOs by phone if we had very low participation from their countries. 
#
