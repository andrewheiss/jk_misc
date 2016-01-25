# Results tables for chapter 2 and 4 - criminalization (2), determinants of documented reaction (4)
# 5 too - effect of reactions on criminalization
# 3 = determinants of media coverage
# 6 = interactions - basic model + interaction with aid (not there), with democracy (there), rest just say nothing there, but don't show tables
# Chapter 6 pred probs to appendix

library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(pander)
library(scales)
panderOptions('pandoc.binary', '/Users/andrew/.cabal/bin/pandoc')
panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', 50)
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

# Locations
base.folder <- "final_tables"
all.tables <- file.path(base.folder, "all_tables.xlsx")


# -----------
# Chapter 1
# -----------
# Table 1.1: Individual and country level drivers of concern for reputation 
df.table1.1 <- read_excel(all.tables, sheet="Table 1.1", col_types=NULL) %>%
  mutate_each(funs(gsub("\r\n", "\\\\ \n", .))) %>%  # Deal with line breaks
  set_colnames(gsub("BLANK", " ", colnames(.)))  # Rename blank columns

filename = "table1_1_reputation_concern"
caption = "Table 1.1: Individual- and country-level drivers of concern for reputation"
table.clean <- pandoc.table.return(df.table1.1, caption=caption)
cat(table.clean, file=file.path(base.folder, paste0(filename, ".md")))


# -----------
# Chapter 4
# -----------
# Table 4.1: Individual and country level drivers of concern for reputation
filename = "table4_1_reputation_concern"
caption = "Table 4.1: Individual- and country-level drivers of concern for reputation"
table.clean <- pandoc.table.return(df.table1.1, caption=caption)
cat(table.clean, file=file.path(base.folder, paste0(filename, ".md")))

# Table 4.2: Comparison of reaction frequencies, US TIP and US Human Rights Report
df.table4.5 <- read_excel(all.tables, sheet="Table 4.5") %>%
  mutate(`Total reports 2001–2009` = comma(`Total reports 2001–2009`),
         `Percent of reports with documented reaction about TIP` = 
           percent(`Percent of reports with documented reaction about TIP`))

filename = "table4_2_reactions_tip_hr"
caption = "Table 4.2: Comparison of reaction frequencies, US TIP and US Human Rights Report"
table.clean <- pandoc.table.return(df.table4.5, caption=caption)
cat(table.clean, file=file.path(base.folder, paste0(filename, ".md")))

# Table 4.3: Comparison countries
df.table4.3 <- read_excel(all.tables, sheet="Table 4.3", col_types=NULL)

filename = "table4_3_comparison_countries"
caption = "Table 4.3: Comparison countries"
table.clean <- pandoc.table.return(df.table4.3, caption=caption)
cat(table.clean, file=file.path(base.folder, paste0(filename, ".md")))

# Table 4.4: Summary of statistical results
df.table4.4 <- read_excel(all.tables, sheet="Table 4.4", col_types=NULL) %>%
  set_colnames(gsub("BLANK", " ", colnames(.)))  # Rename blank columns

filename = "table4_4_stats_summary_results"
caption = "Table 4.4: Summary of statistical analysis results"
table.clean <- pandoc.table.return(df.table4.4, caption=caption)
cat(table.clean, file=file.path(base.folder, paste0(filename, ".md")))


# -----------
# Chapter 5
# -----------
# Table 5.1: Summary of US influence (tally of tables 5.2 and 5.3)
# TODO: Clean this up
# TODO: Add heatmap shading

# Table 5.2: Impact on legislation and implementation in the case studies
# TODO: Clean this up?

# Table 5.3: Impact on legislation and implementation in the case studies
# TODO: Clean this up?


# -----------
# Chapter 6
# -----------
# Table 6.1: Most prominent modifying factors and their presence in the case studies
# TODO: Clean this up


# -----------
# Chapter 7
# -----------
# Table 7.1: Possible motivations or causal pathways: Aggregated evidence
# TODO: Clean this up

# Table 7.2: Evidence of a relationship between scorecard diplomacy and outcomes
# TODO: Clean this up
