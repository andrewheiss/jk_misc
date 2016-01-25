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
# Included in all_tables.xlsx


# -----------
# Chapter 4
# -----------
# Table 4.1: Individual and country level drivers of concern for reputation
# Repeat of Table 1.1
# Included in all_tables.xlsx

# Table 4.2: Comparison of reaction frequencies, US TIP and US Human Rights Report
# Included in all_tables.xlsx

# Table 4.3: Comparison countries
# Included in all_tables.xlsx

# Table 4.4: Summary of statistical results
# Included in all_tables.xlsx


# -----------
# Chapter 5
# -----------
# Table 5.1: Summary of US influence (tally of tables 5.2 and 5.3)
# Included in all_tables.xlsx

# Table 5.2: Impact on legislation and implementation in the case studies
# Included in all_tables.xlsx

# Table 5.3: Impact on legislation and implementation in the case studies
# Included in all_tables.xlsx


# -----------
# Chapter 6
# -----------
# Table 6.1: Most prominent modifying factors and their presence in the case studies
# Included in all_tables.xlsx


# -----------
# Chapter 7
# -----------
# Table 7.1: Possible motivations or causal pathways: Aggregated evidence
# Included in all_tables.xlsx

# Table 7.2: Evidence of a relationship between scorecard diplomacy and outcomes
# Included in all_tables.xlsx
