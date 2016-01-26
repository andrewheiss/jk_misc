# Libraries
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(pander)
library(scales)

# Pandoc options
panderOptions('pandoc.binary', '/Users/andrew/.cabal/bin/pandoc')
panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', 50)
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

# Locations
base.folder <- "final_tables"


# -----------
# Chapter 1
# -----------
# Table 1.1: Individual and country level drivers of concern for reputation
# Included in all_tables.xlsx


# -----------
# Chapter 2
# -----------
# Table 2.x: Regression output for criminalization models
# TODO: Do this
# TODO: Write script to remove all rows with a colspan + add notes row?
# Or
# /Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to odt --infilter="HTML" testing.html
# /Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to odt --infilter="HTML (StarWriter)" testing.html


# -----------
# Chapter 3
# -----------
# Table 3.x: Regression output for determinants of media coverage
# TODO: Do this


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

# Table 4.x: Regression output for determinants of documented reaction models
# TODO: Do this


# -----------
# Chapter 5
# -----------
# Table 5.1: Summary of US influence (tally of tables 5.2 and 5.3)
# Included in all_tables.xlsx

# Table 5.2: Impact on legislation and implementation in the case studies
# Included in all_tables.xlsx

# Table 5.3: Impact on legislation and implementation in the case studies
# Included in all_tables.xlsx

# Table 5.x: Regression output for determinants of effect of reactions on criminalization
# TODO: Do this


# -----------
# Chapter 6
# -----------
# Table 6.1: Most prominent modifying factors and their presence in the case studies
# Included in all_tables.xlsx

# Table 6.x: Regression output for interaction models
# Basic model + interaction with aid + interaction with democracy
# Since the rest have no effect...
# TODO: Do this


# -----------
# Chapter 7
# -----------
# Table 7.1: Possible motivations or causal pathways: Aggregated evidence
# Included in all_tables.xlsx

# Table 7.2: Evidence of a relationship between scorecard diplomacy and outcomes
# Included in all_tables.xlsx
