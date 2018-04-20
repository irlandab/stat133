# ===================================================================
# Title: Cleaning Data
# Description:
#   This script performs cleaning tasks and transformations on 
#   various columns of the raw data file.
# Input(s): data file 'raw-data.csv'
# Output(s): data file 'clean-data.csv'
# Author: Gaston Sanchez
# Date: 2-27-2018
# ===================================================================

# packages
library(readr)    # importing data
library(dplyr)    # data wrangling
library(ggplot2)  # graphics

nba2017_players <- read_csv(file = "../data/nba2017-players.csv")

# ===============================================
# Exporting some data tables to folder 'data/'
# ===============================================

warriors <- arrange(filter(nba2017_players, team == "GSW"), salary)

write.csv(warriors, file = '../data/warriors.csv', row.names = FALSE)

lakers <- filter(nba2017_players, team == "LAL") %>%
  arrange(desc(experience))

write_csv(lakers, path = '../data/lakers.csv')

# ===============================================
# Exporting some R output to folder 'output/'
# ===============================================

sink(file = '../output/summary-height-weight.txt')
summary(dat[ ,c('height', 'weight')])
sink()

sink(file = '../output/data-structure.txt')
str(nba2017_players)
sink()

sink(file = '../otuput/summary-warriors.txt')
summary(warriors)
sink()

sink(file = "../otuput/summary-lakers.txt")
summary(lakers)
sink()

