# Title: "Lab 12: Getting Started with Web Scraping"
# Subtitle: "Stat 133, Spring 2018"
# Author: "Irlanda ayon-Moreno"


library(XML)
library(xml2)
library(rvest)
library(stringr)
library(magrittr)


basket <- "https://www.basketball-reference.com"
gsw <- "/teams/GSW/2017.html"
gsw_url <- paste0(basket, gsw)

download.file(gsw_url, 'gsw-roster-2017.html')

gsw_roster <- readHTMLTable('gsw-roster-2017.html')


nba_html <- paste0(basket, "/leagues/NBA_2017.html")

xml_doc <- read_html(nba_html)


xml_tables <- xml_doc %>%
  html_nodes("table") %>%
  extract(1:2)

hrefs <- xml_tables %>% 
  html_nodes("a") %>%
  html_attr("href")


teams <- str_sub(hrefs, 8, 10)

files <- paste0(teams, '-roster-2017.csv')

team_url <- paste0(basket, hrefs[1])

roster <- read_html(team_url) %>%
  html_nodes("table") %>%
  html_table()

write.csv(roster[[1]], files[1])


for (i in 1:length(teams)) {
  team_url <- paste0(basket, hrefs[i])
  roster <- read_html(team_url) %>%
    html_nodes("table") %>%
    html_table()
  write.csv(roster[[1]], files[i])
}

