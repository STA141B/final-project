
library(leaflet)
library(tidyverse)
library(rvest)
library(maps)
library(webglobe)
library(DT)
library(shinythemes)
library(plotly)
library(jsonlite)
library(httr)
library(lubridate)
library(htmltools)
library(htmlwidgets)
library(geojsonio)

setwd('C:/Users/96238/Google Drive/UCDavis/STA 141B/proj/Coronavirus_info')
source("functions.R")
world <- geojsonio::geojson_read("custom.geo.json", what = "sp")
list <- read_coro_data("https://www.worldometers.info/coronavirus/#countries", world)
data_Map <- list[[1]]
corona_data <- list[[2]]
summ <- list[[3]]

list2 <- read_daily_data()
confirm <- list2[[1]]
death <- list2[[2]]
recover <- list2[[3]]
state <- list2[[4]]
state_map <- list2[[5]]
