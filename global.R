
library(leaflet)
library(tidyverse)
library(rvest)
library(DT)
library(shinythemes)
library(plotly)
library(jsonlite)
library(httr)
library(lubridate)
library(htmltools)
library(htmlwidgets)
library(geojsonio)
library(furrr)
library(shiny)
library(shinybusy)

#setwd('C:/Users/98455/Desktop/winter_SY/STA141B/Final Project')
#source("functions.R")
world <- geojson_read("custom.geo.json", what = "sp")
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


list3 <- c("us", "ca", "au", "gb")
news <- list()
for(i in 1:length(list3)){
  temp <- read_news(list3[i])
  news[[list3[i]]] <- apply(temp, 1, row_new_html)
}

QA <- as.character(
  tags$body(
    div(
      tags$h4(
        tags$strong("Q: Why is the disease being called coronavirus disease 2019, COVID-19?")),
      tags$h5("A: On February 11, 2020 the World Health Organization announced an official name for the disease 
            that is causing the 2019 novel coronavirus outbreak, first identified in Wuhan China. The new name 
            of this disease is coronavirus disease 2019, abbreviated as COVID-19. In COVID-19, 'CO' stands for 
            'corona,' 'VI' for 'virus,' and 'D' for disease. Formerly, this disease was referred to 
            as \"2019 novel coronavirus\" or \"2019-nCoV\".")
    ),
    tags$br(),
    div(
      tags$h4(
        tags$strong("Q: How It Spreads?")),
      tags$h5("A: The virus that causes COVID-19 seems to be spreading easily and sustainably in the community (\"community spread\") in ",
              tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/prepare/transmission.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fabout%2Ftransmission.html",
                     "some affected geographic areas."),
              " Community spread means people have been infected with the virus in an area, including some who are not sure how or where they became infected.")
    ),
    tags$br(),
    div(
      tags$h4(
        tags$strong("Q: How can I protect myself?")),
      tags$h5("A: Visit the ",
              tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/prepare/prevention.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fabout%2Fprevention.html",
                     " COVID-19 Prevention and Treatment"),
              " page to learn about how to protect yourself from respiratory illnesses, like COVID-19.")
    ),
    tags$br(),
    div(
      tags$h4(
        tags$strong("Q: What are the symptoms and complications that COVID-19 can cause?")),
      tags$h5("A: Current symptoms reported for patients with COVID-19 have included mild to severe respiratory illness with fever1, cough, and difficulty breathing. ",
              tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fabout%2Fsymptoms.html",
                     "Read about COVID-19 Symptoms."))
    )  
  )
         
)

linkone <- as.character(
  tags$h5(tags$a(href = "https://www.worldometers.info/coronavirus/#countries",
                 "Data Source: https://www.worldometers.info/coronavirus/#countries")
          )
  )

linktwo <- as.character(
  tags$h5(tags$a(href = "https://github.com/CSSEGISandData/COVID-19",
                 "Data Source: https://github.com/CSSEGISandData/COVID-19")
          )
  )


linkthree <- as.character(
  tags$h5(tags$a(href = "https://newsapi.org/s/us-news-api",
                 "Source: https://newsapi.org/s/us-news-api")
          )
  )

linkfour <- as.character(
  tags$h5(tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/faq.html#basics",
                 "CDC Source: https://www.cdc.gov/coronavirus/2019-ncov/faq.html#basics")
          )
  )


linkfive <- as.character(
  tags$h5(tags$a(href = "https://covidtracking.com/",
                 "Data Source: https://covidtracking.com/")
          )
  )

linksix <- as.character(
  tags$h5(tags$a(href = "https://www.worldometers.info/coronavirus/#countries",
                 "Data Source: https://www.worldometers.info/coronavirus/#countries")
  )
)

linkseven <- as.character(
  tags$h5(tags$a(href = "https://github.com/CSSEGISandData/COVID-19",
                 "Data Source: https://github.com/CSSEGISandData/COVID-19")
  )
)

linkeight <- as.character(
  tags$h5(tags$a(href = "https://news.wisc.edu/uw-madison-researchers-lead-efforts-to-understand-thwart-new-coronavirus/",
                 "Image Source: https://news.wisc.edu/uw-madison-researchers-lead-efforts-to-understand-thwart-new-coronavirus/")
  )
)





