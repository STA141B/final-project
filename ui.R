#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(rvest)
library(maps)
library(webglobe)
library(DT)
library(shinythemes)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  navbarPage("Coronavirus COVID-19 Information", theme = shinytheme("slate"),
             tabPanel("World Trend", fluid = TRUE, icon = icon("globe"),
                      titlePanel("Culmulative Confirmed Cases and Deaths"),
                      br(),
                      fluidRow(
                        column(8, leafletOutput("map")),
                        br(),
                        br(),
                        column(4, h2(htmlOutput("today_info")))),
                      br(),
                      a(herf = "https://www.worldometers.info/coronavirus/#countries", "Data Source: https://www.worldometers.info/coronavirus/#countries")
                      ),
             tabPanel("Detailed Situation", fluid = TRUE, icon = icon("table"),
                      titlePanel("Confirmed Cases and Deaths by Country/Region"),
                      mainPanel(
                        br(),
                        DTOutput("tbl"),
                        br(),
                        a(herf = "https://www.worldometers.info/coronavirus/#countries", "Data Source: https://www.worldometers.info/coronavirus/#countries"))
                      ),
             tabPanel("Daily Report", icon = icon("chart-line"),
                      titlePanel("Culmulative and Daily New Confirmed"),
                      br(),
                      fluidRow(
                        column(2, selectInput(inputId = "country",
                                              label = "Select a Country:",
                                              choices = unique(confirm$`Country/Region`),
                                              selected = "US")),
                        fluidRow(column(4, selectInput(inputId = "countries",
                                                       label = "Add more Countries:",
                                                       choices = unique(confirm$`Country/Region`),
                                                       multiple = TRUE)))),
                      mainPanel(width = 12,
                                br(),
                                tabsetPanel(tabPanel("Confirmed",
                                                     fluidRow(column(6, plotlyOutput("confirmed_line")),
                                                              column(6, plotlyOutput("confirmed_new")))),
                                            tabPanel("Death",
                                                     fluidRow(column(6, plotlyOutput("death_line")),
                                                              column(6, plotlyOutput("death_new")))),
                                            tabPanel("Recovered",
                                                     fluidRow(column(6, plotlyOutput("recover_line")),
                                                              column(6, plotlyOutput("recover_new")))),
                                            br(),
                                            a(herf = "https://github.com/CSSEGISandData/COVID-19",
                                              "Data Source: https://github.com/CSSEGISandData/COVID-19")))
                      ),
             tabPanel("U.S. Testing", icon = icon("user-md"),
                      titlePanel("Daily Testing by State"),
                      br(),
                        mainPanel(offset = 4, width = 8,
                                  br(),
                                  tabsetPanel(tabPanel("Current US Testing",
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       plotlyOutput("US_map")),
                                              tabPanel("Daily State Testing",
                                                       selectInput(inputId = "state",
                                                                   label = "Select a state:",
                                                                   choices = unique(state_map$state),
                                                                   selected = "CA"),
                                                       plotlyOutput("state_line")
                                                       )))),
             tabPanel("Province/State Data", icon = icon("location-arrow"),
                      titlePanel("Confirmed and Deaths by Province/State"),
                      br(),
                      fluidRow(column(8, leafletOutput("pro_map")),
                               column(4,
                                      radioButtons(inputId = "province",
                                                   label = "Select a Country:",
                                                   choices = c("China", "US", "Canada"),
                                                   selected = "China"))),
                      a(herf = "https://github.com/CSSEGISandData/COVID-19",
                        "Data Source: https://github.com/CSSEGISandData/COVID-19")
                      )
             )
  )
)
