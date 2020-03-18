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
                        column(4, h3(htmlOutput("currentTime")),
                               h2(htmlOutput("today_info"))),
                        ),
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
                      titlePanel("Cumulative and New Confirmed Cases"),
                      br(),
                      fluidRow(
                        column(4, selectInput(inputId = "country",
                                              label = "Select Countries:",
                                              choices = sort(unique(confirm$`Country/Region`)),
                                              selected = "US",
                                              multiple = TRUE))),
                      mainPanel(width = 12,
                                br(),
                                tabsetPanel(tabPanel("Confirmed", icon = icon("meh"),
                                                     fluidRow(column(6, 
                                                                     br(),
                                                                     h4("Cumulative Confirmed Cases"),
                                                                     plotlyOutput("confirmed_line")),
                                                              column(6, 
                                                                     br(),
                                                                     h4("Daily New Confirmed Cases"),
                                                                     plotlyOutput("confirmed_new")))),
                                            tabPanel("Death", icon = icon("frown"),
                                                     fluidRow(column(6, 
                                                                     br(),
                                                                     h4("Cumulative Deaths"),
                                                                     plotlyOutput("death_line")),
                                                              column(6,  
                                                                     br(),
                                                                     h4("Daily New Deaths"),
                                                                     plotlyOutput("death_new")))),
                                            tabPanel("Recovered", icon = icon("angellist"),
                                                     fluidRow(column(6, 
                                                                     br(),
                                                                     h4("Cumulative Recovered Cases"),
                                                                     plotlyOutput("recover_line")),
                                                              column(6, 
                                                                     br(),
                                                                     h4("Daily New Recovered Cases"),
                                                                     plotlyOutput("recover_new")))),
                                            br(),
                                            a(herf = "https://github.com/CSSEGISandData/COVID-19",
                                              "Data Source: https://github.com/CSSEGISandData/COVID-19")))
             ),
             
             tabPanel("Province/State Data", icon = icon("location-arrow"),
                      titlePanel("Confirmed Cases and Deaths by Province/State"),
                      br(),
                      fluidRow(column(8, leafletOutput("pro_map")),
                               column(4,
                                      radioButtons(inputId = "province",
                                                   label = "Select a Country:",
                                                   choices = c("China", "US", "Canada"),
                                                   selected = "China"))),
                      a(herf = "https://github.com/CSSEGISandData/COVID-19",
                        "Data Source: https://github.com/CSSEGISandData/COVID-19")
             ),
             tabPanel("Breaking News", icon = icon("bolt"),
                      titlePanel("News & Live Updates"),
                      mainPanel(
                        br(),
                        selectInput(inputId = "country2",
                                            label = "Select Countries to view news:",
                                            choices = c("United State" = "us", "Canada" = "ca", 
                                                        "Australia" = "au", "United Kingdom" = "gb"),
                                            selected = "us",
                                            multiple = FALSE),
                                h6("* Published time is local time"),
                                htmlOutput("news_updates"),
                        br(),
                        a(herf = "https://newsapi.org/s/us-news-api",
                          "Source: https://newsapi.org/s/us-news-api"), width = 9)),
             navbarMenu("More Information", icon = icon("bullhorn"),
                        tabPanel("General Q&A", icon = icon("question-circle"),
                                 titlePanel("Frequently Asked Questions and Answers"),
                                 mainPanel(width = 9,
                                           br(),
                                           htmlOutput("QandA"),
                                           br(),
                                           br(),
                                           htmlOutput("link4")
                                           )),
                        "----",
                        tabPanel("U.S. Testing", icon = icon("flask"),#bolt flask
                                 titlePanel("Daily Testing by State around U.S."),
                                 mainPanel(offset = 4, width = 8,
                                           br(),
                                           tabsetPanel(tabPanel("Current U.S. Testing", icon = icon("flask"),
                                                                br(),
                                                                br(),
                                                                h4("Today's Testing Situation around U.S."),
                                                                plotlyOutput("US_map")), 
                                                       tabPanel("Daily State Testing", icon = icon("user-md"),
                                                                selectInput(inputId = "state",
                                                                            label = "Select a state:",
                                                                            choices = unique(state_map$state),
                                                                            selected = "CA", width = "20%"),
                                                                h4("Daily Testing Tracking by State (4PM Eastern)"),
                                                                plotlyOutput("state_line")
                                                                )
                                                       )
                                           )
                                 )
                        )
                        
             )
             
  )
)
