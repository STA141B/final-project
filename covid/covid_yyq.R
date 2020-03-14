library(shiny)
library(lubridate)
library(tidyverse)
library(plotly)

confirm <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
death <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
recover <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))

# Define UI for application that draws a histogram
ui <- navbarPage("Covid",
                 
                 #tabPanel("main"),
                 
                 tabPanel("History", 
                     
                           fluidRow(
                           column(2,
                           selectInput(inputId = "country", 
                                       label = "Select a Country:",
                                       choices = unique(confirm$`Country/Region`),
                                       selected = "US")
                                 ),
                           
                          
                           fluidRow(
                             column(4,
                                    selectInput(inputId = "countries", 
                                                label = "Select other Countries:",
                                                choices = unique(confirm$`Country/Region`),
                                                multiple = TRUE) 
                                   )
                             )),
                           
                           mainPanel(
                             
                             
                             
                            tabsetPanel(
                             
                               tabPanel("Confirmed",
                                      plotlyOutput("confirmed_line"),
                                      ),
                               tabPanel("Death",
                                      plotlyOutput("death_line")
                                      ),
                               tabPanel("Recovered",
                                      plotlyOutput("recover_line")
                                      )
                               
                           )
                           )
                         )        
                  )
                          
  
server <- function(input, output) {
  
  
  output$confirmed_line <- renderPlotly(
    {
      
      date_ct <- confirm %>%
        filter(`Country/Region` %in% input$country | `Country/Region` %in% input$countries) %>% 
        select(`Country/Region`, "1/22/20":ncol(confirm)) %>% 
        group_by(`Country/Region`) %>% 
        summarise_each(funs(sum)) %>% 
        pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>% 
        mutate(date = mdy(date)) %>% 
        plot_ly(x = ~date, y = ~num) %>% 
        add_trace(color = ~`Country/Region`,
                  mode = 'lines+markers')  %>% 
        layout(
          xaxis = list(title = "Date"), 
          yaxis = list(title = "Cumulative Confirmed")
        )
      
      
    })
  
  output$death_line <- renderPlotly(
    {
      
      date_ct <- death %>%
        filter(`Country/Region` %in% input$country | `Country/Region` %in% input$countries) %>% 
        select(`Country/Region`, "1/22/20":ncol(death)) %>% 
        group_by(`Country/Region`) %>% 
        summarise_each(funs(sum)) %>% 
        pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>% 
        mutate(date = mdy(date)) %>% 
        plot_ly(x = ~date, y = ~num) %>% 
        add_trace(color = ~`Country/Region`,
                  mode = 'lines+markers')  %>% 
        layout(
          xaxis = list(title = "Date"), 
          yaxis = list(title = "Cumulative Deaths")
        )
      
      
    })
  
  output$recover_line <- renderPlotly(
    {
      
      date_ct <- recover %>%
        filter(`Country/Region` %in% input$country | `Country/Region` %in% input$countries) %>% 
        select(`Country/Region`, "1/22/20":ncol(recover)) %>% 
        group_by(`Country/Region`) %>% 
        summarise_each(funs(sum)) %>% 
        pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>% 
        mutate(date = mdy(date)) %>% 
        plot_ly(x = ~date, y = ~num) %>% 
        add_trace(color = ~`Country/Region`,
                  mode = 'lines+markers')  %>% 
        layout(
          xaxis = list(title = "Date"), 
          yaxis = list(title = "Cumulative Recovered")
        )
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
