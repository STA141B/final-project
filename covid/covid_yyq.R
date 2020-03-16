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
                                                label = "Add another countries:",
                                                choices = unique(confirm$`Country/Region`),
                                                multiple = TRUE) 
                                   )
                             )),
                           
                           mainPanel(
                             
                               
                               tabsetPanel(
                             
                               tabPanel("Confirmed",
                                      plotlyOutput("confirmed_line"),
                                      plotlyOutput("confirmed_new")
                                      ),
                               tabPanel("Death",
                                      plotlyOutput("death_line"),
                                      plotlyOutput("death_new")
                                      ),
                               tabPanel("Recovered",
                                      plotlyOutput("recover_line"),
                                      plotlyOutput("recover_new")
                                      )
                           ),
                           
                           a(herf = "https://github.com/CSSEGISandData/COVID-19", "Data Source: https://github.com/CSSEGISandData/COVID-19")
                           
                            
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
          yaxis = list(title = "Cumulative Confirmed"),
          title = "Cumulative Number of Confirmed Cases"
        )
      
      
    })
  
  output$confirmed_new <- renderPlotly(
    {
      
      date_ct <- confirm %>%
        filter(`Country/Region` %in% input$country | `Country/Region` %in% input$countries) %>% 
        select(`Country/Region`, "1/22/20":ncol(confirm)) %>% 
        group_by(`Country/Region`) %>% 
        summarise_each(funs(sum)) %>% 
        pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>% 
        mutate(date = mdy(date)) %>%
        group_by(`Country/Region`) %>% 
        mutate(new = num - lag(num)) %>% 
        plot_ly(x = ~date, y = ~new) %>% 
        add_trace(color = ~`Country/Region`,
                  mode = 'lines+markers')  %>% 
        layout(
          xaxis = list(title = "Date"), 
          yaxis = list(title = "New Confirmed"),
          title = "Daily New Cases"
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
          yaxis = list(title = "Cumulative Deaths"),
          title = "Cumulative Number of Deaths"
        )
      
      
    })
  
  output$death_new <- renderPlotly(
    {
      
      date_ct <- death %>%
        filter(`Country/Region` %in% input$country | `Country/Region` %in% input$countries) %>% 
        select(`Country/Region`, "1/22/20":ncol(death)) %>% 
        group_by(`Country/Region`) %>% 
        summarise_each(funs(sum)) %>% 
        pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>% 
        mutate(date = mdy(date)) %>% 
        group_by(`Country/Region`) %>% 
        mutate(new = num - lag(num)) %>% 
        plot_ly(x = ~date, y = ~new) %>% 
        add_trace(color = ~`Country/Region`,
                  mode = 'lines+markers')  %>% 
        layout(
          xaxis = list(title = "Date"), 
          yaxis = list(title = "New Deaths"),
          title = "Daily New Deaths"
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
          yaxis = list(title = "Cumulative Recovered"),
          title = "Cumulative Number of Recovered cases"
        )
    }
  )
  
  output$recover_new <- renderPlotly(
    {
      
      date_ct <- recover %>%
        filter(`Country/Region` %in% input$country | `Country/Region` %in% input$countries) %>% 
        select(`Country/Region`, "1/22/20":ncol(recover)) %>% 
        group_by(`Country/Region`) %>% 
        summarise_each(funs(sum)) %>% 
        pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>% 
        mutate(date = mdy(date)) %>% 
        group_by(`Country/Region`) %>% 
        mutate(new = num - lag(num)) %>% 
        plot_ly(x = ~date, y = ~new) %>% 
        add_trace(color = ~`Country/Region`,
                  mode = 'lines+markers')  %>% 
        layout(
          xaxis = list(title = "Date"), 
          yaxis = list(title = "New Recovered"),
          title = "Daily New Recovered Cases"
        )
    }
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
