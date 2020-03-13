library(shiny)
library(lubridate)
library(dplyr)

confirm <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
death <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
recover <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))

# Define UI for application that draws a histogram
ui <- navbarPage("Covid",
                 
                 #tabPanel("main"),
                 
                 tabPanel("History", 
                          
                    tabsetPanel(
                      
                      tabPanel("Confirmed",
                    
                        fluidRow(
                           column(2,
                           selectInput(inputId = "confirmed_country", 
                                       label = "Country",
                                       choices = unique(confirm$`Country/Region`),
                                       selected = "US",
                                       multiple = T)
                                 )
                           
                           
                         
                           
                         ),
                         
                         mainPanel(
                           plotlyOutput("confirmed_line")
                         )
                    ),
                    
                    tabPanel("Deaths",
                             
                             fluidRow(
                               column(2,
                                      selectInput(inputId = "death_country", 
                                                  label = "Country",
                                                  choices = unique(death$`Country/Region`),
                                                  selected = "US",
                                                  multiple = T)
                               )
                               
                               
                               
                               
                             ),
                             
                             mainPanel(
                               plotlyOutput("death_line")
                             )
                    ),
                    
                    tabPanel("Recovered",
                             
                             fluidRow(
                               column(2,
                                      selectInput(inputId = "recover_country", 
                                                  label = "Country",
                                                  choices = unique(death$`Country/Region`),
                                                  selected = "US",
                                                  multiple = T)
                               )
                               
                               
                               
                               
                             ),
                             
                             mainPanel(
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
       filter(`Country/Region` %in% input$confirmed_country) %>% 
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
        filter(`Country/Region` %in% input$death_country) %>% 
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
        filter(`Country/Region` %in% input$recover_country) %>% 
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
