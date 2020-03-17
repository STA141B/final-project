library(shiny)
library(lubridate)
library(tidyverse)
library(plotly)
library(jsonlite)
library(httr)


confirm <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
death <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
recover <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))

r <- GET(
  "https://covidtracking.com/api/states",
)
stop_for_status(r)
json <- content(r, as = "text")
state_current <- fromJSON(json)

r <- GET(
  "https://covidtracking.com/api/states/daily",
)
stop_for_status(r)
json <- content(r, as = "text")
state <- fromJSON(json) %>% 
  mutate(date = ymd(date))



state_map <- state_current %>% 
  filter(state %in% datasets::state.abb)

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
                                        fluidRow(
                                          column(6,
                                                 plotlyOutput("confirmed_line")),
                                          column(6, 
                                                 plotlyOutput("confirmed_new")),
                                        )
                                     ),
                               tabPanel("Death",
                                        
                                        fluidRow(
                                          column(6,
                                                 plotlyOutput("death_line")),
                                          column(6, 
                                                 plotlyOutput("death_new")),
                                        )
                                        
                                     # plotlyOutput("death_line"),
                                     # plotlyOutput("death_new")
                                      ),
                               tabPanel("Recovered",
                                        
                                        fluidRow(
                                          column(6,
                                                 plotlyOutput("recover_line")),
                                          column(6, 
                                                 plotlyOutput("recover_new")),
                                        )
                                     # plotlyOutput("recover_line"),
                                     # plotlyOutput("recover_new")
                                      )
                           ),
                           
                           a(herf = "https://github.com/CSSEGISandData/COVID-19", "Data Source: https://github.com/CSSEGISandData/COVID-19")
                           
                            
                           )
                         ),
                 
                 tabPanel("Testing across the U.S.",
                          
                          fluidRow(
                            column(2,
                                   selectInput(inputId = "state", 
                                               label = "Select a state:",
                                               choices = unique(state_map$state),
                                               selected = "CA")
                            ),
                          
                          mainPanel(
                            
                            plotlyOutput("US_map"),
                            plotlyOutput("state_line")
                          )
                          ))
                  )
                          
  
server <- function(input, output) {
  
  output$US_map <- renderPlotly(
    {
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        lakecolor = toRGB('white')
      )
      
      map_us <- plot_geo() %>%
        add_trace(
          z = ~state_map$total,
          color = ~state_map$total,
          colors = "Reds",
          text = state_map$state, 
          hoverinfo = "text",
          hovertext = paste("<b>", state_map$state, "</b>",
                            "<br>Positive: ", state_map$positive,
                            "<br>Negative:", state_map$negative,
                            "<br>Pending: ", state_map$pending,
                            "<br>Total: ", state_map$total,
                            "<br>LastUpdateEt: ", state_map$lastUpdateEt),
          #span = I(0),
          locations = state.abb, 
          locationmode = 'USA-states'
        ) %>%
        colorbar(title = "Total number of <br> tests by state") %>% 
        layout(geo = g, 
               title = "Current Testing for COVID-19 by State")
      
      map_us  %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selecting")
    }
  )

  
  output$state_line <- renderPlotly(
    {
      
      state %>% 
        filter(state == input$state) %>% 
        plot_ly(x = ~date, y = ~positive, type = 'bar', name = "positive") %>% 
        add_trace(y = ~negative, type = 'bar', name = "negative") %>% 
        add_trace(y = ~pending, type = 'bar', name = "pending") %>%
        layout(barmode = 'stack',
               yaxis = list(title = 'Count'),
               title = paste("Daily Testing Tracking for", input$state, "(4pm Eastern)"))
       
    }
  )
  
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
                  type = 'scatter',mode = 'lines+markers')  %>% 
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
                  type = 'scatter',mode = 'lines+markers')  %>% 
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
                  type = 'scatter',mode = 'lines+markers')  %>% 
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
                  type = 'scatter',mode = 'lines+markers')  %>% 
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
                  type = 'scatter',mode = 'lines+markers')  %>% 
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
                  type = 'scatter',mode = 'lines+markers')  %>% 
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
