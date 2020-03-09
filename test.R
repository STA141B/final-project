library(shiny)
library(jsonlite)
library(dplyr)

# Define UI for application that draws a histogram
ui <- navbarPage("Title",
                 
                 #tabPanel("main"),
                 
                 tabPanel("Restaurants", 
                          
                          
                          fluidRow(
                            column(2,
                            selectInput("price", "Price",
                                          c("$","$$", "$$$"), 
                                        selected = c("$","$$", "$$$"),
                                        multiple = T)
                            ),
                            column(2,
                            selectInput("category", "Category",
                                          c("a","b","c","d"), 
                                        selected = c("a","b","c","d"),
                                        multiple = T)),
                            column(6,
                            selectInput("pick", "Transactions",
                                        c("delivery", "pickup", "restaurant_reservation"),
                                        selected = c("delivery", "pickup", "restaurant_reservation"),
                                        multiple = T)),
                            column(2,
                            selectInput("open", "Open",
                                        c("Yes", "No"))),
                          
                            column(5,
                            dataTableOutput("data")
                          )),
                          
                        
                         mainPanel(
                           
                         )    
                          )
                          
                          
                          )

                 
                
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$data <- DT::renderDataTable(
    {
      testdata <- fromJSON("test_data.json", flatten = T)
      
      #testdata <- read.table("test_data.txt")
      
      select_price <- input$price
      select_cat <- input$category
      
      
      testdata$businesses %>%
        select(name, price, rating) %>% 
        filter(price == select_price)
      
    
    }, 
    options = list(pageLength = 10)
      
    )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
