#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


cournt<<-NULL
# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Data"),

   # Sidebar with a slider input for number of bins
   fluidPage(

     textInput("apiInput", label = h3("Input API here"), value = "Enter API..."),
     hr(),
     fluidRow(column(3,
                     verbatimTextOutput("value"))
     ),
     fluidRow(column(3,
                      tableOutput("datTable"))
     )
   )
)

# Define server logic required
server <- function(input, output) {

  output$value <- renderText({ input$apiInput })


  apiJSON <- reactive({

    ddd<<-input$apiInput
    read_json(input$apiInput)


  })


 members<- reactive({

   fff<<-apiJSON()
              b <- lapply(apiJSON(),function(x){
              as_tibble(as.data.frame(x$member))
              })

              b %>%  reduce(., full_join)

})


 output$datTable <- renderTable(members())








}

# Run the application
shinyApp(ui = ui, server = server)

