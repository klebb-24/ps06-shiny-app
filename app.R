library(shiny)
library(tidyverse)
library(ggplot2)

data <- read_delim("Rail_Equipment_Accident_Incident_Data.csv")

ui <- fluidPage(
  titlePanel("PS06-Assignment"),
  tabsetPanel(
    tabPanel(
      "General Info",
      h1("Railroad Accident & Incident Data"),
      p("Safety data related to Railway Equipment Failures and Accidents"),
      a("https://www.kaggle.com/datasets/chrico03/railroad-accident-and-incident-data"),
      p("Dataset published by the Federal Railroad Administration, Office of Railroad Safety; contains data on railway incidents from 1975 to 2022."),
      p("Includes data on:"),
      tags$ul(
        tags$li("Railway company involved"),
        tags$li("Hazmat cars involved"),
        tags$li("Number of people evacuated"),
        tags$li("Number of employees on-duty")
      ),
      textOutput("nrow"),
      textOutput("ncol"),
    ),
    
    tabPanel(
      "Weather and Visibility Trends",
      sidebarLayout(
        sidebarPanel(
          p("Observe the effect on weather on incidents."),
          radioButtons("weather", 
                       "Choose weather type to veiw",
                       choices = unique(data$`Weather Condition`),
                       selected = "Clear"), ## button for selecting weather
          radioButtons("vis_button",
                       "Display time of day accident occured?",
                       choices = c("Yes", "No"),
                       selected = "No") ## button for selecting time of day
        ),
        mainPanel(
          plotOutput("weather_graph"), ## graph output from user input
          textOutput("weatherobs") ## note about how many observations user is seeing
        ))
    ), 
    tabPanel(
      "Table Output",
      mainPanel(
        selectInput("num_rows", "Number of rows to display:", choices = c(10, 50, 100)),
        strong("Sample:"),
        tags$div(style="height: 600px; overflow-x: scroll; width: 1600px; overflow-y: scroll;", tableOutput("sample_table"))),
    )
  )
)


server <- function(input, output) {
  
  
  
  #calc the number of rows and columns and their output functions
  nrow <- nrow(data)
  output$nrow <- renderText(paste("The number of rows is:", nrow))
  ncol <- ncol(data)
  output$ncol <- renderText(paste("The number of columns is:", ncol))
  
  #get some data to display with output function
  sample_data <- data[sample(nrow(data), ncol(data)), ]
  output$sample_table <- renderTable(sample_data, options = list(scrollX = TRUE, scrollY = "300px"))  
  
  output$table <- renderTable({
    head(data, input$num_rows)
  })
  
  weathergraph <- reactive({
    data %>% 
      filter(`Weather Condition` %in% input$weather) %>% 
      filter(`Report Year` == 2022)
  })  ## weather crash graph 
  
  output$weather_graph <- renderPlot ({
    if(input$vis_button == "Yes")({
      ggplot(weathergraph(), (aes (y= `Accident Type`, fill = factor(Visibility))))+
        geom_histogram(stat = "count")
    })else ({
      ggplot(weathergraph(), (aes (y= `Accident Type`)))+
        geom_histogram(stat = "count")
    }) 
  }) ## time of day button for weather crash graph 
  
  output$weatherobs <- renderPrint({
    data %>% 
      filter(`Weather Condition` %in% input$weather) %>% 
      filter(`Report Year` == 2022) %>%  
      nrow() %>% 
      p("There were", . , "total collisions in the selected weather type.")
  }) ## code for text output of how many observations user is seeing in weather crash graph
}


shinyApp(ui = ui, server = server)