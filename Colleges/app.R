

library(shiny)
library(tidyverse)
library(tidytext)
library(plotly)
library(shinythemes)

map <- read_rds("map")
high_2009 <- read_rds("high_2009")
high_2010 <- read_rds("high_2010")
high_2011 <- read_rds("high_2011")
high_2012 <- read_rds("high_2012")
high_2013 <- read_rds("high_2013")
high_2014 <- read_rds("high_2014")
high_2015 <- read_rds("high_2015")
high_2016 <- read_rds("high_2016")
high_2017 <- read_rds("high_2017")
high_2018 <- read_rds("high_2018")

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("slate"), 
                 "Exploring College Representation in NFL Draft",
                 
                 tabPanel("Map",
                          
  
  fluidPage(
  
  # Application title
  titlePanel(h3("NFL Draftee Representation Sorted by College: 2009-2018")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("top_n",
                  "Select Number of Most Represented Colleges",
                  min = 1,
                  max = 30,
                  value = 10),
      selectInput("data", 
                  "Select Time Period",
                  choices = c("2009", "2010", "2011", "2012", "2013",
                              "2014", "2015",
                              "2016", "2017", 
                              "2018")
                  )
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("geographical visualization"),
      plotlyOutput("map")
    )
  ))),
  
  tabPanel("Top Programs",
           fluidPage())
  
  ))


## server stuff

server <- function(input, output) {
  
  output$map <- renderPlotly({
    
  selected_data <- switch(input$data,
                          "2009" = high_2009 %>% slice(1:input$top_n),
                          "2010" = high_2010 %>% slice(1:input$top_n),
                          "2011" = high_2011 %>% slice(1:input$top_n), 
  "2012" = high_2012 %>% slice(1:input$top_n),
  "2013" = high_2013 %>% slice(1:input$top_n), 
"2014" = high_2014 %>% slice(1:input$top_n), 
"2015" = high_2015 %>% slice(1:input$top_n), 
"2016" = high_2016 %>% slice(1:input$top_n), 
  "2017" = high_2017 %>% slice(1:input$top_n), 
  "2018" = high_2018 %>% slice(1:input$top_n))
     
    
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray90"),
    subunitwidth = 1,
    countrywidth = 1,
    subunitcolor =toRGB("white"),
    countrycolor = toRGB("white")
  )
  
  plot_geo(data = selected_data, locationmode = 'USA-states', sizes = c(50, 300),
           mode = "markers") %>% 
    add_trace(
      x = ~long, y = ~lat, size = ~n, color = 'rgb(17, 157, 255)', opacity = 0.6,
      color = 'rgb(231, 99, 250)', width = 2,
  hoverinfo = "text",
      text = ~paste(selected_data$college_univ)
    ) %>%
    layout(title = "How College Representation in the NFL Draft Has Changed
           Over the Past 10 Years<br>(hover to toggle)", geo = g) %>%
    layout(plot_bgcolor = 'black')
    
  })  
}

## Run the App

shinyApp(ui = ui, server = server)
