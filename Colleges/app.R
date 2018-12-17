
# Read in necessary libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(plotly)
library(shinythemes)
library(ggplot2)

# Read in necessary data and functions

Bar <- read_rds("Bar")
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

# Define UI for our navbar tab panel app setup
# Incorporate shiny theme and title
# Label our first tab panel

ui <- navbarPage(theme = shinytheme("slate"), 
                 "Exploring College Representation in NFL Draft",
                 
                 tabPanel("Map",
                          
  # Create fluid page to work with in our first tab panel
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
  # Input for the drop down menu
  # Title and choices for drop down menu
  selectInput("data", 
                  "Select Time Period",
                  choices = c("2009", "2010", "2011", "2012", "2013",
                              "2014", "2015",
                              "2016", "2017", 
                              "2018")
                  )
      ),
    
    # Display a map of the associated data, which was created in server
    mainPanel(
      plotlyOutput("map")
    )
  ))),
  
  # Insert our next navbar tab panel
  tabPanel("Top Programs",
           
  # Create a fluid page to work with
  # Only include a main panel this time, with the associated data for
  # plot output
           fluidPage(titlePanel(h3("Top 10 Programs")),
                       mainPanel(plotOutput(outputId = "Bar"))
                     )))


## server parameters

server <- function(input, output) {

  # Call on our data for the map and render through plotly  
  output$map <- renderPlotly({
  
  # Create new object that defines parameters for 
  #  each year's plot markers    
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
     
  # Create separate object for which we can create the base map
  # Define color, width, size, and format
  # Also define borders
    
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
  
  # Plot the points on the map
  # Attempt to add tracers around each point
  # Make sure points correspond to varying longitudes 
  # and latitudes
  # Define color and opacity so the overlap isn't invisible
  
  plot_geo(data = selected_data, locationmode = 'USA-states', sizes = c(50, 300),
           mode = "markers") %>% 
    add_trace(
      x = ~long, y = ~lat, size = ~n, color = 'rgb(17, 157, 255)', opacity = 0.6,
      color = 'rgb(231, 99, 250)', width = 2,
      
    # Create interactive hover feature
    # Include pop-up names for each point on each map
    # Define overall layout and attempt to define background color
    # Remember to set geo equal to g
  hoverinfo = "text",
      text = ~paste(selected_data$college_univ)
    ) %>%
    layout(title = "How College Representation in the NFL Draft Has Changed
           Over the Past 10 Years<br>(hover to toggle)", geo = g) %>%
    layout(plot_bgcolor = 'black')
  })
  
  # Render bar plot by calling on the correct data frame
  # Stacked bar chart by defining fill
  # Set scale so that the bars will stack corresponding to
  # chronological order
  # Use coord_flip so that the x-labels are visible
  # Use a theme of choice to display the graph
  
  output$Bar <- renderPlot({ggplot(new3, aes(x = college_univ, y = number_drafted, 
                                             fill = time_period)) +
      geom_col() + scale_fill_manual(values = c("red", "blue"),
                        labels = c("`2014-2018`", "`2009-2013`")) + 
      coord_flip() +
    theme_ipsum() + 
      labs(x = "College", y ="Number of Draft Picks",
           
  # Set titles, subtitles, and captions
           title = "Distribution of Draft Picks for the Top 10 Programs",
           subtitle = "Grouped by 5-year Periods",
           caption = "2009-2018") + 
      scale_colour_ipsum(guide = FALSE)})
}

## Run the App

shinyApp(ui = ui, server = server)
