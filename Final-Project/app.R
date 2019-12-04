#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Turned off the use of scientific notation.

options(scipen = 999)

# Loaded in all the libraries the app will need to run.

library(shiny)
library(shinyWidgets)
library(fs)
library(sf)
library(shinythemes)
library(moderndive)
library(gt)
library(tidymodels)
library(broom)
library(gifski)
library(gganimate)
library(png)
library(ggthemes)
library(tidyverse)

# Read in the Zillow cities data.

cities <- read_csv("cities.csv")

# Created a vector of the city names that will be used in the pickerInput.

citiesVector <- as.vector(cities$RegionName)

# Read in the Zillow neighborhood data.

neighborhoods <- read_csv("neighborhoods.csv")

# Created a vector of neighborhood names that will be used in the pickerInput.

neighborhoodsVector <- as.vector(neighborhoods$RegionName)

# Oregon county level median single-family home price data

# countiesOregon_map <- st_read("counties_map_or.shp")

# Mapped permits data for Oregon

# permitsOregon_map <- st_read("permits_map_or.shp")

zillowOR_county <- read_csv("zillow-or.csv")

permitsOR_county <- read_csv("permits-or.csv")

# Define UI for application.
# Set theme 
# Titled it United States Housing Market

ui <- navbarPage("United States Housing Market", theme = shinytheme("sandstone"),
                 
                 # Created multiple tabs in the app
                 # First one is titled National Overview
                 
                 tabPanel("National Overview", 
                          
                          # Tab title
                          
                          titlePanel("Current National Overview"),
                          
                          # Sidebar with a pickerInput to select the cities to display in the graph.
                          
                          sidebarLayout(
                            sidebarPanel(
                              pickerInput("selectCities",
                                          "Select Cities",
                                          choices = citiesVector,
                                          options = list("actions-box" = TRUE),
                                          multiple = TRUE,
                                          selected = citiesVector
                              )
                            ),
                            
                            # Main panel
                            # Created two tabs, one for the plot and one for the model.
                            
                            mainPanel(
                              tabsetPanel(id = "tabsMain",
                                          tabPanel("Plot",
                                                   plotOutput("plot1")
                                          ),
                                          tabPanel("Model",
                                                   gt_output("model")
                                          )
                              )
                            )
                          )
                 ),
                 
                 # Second tab
                 # Oregon state level data
                 
                 tabPanel("Oregon",
                          titlePanel("State Level Data"),
                          tabsetPanel(
                            id = "tabsOregon",
                            tabPanel("Home Value",
                                     plotOutput("plot4")
                                       ),
                                     
                          
                            tabPanel("Building Permits",
                                     
                                         plotOutput("plot3"))
                                     
                            
                          )
                 ),
                 
                 # Third tab
                 # Titled Portland, OR
                 
                 tabPanel("Portland",
                          titlePanel("Neighborhood Trends Over Time in Portland, Oregon"),
                          tabsetPanel(
                            id = "tabsPortland",
                            tabPanel(
                              "Animation",
                               plotOutput("animation")
                            ),
                            tabPanel(
                              "Plot",
                              sidebarLayout(
                                sidebarPanel(
                                  pickerInput("selectNeighborhoods",
                                              "Select Neighborhoods",
                                              choices = neighborhoodsVector,
                                              options = list("actions-box" = TRUE),
                                              multiple = TRUE,
                                              selected = neighborhoodsVector[952]
                                  )),
                                mainPanel(
                                  plotOutput("plot2")
                                )
                              )
                            )
                          )
                 ),
                 
                 # About panel.
                 
                 tabPanel("About",
                          titlePanel("About"),
                          
                          br(),
                          
                          p(paste("My aim, in the final project, is to examine the national housing market with a special focus on the state of Oregon and its largest city, Portland. During the summer after my sophomore year, I spent time working with a company that builds accessory dwelling units in Portland. One of the aims of the company was to help solve the housing stock shortage within the city. As a city in high demand and zoned primarily for single family homes, Portland, like many large American cities, is facing a housing crisis. My hope for this project is to use available data to learn more about how the housing market both in the state as a whole and in Portland have changed over recent years and how that has affected the affordability and accessibility of housing in the metropolitan area.")),
                          
                          br(),
                          
                          p(paste("The data I am planning on using for the project is from two main sources. I have data from Zillow to show national trends in urban and suburban housing and rental markets. I also have annual data from the Census Bureau in conjunction with the Department of Housing and Urban Development, that gives me more localized data on the frequency with which building permits are issued in the Oregon housing market. This data allows me to analyze both the prices/costs as well as the types of housing that make up the state market and how they have changed over the past several decades.")),
                          
                          br(),
                          
                          p(paste("I, Pieter Quinton, am a senior at Harvard College studying Government with a secondary focus in Economics. You can access the source code for the project at https://github.com/PGQuinton/gov1005-final-project.")),
                          
                          br()
                          
                 )
)


# Define server logic required to draw plots and models.

server <- function(input, output) {
  
  # First plot.
  # Simple ggplot.
  # Subset the data to just include the cities selected in the pickerInput using a filter funciton.
  
  output$plot1 <- renderPlot({
    
    plot1 <- cities %>%
      filter(RegionName %in% input$selectCities) %>%
      ggplot(aes(x=SizeRank, y=X2019.08)) +
      geom_point(alpha = 0.75) +
      scale_y_log10(lim = c(50000, 1000000)) + 
      scale_x_reverse(lim =  c(805, 0)) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Relationship Between Population and Median House Price",
           subtitle = "Data from the 804 largest cities in the United States as of August, 2019",
           y = "Median House Price ($)",
           x = "Population Ranked",
           caption = "As the plot shows, there is a positive relationship between population and median house price.\nThe scale on the x-axis has been flipped so the most populous cities are on the right and the least populous cities are on the left. \nThe scale on the y-axis has been logged so as to better illustrate the relationship between the two variables. \nSource: Zillow") +
      theme(
        plot.caption = element_text(hjust = 0)
      )
    plot1
    
  })
  
  # Model
  # Used render_gt() so my gt table shows up.
  # Again, subset the data to just include the cities selected.
  
  output$model <- render_gt({
    
    model_data <- cities %>%
      filter(RegionName %in% input$selectCities)
    
    
    
    if(nrow(model_data) > 2){
      
      model <- tidy(lm(X2019.08 ~ rowname, data = model_data), conf.int = TRUE, conf.level = .90)
      
      model %>%
        select(term, estimate, conf.low, conf.high) %>%
        mutate(term = c("Intercept", "Population Ranked")) %>%
        gt() %>%
        cols_label(
          term = "",
          estimate = "Coefficient",
          conf.low = "5th percentile",
          conf.high = "95th percentile"
        ) %>%
        fmt_number(
          columns = 2:4,
          decimals = 2
        ) %>%
        tab_header(
          title = "City Population and Median Home Price",
          subtitle = "Median single family home prices rise as the city population increases."
        ) %>%
        tab_source_note(
          source_note = "Data from Zillow."
        )
      
    } else{
      HTML("Select at least 3 cities from the drop-down menu on the left.")
    }
  })
  
  output$plot4 <- renderPlot ({
    plot4 <- zillowOR_county %>%
      group_by(MunicipalCodeFIPS) %>%
      ggplot(aes(x = year, y = avg)) +
      geom_point() +
      scale_y_log10(lim = c(50000, 1000000)) + 
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Change in Median House Price over Time by County",
           subtitle = "Data from 2001 to 2019 limited to just the state of Oregon.",
           y = "Median House Price ($)",
           x = "Year",
           caption = "The scale on the y-axis has been logged so as to better illustrate the relationship between the two variables. \nSource: Zillow") +
      theme(
        plot.caption = element_text(hjust = 0)
      )
    plot4
  })
  
  output$plot3 <- renderPlot ({
    plot3 <- permitsOR_county %>%
      ggplot(aes(x = date, y = median)) + 
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Change in Number of Multi-Unit Housing Permits Issued Each Year by County",
           subtitle = "Data from 2001 to 2019 limited to just the state of Oregon.",
           y = "Number of Permits Issued",
           x = "Year",
           caption = "Data Sources: US Census Bureau, Department of Housing and Urban Development, & Zillow") +
        theme(
          plot.caption = element_text(hjust = 0)
        )
    plot3
  })
  
   output$animation <- renderImage({
    animation <- read_rds("animated_map.rds")
    
    anim_save("animation.gif", animation)
    
    list(src = "animation.gif",
         contentType = "image/gif")
  }, deleteFile = FALSE)
  
  # Second plot
  # Basically the same as above.
  
  output$plot2 <- renderPlot({
    
    neighborhoods %>%
      filter(RegionName %in% input$selectNeighborhoods) %>%
      group_by(RegionName) %>%
      ggplot(aes(x = date, y = median, group = RegionName, color = RegionName)) +
      geom_point() +
      theme(
        legend.position = "none"
      ) +
      scale_y_continuous(lim = c(100000, 900000)) +
      labs(title = "Changes in Median House Price over Time Subsetted by Neighborhoods in Portland, OR",
           subtitle = "April, 1996 to August, 2019",
           y = "Median House Price ($)",
           x = "Date",
           caption = "As the plot shows, house prices have generally been increasing. \nHowever, there was a decline following the bursting of the housing market bubble around 2008 with prices bottoming out around 2012. \nThis trend occurred across all neighborhoods. \nSource: Zillow") +
      theme(
        plot.caption = element_text(hjust = 0)
      )
  })
}

# Run the application 

shinyApp(ui = ui, server = server)

