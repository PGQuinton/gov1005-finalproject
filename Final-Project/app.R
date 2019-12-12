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
library(plotly)
library(tidymodels)
library(broom)
library(gifski)
library(gganimate)
library(png)
library(ggthemes)
library(tidyverse)

# Read in the Zillow cities data.

cities <- read_csv("cities.csv", 
                   col_types = cols(
                     rowname = col_double(),
                     region_name = col_character(),
                     size_rank = col_double(),
                     date = col_character(),
                     price = col_double(),
                     Rank = col_double(),
                     Price = col_double()
                   ))

# Created a vector of the city names that will be used in the pickerInput.

citiesVector <- as.vector(cities$region_name)

# Read in the Zillow neighborhood data.

neighborhoods <- read_csv("neighborhoods.csv",
                          col_types = cols(
                            RegionName = col_character(),
                            date = col_datetime(format = ""),
                            median = col_double()
                          ))

# Created a vector of neighborhood names that will be used in the pickerInput.

neighborhoodsVector <- neighborhoods %>%
  distinct(RegionName)

neighborhoodsVector <- as.vector(neighborhoodsVector$RegionName)

# Oregon county level median single-family home price data

# Mapped permits data for Oregon

counties_data <- read_csv("counties.csv",
                          col_types = cols(
                            id = col_character(),
                            year = col_double(),
                            name = col_character(),
                            pop = col_double(),
                            total = col_double(),
                            avg = col_double(),
                            diff = col_double()
                          ))

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
                                                   plotlyOutput("plot1"),
                                                   br(),
                                                   p(paste("The above plot displays the relationship between population and median single-family house price for the 804 most populous cities in the United States. The X-axis displays the ranked population of the city with the most populous city on the right and the least on the left. The Y-axis displays the median single-family house price. The data is a courtesy of Zillow and is from August 2019. The picker input on the left sidebar allows you to select the cities which you want to display on the plot. As the plot of the entire dataset illustrates, there is a positive relationship between population and median house price, more populous cities are more likely to have higher median house prices than less populous cities."))
                                          ),
                                          tabPanel("Model",
                                                   p(paste("Select at least 3 cities to view regression outputs.")),
                                                   br(),
                                                   gt_output("model"),
                                                   br(),
                                                   p(paste("The above model represents the regression output from the plot displayed in the plot tab. The table values are calculated based on the current selection of cities which can be changed via the picker input. The coefficient for population represents the expected increase in median single-family house price relative to the intercept for each increase in population ranking. The upper and lower bounds of the confidence interval indicate the statisticall significance of the relationship. Looking at the calculations for the entire list of cities, there is a positive relationship between population and median house price that is statistically significant at the 10% level."))
                                          )
                              )
                            )
                          )
                 ),
                 
                 # Second tab
                 # Oregon state level data
                 
                 tabPanel("Counties",
                          titlePanel("County Level Data - Portland, Oregon"),
                          tabsetPanel(
                            id = "tabsOregon",
                            tabPanel("Home Value",
                                     plotOutput("plot4"),
                                     br(),
                                     p(paste("The above plot illustrates the changes in median single-family home price over from 2001 to 2019. The data is from the six counties in Oregon that make-up the metropolitan area of Portland. Portland metro also includes two counties in Washington but for the purposes of this analysis, those were excluded. The plot shows that across all six counties, home prices have generally risen over the past 18 years. As is well documented at this point, the home prices began to decline following the housing market crash and subsequent recession. Home prices bottomed out in roughly 2012 before beginning to rise again. This trend is consistent for all the counties included."))
                                       ),
                            tabPanel("Building Permits",
                                     plotOutput("plot3"),
                                     br(),
                                     p(paste("The above plot illustrates the relationship between building permits and population at the county level between 2001 and 2019. The X axis displays the year while the Y axis displays the number of building permits for housing structures issued that year. The size of the points on the plot represents the population of the county for that year. The five counties included in the analysis are the five counties in Oregon that make-up the metropolitan area of Portland. There are two main takeaways from the plot. The first is that, as seen in the median house prices, building permit rates sharply declined following the housing market crash in 2008 and took several years to rebound. This decline was more pronounced in the more populated counties. The second main takeaway is that the more populated counties have a higher number of building permits issued each year which is rather intuitive but still important to understand."))
                                     ),
                            tabPanel("Model",
                                     uiOutput("lm1"),
                                     br(),
                                     p(paste("The regression output displayed captures a linear regression testing the effects of population and building permit numbers on the annual change in median housing price at the county level. The regression controls for year and county effects. Year effects effectively just capture an estimation of inflation in housing prices. The regression results show that the number of housing permits issued each year is statistically significant at the 1% level and the year is statistically significant at the 5% level. The coefficient on permits was 12.668 indicating that for every one housing permit issued, median prices can be expected to rise by $12.668. The coefficient on year was much higher at 1,331.030 indicating that each year prices can be expected to rise by $1,331.030. Population was not statistically significant. None of the counties were statistically significant. Clackamas County was not listed on the regression output because it is the base category so the other county coefficients are all relatively to Clackamas County."))
                            )
                          )
                 ),
                 
                 # Third tab
                 # Titled Portland, OR
                 
                 tabPanel("Neighborhoods",
                          titlePanel("Neighborhood Trends Over Time in Portland, Oregon"),
                          tabsetPanel(
                            id = "tabsPortland",
                            tabPanel(
                              "Animation",
                               sidebarLayout(
                                 sidebarPanel(
                                   p(paste("The plot to the right displays an animation of the changing median house prices at the neighborhood level from 1996 to 2019. The neighborhoods included make up the Portland metropolitan area. As with the county level data, the neighborhood data shows a dip in median house price following the 2008 crash. This dip appears to affect all neighborhoods somewhat equally."))
                                 ),
                                 mainPanel(
                                   p(paste("Disclaimer: This animation takes a few seconds to load.")),
                                   br(),
                                   plotOutput("animation")
                                 )
                            )),
                            tabPanel(
                              "Plot",
                              sidebarLayout(
                                sidebarPanel(
                                  pickerInput("selectNeighborhoods",
                                              "Select Neighborhoods",
                                              choices = neighborhoodsVector,
                                              options = list("actions-box" = TRUE),
                                              multiple = TRUE,
                                              selected = neighborhoodsVector[39]
                                  )),
                                mainPanel(
                                  plotOutput("plot2"),
                                  br(),
                                  p(paste("The above plot is the same as the animated plot but a static version. The year is on the Y-axis and the median house price is on the X-axis. Neighborhoods can be selected to include in the plot from the dropdown menu on the left. I set the initial plot to just include the neighborhood in which I grew up, Irvington. The general trend across all neighborhoods is a steady increase in median house price with a depression for several years following the crash in 2008. Neighborhood prices have rebounded strongly and have made up for the several year decline."))
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
  
  output$plot1 <- renderPlotly({
    
    plot1 <- cities %>%
      filter(region_name %in% input$selectCities) %>%
      ggplot(aes(x=Rank, y=Price)) +
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
    ggplotly(plot1)
    
  })
  
  # Model
  # Used render_gt() so my gt table shows up.
  # Again, subset the data to just include the cities selected.
  
  
    
  output$model <- render_gt({
      
      
     
    if(length(input$selectCities) > 2){
      model_data <- cities %>%
        filter(region_name %in% input$selectCities)
      
      model <- tidy(lm(price ~ rowname, data = model_data), conf.int = TRUE, conf.level = .90)
      
      gt <- model %>%
        select(term, estimate, conf.low, conf.high) %>%
        mutate(term = c("Intercept", "Population Ranked")) %>%
        gt() %>%
        cols_label(
          term = "",
          estimate = "Coefficient",
          conf.low = "5th percentile",
          conf.high = "95th percentile"
        ) %>%
        tab_spanner(
          label = "Confidence Interval",
          columns = 3:4
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
      
      gt
    } else {
      
    }
      
    })

  
  
  output$plot4 <- renderPlot ({
    plot4 <- counties_data %>%
      ggplot(aes(x = year, y = avg)) +
      geom_point(aes(color = name), alpha = 0.9) +
      scale_y_log10(lim = c(50000, 1000000)) + 
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Change in Median House Price over Time by County",
           subtitle = "Data from 2001 to 2019 limited to just the counties within the metropolitan area of Portland, Oregon.",
           y = "Median House Price ($)",
           x = "Year",
           color = "County",
           caption = "The scale on the y-axis has been logged so as to better illustrate the relationship between the two variables. \nSource: Zillow") +
      theme(
        plot.caption = element_text(hjust = 0)
      )
    plot4
  })
  
  output$plot3 <- renderPlot ({
    plot3 <- counties_data %>%
      ggplot(aes(x = year, y = total, color = name)) + 
        geom_point(aes(size = pop)) +
        geom_line() +
        labs(title = "Change in Number of Housing Permits Issued Each Year by County",
           subtitle = "Data from 2001 to 2019 limited to just the counties within the metropolitan area of Portland, Oregon.",
           y = "Number of Permits Issued",
           x = "Year",
           caption = "Data Sources: US Census Bureau, Department of Housing and Urban Development, & Zillow",
           color = "County",
           size = "Population"
           ) +
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
  
  output$plot5 <- renderPlot ({
    permitsOR_county %>%
      filter(year == 2010) %>%
      ggplot(aes(x = median, y = diff)) +
        geom_point() +
        geom_smooth(method = "lm")
  })
  
  output$lm1 <- renderUI({
    reg_permits1 <- lm(diff ~ total + pop + year + name, data = counties_data)
    
    reg <- stargazer(reg_permits1,
                     title = "OLS Regression: Determinants of Changing House Prices",
                     type = "html",
                     covariate.labels = c(
                       "Permits",
                       "Population",
                       "Year",
                       "Columbia County",
                       "Multnomah County",
                       "Washington County",
                       "Yamhill County"
                     ))
    
    HTML(reg)
    })
}

# Run the application 

shinyApp(ui = ui, server = server)

