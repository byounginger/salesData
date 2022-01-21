library(shiny)
library(ggplot2)
library(tidyverse)
# library(ggpubr)
library(DT)
# library(phyloseq)
library(RColorBrewer)
library(plotly)
library(reshape2)
# library(gt)
# library(heatmaply)
library(cowplot)
# library(paws)
# library(paws.common)
# library(R.utils)
# library(memoise)
# library(survival)
# library(survminer)
# library(vegan)
# library(ggcorrplot)
library(shinyjs)
library(shinycssloaders)
# library(scales)
# library(microbiome)
# library(ANCOMBC)
# library(Rtsne)
# library(Rdpack)
# library(rlang)
# source("helpers.R")
# library(profvis)
# library(orca)
# library(processx)
# library(wrapr)

######################################
# Functions to go in helpers.R

# Add line breaks more conveniently
linebreaks <- function(n){HTML(strrep(br(), n))}
######################################
# Data loading (Usually better within a tabPanel for complex data)
walmart <- walmart <- read.csv("~/Documents/Shiny/Data/Walmart.csv")

# Convert store to a factor
walmart$Store <- as.factor(walmart$Store)

# Order the dataset by median weekly sales
walmart <- walmart %>% group_by(Store) %>% 
  mutate(MedianSales = median(Weekly_Sales)) %>% 
  arrange(desc(MedianSales))

# Change the date format
walmart <- walmart %>% separate(Date, into = c("Day", "Month", "Year"), sep = "-")
walmart <- walmart %>% unite("Date", c("Year", "Month", "Day"), sep = "-")
######################################

# Define UI for application that draws a histogram
ui <- fluidPage(title = "SalesData", 
                
                tags$head(tags$style(HTML('*{font-family: "verdana"};'))),
                
                useShinyjs(),
                
                titlePanel(h1("Walmart Sales Data", align = "center", 
                              style = "font-family: verdana;")),
                
                titlePanel(h4("2010-02-05 to 2012-11-01", align = "center", 
                              style = "font-family: verdana;")),
                
                titlePanel(h3("Brett Younginger", align = "center", 
                              style = "font-family: verdana;")),
                
                titlePanel(h4("b.younginger@gmail.com", align = "center", 
                              style = "font-family: verdana;")),
                
                linebreaks(4),
                
                navlistPanel(widths = c(2,10),
                             
                             tabPanel("Summary Statistics", 
                                      
                                      fluidPage(
                                        
                                        fluidRow(column(width = 8, offset = 1, p(span(strong("Table 1."), "Summary statistics for the Walmart weekly sales dataset")), DT::dataTableOutput("sumStatsTable"))), 
                                        
                                        linebreaks(2)
                                      )),
                             
                             tabPanel("Weekly Sales", 
                                      
                                      fluidPage(
                                          
                                          fluidRow(column(width = 10, plotlyOutput("weeklySalesPlot"), p(span(strong("Figure 1."), "Weekly sales of 45 stores"))), column(width = 2, wellPanel(selectInput("holidayVal", label = "Holiday status", choices = c("Both", "Holiday", "Non-holiday"))))),
                                          
                                          linebreaks(2), 
                                          
                                          fluidRow(column(width = 2, wellPanel(selectInput("storeRank", label = "Store rank", choices = c('Top third', 'Middle third', 'Bottom third')))), column(width = 6, plotlyOutput("dynamicSalesVsTempPlot"), p(span(strong("Figure 2."), "Effect of temperature on weekly sales for top, middle and bottom ranking stores by average sales")))), 
                                          linebreaks(2)
                                      ))
                             
                             

))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##### Summary Statistics #####
  # Summary stats data table
  output$sumStatsTable <- renderDataTable({
    
    # Summarise function for generating the tibble
    sumTable <- walmart %>% dplyr::group_by(Store) %>% 
      summarise(MeanWeeklySales = mean(Weekly_Sales), 
                MedianWeeklySales = median(Weekly_Sales), 
                MeanTemp = mean(Temperature), 
                MeanFuel = mean(Fuel_Price), 
                MeanCPI = mean(CPI), 
                MeanUnemp = mean(Unemployment))
    
    # Make the table
    datatable(sumTable, rownames = FALSE, 
              colnames = c("Mean Weekly Sales", "Median Weekly Sales", 
                           "Mean Temp (\u00B0F)", "Mean Fuel Price ($)", 
                           "Mean CPI", "Mean Unemployment"),
              options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")), 
      pageLength = 10)) %>% formatRound(colnames(sumTable), 
                                        digits = c(0, 0, 0, 1, 2, 2, 2)) 
  })
  
  ##### Weekly Sales #####
  # Weekly sales plot selected by holiday
  output$weeklySalesPlot <- renderPlotly({
      
      # Conditionals for holiday status with ylabels
      if(input$holidayVal == "Holiday"){
        
        walmart <- walmart[walmart$Holiday_Flag == 1, ]
        ylab <- "Weekly holiday\nsales"
        
      } else if(input$holidayVal == "Non-holiday"){
        
        walmart <- walmart[walmart$Holiday_Flag == 0, ]
        ylab <- "Weekly non-holiday\nsales"
        
      } else{
        
        ylab <- "Weekly sales"
      }
    
    # The plot
    walmart %>%
      plot_ly(x = ~reorder(Store, MedianSales), y = ~Weekly_Sales, 
              color = ~Store, type = "box", hoverinfo = "text",
              showlegend = FALSE,
              text = ~paste("Store:", Store, "<br>", 
                            "Date:", Date, "<br>", 
                            "Weekly sales:", Weekly_Sales)) %>%
      layout(yaxis = list(showline = TRUE, title = ylab), 
             xaxis = list(showline = TRUE, tickangle = -45, hjust = -1, 
                          title = "Store"))
    })
  
  # dynamicSalesVsTempPlot
  output$dynamicSalesVsTempPlot <- renderPlotly({
    
    # Rank the stores by average sales
    avgSales <- walmart %>% dplyr::group_by(Store) %>% 
      summarise(AvgSales = mean(Weekly_Sales))
    avgSales <- avgSales[order(avgSales$AvgSales), ]
    bottomThird <- avgSales[1:15,]$Store
    midThird <- avgSales[16:30,]$Store
    topThird <- avgSales[31:45,]$Store
    
    # Select for store rank
    storeSelect <- switch(input$storeRank, 
                          'Top third' = topThird, 
                          'Middle third' = midThird, 
                          'Bottom third' = bottomThird)
    
    # The plot
    walmart[walmart$Store %in% storeSelect, ] %>%
      plot_ly(x = ~Temperature, y = ~Weekly_Sales, 
              color = ~Store, 
              frame = ~Date, 
              type = 'scatter', mode = 'markers', 
              hoverinfo = 'text', 
              text = ~paste('Store:', Store, '<br>', 
                            'Wk Sales:', Weekly_Sales, '<br>',
                            'Temp:', Temperature, '<br>')) %>%
      layout(xaxis = list(showline = TRUE, title = 'Temperature (\u00B0F)'), 
             yaxis = list(showline = TRUE, title = 'Weekly sales'), 
             legend = list(title = list(text = ' Store')))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
