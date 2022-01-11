library(shiny)
library(ggplot2)
library(tidyverse)
# library(ggpubr)
# library(DT)
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
                             
                             tabPanel("Weekly Sales", 
                                      
                                      fluidPage(
                                          
                                          fluidRow(column(width = 10, plotlyOutput("weeklySalesPlot"), p(span(strong("Figure 1."), "Weekly sales of 45 stores"))), column(width = 2, wellPanel(selectInput("holidayVal", label = "Holiday status", choices = c("Both", "Yes", "No"))))),
                                          
                                          linebreaks(2)
                                      ))
                             
                             

))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##### Weekly Sales #####
  # Weekly sales plot selected by holiday
  output$weeklySalesPlot <- renderPlotly({
      
      if(input$holidayVal == "Yes"){
        
        walmart <- walmart[walmart$Holiday_Flag == 1, ]
        ylab <- "Weekly holiday\nsales"
        
      } else if(input$holidayVal == "No"){
        
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
    
    # ggplot(walmart, aes(x = reorder(Store, MedianSales), 
    #                            y = Weekly_Sales, 
    #                            fill = Store)) + 
    #   geom_boxplot() + 
    #   theme_bw() + 
    #   theme(legend.position = "none", 
    #         panel.grid = element_blank(), 
    #         axis.text.x = element_text(angle = 70, hjust = 1)) + 
    #   ylab(ylab) + 
    #   xlab("Store")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
