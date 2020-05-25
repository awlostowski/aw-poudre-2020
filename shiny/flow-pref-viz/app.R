remove(list = ls()) # clear all workspace variables
cat("\014")         # clear command line

library(shiny)
library(here)
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())

# load data --------------------------------------------------

load(file = "data/respondend-attributes.Rdata")
load(file = "data/flow-pref-data.Rdata")

#------------------------------------------------------------------------  
# Define UI for app ----
ui <- fluidPage(titlePanel("Poudre River Flow Preference Results"),
                # App title ----
                
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(# Sidebar panel for inputs ----
                              sidebarPanel( 
                                           selectInput(
                                             "segment", "Select a river segment:",
                                             unique(flowpref.dat$segment.name)
                                           )),
                              
                              # Main panel for displaying outputs ---
                              mainPanel(fluidRow(
                                verticalLayout(
                                  plotOutput("plot1")
                                )
                              ))))


#------------------------------------------------------------------------  
server <- function(input, output) {
  
  # Flow Preference Curve ----------------------------------
  output$plot1 <- renderPlot({
    
    # pull flow preference data for the specified segment name
    dat <- filter(flowpref.dat, segment.name == input$segment)
    
    # compute results
    results <- dat %>%
      
      # Remove NAs and select a specific segment
      filter(is.na(preference.code) == 0) %>%
      
      # calculate the average pref score and PCI2 statistic of each flow bin
      group_by(as.numeric(flow)) %>%
      summarize(pref.average = mean(preference.code),   # average preference
                d = sum(abs(dist(preference.code))),    # sum of the score distance vector
                m = sum(abs(dist(rep(c(-3,3),n())))),   # maximum possible sum of distance vector
                pci2 = d/m) %>%                         # PCI2 = d/m
      
      # rename a variable
      rename(flow = `as.numeric(flow)`)
    
    # plot exceedence probability curves for selected county, cloud showing extent across all counties
    
    if (input$segment == "Whitewater Park" | input$segment == "Filter Plant" | input$segment == "Big South") {
      
      plot1 <-
        ggplot() +
        geom_point(data = results, aes(x = flow, y = pref.average, size = pci2), color = 'blue') +
        scale_size(range = c(1,10), 
                   limits = c(0.01,0.2), 
                   breaks = c(0.01,0.05,0.1,0.15,0.2),
                   name = "PCI") +
        geom_jitter(data = dat, aes(x = as.numeric(flow), y = preference.code), size = 1, alpha = 0.3) +
        labs(x = "Flow (cfs)",
             y = "Preference Score") +
        geom_hline(yintercept = 0) +
        theme(text=element_text(size=21))
      
      print(plot1)
      
    } else {
      
      plot1 <-
        ggplot() +
        geom_point(data = results, aes(x = flow, y = pref.average, size = pci2), color = 'blue') +
        scale_size(range = c(1,10), 
                   limits = c(0.01,0.2), 
                   breaks = c(0.01,0.05,0.1,0.15,0.2),
                   name = "PCI") +
        geom_jitter(data = dat, aes(x = as.numeric(flow), y = preference.code), size = 1, alpha = 0.3) +
        labs(x = "Rock Stage (ft)",
             y = "Preference Score") +
        geom_hline(yintercept = 0) +
        theme(text=element_text(size=21))
      
      print(plot1)
      
    }
    
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)  