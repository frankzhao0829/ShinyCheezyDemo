library(IsolationForest)
library(shiny)

setwd("C:/Users/zhaoch/Desktop/Project/R Shiny")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Demo of Tickets/Incidents Problems"),
  
  # Sidebar with controls to select the threshold value
  sidebarLayout(
    sidebarPanel(
      h4("Please select Threshold value and then press Start"),
      
      br(),
      
      sliderInput("Threshold", "Threshold:", min=0, max=1, value=0.54, step=0.01),
      
      br(),
      
      actionButton("go", "Start"),
      
      # actionButton("iStop", label = "Pause"),
      # 
      # actionButton("iResume", label = "Resume"),
      
      br(),
      hr(),
      
      "Progress: ",
      textOutput("percentage"),
      
      br(),

      # "Elapsed Time (seconds):",
      # textOutput("elapsed"),

      width=3
    ),
    
    # Show a tabset that includes a plot
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", 
                           
                           fluidRow(
                             column(9, imageOutput("map1", width=900, height= 500)),
                             column(3, imageOutput("alert", width=400, height= 500))
                             ),
                           
                           fluidRow(
                             column(4, plotOutput("SkypePlot", width=330, height= 230)),
                             column(4, plotOutput("OutlookPlot", width=330, height= 230)),
                             column(4, plotOutput("EOLPlot", width=330, height= 230))
                             ),
                           
                           fluidRow(
                             column(4, div(tableOutput("SkypeText"), style = "font-size:80%")),
                             column(4, div(tableOutput("OutlookText"), style = "font-size:80%")),
                             column(4, div(tableOutput("EOLText"), style = "font-size:80%"))
                             )
                           )
      )
    )
  )
))