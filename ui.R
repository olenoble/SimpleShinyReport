library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Volatility Fit Visualization Tool"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectInput("stratselect", 
                  label = h3("Underlying Selection"), 
                  choices = list("SPX" = "spx", 
                                 "UKX" = "ukx"), 
                  selected = "spx"),
      br(),
      downloadButton("downloadData", 'Download Data'),
      br(), #br(), #br(),
      h3('R Squared Summary'),
      fluidRow(column(5, tableOutput('metrics'))),
      br(), #br(), #br(),
      h3('Volatility Plotting'),
      numericInput('maturity', 'Maturity in Months', 3, min = 1, max = 60),
      numericInput('moneyness', 'Forward Moneyness', 100, min = 10, max = 200)
    ),
    
    mainPanel(
      #h4("My main panel"),
      #a("Google", href="http://www.google.com"),
      #br(),
      #h4("Some other stuff"),
      #p("Good read:",
      #a("Python", href="https://www.continuum.io/")),
      #fluidRow(column(5, dataTableOutput('contents')))
      #plotOutput('plot', width = "auto")#, height = "100%")
      ## Need to add tab to visualize all tenors (against raw vol) on given date
      tabsetPanel(
        tabPanel("Plot",  plotOutput("plot"), tableOutput('metrics_ym')),
        tabPanel("Data", dataTableOutput('contents')),
        #tabPanel("Historical Volatility", dataTableOutput('contents')),
        tabPanel("Historical Volatility", plotOutput("HistoricalVol"))
        )
      
    )
  )
))