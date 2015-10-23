
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(rCharts)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Intraday Stock Price Explorer"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel( width=3,
    textInput("txtSym", label="Symbol", value = "GOOG"),
    numericInput("numDays", "Days", 12, min=1, max=28),
    selectInput("ddlBarInterval", "Interval", selected = 900,
                choices=list("5 min" = 300, "15 min" = 900, "30 min" = 1800))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Price/Volume",
        h2("Daily Overlaid Prices and Volumes"),
        selectInput("ddlOutput", "Output Option", 
                    choices=list("Raw", "%-change"), width=180),
        h3("Daily Overlaid Prices"),
        showOutput("priceChart","nvd3"),
        h3("Daily Stacked Volumes"),
        showOutput("volumeChart","nvd3")
      ),
      tabPanel("Correlations",
               h2("Daily Correlations"),
               sliderInput("corrRefPoint", "Correleation Reference Point", min=2, max=3, value=2, step = 1),
               h3("Correlation of Initial Price Move with Later Price Moves"),
               plotOutput("correlation"),
               h3("Correlation Data Table"),
               tableOutput("corrdata")
      ),
      
      tabPanel("Documentation",
               h2("Documentation"),
               h5(paste("This application looks for patterns in stock price movements", 
                        "on a daily cycle. It is provided as an exploratory class project",
                        "only, no warranty is expressed or implied;  nor does this ",
                        "application comprise actionable investment advice.")),
               h4("Input Options"),
               h5(paste("The application uses an undocumented Google Finance API",
                        "for finding intraday price and volume bars: http://www.google.com/finance/getprices",
                        "The input options are:")),
               h5("-Symbol:  A valid NYSE stock symbol, eg- 'GOOG'"),
               h5("-Days:  A number of days going back from today to include in the study.  (Currently limited to 28 days maximum.)"),
               h5("-Interval:  The length of time between price reports in minutes."),
               h5("-Output:  (Price/Volume tab only) Whether prices and volumes will be plotted raw or in %-change from the initial price movement of the day."),
               h4("Output"),
               h5("Price/Volume Tab top: Daily prices plotted vs minutes from open."),
               h5("Price/Volume Tab bottom: Stacked trade volume plotted vs minutes from open."),
               h5("Correlation Tab top left: (Rolling) Correlation of the first interval price movement against the price movement of subsequent intervals"),
               h5("Correlation Tab top right: (Fixed) Correlation of the first interval price movement against subsequent prices relative to the daily opening price"),
               h5("Correlation Tab bottom: Data table containing the above data points.")
               
      )
      
      
    )
  )
))
