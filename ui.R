
shinyUI(pageWithSidebar(
  
  headerPanel("Intraday Stock Price Explorer"),
  
  sidebarPanel( width=3,
    # Choose stock symbol            
    textInput("txtSym", 
              label="Symbol", 
              value = "GOOG"),

    # Choose number of days to analyze
    numericInput("numDays", 
                 "Days", 
                 12, 
                 min=1, 
                 max=28),
    
    # Choose the width of the reported price bars
    selectInput("ddlBarInterval", 
                "Interval", 
                selected = 900,
                choices=list("5 min" = 300, "15 min" = 900, "30 min" = 1800))
  ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Price/Volume",
               
        h2("Daily Overlaid Prices and Volumes"),
        
        # Additional input to affect display
        selectInput("ddlOutput", 
                    "Output Option", 
                    choices=list("Raw", "%-change"), 
                    width=180),
        
        h3("Daily Overlaid Prices"),
        
        showOutput("priceChart","nvd3"),
        
        h3("Daily Stacked Volumes"),
        
        showOutput("volumeChart","nvd3")
      ),
      
      tabPanel("Correlations",
               
               h2("Daily Correlations"),
               
               # Additional option to choose reference point
               sliderInput("corrRefPoint", 
                           "Correleation Reference Point", 
                           min=2, 
                           max=3, 
                           value=2, 
                           step = 1),
               
               h3("Correlation of Initial Price Move with Later Price Moves"),
               
               plotOutput("correlation"),
               
               h3("Correlation Data Table"),
               
               tableOutput("corrdata")
      ),
      
      tabPanel("Documentation",
               includeHTML("doc.html")
      )
      
    )
  )
))
