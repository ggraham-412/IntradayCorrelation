
shinyUI(pageWithSidebar(
  
  headerPanel("Intraday Stock Price Explorer"),
  
  sidebarPanel( width=3,
    # Choose stock symbol            
    textInput("txtSym", 
              label="Symbol", 
              value = defaultSymbol),
    
    h6("(eg- 'GOOG', 'XOM', 'T', etc.)"),

    # Choose number of days to analyze
    numericInput("numDays", 
                 "Days", 
                 defaultDays, 
                 min=minDays, 
                 max=maxDays),
    
    # Choose the width of the reported price bars
    selectInput("ddlBarInterval", 
                "Interval", 
                selected = defaultIntervalSec,
                choices = intervalChoices)
  ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel("Price/Volume",
               
        uiOutput("PriceVolumeTitle"),
               
        # Additional input to affect display
        selectInput("ddlOutput", 
                    "Output Option", 
                    choices=defaultOutputChoices, 
                    selected = defaultOutputChoice,
                    width=180),
        
        h3("Daily Overlaid Prices"),
        
        showOutput("priceChart","nvd3"),
        
        h3("Daily Stacked Volumes"),
        
        showOutput("volumeChart","nvd3")
      ),
      
      tabPanel("Correlations",
               
               uiOutput("CorrelationTitle"),
               
               # Additional option to choose reference point
               sliderInput("corrRefPoint", 
                           "Correleation Reference Point", 
                           min=defaultIntervalMin, 
                           max=(2*defaultIntervalMin), 
                           value=defaultIntervalMin, 
                           step = defaultIntervalMin),
               
               h3("Correlation of Initial Price Move with Later Price Moves"),
               
               plotOutput("correlation"),
               
               uiOutput("CorrelationTableTitle"),
               
               tableOutput("corrdata")
      ),
      
      tabPanel("Documentation",
               includeHTML("doc.html")
      )
      
    )
  )
))
