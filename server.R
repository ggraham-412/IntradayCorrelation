
shinyServer(function(input, output, session) {
   
    df_all <- reactive({
      barInterval <- as.numeric(input$ddlBarInterval)
      raw <- gf_getdata(input$txtSym, as.numeric(input$numDays)+1, barInterval)
      validate(
        need(!(is.na(raw)[1]), "No data was returned.  Please check that the symbol is a correct NYSE or NASDAQ symbol with normal opening and closing times.")
      )
      prep <- gf_prepdata(raw, barInterval)
      validate(
        need(!(any(is.na(prep$value))), "Some data was missing.  Please check that the symbol trades regularly with normal opening and closing times.")
      )
      prep
    })
    
    df_price <- reactive({
      gf_filterdata(df_all(), "APRICE", input$ddlOutput )
    })
    
    df_volume <- reactive({
      gf_filterdata(df_all(), "VOLUME", input$ddlOutput )
    })
    
    df_diff <- reactive({
      tmp <- df_all()
      barIntervalMin <- as.numeric(input$ddlBarInterval) / 60
      tmpRefCol <- (as.numeric(input$corrRefPoint)) / barIntervalMin
      refcol <- min(tmpRefCol + 1, length(unique(tmp$PERIOD)))
      gf_correlationData(tmp, refcol)
    })
    
    output$PriceVolumeTitle <- renderUI({
      h2(paste("Daily Overlaid Prices and Volumes for",input$txtSym))
    })
    
    output$priceChart <- renderChart2({
      pxdata <- df_price()
      gf_priceChart(pxdata)
    })
    
     output$volumeChart <- renderChart2({
       vol <- df_volume()
       gf_volumeChart(vol, input$ddlOutput)
     })
     
     output$CorrelationTitle <- renderUI({
       h2(paste("Daily Intraday Correlations for",input$txtSym))
     })

     output$CorrelationTableTitle <- renderUI({
       h3(paste("Daily Intraday Correlations relative to",input$corrRefPoint, "minutes."))
     })
     
     output$correlation <- renderPlot({
      gf_correlationPlots(df_diff())
    })
    
    output$corrdata <- renderTable({df_diff()})
    
    observe({
      tmp <- df_diff()
      maxnum <- max(tmp$Minute)
      selected <- min(as.numeric(input$corrRefPoint), maxnum)
      barInterval <- as.numeric(input$ddlBarInterval) / 60
      updateSliderInput(session, 
                        "corrRefPoint", 
                        min=barInterval, 
                        max = maxnum, 
                        step=barInterval, 
                        value=selected)
    })
})
