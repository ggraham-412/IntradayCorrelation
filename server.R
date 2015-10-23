library(shiny)

shinyServer(function(input, output, session) {
   
    df_all <- reactive({
      barInterval <- as.numeric(input$ddlBarInterval)
      raw <- gf_getdata(input$txtSym, input$numDays, barInterval)
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
      refcol <- min(as.numeric(input$corrRefPoint), length(unique(tmp$PERIOD)))
      gf_correlationData(tmp, refcol)
    })
    
    output$priceChart <- renderChart2({
      pxdata <- df_price()
      gf_priceChart(pxdata)
    })
    
     output$volumeChart <- renderChart2({
       vol <- df_volume()
       gf_volumeChart(vol, input$ddlOutput)
     })
     
    output$correlation <- renderPlot({
      gf_correlationPlots(df_diff())
    })
    
    output$corrdata <- renderTable({df_diff()})
    
    observe({
      maxnum <- nrow(df_diff())
      selected <- min(as.numeric(input$corrRefPoint), maxnum)
      updateSliderInput(session, "corrRefPoint", min=2, max = maxnum, step=1, value=selected)
    })
})
