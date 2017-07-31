library(DT)
library(forecast)
library(ggplot2)
library(shiny)
library(tseries)


indicator <- data.frame(id = character(), name = character(), stringsAsFactors = FALSE)
units <- data.frame(id = character(), name = character(), code = character(), stringsAsFactors = FALSE)

peIndicator <- data.frame(id = character(), name = character(), stringsAsFactors = FALSE)
peUnits <- data.frame(id = character(), name = character(), code = character(), stringsAsFactors = FALSE)

ui_groups <- parseIndicators()

shinyServer(function(input, output, session){
  
  observe({
    group <- input$dxGrp

    # filter data frame to get id
    id <- ui_groups$id[ui_groups$name == group]

    indicator <<- parseIndicators(id = id)
    # update select input
    updateSelectInput(session, "dx",
      choices = indicator$name
    )
  })

  observe ({
    level <- as.integer(input$orgLevel)
    
    units <<- parseOrganisationUnits(level)
    if (level == 2)
      units <<- subset(units, substr(units$code, 1, 3) == "OU_")
    
    output$tableLevels <- DT::renderDataTable(units["name"])
  })
  
  observe ({
    
    dx <- input$dx
    ou <- input$tableLevels_rows_selected
    
    level <- as.integer(input$orgLevel)
    
    if (is.null(ou)) return()
    
    # get ids of organisation units picked
    ou_id <- units$id[ou]
    # get id of indicators picked
    dx_id <- indicator$id[indicator$name == dx]
    # bins
    
    values <- parseAnalytics(dx = dx_id, ou = ou_id)
    
    bins <- seq(min(values$dx), max(values$dx), length.out = input$bins +1)

    output$chartDisplay <- renderPlot({
      chartType <- input$graphType
      
      # if (chartType == "histo") {
      #   ggplot2::ggplot(values, ggplot2::aes(x=period, y=dx, fill=..count..)) +
      #     ggplot2::geom_histogram(breaks=bins) +
      #     ggplot2::ggtitle(paste(dx, " | ", units$name[ou])) +
      #     ggplot2::theme_light()
      # }
      
      graphPlot(chartType, ou, values, dx, bins)
    })
    updateTextInput(session, "demo", value = values$dx)
  })  

  # ==============================================================================
  # ==============================================================================
  observe({
    group <- input$peDxGrp
    
    # filter data frame to get id
    id <- ui_groups$id[ui_groups$name == group]
    
    peIndicator <<- parseIndicators(id = id)
    # update select input
    updateSelectInput(session, "peDx",
                      choices = peIndicator$name
    )
  })
  
  observe ({
    level <- as.integer(input$peOrgLevel)
    
    peUnits <<- parseOrganisationUnits(level)
    
    output$peTableLevels <- DT::renderDataTable(peUnits["name"])
  })
  
  observe ({
    
    dx <- input$peDx
    ou <- input$peTableLevels_rows_selected
    
    level <- as.integer(input$peOrgLevel)
    
    if (is.null(ou)) return()
    
    # get ids of organisation units picked
    ou_id <- peUnits$id[ou]
    ou_name <- peUnits$name[ou]
    # get id of indicators picked
    dx_id <- peIndicator$id[peIndicator$name == dx]
    
    values <- parseAnalytics(dx = dx_id, ou = ou_id)
    
    output$predictionDisplay <- renderPlot({
      TS <- ts(values, start = c(2016,7), end = c(2017,1), frequency = 12)
      # fit <- forecast::auto.arima(TS[, "dx"], seasonal = TRUE)
      fit <- arima(TS[, "dx"], order = c(0,0,3))
      fcast <- forecast::forecast(fit, h = 1 * 6)
      plot(fcast, main = paste("T Series: ", ou_name), xlab="period (months)", ylab="Indicator")
      # ggplot2::ggplot(fcast, ggplot2::aes(x))
    })
    updateTextInput(session, "peDemo", value = values$dx)
  })
  
})

