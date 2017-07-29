library(shiny)
library(DT)
library(rPython)

source("api/api.R")

indicator <- data.frame(id = character(), name = character(), stringsAsFactors = FALSE)
units <- data.frame(id = character(), name = character(), code = character(), stringsAsFactors = FALSE)

peIndicator <- data.frame(id = character(), name = character(), stringsAsFactors = FALSE)
peUnits <- data.frame(id = character(), name = character(), code = character(), stringsAsFactors = FALSE)

shinyServer(function(input, output, session){
  
  observe({
    group <- input$dxGrp

    # filter data frame to get id
    id <- ui_groups$id[ui_groups$name == group]

    indicator <<- indicators(group = "true", members = id)
    # update select input
    updateSelectInput(session, "dx",
      choices = indicator$name
    )
  })

  observe ({
    level <- as.integer(input$orgLevel)
    
    units <<- organisationUnits(level)
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
    id <- units$id[ou]
    # get id of indicators picked
    dx_id <- indicator$id[indicator$name == dx]
    
    values <- analytics(dx = dx_id, ou = id)

    output$chartDisplay <- renderPlot({
      ggplot2::ggplot(values, ggplot2::aes(x=period, y=dx)) +
        ggplot2::geom_bar(stat = "identity", fill = "steelblue")
    })
    updateTextInput(session, "demo", value = values$dx)
  })  

  # ==============================================================================
  # ==============================================================================
  observe({
    group <- input$peDxGrp
    
    # filter data frame to get id
    id <- ui_groups$id[ui_groups$name == group]
    
    peIndicator <<- indicators(group = "true", members = id)
    # update select input
    updateSelectInput(session, "peDx",
                      choices = peIndicator$name
    )
  })
  
  observe ({
    level <- as.integer(input$peOrgLevel)
    
    peUnits <<- organisationUnits(level)
    if (level == 2)
      peUnits <<- subset(peUnits, substr(peUnits$code, 1, 3) == "OU_")
    
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
    
    values <- analytics(dx = dx_id, ou = ou_id)
    
    output$predictionDisplay <- renderPlot({
      TS <- ts(values, start = c(2016,7), end = c(2017,8), frequency = 12)
      fit <- arima(TS[, ou_name], c(0,0,1), list(order = c(0,0,1), period = 12))
      fcast <- forecast::Arima(fit, h = 1 * 6)
      plot(fcast, main = paste("T Series: ", ou_name), xlab="period (months)", ylab="Indicator")
    })
    updateTextInput(session, "peDemo", value = values$dx)
  })  
  
})

