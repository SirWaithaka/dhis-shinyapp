library(shiny)
library(DT)
library(rPython)

indicator <- data.frame(id = character(), name = character(), stringsAsFactors = FALSE)
units <- data.frame(id = character(), name = character(), code = character(), stringsAsFactors = FALSE)

shinyServer(function(input, output, session){
  source("api/api.R")
  
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
    
    values <- analytics(dx, ou)
    updateTextInput(session, "demo", value = str(id))
    
  })  

})

