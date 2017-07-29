chartsTab <- tabItem(
  tabName = "charts",
  fluidRow(
     tabBox(
       title = "Select Data",
       id = "selector",
       width = "4",
       
       # panel to choose Data
       tabPanel(
         title = "Data",
          # Select Input for Organisation Unit Groups
          #uiOutput(
          #  outputId = "dxGrp"
          #),
          selectInput(
            inputId = "dxGrp",
            width = "100%",
            choices = parseIndicators()$name,
            label = "Choose Indicator Group"
          ), # end Select
          
          # Select Input for Individual Indicators
          selectInput(
            inputId = "dx",
            width = "100%",
            choices = "",
            
            label = "Choose Indicator"
          )
       ),
       
       tabPanel(
         title = "Organisation Units",
         
         # Select Input for Levels
         selectInput(
           inputId = "orgLevel",
           width = "100%",
           choices = c("1", "2", "3", "4"),
           label = "Select Unit Level"
           
         ),
         DT::dataTableOutput("tableLevels")
       ) # End Tab for Org units
     ), # End Tab Box
     
     
     box(
       title = "Chart",
       width = "8",
       textInput("demo", "Selected"),
       plotOutput("chartDisplay", "100%")
       
     ) # End Box
  )
)
