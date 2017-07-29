predictionTab <- tabItem(
  tabName = "prediction",
  fluidRow(
    tabBox(
      title = "Select Data",
      id = "peSelector",
      width = "4",

      # panel to choose Data
      tabPanel(
        title = "Data",
        # Select Input for Organisation Unit Groups
        #uiOutput(
        #  outputId = "dxGrp"
        #),
        selectInput(
          inputId = "peDxGrp",
          width = "100%",
          choices = indicators(group = "true")$name,
          label = "Choose Indicator Group"
        ), # end Select

        # Select Input for Individual Indicators
        selectInput(
          inputId = "peDx",
          width = "100%",
          choices = "",

          label = "Choose Indicator"
        )
      ),

      tabPanel(
        title = "Organisation Units",

        # Select Input for Levels
        selectInput(
          inputId = "peOrgLevel",
          width = "100%",
          choices = c("1", "2", "3", "4"),
          label = "Select Unit Level"

        ),
        DT::dataTableOutput("peTableLevels")
      ) # End Tab for Org units
    ), # End Tab Box


    box(
      title = "Performance",
      width = "8",
      textInput("peDemo", "Selected"),
      plotOutput("predictionDisplay", "100%")

    ) # End Box
  )
)