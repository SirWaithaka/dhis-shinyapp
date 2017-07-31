library(shinydashboard)

source("api/parser.R")
source("api/plots.R")

source("dashboard/index.R")
source("dashboard/charts.R")
source("dashboard/maps.R")
source("dashboard/prediction.R")


dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = "Home",
    dropdownMenu(type = "messages"),
    dropdownMenu(type = "notifications")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Dashboard",tabName = "dashboard",icon = icon("dashboard")),
      menuItem(text = "Charts",tabName = "charts",icon = icon("bar-chart", "fa")),
      menuItem(text = "Maps",tabName = "maps",icon = icon("map-marker")),
      menuItem(text = "Prediction", tabName = "prediction", icon = icon("line-chart"))
    ),
    disable = FALSE
  ),
  
  dashboardBody(
    tabItems(dashboardTab, chartsTab, mapsTab, predictionTab)
  )
)
