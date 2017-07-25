library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Home", dropdownMenu(type = "messages",
                                               messageItem(from = "CHRIO-Bo",message = "pregnancy complications are high this month"),
                                               messageItem(from = "CHRIO-Bonthe",message="The is low anc1 coverage this month"),
                                               messageItem(from = "CHRIO-Kambia", message = "There is low turn up this month")),
                  dropdownMenu(type = "notifications",
                               notificationItem(text = "5 new users today",icon("users")),
                               notificationItem(text = "Rapid increase of pregnancy compications this month",status = "warning",icon("exclamation-triangle")))),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
      menuItem(text = "Dashboard",tabName = "dashboard",icon = icon("dashboard")),
      menuItem(text = "Organisation Unit",tabName = "organisationUnits",icon = icon("th-large", lib = "glyphicon")),
      menuItem(text = "Data elements",tabName = "dataelements",icon = icon("list-alt", lib = "glyphicon")),
      menuItem(text = "Charts",tabName = "charts",icon = icon("bar-chart", "fa")),
      menuItem(text = "Maps",tabName = "maps",icon = icon("map"))
      
    )
  ),
  dashboardBody(
    tabItems(
      #First tabItem
      tabItem(tabName = "dashboard",
              fluidRow(
                conditionalPanel(condition="input.selectedtab==2",
                                 box(width = 4,
                                     title = "Summary",status = "primary",solidHeader = TRUE,
                                     "ANC1 coverage",br(),"Last 12 months",
                                     tableOutput("org_rank_anc1"))),
                conditionalPanel(condition="input.selectedtab==3",
                                 box(width = 4,
                                     title = "Summary",status = "warning",solidHeader = TRUE,
                                     "Pregancy related complications",br(),"Last 12 months",
                                     tableOutput("org_rank_preg"))),
                conditionalPanel(condition="input.selectedtab==1",
                                 box(width = 4,
                                     title = "Summary",status = "success",solidHeader = TRUE,
                                     "Performance of organzation units",br(),"Last 12 months",
                                     tableOutput("org_rank_best"))),
                tabBox(
                  title = "Summary",id="selectedtab",width = 8,
                  tabPanel("Performance", value = 1, plotOutput("org_performance_line")),
                  tabPanel("ANC1 coverage",value = 2, plotOutput("org_pie_anc1")),
                  tabPanel("Pregnancy-related complications",value = 3, plotOutput("org_pie_preg"))
                )
              )
      ),
      tabItem(tabName = "organisationUnits",
              # set layout
              fluidRow(
                box(width = 8,
                    dataTableOutput("orgUnitTable")
                )
                
              )
              
      ),
      tabItem(tabName = "dataelements",
              # set layout
              fluidRow(
                box(width = 10,
                    dataTableOutput("dataElementsTable")
                )
              )
      ),
      tabItem(tabName = "charts",
              fluidPage(
                headerPanel(title = ""),
                sidebarLayout(
                  sidebarPanel = conditionalPanel(condition="input.tabselected==1",
                                                  box(width = 4,
                                                      title = "Input",status = "primary",color="navy",solidHeader = TRUE,
                                                      selectInput(inputId="dataelement",label = "Select data element",choices = c("ANC1 visit","Pregnancy-related complications"),selected = "ANC1 visit"),
                                                      #br(),
                                                      selectInput("orgUnit_select", label = "Organisation unit", choices = c("Bo", "Bonthe", "Kailahun", "Kambia", "Kenema"),multiple = F),
                                                      #br(),
                                                      radioButtons("chart_type", label = "Select chart type", choices = c("bar graph","line graph","histogram","scatter plot","density histogram")),
                                                      #br(),
                                                      selectInput(inputId="dataelement2",label = "Select the data lement to compare",choices = c("ANC1 visit","Pregnancy-related complications"),selected = "Pregnancy-related complications"),
                                                      sliderInput(inputId="bins",label = "Set the number of bins",min = 5,max = 100,value = 10),
                                                      dateRangeInput(inputId="range",label = "Select period"))),
                  mainPanel(
                    tabsetPanel(type="pills", id="tabselected",
                                tabPanel("charts",value = 1, box(width = 12,
                                                                 plotOutput("anc1"),
                                                                 br(),
                                                                 radioButtons(inputId="type_download",label = "select the file type",choices = list("png","pdf")),
                                                                 downloadButton(outputId = "download", label = "Download the chart"))),
                                tabPanel("table", value = 2, fluidRow(box(width = 9,
                                                                          dataTableOutput("chart_table")),
                                                                      box(width = 3,
                                                                          title = "Download file",status = "primary",solidHeader = TRUE,
                                                                          radioButtons(inputId="type_download_table",label = "Choose the file type",choices = list("Excel (CSV)", "Text (TSV)", "Text (Space Separated)", "Doc")),
                                                                          downloadButton(outputId="download2",label = "Download table"))))
                                
                                
                    )
                    
                  )
                )
              )
      ),
      tabItem(tabName = "maps",
              fluidRow(
                box(width = 12,
                    leafletOutput("map"))
                
              )
      )
      
    )
    
  )
)


