## app.R ##
#install.packages("shinydashboard")
#Code Written by John Roche
#Student #: 

library(shiny)
library(shinydashboard)
library(DT)
library(stringr)

#setwd("/Data Science and Analytics/DATA8008 Data Visualisation Analytics/Assignment 1/")
#getwd()

source("Met Eireann Display Graphs.R")

# #############################################
# Step 1 - Get Station Details
# #############################################
stnsData<-getStationDetails()

# #############################################
# Step 2 - Get Weather Details
# #############################################
dlyWthrData<-getDlyStationData()

# ###############
# Step 2b - Format Data
# ###############
frmtWthrData<-frmtWthr(dlyWthrData)

# #############################################
# Step 3 - Get Storm Details
# #############################################
strmsData <- getMajorStormEvents() #getMajorWeatherEvents()

# #############################################
# Step 4 - Get Storm Weather
# #############################################
strmWthrData<-smryStrmWthr(strmsData, frmtWthrData)

# #############################################
# Step 5 - 
# #############################################


ui <- dashboardPage(skin="red",
  dashboardHeader(title="STAT 8008 - Data Visualisation & Analytics Presentation", titleWidth=500),
  dashboardSidebar(
    sidebarMenu(menuItem("Data Selection", tabName="DataSelection", icon=icon("cog"))),
    sidebarMenu(menuItem("Weather Stations", tabName="WeatherStations", icon=icon("dashboard"))),
    sidebarMenu(menuItem("Global Warming", tabName="GlobalWarming", icon=icon("line-chart"))),
    sidebarMenu(menuItem("About", tabName="About", icon=icon("briefcase")))
  ),
  dashboardBody(
    tabItems(
      #Data Selection of www.met.ie details
      tabItem(tabName="DataSelection",
              fluidRow(
                tabBox(selected="", height=800, width=12,
                       tabPanel("Weather Station Details",
                                box(DT::dataTableOutput("stnsTbl"), height=200, width=400)
                       ),
                       tabPanel("Storm Details",
                                box(DT::dataTableOutput("strmsTbl"), height=200, width=400)
                                )
                       )
                )
      ),
      #Data Selection of www.met.ie details
      tabItem(tabName="WeatherStations",
              fluidRow(
                h3("Open, Closed & Reporting Weather Staions"),
                box(plotOutput("pltWthrStns"), height=400, width=800)
              )
      ),
      #Data Selection of www.met.ie details
      tabItem(tabName="GlobalWarming",
              fluidRow(
                h3("Is Global Warming Fake News?"),
                tabBox(selected="", height=800, width=12,
                   tabPanel("Box Plot by Season",
                            box(plotOutput("pltWthrSeas"), height=400, width=10),
                            box("This graph gives a general overview on whether or not there is a positive or negative slope across the dataset", height=200, width=10)
                   ),
                   tabPanel("Min, Mean & Max Temp ~ Year",
                            box(plotOutput("pltTempYrly"), height=400, width=10),
                            box(
                              h4("Analysis of Temp Linear Regression"),
                              textOutput("smryTempYrly"),
                              height=200, width=10
                              )
                   ),
                   tabPanel("Radar plot of Storm Attibutess",
                            box(plotOutput("pltWthrRdr"), height=600, width=10)
                   )
                )
              )
      ),
      #Data Selection of www.met.ie details
      tabItem(tabName="About",
              fluidRow(
                box(title="Disclaimer", status="warning",
                "Copyright Met Éireann", br(),
                "Source www.met.ie, [Major Weather Events](https://www.met.ie/climate/major-weather-events)", br(),
                "This data is published under a Creative Commons Attribution 4.0 International (CC BY 4.0)", br(),
                "Met Éireann does not accept any liability whatsoever for any error or omission in the data, their availability, or for any loss or damage arising from their use.", br(),
                "This material has been modified from the original", height=200, width=800),
                tags$image(src='CIT MAIN.jpg', height=100, width=150),
                box(title="Project Details", status="primary",
                    "Student #       : ", br(),
                    "Student Name    : John Roche", br(),
                    "Course          : STAT 8008 - Data Visualisation and Analytics", br(),
                    "Blast from Past : [https://www.deviantart.com/jonnyro/art/Web-Developer-212315034]", height=300, width=650)
              )
      )
    )
  )
)

server <- function(input, output) {
  output$stnsTbl<-DT::renderDataTable({
    datatable(stnsData)
  })
  output$strmsTbl<-DT::renderDataTable({
    datatable(strmsData)
  })
  output$pltWthrStns<-renderPlot({
    dispUpdWthrGrph(frmtWthrData)
  })
  output$pltWthrSeas<-renderPlot({
    dispBoxPltBySeason(frmtWthrData)
  })
  output$pltTempYrly<-renderPlot({
    dispLmTempYrly(frmtWthrData)
  })
  output$smryTempYrly<-renderText({
    smryTempYrly(frmtWthrData)
  })
  output$pltWthrRdr<-renderPlot({
    dispRdrStrmPlt(smryStrmWthr(strmsData,frmtWthrData))
  })
}

shinyApp(ui, server)

