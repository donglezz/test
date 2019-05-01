library(shinydashboard)
library(shiny)
library(lubridate)
library(dplyr)
library(dygraphs)
library(rsconnect)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(plotly)
source("./help.R")


## Un-hash these ##
#citi <- read.csv("201903-citibike-tripdata.csv", header = TRUE)
#citi$gender <- ifelse(citi$gender == 1, "Male", ifelse(citi$gender == 2, "Female", "Unknown"))
#citi$date <- substr(citi$starttime,0,10)
#citi$date <- ymd(citi$date)
#citi$days <- weekdays(citi$date)
#citi$days <- ordered(citi$days, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
#citi$starthour <- substr(citi$starttime,12,19)
#citi$age <- 2019 - citi$birth.year
#citi$agegroup <- ifelse(citi$age < 20, "10s",ifelse(citi$age < 30, "20s", ifelse(citi$age < 40, "30s", ifelse(citi$age < 50, "40s", ifelse(citi$age < 60, "50s", ifelse(citi$age < 70, "60s", ifelse(citi$age < 80, "70s", "80+")))))))


## Leave this as it is ##
#rsconnect::setAccountInfo(name='daniel-dh-lee', token='9EF8C3CFFDC3C68E3B89EB50CDA5E9D8', secret='ZUQoq/WKB/0/dgBGvFC+RJDqTbIzCeKcvZmzJsv1')
#rsconnect::deployApp(server = "shinyapps.io")

# Run this #
ui <- dashboardPage(
  dashboardHeader(title = "Stat graphics project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram", tabName = "hist", icon=icon("graph")),
      menuItem("Time series", tabName = "timeseries", icon=icon("time")),
      menuItem("Mapping", tabName = "mapping", icon=icon("map"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "hist", h2("Bike usage by dates"),
              fluidRow(
                box(plotOutput("plot1", width = 500))),
              fluidRow(
                box(
                  title = "Controls",
                  selectInput(inputId = "genderInput", label = "Gender:",
                              choices = c("Male","Female","Unknown")),
                  sliderInput("bins", "Number of bins:", 31, min = 1, max = 50)
                  )
                )
              ),
    tabItem(tabName = "timeseries", h2("Time series graph"),
            fluidRow(
              box(dygraphOutput("plot2", width = 500))
            )),
    tabItem(tabName = "mapping", h2("Heatmap of station usage"),
            fluidRow(
              box(
                title = "Start station or End station?",
                selectInput(inputId = "locInput", label="start/end",
                  choices = c("Start station", "End station")
                )
              )
            ),
            fluidRow(
              box(leafletOutput("plot3", width= 500))
            ))
    )
))

# run this #
server <- function(input, output) {
  output$plot1 <- renderPlot({
    x    <- citi[citi$gender == input$genderInput,][,"date"]
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Date",
         main = "Histogram of Citi bike usage")
    })
  # time series
    ts <- reactive({
      citi.ts <- citi %>%
        group_by(date) %>%
        summarize(count=n())
      tt <- as.ts(citi.ts[,2],start=1,end=31)
      return(tt)
    })
  output$plot2 <- renderDygraph({
    dygraph(ts()) %>%
      dyAxis("x", label = "Days of March", valueRange = c(1, 31)) %>%
      dyAxis("y", label = "Counts") %>%
      dyRangeSelector(height = 20)
  })
  startdat <- reactive({
    a <- citi %>%
      select(`start.station.id`,`start.station.latitude`,`start.station.longitude`) %>%
      unique()
    b <- citi %>%
      group_by(`start.station.id`) %>%
      summarise(count=n())
    abjoin<-left_join(a,b) %>%
      arrange(desc(count))
    colnames(abjoin) <- c("station.id","station.latitude","station.longitude","count")
    return(abjoin)
  })
  enddat <- reactive({
    a2<-citi %>%
      select(`end.station.id`,`end.station.latitude`,`end.station.longitude`) %>%
      unique()
    b2<-citi %>%
      group_by(`end.station.id`) %>%
      summarise(count=n())
    abjoin2<-left_join(a2,b2) %>%
      arrange(desc(count))
    colnames(abjoin2) <- c("station.id","station.latitude","station.longitude","count")
    return(abjoin2)
  })
  mapdat <- reactive({
    if(input$locInput == "Start station"){
      a <- startdat()
      return(a)
    }
    else {
      a <- enddat()
    }
    return(a)
  })
  output$plot3 <- renderLeaflet({
    leaflet(mapdat()) %>%
      addTiles() %>%
      setView(-73.93, 40.76, zoom = 11) %>%
      addHeatmap(lng = ~`station.longitude`, lat = ~`station.latitude`, intensity = ~count, blur = 20, max = 0.05, radius = 15)
  })

}

# This will launch the app #
shinyApp(ui, server)

