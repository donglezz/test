library(shiny)
library(shinydashboard)
library(plotly)
library(dygraphs)
library(leaflet)
source("./help.R")

dashboardPage(
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