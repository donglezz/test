library(shiny)
source("./help.R")


function(input, output) {
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
deployApp()
