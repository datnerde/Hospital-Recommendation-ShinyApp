#load packages
packages.used=c("dplyr", "plotly", "shiny", "leaflet", "scales", 
                "lattice", "htmltools", "maps", "data.table", 
                "dtplyr", "mapproj", "randomForest", "ggplot2", "rpart")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(shiny)
library(leaflet)
library(scales)
library(lattice)
library(tidyverse)
library(htmltools)
library(maps)
library(plotly)
library(data.table)
library(dtplyr)
library(mapproj)
library(randomForest)
library(ggplot2)
library(rpart)

# calculate scores for a hospital 

calScore <- function(row,care.vec){
  # weight suggested for 7 criterion
  origin.weight <- c(11,11,11,11,2,2,2) 
  # care weight for 7 criterion
  care.weight <- origin.weight*care.vec/sum(origin.weight*care.vec)
  # hospital scores for 7 criterion
  criterion.score <- as.numeric(c(row[32:38]))
  
  temp <- ifelse(is.na(criterion.score),0,care.weight)
  update.weight <- temp/sum(temp)
  
  score <- update.weight*criterion.score
  return(sum(score,na.rm = TRUE))
}

# switch overall rating

orswitch <- function(rating){
  if(is.na(rating)){return("Not Available")}
  else {return(as.numeric(rating))}
}

#load data

f<-read.csv('/Users/zhongming/Library/Containers/com.tencent.xinWeChat/Data/Library/Application\ Support/com.tencent.xinWeChat/2.0b4.0.9/5d0d51d52f72e0e3697907110ffc0230/Message/MessageTemp/313cb523c01109e8cbce2d03f1fc8c5e/File/cleaned_data.csv',stringsAsFactors = F)

shinyServer(function(input, output){
  
  state<-reactive({state<-input$state})
  type <- reactive({type <- input$type})
  
  care1 <- reactive({input$care1}) # Mortality
  care2 <- reactive({input$care2}) # Safety of care
  care3 <- reactive({input$care3}) # Readmission rate
  care4 <- reactive({input$care4}) # Patient experience
  care5 <- reactive({input$care5}) # Effectiveness of care
  care6 <- reactive({input$care6}) # Timeliness of care
  care7 <- reactive({input$care7}) # Efficient use of medical imaging
  
  v1<-reactive({
    if (state() == "Select") {v1<-f} 
    else {
      selectstate<-state()
      v1<- f %>% filter(Provider.State == state())}})  
  
  v2 <- reactive({
    if (type() == "Select") {v2 <- v1()}
    else{ 
          selecttype <- type()
          v2 <- v1() %>% filter(mdc == type())}})
  
  care.origin <- reactive(care.origin <- c(care1(),care2(),care3(),
                                           care4(),care5(),care6(),care7()))
  # Dataset for the selected state
  data.state <- reactive(data.state <- v2())
  
  # Care vector for 7 criterion
  care.vec <- reactive(as.numeric(care.origin()))
  
  # Scores of hospitals in the selected state
  score <- reactive(apply(data.frame(data.state()),1,calScore,care.vec = care.vec()))
  
  # orders for hospitals
  ord <- reactive(order(score(),decreasing = TRUE))
  
  # ranks for hospitals
  rk <- reactive(floor(frankv(score(),order = -1,ties.method = "min")))
  
  v3 <- reactive({v3 <- cbind(v2(),Order = ord(),Rank = rk())})
  
  #Icon for the markers
  hospIcons <- iconList(emergency = makeIcon(iconUrl = 'https://github.com/Grandeurwang/class-activity-1/blob/master/emergency_icon.png', iconWidth = 25, iconHeight =30),
                        critical = makeIcon(iconUrl = "https://github.com/Grandeurwang/class-activity-1/blob/master/critical_icon.png", iconWidth = 25, iconHeight =30),
                        children = makeIcon(iconUrl = "https://github.com/Grandeurwang/class-activity-1/blob/master/children_icon.png", iconWidth = 25, iconHeight =30))
  
  html_legend <- "<img src='https://github.com/Grandeurwang/class-activity-1/blob/master/emergency_icon.png'>Acute Care Hospitals<br/>
  <img src='https://github.com/Grandeurwang/class-activity-1/blob/master/critical_icon.png'>Critical Access Hospitals<br/>
  <img src='https://github.com/Grandeurwang/class-activity-1/blob/master/children_icon.png'>Children Hospitals"
  
  output$map <- renderLeaflet({
    content <- paste(sep = "<br/>",
                     paste("<font size=1.8>","<font color=green>","<b>",v3()$Provider.Name,"</b>"),
                     paste("<font size=1>","<font color=black>",v3()$Provider.Street.Address),
                     paste(v3()$Provider.City, v3()$Provider.State, v3()$Provider.Zip.Code, sep = " "),  
                     paste("(",substr(v3()[ ,"Phone.Number"],1,3),") ",substr(v3()[ ,"Phone.Number"],4,6),"-",substr(v3()[ ,"Phone.Number"],7,10),sep = ""), 
                     paste("<b>","Hospital Type: ","</b>",as.character(v3()$Hospital.Type)),  
                     paste("<b>","Provides Emergency Services: ","</b>",as.character(v3()[ ,"Emergency.Services"])),
                     paste("<b>","Overall Rating: ","</b>", as.character(v3()[ ,"Hospital.overall.rating"])),
                     paste("<b>","Personalized Ranking: ","</b>",v3()$Rank),
                     paste("<b>","Average money for each discharge of chosen disease: ", "</b>",as.character(v3()[ ,"averagepay_MDC_hos_per_discharge"])))
      
    mapStates = map("state", fill = T, plot = FALSE)
    leaflet(data = mapStates) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = 0.1), stroke = FALSE,
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE)) %>%
      addMarkers(v2()$lon, v2()$lat, popup = content, icon = hospIcons[v2()$TF], clusterOptions = markerClusterOptions())%>%
      addControl(html = html_legend, position = "bottomleft")
  })
  
})