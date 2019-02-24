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

calScore <- function(row,care.vec){
  # weight suggested for 7 criterion
  origin.weight <- c(11,11,11,11,2,2,2) 
  # care weight for 7 criterion
  care.weight <- origin.weight*care.vec/sum(origin.weight*care.vec)
  # hospital scores for 7 criterion
  criterion.score <- as.numeric(c(row[c(15,17,19,21,23,25,27)]))
  
  temp <- ifelse(is.na(criterion.score),0,care.weight)
  update.weight <- temp/sum(temp)
  
  score <- update.weight*criterion.score
  return(sum(score,na.rm = TRUE))
}

# switch payment to dollar signs

payswitch <- function(payment){
  if(is.na(payment)) {return("Not Avaliable")}
  else {if(payment<=1.5) {return("$")}
    else{if(payment<2.5) {return("$$")}
      else{return("$$$")}}}
}

# switch overall rating

orswitch <- function(rating){
  if(is.na(rating)){return("Not Available")}
  else {return(as.numeric(rating))}
}

#####server#########
shinyServer <- function(input, output) {
  load("./hos.RData")
  load("./importance.RData")
  load("./df.RData")
  load("./hospital_ratings.RData")
  load("plot1data.RData")
  hospital <- read.csv("/Users/zhongming/Downloads/temp2/Cleaned\ Payment.csv")
# ##########plot 1##########
#   output$HosNumByState <- renderPlotly({
#     c <- ggplot(HosNumByState, aes(x = State, y = Freq)) +
#       geom_bar(stat = "identity", aes(fill = HosNumByState$Freq)) +
#       labs(title = "Hospital Number by State", x = "State", y = NULL) +
#       theme_classic()+
#       theme(axis.text.x = element_text(angle = 90, size = 8)) +
#       theme(plot.title = element_text(hjust = 0.5, vjust = 1)) +
#       scale_y_continuous(expand = c(0,0)) +
#       theme(plot.margin = unit(c(1,1,1,1), "cm"))
#     ggplotly(c) %>% layout(height = 700, width = 1000)
#     c + scale_fill_continuous(name = "Frequency")
#   }
#   )
#   library(dplyr)
#   library(plyr)
#   library(choroplethr)
#   myFunction <- function(hospital, topic) {
#     output <- hospital %>%
#       filter(sub %in% as.vector(topic)) %>%
#       ddply(.(Provider.State), summarise,
#             expected_cost =
#               sum(as.vector(as.numeric(Total.Discharges)) * (as.vector(as.numeric(Average.Covered.Charges)) +
#                                                                as.vector(as.numeric(Average.Total.Payments)))) /
#               sum(as.vector(as.numeric(Total.Discharges)))) %>%
#       select(Provider.State, expected_cost)
#     return(output)
#   }
# ##############plot2################
#   output$map <- renderPlot({
#     df <- myFunction(hospital_payment, input$sub)
#     colnames(df) <- c("region","value")
#     df$region <- state.name[match(df$region,state.abb)]
#     df$region[is.na(df$region)] <- "district of columbia"
#     df$region <- tolower(df$region)
#     state_choropleth(df, legend = "Cost (USD)")
#   })
  
########map###############
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
    if (state() == "Select") {v1<-hos} 
    else {
      selectstate<-state()
      v1<- hos %>% filter(State == state())}})  
  
  v2 <- reactive({
    if (type() == "Select") {v2 <- v1()}
    else{
      selecttype <- type()
      v2 <- v1() %>% filter(Hospital.Type == type())}})
  
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
  
  
  # Care vector for 7 criterion
  care.vec <- reactive(as.numeric(care.origin()))
  
  # Scores of hospitals in the selected state
  score <- reactive(apply(data.frame(v2()),1,calScore,care.vec = care.vec()))
  
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
  
  output$intermap <- renderLeaflet({
    content <- paste(sep = "<br/>",
                     paste("<font size=1.8>","<font color=green>","<b>",v3()$Hospital.Name,"</b>"),
                     paste("<font size=1>","<font color=black>",v3()$Address),
                     paste(v3()$City, v3()$State, v3()$ZIP.Code, sep = " "),  
                     paste("(",substr(v3()[ ,"Phone.Number"],1,3),") ",substr(v3()[ ,"Phone.Number"],4,6),"-",substr(v3()[ ,"Phone.Number"],7,10),sep = ""), 
                     paste("<b>","Hospital Type: ","</b>",as.character(v3()$Hospital.Type)),  
                     paste("<b>","Provides Emergency Services: ","</b>",as.character(v3()[ ,"Emergency.Services"])),
                     
                     paste("<b>","Overall Rating: ","</b>", as.character(v3()[ ,"Hospital.overall.rating"])),
                     paste("<b>","Personalized Ranking: ","</b>",v3()$Rank))
    
    
    mapStates = map("state", fill = TRUE, plot = FALSE)
    leaflet(data = mapStates) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
      addMarkers(v2()$lon, v2()$lat, popup = content, icon = hospIcons[v2()$TF], clusterOptions = markerClusterOptions())
  })
}