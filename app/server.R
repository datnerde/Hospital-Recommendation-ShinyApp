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
library(plyr)
library(choroplethr)
library(shinydashboard)

calScore <- function(row,care.vec){
  # weight suggested for 7 criterion
  origin.weight <- c(11,11,11,11,2,2,2) 
  # care weight for 7 criterion
  care.weight <- origin.weight*care.vec/sum(origin.weight*care.vec)
  # hospital scores for 7 criterion
  criterion.score <- as.numeric(c(row[row[32:38]]))
  
  temp <- ifelse(is.na(criterion.score),0,care.weight)
  update.weight <- temp/sum(temp)
  
  score <- update.weight*criterion.score
  return(sum(score,na.rm = TRUE))
}

# switch payment to dollar signs

payswitch <- function(payment){
  if(is.na(payment)) {return("Not Avaliable")}
  else {if(payment<=4328) {return("$")}
    else{if(payment<=5837) {return("$$")}
      else{if(payment<=8383) {return("$$$")}
        else{return("$$$$")}}}}
}

# switch overall rating

orswitch <- function(rating){
  if(is.na(rating)){return("Not Available")}
  else {return(as.numeric(rating))}
}

#####load##########
load("./hos.RData")
load("./importance.RData")
load("./df.RData")
load("./hospital_ratings.RData")
load("./plot1data.RData")
load("./f.RData")
#####server#########
shinyServer <- function(input, output) {
  
##########plot 1##########
  output$HosNumByState <- renderPlotly({
    c <- ggplot(HosNumByState, aes(x = State, y = Freq)) +
      geom_bar(stat = "identity", aes(fill = HosNumByState$Freq)) +
      labs(title = "Hospital Number by State", x = "State", y = NULL) +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, size = 8)) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 1)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
    ggplotly(c) %>% layout(height = 700, width = 1000)
    c + scale_fill_continuous(name = "Frequency")
  }
  )

  ##############plot2################
  output$map <- renderPlotly({
    library(shiny)
    library(plotly)  
    library(dplyr)
    library(plyr)
    library(choroplethr)
    myFunction <- function(hospital, topic) {
      output <- hospital %>%
        filter(sub %in% as.vector(topic)) %>%
        ddply(.(Provider.State), summarise, 
              expected_cost = 
                sum(as.vector(as.numeric(Total.Discharges)) * (as.vector(as.numeric(Average.Covered.Charges)) +
                                                                 as.vector(as.numeric(Average.Total.Payments)))) / 
                sum(as.vector(as.numeric(Total.Discharges)))) %>%
        select(Provider.State, expected_cost)
      return(output)
    }
    df <- myFunction(hospital_payment, input$sub)
    colnames(df) <- c("region","value")
    # df$region <- state.name[match(df$region,state.abb)]
    df$region[is.na(df$region)] <- "DC"
    #df$region <- tolower(df$region)
    df$hover <- with(df, paste("state",region, '<br>', "value", value))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    plot_geo(df, locationmode = 'USA-states') %>%
      add_trace(
        z = ~value, text = ~hover, locations = ~region,
        color = ~value, colors = 'Blues'
      ) %>%
      colorbar(title = "Millions USD") %>%
      layout(
        geo = g
      )
  })
  
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
    if (state() == "Select") {v1<-f%>%
      filter(Mortality>=care1())%>%
      filter(Safety>=care2())%>%
      filter(Readmission>=care3())%>%
      filter(Patient.experience>=care4())%>%
      filter(Effectiveness>=care5())%>%
      filter(Timeliness>=care6())%>%
      filter(Efficient.use.of.medical.image>=care7())
             } 
    else {
      selectstate<-state()
      v1<- f %>% filter(Provider.State == state())%>%
      filter(Mortality>=care1())%>%
      filter(Safety>=care2())%>%
      filter(Readmission>=care3())%>%
      filter(Patient.experience>=care4())%>%
      filter(Effectiveness>=care5())%>%
      filter(Timeliness>=care6())%>%
      filter(Efficient.use.of.medical.image>=care7())
      }
    })
  
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
  hospIcons <- iconList(emergency = makeIcon("emergency_icon.png", iconWidth = 25, iconHeight =30),
                        critical = makeIcon("critical_icon.png", iconWidth = 25, iconHeight =30),
                        children = makeIcon("children_icon.png", iconWidth = 20, iconHeight =30))
  
                        
  output$intermap <- renderLeaflet({
    content <- paste(sep = "<br/>",
                     paste("<font size=4>","<font color=green>","<b>",v3()$Provider.Name,"</b>"),
                     paste("<font size=1>","<font color=black>",v3()$Address),
                     paste(v3()$City, v3()$State, v3()$ZIP.Code, sep = " "),  
                     paste("(",substr(v3()[ ,"Phone.Number"],1,3),") ",substr(v3()[ ,"Phone.Number"],4,6),"-",substr(v3()[ ,"Phone.Number"],7,10),sep = ""), 
                     paste("<b>","Hospital Type: ","</b>",as.character(v3()$Hospital.Type)),  
                     paste("<b>","Hospital Ownership: ","</b>",as.character(v3()$Hospital.Ownership)), 
                     paste("<b>","Provides Emergency Services: ","</b>",as.character(v3()[ ,"Emergency.Services"])),
                     paste("<b>","Overall Rating: ","</b>", as.character(v3()[ ,"Hospital.overall.rating"])),
                     paste("<b>","Personalized Ranking: ","</b>",v3()$Rank),
                     paste("<b>","Average cost of chosen disease for each discharge: ", "</b>",as.character(v3()[ ,"averagepay_MDC_hos_per_discharge"])))
    
    
    mapStates = map("state", fill = TRUE, plot = FALSE)
    leaflet(data = mapStates) %>% 
      addProviderTiles(providers$HikeBike.HikeBike) %>%
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE,
        position = 'bottomleft') %>%
      addPolygons(stroke = T,color = 'grey',weight = 1,fillOpacity = 0,
                  highlightOptions = highlightOptions(color = "black", weight = 2,bringToFront = TRUE)) %>%
      addMarkers(v2()$lon, v2()$lat, popup = content, icon = hospIcons[v2()$TF], clusterOptions = markerClusterOptions())%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
  output$tablerank = renderDataTable({
    rankedtable <- cbind(v3()$Rank[ord()],v3()[ord(),c(2, 3, 4, 11,9)])
    rankedtable$averagepay_MDC_hos_per_discharge <- apply(data.frame(rankedtable$averagepay_MDC_hos_per_discharge),1,payswitch)
    colnames(rankedtable) <- c("Rank","Hospital Name","Address","City",
                               "TEL","COST")
    rankedtable
  },options = list(orderClasses = TRUE, iDisplayLength = 5, lengthMenu = c(5, 10, 15, 20)))
}