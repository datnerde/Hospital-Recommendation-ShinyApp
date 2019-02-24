## Load Packages -----
library("shiny")
library("leaflet")
library("dplyr")
library("RColorBrewer")
library("stringr")
library("parcoords")
library("ggplot2")
library("reshape2")
library("geosphere")
library("ggthemes")
library("formattable")
library("base64enc")
library("plotly")

#devtools::install_github("timelyportfolio/parcoords")




##Import Data -----
final_hos<-read.csv('/Users/zhongming/Documents/GitHub/shinny\ app/cleaned_data.csv',stringsAsFactors = F)

##1.Find Hospital -----
tab1 <- tabPanel("Find Your Hospital",
                 
                 #CSS file for page style
                 includeCSS("theme.css"),
                 tags$div(
                 leafletOutput("intermap",width="120%", height= "800px"),
                 absolutePanel(id = "controls", class ="City_Carrier_panel panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 60, left = "auto", right = 20,
                               bottom = "auto", width = 330, height = "auto",
                               h2("Hospital Selection"),
                               selectInput("state", label = "State", 
                                           choices = c("Select","AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN",
                                                       "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV",
                                                       "NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
                                                       "TX","UT","VT","VA","WA","WV","WI","WY"), selected = "Select"),
                               selectInput("type", label = "Disease Type",
                                           choices = c("Nervous system" ,"Respir-atory System","Circul-atory System",
                                                       "Digestive System" ,  "Hepato-biliary System & Pancreas"   ,
                                                       "Muscul-osk-eletal System & Connective Tissue",
                                                       "Skin, Subcut-aneous Tissue & Breast" ,
                                                       " Endocrine, Nutrit-ional & Metabolic System" ,
                                                       " Kidney & Urinary Tract" ,
                                                       " Blood, Blood Forming Organs & Immuno-logical Disorders",
                                                       " Infectious & Parasitic Disease & Disorders"  ,
                                                       " Mental Diseases & Disorders", "Alcoho-l/Drug Use or Induced Mental Disorders",
                                                       " Injuries, Poison & Toxic Effects of Drugs" ,
                                                       " Factors influe-ncing Health Status",
                                                       "Ear, Nose, Mouth & Throat"), selected = "Ear, Nose, Mouth & Throat")),
                               # selectInput("type", label = "Type", 
                               #             choices = c("Select","Acute Care Hospitals","Critical Access Hospitals","Childrens"), selected = "Select")),
                absolutePanel(id = "Pop_Panel", class = "City_Carrier_panel panel panel-default",top=60,draggable=TRUE,right="auto",bottom="auto",width = 330,height="auto",
                               radioButtons("care1",label = "Mortality",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 2, inline = T),
                               radioButtons("care2",label = "Safety of Care",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 2, inline = T),
                               radioButtons("care3",label = "Readmission Rate",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 2, inline = T),
                               radioButtons("care4",label = "Patient Experience",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 2, inline = T),
                               radioButtons("care5",label = "Effectiveness of Care",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 2, inline = T),
                               radioButtons("care6",label = "Timeliness of Care",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 2, inline = T),
                               radioButtons("care7",label = "Efficient Use of Medical Imaging",
                                            choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                                            selected = 2, inline = T))))
          

##2.Descriptive Statistics -----
tab2 <- navbarMenu("Hospital Statistics",
                  
                   tabPanel(title = "Total Cost",
                            h3("Total Cost (USD)",style="color:	white",align="center",offset = -1,astyle ="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 600px; opacity: 0.9; background-color: #ffffff;",
                                                column(width = 9, plotOutput("map")),
                                                column(width = 3,  selectInput("sub",
                                                                               label = "Choose a the sub directory",
                                                                               choices = unique(hospital$sub)),helpText("Select tyeps of dieases")))
                            )),
                   tabPanel(title = "Hospital Number",
                            h3("Number of Hospitals",style="color:	white",align="center",offset = -1,astyle ="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 600px; opacity: 0.9; background-color: #ffffff;",
                                                plotlyOutput("HosNumByState")))))
                                               


##3.Insturction -----
tab3 <-navbarMenu("Insturction",
    tabPanel(title="Measurements",
                fluidRow(
                  wellPanel(style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;",
                            h1("Introduction"),
                            p("Our group has created an app helping you to find the best hospitals based on your preference"),
                            h1("Measurements"),
                            strong("Cost"),
                            br(),
                            strong("Mortality"),
                            br(),
                            strong("Safety of care"),
                            br(),
                            strong("Readmission rate"),
                            br(),
                            strong("Patient experience"),
                            br(),
                            strong("Effectiveness of care"),
                            br(),
                            strong("Timeliness of care"),
                            br(),
                            strong("Efficient use of medical imaging")))),
    
    tabPanel(title="User Guide",
             fluidRow(
               wellPanel(style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;",
                            h1("User Guide"),
                            strong("Step1:"),p("Choose the state and types of dieases"),
                            br(),
                            strong("Step2:"),p("Choose your preferences of hospital"),
                            br(),
                            strong("Step3:"),p("Check the Medicare Assessment table for the basic information of all hospitals"),
                            br(),
                            strong("Step4:"),p("Click on the map to see the exact location of the hospital")))),
    
    tabPanel(title="Developers",
             fluidRow(
               wellPanel(style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;",
                         h1("Developers"),
                            strong("Ming Zhong, mz2692@columbia.edu")))))




## UI 
ui <- shinyUI(navbarPage(title = strong("Hospital Money Saver"),
                         tab1,
                         tab2,
                         tab3
))


