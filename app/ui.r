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
library(shinydashboard)

#devtools::install_github("timelyportfolio/parcoords")




##Import Data -----
#final_hos<-read.csv('E:/GitHub/shiny app/cleaned_data.csv',stringsAsFactors = F)
load("./hos.RData")
load("./importance.RData")
load("./df.RData")
load("./hospital_ratings.RData")
load("./plot1data.RData")
load("./f.RData")
##1.Find Hospital -----
tab1 <- tabPanel("Find Your Hospital",
                 
                 #CSS file for page style
                 includeCSS("theme.css"),
                 tags$div(
                 leafletOutput("intermap",width="100%", height= "600px"),
                 absolutePanel(id = "controls", class ="City_Carrier_panel panel panel-default", fixed =F,
                               draggable =FALSE, top = 80, left = "auto", right = 20,
                               bottom = "auto", width = 300, height = "auto",
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
                                                       "Ear, Nose, Mouth & Throat"), selected = "Ear, Nose, Mouth & Throat"),
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
                                            selected = 2, inline = T)),
                 tabBox(width = 12,
                        tabPanel('Personalized Ranking',
                                 dataTableOutput("tablerank"),
                                 tags$style(type="text/css", '#myTable tfoot {display:none;}')
                        ))))
          

##2.Descriptive Statistics -----
tab2 <- navbarMenu("Hospital Statistics",
                  
                   tabPanel(title = "Total Cost",
                            h3("Total Cost (USD)",style="color:	black",align="center",offset = -1,astyle ="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 600px; opacity: 0.9; background-color: #ffffff;",
                                                column(width = 9, plotlyOutput("map")),
                                                column(width = 3,  selectInput("sub",
                                                                               label = "Choose a the sub directory",
                                                                               choices = unique(hospital$sub)),helpText("Select tyeps of dieases")))
                            )),
                   
                   tabPanel(title = "Total Cost by Ownership",
                            h3("Total Cost (USD)",style="color:	black",align="center",offset = -1,astyle ="font-family:helvetica;"),
                            fluidRow( wellPanel(style = "overflow-y:scroll;  height: 600px; opacity: 0.9; background-color: #ffffff;",
                                                column(width = 9, plotlyOutput("map1")),
                                                column(width = 3,  selectInput("sub1",
                                                                               label = "Choose a the sub directory",
                                                                               choices = unique(hospital$Hospital.Ownership)),helpText("Select tyeps of dieases")))
                            )),
                   
                   tabPanel(title = "Hospital Number",
                            h3("Number of Hospitals",style="color:	black",align="center",offset = -1,astyle ="font-family:helvetica;"),
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
                            br(),
                            strong("Mortality:"),p("the death rate of patients"),
                            br(),
                            strong("Safety of care:"),p(" the rate of certain complications and infections"),
                            br(),
                            strong("Readmission rate:"),p("the rate of unplanned readmission after treatment"),
                            br(),
                            strong("Patient experience"),p("how well patients feel during treatment, surgery and hospitalization"),
                            br(),
                            strong("Effectiveness of care"),p("how appropriately patients are treated"),
                            br(),
                            strong("Timeliness of care"),p("the time patients waiting"),
                            br(),
                            strong("Efficient use of medical imaging"),p("how efficiently the hospitals using medical imaging such as MRI and CT scans"),
                            br(),
                            p("For more information, click the link below:"),
                            a("Here",href = "https://www.medicare.gov/hospitalcompare/Data/Measure-groups.html")))),
    
    tabPanel(title="User Guide",
             fluidRow(
               wellPanel(style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;",
                            h1("User Guide"),
                            strong("Step1:"),p("Choose the state and types of dieases"),
                            br(),
                            strong("Step2:"),p("Choose your preferences of hospital"),
                            br(),
                            strong("Step3:"),p("Check the Personalized Ranking table for the basic information of all hospitals"),
                            br(),
                            strong("Step4:"),p("Click on the map to see the exact location of the hospital"),
                            br(),
                            strong("Step5:"),p("You can also locate yourself by clicking the location button on the left side of the map")))),
    tabPanel(title="Developers",
             fluidRow(
               wellPanel(style = "overflow-y:scroll; height: 600px; opacity: 0.9; background-color: #ffffff;",
                         h1("Developers"),
                            strong("Cai, Zongbo, zc2455@columbia.edu"), br(),
                            strong("Li, Jingyue, jl5283@columbia.edu"), br(),
                            strong("Wang, Guanren, gw2380@columbia.edu"), br(),
                            strong("Zhong, Ming, mz2692@columbia.edu")))))




## UI 
ui <- shinyUI(navbarPage(title = strong("Health Intelligence"),
                         tab1,
                         tab2,
                         tab3
))


