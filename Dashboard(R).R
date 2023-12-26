#GLOBAL TERRORISM DATA#
#OBJECTIVE:Detect pattern or fluctuation in the terrorism rate .

#####calling the libraries####
#install.packages("leaflet.extras")

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr) 
library(readr)
library(tidyverse)
library(leaflet.extras)






#### Reading the data file####
gtd <- read_csv("C:\\Users\\DELL\\Downloads\\gtd_clean.csv")
####understanding the dataset####
View(gtd)
is.null(gtd)
dim(gtd) #shape of the dataset 

## using dplyr to group the target type and then grouping them year wise 
gtd_top_targets <- gtd %>%
  filter(target_type_txt %in% c('Citizens', 'Military', 'Police',
                                'Government', 'Business', 'Religious Institutions', 'Airports','Journalists'))

gtd_19_17 <- gtd %>%
  filter(year %in% c(1970, 2017))


####Dashboard header carrying the title of the dashboard####
header <- dashboardHeader(title = "Global Terrorism Dashboard")  


#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
 sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))))


#body if the dashboard 

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3"))


frow2 <- fluidRow(
  box(title = "Comparison of Attacks overtime"
      ,status = "primary"
      ,solidHeader = TRUE
      ,collapsible = TRUE
      ,leafletOutput("mymap", height = "300px")
      ,sliderInput("slider", "Years:", 1970, 2017, 2000))
  ,box(
    title = "Number of Kills by Attack Type"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Killbyattacktype", height = "300px"),
    selectInput("selector", h3("Select Region"),
                choices = list("Australasia" = "Australasia & Oceania", "Central America" = "Central America & Caribbean",
                               "Central Asia" = "Central Asia", "East Asia" = "East Asia", "Eastern Europe" = "Eastern Europe",
                               "MENA" = "Middle East & North Africa", "North America" = "North America",
                               "South America" = "South America", "South Asia" = "South Asia", "Southeast Asia" = "Southeast Asia",
                               "Sub-Saharan Africa" = "Sub-Saharan Africa", "Western Europe" = "Western Europe"), selected = 1)))

frow3 <- fluidRow(
  box(
    title = "Who were the main targets"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("targetkilled", height = "300px")
    ,radioButtons("radio", h3("Select Year"),
                  choices = list("1970" = 1970, "1975" = 1975, "1980" = 1980, "1985" = 1985,
                                 "1990" = 1990, "1995" = 1995, "2000" = 2000, "2005" = 2005,
                                 "2010" = 2010,"2015" = 2015, "2017" = 2017),selected = 1970, inline = TRUE))
  ,box(
    title = "Number of kills per Region"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("killbyregion", height = "300px")
    ,radioButtons("yearsradio", h3("Select Year"), choices = list("1970" = 1970, "1975" = 1975,
                                                                  "1980" = 1980, "1985" = 1985, "1990" = 1990, "1995" = 1995, "2000" = 2000, "2005" = 2005,
                                                                  "2010" = 2010,"2015" = 2015, "2017" = 2017), selected = 1970, inline = TRUE)))





# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)    


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Global Terrorism Statistics Dashboard', header,sidebar, body, skin='yellow')    



# create the server functions for the dashboard  
server <- function(input, output) {
  
  #some data manipulation to derive the values
  total_kills <- sum(gtd$nkill)
  countries <- gtd %>% group_by(country) %>% summarise(value = sum(nkill)) %>% filter(value==max(value))
  weapon.type <- gtd %>% group_by(weapon_type) %>% summarise(value = sum(nkill)) %>% filter(value==max(value))
  attacker <- gtd %>% group_by(nationality) %>% summarise(value = sum(nkill)) %>% filter(value==max(value))
  
  
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(countries$value, format="d", big.mark=',')
      ,paste('Top Country:',countries$country)
      ,color = "purple")
    
    
  })

  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total_kills, format="d", big.mark=',')
      ,paste('Top nationality:',attacker$nationality)
      ,color = "green")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(weapon.type$value, format="d", big.mark=',')
      ,paste('Top Weapon Used:',weapon.type$weapon_type)
      ,color = "black")
    
  })
  
  
  #creating the plotOutput content
  datasetInput <- reactive({
    
    filter(gtd, gtd$year <= input$yearsradio) #%>% arrange(desc(Sales))
    
  })
  
  output$killbyregion <- renderPlot({
    data_filter <- datasetInput()
    
    ggplot(data = data_filter,
           aes(x=region, y=nkill)) +
      geom_bar(position = "dodge", stat = "identity", fill="#FF5733")+ ylab("Count of kills") +
      xlab("Region") + theme(legend.position="bottom"
                             ,plot.title = element_text(size=15, face="bold"), axis.text.x=element_text(angle=90)) +
      ggtitle("Kills per Region")
  })
  
  datasetregion <- reactive({
    
    filter(gtd, gtd$region <= input$selector)
  })
  
  output$Killbyattacktype <- renderPlot({
    data_region <- datasetregion()
    
    ggplot(data = data_region,
           aes(x=attacktype_txt, y=nkill)) +
      geom_bar(position = "dodge", stat = "identity", fill="#007BFF") + ylab("Count of Kills") +
      xlab("Attack Type") + theme(legend.position="bottom"
                                  ,plot.title = element_text(size=15, face="bold")) +
      ggtitle("Kills by Type of Attack")
  })
  
  datasettarget <- reactive({
    
    filter(gtd_top_targets, gtd_top_targets$year %in% input$radio)
  })
  
  output$targetkilled <- renderPlot({
    data_target <- datasettarget()
    
    ggplot(data = data_target,
           aes(y=target_type_txt, x=nkill,color=target_type_txt)) +
      geom_bar(position = "dodge", stat = "identity",fill="yellow") + xlab("Count of Kills") + ylab("Target Type") +
      theme(legend.position="bottom",plot.title = element_text(size=15, face="bold")) +
      ggtitle("Type of Target") + coord_polar(theta="y")
  })
  
  datasetyears <- reactive({
    
    filter(gtd, gtd$year %in% input$slider)
  })
  ?filter
  
  #create the map
  output$mymap <- renderLeaflet({
    data_years <- datasetyears()
    
    leaflet(data_years) %>%
      setView(lng = 43.6793, lat = 33.2232, zoom = 2) %>%
      addTiles() %>%
      addCircles(data = data_years, lat = ~ latitude, lng = ~ longitude, weight = 1,
                 radius = ~sqrt(nkill)*25000, popup = ~as.character(nkill),
                 label = ~as.character(paste0("Kills: ", sep = " ", nkill)), fillOpacity = 0.5)
  })
  
}


shinyApp(ui, server) #this helps te shiny app to run all the codes on the browser.



