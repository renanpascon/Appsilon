library(data.table)
library(tidyverse)
library(bit64)
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(leaflet)
library(geosphere)
library(lubridate)

# loading the data
data <- fread("data/ships.csv", encoding = "UTF-8")

#removing non used columns 
data <- data[,-c(3,4,5,10,15,17,19)]
data$SHIP_ID <- as.character(data$SHIP_ID)

#separate all the last observations for each vessel
lastData <- data %>% group_by(ship_type,SHIP_ID,SHIPNAME) %>% filter(DATETIME == max(DATETIME))

# all vessels by type, ID and name
vessels <- data %>% select(ship_type,SHIP_ID,SHIPNAME) %>% unique()


# UI Module for the input selection and rendering the map
vesselUI <- function(id){
    splitLayout(cellWidths = c("20%", "35%","20%","25%"), style = "height: fit-content;",
     dropdown_input(NS(id,"type"),  unique(vessels$ship_type) , default_text = "TYPE" , type = "search dropdown"),
     dropdown_input(NS(id,"shipname"),  choices = NULL ,default_text = "NAME", type = "search dropdown"),
     dropdown_input(NS(id,"shipid"),  choices = NULL ,default_text = "ID", type = "search dropdown"),
     action_button(NS(id,"render"),"Render Map")
      )
}

# Filter vessel by name+
filterID <- function(name){
  vselect <- vessels %>% filter(SHIPNAME == name) 
  vselect$SHIP_ID %>% unique()
}

# Filter vessel by type
filterName <- function(type){
  vselect <- vessels %>% filter(ship_type == type)
  vselect$SHIPNAME %>% unique()
}


# Server Module for vessel selection
vesselServerType <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$type, {
      update_dropdown_input (session, "shipname", choices = filterName(input$type))
    })
  })
}

# Server Module for vessel selection
vesselServerName <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$shipname, {
      update_dropdown_input (session, "shipid", choices = filterID(input$shipname))
    })
    
  })
}


# Function to find longest sailed distance
distanceF <- function(data){
  distance <- data.frame()
  #rearranging data by chronological order
  data$DATETIME <- as_datetime(data$DATETIME) 
  data <- data %>% arrange(-desc(DATETIME))
  
  #using distm function to find distance in the earth sphere by long and lat
  for(i in 1:(nrow(data)-1)){
  distance <- rbind(distance,distm (c(data$LON[i], data$LAT[i]), c(data$LON[i+1], data$LAT[i+1]), fun = distHaversine))
  }
  
  #creating distance vector, first observation doesn't count
  data$distance <- c(0,distance$V1)
  
  #selecting maximum distance
  maxdistance <- data %>% filter(distance == max(distance))
  if (nrow(maxdistance) > 1){
    #selecting most recent if there is any tie in the maximum distance
    maxdistance <- maxdistance %>% filter(DATETIME == max(DATETIME))
  }
  
  #row when happens the observation
  when <- which(data$distance == maxdistance$distance & data$DATETIME == maxdistance$DATETIME)
  
  #return 2 rows beginning and end of the observation
  return(data[c(when-1, when),])
}

# Function that return only the value of longest distance
distanceNumber <- function(data){
  distance <- data.frame()
  data$DATETIME <- as_datetime(data$DATETIME) 
  data <- data %>% arrange(-desc(DATETIME))
  for(i in 1:(nrow(data)-1)){
    distance <- rbind(distance,distm (c(data$LON[i], data$LAT[i]), c(data$LON[i+1], data$LAT[i+1]), fun = distHaversine))
  }
  round(max(distance))
}

#UI module calling the map
mapUI <- function(id){
  fluidRow(
    leafletOutput(NS(id, "map"),height = 500)
  )}

# Function that does the map logic
mapfunction <- function(shipID,shipName,shipType){
  
  #if no vessel is selected
  
  #adding note to the map
  if (is.null(c(shipID,shipName,shipType))){
    tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(0%,0%);
    position: relative;
    right: 0%;
    text-align: center;
    padding-left: 5px;
    padding-right: 5px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
    width: fit-content;
    margin-top: 10px;
  }
"))
    
    title <- tags$div(
      tag.map.title, HTML(paste0("Last Observed Position"))
      
    ) 
    
    # text for the hover
    hoverText <- paste0("Name: ",lastData$SHIPNAME, ' <i class="',tolower(lastData$FLAG),' flag"></i>' ,"<br>",
                        "Type: ", lastData$ship_type, "<br>",
                        "Length: ", lastData$LENGTH, " m <br>",
                        "DeadWeight: ", lastData$DWT, " tones")
    
    # palette for vessel type
    pal <- colorFactor(palette = c("#292f36","#4ecdc4","#f7fff7","#ff6b6b","#ffe66d","#a5668b","#006400","#5a189a","#9f86c0"),
                       levels = unique(vessels$ship_type))
    
    # map
    map <- leaflet(lastData) %>%
      addCircleMarkers(~LON,~LAT,
                       color = "grey",
                       opacity = 1,
                       weight = 1,
                       popup = hoverText,
                       fillColor = ~pal(ship_type),
                       fillOpacity = 0.8) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addLegend(position = "bottomright",pal,values = lastData$ship_type, opacity = 0.8,title = "Ship Types") %>%
      addControl(title, position = "topright", className="map-title")
  } else {
    
    # after the vessel is selected
    data <- data %>% filter(ship_type == shipType & SHIP_ID == shipID & SHIPNAME == shipName)
    
    # calling the longest distance function 
    dataMax <- distanceF(data)
    
    # adding note to the map
    tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(0%,0%);
    position: relative;
    left: 0%;
    text-align: center;
    padding-left: 10px;
    padding-right: 5px;
    background: rgba(255, 255, 255, 0.75);
    font-weight: 500;
    font-size: 13px;
    width: fit-content;
    margin-top: 1vh;
  }
"))
    
    title <- tags$div(
      tag.map.title, HTML(paste0('<i class="ship icon"></i>',dataMax$SHIPNAME[1],
                                 ' <i class="',tolower(dataMax$FLAG)[1],' flag"></i>' ,"<br>",
                                 "Type: ", dataMax$ship_type[1], "<br>",
                                 "Length: ", dataMax$LENGTH[1], " m <br>",
                                 "DeadWeight: ", dataMax$DWT[1], " tones <br>"))
      
    ) 
    
    # text for the hover of longest distance
    hoverText <- paste0("Name: ",dataMax$SHIPNAME, ' <i class="',tolower(dataMax$FLAG),' flag"></i>' ,"<br>",
                        "Destination: ", dataMax$DESTINATION,"<br>",
                        "Time: ", dataMax$DATETIME)
   
    # text for the hover of the full path of the vessel
    hoverTextPath <- paste0("Name: ",data$SHIPNAME, ' <i class="',tolower(data$FLAG),' flag"></i>' ,"<br>",
                            "Time: ", data$DATETIME)
    # map
    map <- leaflet(dataMax) %>% 
      addCircleMarkers(group = "Path",data$LON,data$LAT,
                       color = "black",
                       opacity = 1,
                       weight = 1,
                       fillColor = "grey",
                       fillOpacity = 1,
                       popup = hoverTextPath) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(group = "Max Distance",~LON,~LAT,popup = hoverText) %>%
      addLayersControl(
        overlayGroups = c("Max Distance","Path"),options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup("Path") %>% 
      addControl(title, position = "topright", className="map-title")
  }
  
}


# Server Module for the map, inputs are filtered when clicking render button
mapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    shipID <- reactiveVal(NULL)
    shipName <- reactiveVal(NULL)
    shipType <- reactiveVal(NULL)
    observeEvent(input$render,{
      shipID <- shipID(input$shipid)
      shipName <- shipName(input$shipname)
      shipType <- shipType(input$type)
    })
    observe({
    output$map <- renderLeaflet(mapfunction(shipID(),shipName(),shipType()))
    })
  })
}

# UI module for the note, the value box 
noteUI <- function(id){
  value_box_output(NS(id, "note"))
}

# formatting numbers
formatNum <- function(x, digits = 3, transform = identity, Mil = ".", Decimal = ",") {
  format(round(transform(x), digits), trim = TRUE, scientific = FALSE, big.mark = Mil, decimal.mark = Decimal)
}

# Function that generates the values for the value box
valueBoxF <- function(shipID,shipName,shipType,type){
  #if no vessel is selected
  if (is.null(c(shipID,shipName,shipType))){
    if (type == "text"){
    "Total Vessels"
    } else {
      nrow(vessels)
    }
  } else {
    # if vessel is selected
    if (type == "text"){
      "Longest Distance Sailed (mts)"
    } else {
      data <- data %>% filter(ship_type == shipType & SHIP_ID == shipID & SHIPNAME == shipName)
      #calling longest distance function that returns only a value
      paste0(formatNum(distanceNumber(data)))
      

      }
  }
}

# server module for the note, value box , reacts to the render button
noteServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    shipID <- reactiveVal(NULL)
    shipName <- reactiveVal(NULL)
    shipType <- reactiveVal(NULL)
    observeEvent(input$render,{
      shipID <- shipID(input$shipid)
      shipName <- shipName(input$shipname)
      shipType <- shipType(input$type)
    })

    output$note <- render_value_box({
      value_box(valueBoxF(shipID(),shipName(),shipType(),"text"), value =  valueBoxF(shipID(),shipName(),shipType(),"value"),size = "tiny",icon("ship")
)
    })
      
  })
}



# UI module for the title of the app, or vessel selected
titleUI <- function(id){
  textOutput(NS(id,"title"))
}

# server module for the title, reacts to the render button
titleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    shipID <- reactiveVal(NULL)
    shipName <- reactiveVal(NULL)
    shipType <- reactiveVal(NULL)
    observeEvent(input$render,{
      shipID <- shipID(input$shipid)
      shipName <- shipName(input$shipname)
      shipType <- shipType(input$type)
    })
    output$title <- renderText({
      paste0(titleFoo(shipID(),shipName(),shipType()))
      
    })
    
  })
}


# function that generate the title's text
titleFoo <- function(shipID,shipName,shipType){
  if (is.null(c(shipID,shipName,shipType))){
    paste0("All Vessels")
  } else {
    lastData <- lastData %>% filter(ship_type == shipType & SHIP_ID == shipID & SHIPNAME == shipName)
    lastData$SHIPNAME %>% as.character()
  }
}

# main grid for the app
grid1 <- grid_template(default = list(areas = rbind(
  c("header"),
  c("main"),
  c("names"),
  c("menu")
),
rows_height = c("60px","500px","25px","40px"),
cols_width = c("auto")
))

# grid for the header
subGrid1 <- grid_template(default = list(
  areas = rbind(
    c("title", "note")
  ),
  cols_width = c("70%", "30%")
))

# grid for the titles of the input
subGrid2 <- grid_template(default = list(
  areas = rbind(
    c("type", "name", "ID", "render")
  ),
  cols_width = c("20%", "35%","20%","25%")
  
))

# generating the UI
ui <- semanticPage(
  includeCSS("styles/customStyle.css"),
  
  semanticPage(
    grid(grid1,
         container_style = "padding: 2vh;background: #fff;height: fit-content;border-radius: 10px;",
         area_styles = list(header = "background: #fff",
                            names = "margin-left: 20px; margin-top:10px;background: #fff",
                            menu = "margin-left: 20px;margin-top:10px;background: #fff"),
         header = grid(subGrid1,
                       title = div(tags$h1(titleUI("ships"))),
                       note = noteUI("ships")),
         main = mapUI("ships"),
         names = grid(subGrid2,
                      type = div("TYPE:"),
                      name = div("NAME:"),
                      ID = div("ID:"),
                      render = div("UPDATE")),
         menu =  vesselUI("ships"))

 )
)

# generating the Server 
server <- shinyServer(function(input, output, session) {
  titleServer("ships")
  mapServer("ships")
  noteServer("ships")
  vesselServerType("ships")
  vesselServerName("ships")
  

})

#running the app
shinyApp(ui = ui, server = server)

