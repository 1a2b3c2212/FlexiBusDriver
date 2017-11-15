#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shinythemes)
library(shinydashboard)


library(jsonlite)
library(httr)

# Return the list of buses
returnBuses <- function(){
  buses <- fromJSON(content(GET("https://bt3103-ef12e.firebaseio.com/buses.json"), as="text"))
  buses <- buses$busID
  buses <- as.list(buses)
  buses[[1]] <- NULL
  return(buses)
}

# Return the service of a given busID
returnService <- function(busID){
  return (content(GET(paste0("https://bt3103-ef12e.firebaseio.com/buses/",busID,".json")), as="parsed")$route)
}

# Takes in a bus ID and returns the route for that bus in a list
returnCurrentRouteFromBusID <- function(bus){
  service <- returnService(bus)
  route <- GET(paste0("https://bt3103-ef12e.firebaseio.com/latestRoutes/",service,".json"))
  route <- content(route, as="parsed")
  
  date1 = route[[1]]$effDate
  date2 = route[[2]]$effDate
  
  ## Both routes are outdated and date1 is latest
  if(Sys.Date()>=date1 & Sys.Date()>=date2 & date1>date2){
    curr = route[[1]]
  }
  ## Today is in between date1 and date2
  else{
    if(date1>date2){
      curr = route[[2]]
    }
    else{
      curr = route[[1]]
    }
  }
  
  curr = curr$stops
  curr[1] <- NULL
  
  return (curr)
}

returnRouteTable <- function(raw_route){
  reactiveRoute <- raw_route
  routeObject <- t(data.frame(t(raw_route)))
  routeObject <- cbind(id=seq.int(nrow(routeObject)),routeObject)
  colnames(routeObject) <- c("No.","Stop")
  return(routeObject)
}

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage("",
    tabPanel("FlexiBus | Driver", icon = icon("road"),
      fluidRow(
        selectInput("bus",h3("Select Bus ID"),returnBuses()),
        h3("Route"),
        tableOutput("route")
      )  
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$route <- renderTable(returnRouteTable(returnCurrentRouteFromBusID(input$bus)),digits=0)
}

# Run the application 
shinyApp(ui = ui, server = server)

