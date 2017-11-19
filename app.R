library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
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

# Return table of stops for a route
returnRouteTable <- function(raw_route){
  reactiveRoute <- raw_route
  routeObject <- t(data.frame(t(raw_route)))
  routeObject <- cbind(id=seq.int(nrow(routeObject)),routeObject)
  colnames(routeObject) <- c("No.","Stop")
  return(routeObject)
}


ui <- dashboardPage(
  dashboardHeader(title = "FlexiBus | Driver"),
  dashboardSidebar(disable = TRUE),                  
  dashboardBody(
    fluidPage(theme = shinytheme("cerulean"),
      selectInput("bus",h3("Select Bus ID"),returnBuses(),selectize = FALSE),
      h3("Route"),
      h4(textOutput("service")),
      withSpinner(tableOutput("route"))
    )
  )  
)
  
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$route <- renderTable(returnRouteTable(returnCurrentRouteFromBusID(input$bus)),digits=0)
  
  output$service <- renderText(
    paste0("Service: ",toupper(as.character(returnService(input$bus))))
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

