library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(jsonlite)
library(RJSONIO)
# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 1000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

shinyServer(function(input, output, session) {
  v <- reactiveValues(msg = "")
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      setView(lng = -74.2890, lat = 43.3582, zoom = 7) 
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(zipdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
           longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    hist(zipsInBounds()$centile,
         breaks = centileBreaks,
         main = "Withdrawal (MGD)",
         xlab = "Percentile",
         xlim = range(allzips$centile,na.rm=TRUE),
         col = '#00DD00',
         border = 'white')
  })
  
  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(adultpop ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$adultpop)))
  })
  
  #Add and clear markers
  observeEvent(input$map_click, {
    v$msg <- paste("Clicked map at", input$map_click$lat, "/", input$map_click$lng)
    if (input$addMarker) {
      leafletProxy("map") %>%
        addMarkers(lng = input$map_click$lng, lat = input$map_click$lat)
    }})
  
  observeEvent(input$clearMarkers, {
    leafletProxy("map") %>% clearMarkers()
  })
 
  observeEvent(input$map_click, { 
    withProgress(message='Delineating Watershed...',{
    
    #Delineate watershed
    url <- paste('http://streamstatsags.cr.usgs.gov/streamstatsservices/watershed.geojson?rcode=NY&xlocation=',round(input$map_click$lng,4),'&ylocation=',round(input$map_click$lat,4),'&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=true',sep="")
    print(url)
    #url <- "watershed.geojson"
    dat <- readLines(url, warn='F') %>%
      jsonlite::fromJSON(simplifyDataFrame=FALSE)%>%
      .[["featurecollection"]]
    geojsonList <- Filter(function(x) x["name"]=="globalwatershed",dat)[[1]][["feature"]]
    geojsonJSON <- jsonlite::toJSON(geojsonList,auto_unbox=TRUE,digits=5)
    
    if (input$addMarker) {
      leafletProxy("map") %>%
      addGeoJSON(geojsonJSON)
      }
    })
  })
  
  #Get basin characteristics
  showBasinCharac <- function(lat,lng){
    dat2 <- readLines(url, warn='F') %>%
      jsonlite::fromJSON(simplifyDataFrame=FALSE)%>%
      .[["workspaceID"]]
      print(dat2)
      url2 <- paste('http://streamstatsags.cr.usgs.gov/streamstatsservices/parameters.json?rcode=NY&workspaceID=',dat2,'&includeparameters=true',sep="")
      basin_char <- readLines(url2,warn='F')%>%
        jsonlite::fromJSON(simplifyDataFrame=FALSE)
    content <- as.character(tagList(
      tags$h4("Basin Characteristics:"), tags$br(),
      paste("Drainage Area (mi^2):", basin_char$parameters[[1]]$value), tags$br(),
      paste("Area Covered by Forest (%):", basin_char$parameters[[12]]$value), tags$br(),
      paste("Impervious Area (%):", basin_char$parameters[[27]]$value), tags$br(), #(determined from NLCD 2011 impervious dataset)
      paste("Mean Annual Precipitation (in):", basin_char$parameters[[15]]$value)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content)
  }
  
  # When map is clicked, show a popup with basin info
  observe({
    withProgress(message='Getting Basin Characteristics...',{
    leafletProxy("map") %>% clearPopups()
    event <- input$map_geojson_click
    if (is.null(event))
      return()
      isolate({
        showBasinCharac(event$lat, event$lng)
      })
    })
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
#   observe({
#     colorBy <- input$color
#     sizeBy <- input$size
#   
#     if (colorBy == "centile") {
#     # Color and palette are treated specially in the "superzip" case, because
#     # the values are categorical instead of continuous.
#     colorData <- ifelse(zipdata$centile >= (2 - input$threshold), "yes", "no")
#     pal <- colorFactor("Spectral", colorData)
#     } else {
#     colorData <- zipdata[[colorBy]]
#     pal <- colorBin("Spectral", colorData, 2, pretty = FALSE)
#     }
#   
#     if (sizeBy == "centile") {
#     # Radius is treated specially in the "superzip" case.
#     radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
#     } else {
#     radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
#     }
#   
#   leafletProxy("map", data = zipdata) %>%
#   clearShapes() %>%
#   addCircles(~longitude, ~latitude, radius=radius,
#   stroke=FALSE, fillOpacity=0.4, fillColor="pal(colorData)") %>%
#   addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
#   layerId="colorLegend")
#   })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  #observe({
    #colorBy <- input$color
    #sizeBy <- input$size
    
    #if (colorBy == "centile") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      #colorData <- ifelse(zipdata$centile >= (2 - input$threshold), "yes", "no")
      #pal <- colorFactor("Spectral", colorData)
    #} else {
      #colorData <- zipdata[[colorBy]]
      #pal <- colorBin("Spectral", colorData, 2, pretty = FALSE)
    #}
    
    #if (sizeBy == "centile") {
      # Radius is treated specially in the "superzip" case.
      #radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    #} else {
      #radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    #}
    
    #leafletProxy("map", data = zipdata) %>%
      #clearShapes() %>%
      #addCircles(~longitude, ~latitude, radius=radius,
                 #stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      #addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                #layerId="colorLegend")
    
  #})
  
  # Show a popup at the given location
  #showZipcodePopup <- function(zipcode, lat, lng) {
    #selectedZip <- allzips[allzips$zipcode == zipcode,]
    #content <- as.character(tagList(
      #tags$h4("Score:", as.integer(selectedZip$centile)),
      #tags$strong(HTML(sprintf("%s, %s %s",
      #                         selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      #))), tags$br(),
      #sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      #sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      #sprintf("Adult population: %s", selectedZip$adultpop)
    #))
    #leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  #}
  
  # When map is clicked, show a popup with city info
  #observe({
    #leafletProxy("map") %>% clearPopups()
    #event <- input$map_shape_click
    #if (is.null(event))
      #return()
    
    #isolate({
    #  showZipcodePopup(event$id, event$lat, event$lng)
    #})
  #})
  
  
  ## Data Explorer ###########################################
  
#   observe({
#     cities <- if (is.null(input$states)) character(0) else {
#       filter(cleantable, State %in% input$states) %>%
#         `$`('City') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$cities[input$cities %in% cities])
#     updateSelectInput(session, "cities", choices = cities,
#                       selected = stillSelected)
#   })
  
#   observe({
#     zipcodes <- if (is.null(input$states)) character(0) else {
#       cleantable %>%
#         filter(State %in% input$states,
#                is.null(input$cities) | City %in% input$cities) %>%
#         `$`('Zipcode') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#     updateSelectInput(session, "zipcodes", choices = zipcodes,
#                       selected = stillSelected)
#   })
  
#   observe({
#     if (is.null(input$goto))
#       return()
#     isolate({
#       map <- leafletProxy("map")
#       map %>% clearPopups()
#       dist <- 0.5
#       zip <- input$goto$zip
#       lat <- input$goto$lat
#       lng <- input$goto$lng
#       showZipcodePopup(zip, lat, lng)
#       map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
#     })
#   })
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Withdrawal >= input$minScore,
        Withdrawal <= input$maxScore,
        #is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities
        #is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', City, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
      action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
})