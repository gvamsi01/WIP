library(shiny)
library(leaflet)
library(ShinyDash)
library(shinyGridster)
library(RColorBrewer)
library(shinyBS)
library(shinyapps)

# Choices for drop-downs
vars <- c(
  "Withdrawal (MGD)" = "centile",
  "Consumption (MGD)" = "superzip"
  #"Consumption (MGY)" = "college",
  #"Withdrawal (MGY)" = "adultpop"
)
allzips <- readRDS("data/superzip.rds")
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 1000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

shinyUI(navbarPage("Water Intelligence Platform", id="nav",
                   
                   tabPanel("Interactive map",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("Water Use Explorer"),
                                              checkboxInput('addMarker','Delineate Watershed'),
                                              actionButton('clearMarkers','Clear all markers'),
                                              selectInput("variable","Water Use Data",vars,selected=vars[[1]])
                                              #selectInput("color", "Color", vars),
                                              #selectInput("size", "Size", vars, selected = "centile"),
                                              #conditionalPanel("input.color == 'zipdata' || input.size == 'zipdata'",
                                                               # Only prompt for threshold when coloring or sizing by superzip
                                              #                 numericInput("threshold", "Water Use threshold (top n percentile)", 1)
                                              #)
                                              
                                              #plotOutput("histCentile", height = 200),
                                              #plotOutput("scatterCollegeIncome", height = 250)
                                ),
                                
                                tags$div(id="cite",
                                         'Data compiled for ', tags$em('Water Intelligence Platform, The Nature Conservancy'), ' by Sarah Whateley (2016).'
                                )
                            )
                   ),
                   
                   tabPanel("Data explorer",
                            fluidRow(
                              column(3,
                                     selectInput("counties", "County", as.character(zipdata$county), multiple=FALSE)
                              )
                              #column(3,
                              #       conditionalPanel("input.cities",
                              #                        selectInput("facility", "Facility",as.list(as.character(zipdata$city.y[which(zipdata$city.x=="input.cities")])), multiple=FALSE)
                              #       )
                              #)
                      
                            ),
                            fluidRow(
                              column(1,
                                     numericInput("minScore", "Min score", min=0, max=100, value=0)
                              ),
                              column(1,
                                     numericInput("maxScore", "Max score", min=0, max=100, value=100)
                              )
                            ),
                            hr(),
                            DT::dataTableOutput("ziptable")
                   ),
                   
                   conditionalPanel("false", icon("crosshair"))
))




