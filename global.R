library(dplyr)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(jsonlite)
library(RJSONIO)
library(ShinyDash)
library(shinyGridster)
library(shinyBS)
library(shinyapps)
library(httr)

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college 
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    #City = city.x,
    County = county,
    State = state.x,
    Facility = city.y,
    #Zipcode = zipcode,
    Rank = rank,
    #Score = centile,
    Withdrawal = centile,
    Consumption = superzip,
    #Population = adultpop,
    #College = college,
    #Income = income,
    Lat = latitude,
    Long = longitude
  )