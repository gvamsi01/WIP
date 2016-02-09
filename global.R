library(dplyr)

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college 
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
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