## load rtweet package
library(rtweet)
library(tidyverse)

# Scrape twitter

app <- "my_research_2000"

create_token(app, consumer_key, consumer_secret, access_token, access_secret)

following <- get_friends("J_D_Klein")

following_data <- lookup_users(following$user_id)

# Data cleaning

is.na(following_data$location) <- following_data$location == ""

location <- select(following_data, screen_name, location)

location$NY[grepl("New York", location$location, ignore.case = T) | 
              grepl("NY", location$location, ignore.case = F) | 
              grepl("NYC", location$location, ignore.case = T) | 
              grepl("manhattan", location$location, ignore.case = T)] <- "NY"
location$BK[grepl("brooklyn", location$location, ignore.case = T) | 
                    grepl("bk", location$location, ignore.case = T)] <- "Brooklyn"
location$NYC[!is.na(location$BK) | !is.na(location$NY) | grepl("queens", location$location, ignore.case = T) | 
               grepl("bronx", location$location, ignore.case = T)] <- "NYC"
location$NJ[grepl("NJ", location$location, ignore.case = F) | grepl("jersey", location$location, ignore.case = T) | 
              grepl("princeton", location$location, ignore.case = T)] <- "NJ"
location$NY_metro[!is.na(location$NYC) | !is.na(location$NJ) | grepl("stony", location$location, ignore.case = T)] <- "NY metro"

location$DC[grepl("washington,", location$location, ignore.case = T) | 
              grepl("DC", location$location, ignore.case = T) | grepl("D.C", location$location, ignore.case = T) | 
              grepl("^washington", location$location, ignore.case = T) | grepl("georgetown", location$location, ignore.case = T)] <- "DC"
location$Baltimore[grepl("balt", location$location, ignore.case = T)] <- "Baltimore"
location$MD[!is.na(location$Baltimore) | grepl("MD", location$location, ignore.case = F) | 
              grepl("maryland", location$location, ignore.case = T) | 
              grepl("bethe", location$location, ignore.case = T)] <- "MD"
location$VA[grepl("VA", location$location, ignore.case = F)] <- "VA"
location$DC_metro[!is.na(location$DC) | !is.na(location$MD) | !is.na(location$VA)] <- "Metro DC"

location$LA[(grepl("los angeles", location$location, ignore.case = T) | 
               grepl("LA", location$location, ignore.case = F) | 
               grepl("irvine", location$location, ignore.case = T) | 
               grepl("westwood", location$location, ignore.case = T) | 
               grepl("santa monica", location$location, ignore.case = T) | 
               grepl("southern cal", location$location, ignore.case = T) | 
               grepl("hawthorne", location$location, ignore.case = T) | 
               grepl("redlands", location$location, ignore.case = T) | 
               grepl("el segundo", location$location, ignore.case = T)) & 
              !grepl("orleans", location$location, ignore.case = T)] <- "LA"
location$SF[grepl("san f", location$location, ignore.case = T) | grepl("sf", location$location, ignore.case = T)] <- "SF"
location$bay_area[!is.na(location$SF) | grepl("san m", location$location, ignore.case = T) |
                    grepl("berk", location$location, ignore.case = T) | grepl("oak", location$location, ignore.case = T) | 
                    grepl("silicon", location$location, ignore.case = T) | grepl("mountain v", location$location, ignore.case = T) | 
                    grepl("palo", location$location, ignore.case = T) | grepl("stanford", location$location, ignore.case = T) | 
                    grepl("menlo park", location$location, ignore.case = T)] <- "Bay Area"
location$CA[!is.na(location$LA) | !is.na(location$bay_area) | grepl("CA", location$location, ignore.case = F) | 
              grepl("california", location$location, ignore.case = T)] <- "CA"

location$cambridge_ma[(grepl("cambridge", location$location, ignore.case = T) | 
                         grepl("harvard", location$location, ignore.case = T)) & 
                        !grepl("uk", location$location, ignore.case = T) & 
                        !grepl("england", location$location, ignore.case = T)] <- "Cambridge, MA"
location$boston[grepl("boston", location$location, ignore.case = T)] <- "Boston"
location$MA[grepl("MA", location$location, ignore.case = F) | grepl("mass", location$location, ignore.case = T) | 
              !is.na(location$cambridge_ma) | !is.na(location$boston)] <- "MA"

location$AA[grepl("ann arbor", location$location, ignore.case = T) | 
              grepl("university of michigan", location$location, ignore.case = T)] <- "Ann Arbor"
location$MI[!is.na(location$AA) | grepl(", mi", location$location, ignore.case = T) | 
              grepl("mich", location$location, ignore.case = T) | 
              grepl("motown", location$location, ignore.case = T)] <- "MI"

location$ATL[grepl("atl", location$location, ignore.case = T)] <- "Atlanta"
location$GA[!is.na(location$ATL) | grepl(", ga", location$location, ignore.case = T)] <- "GA"

location$Chicago[grepl("chic", location$location, ignore.case = T)] <- "Chicago"

location$MN[grepl("MN", location$location, ignore.case = F) | grepl("minnesota", location$location, ignore.case = T)] <- "MN"

location$Seattle[grepl("seattle", location$location, ignore.case = T)] <- "Seattle"
location$WA[!is.na(location$Seattle) | grepl("WA", location$location, ignore.case = F)] <- "WA"

location$Denver[grepl("denver", location$location, ignore.case = T)] <- "Denver"
location$CO[!is.na(location$Denver) | grepl("CO", location$location, ignore.case = F)] <- "CO"

location$Austin[grepl("austin", location$location, ignore.case = T)] <- "Austin"
location$TX[!is.na(location$Austin) | grepl("TX", location$location, ignore.case = F)] <- "TX"

location$Philly[grepl("phil", location$location, ignore.case = T)] <- "Philly"
location$PA[!is.na(location$Philly) | grepl("PA", location$location, ignore.case = F) | 
              grepl("pennsylvania", location$location, ignore.case = T) | grepl("pittsburgh", location$location, ignore.case = T)] <- "PA"

location$BTown[grepl("bloomington", location$location, ignore.case = T)] <- "Bloomington"
location$IN[!is.na(location$BTown) | grepl("IN", location$location, ignore.case = F)] <- "IN"

location$NewHaven[grepl("new haven", location$location, ignore.case = T)] <- "New Haven"
location$CT[!is.na(location$NewHaven) | grepl("CT", location$location, ignore.case = F) | 
              grepl("connecticut", location$location, ignore.case = T)] <- "CT"

location$NO[grepl("new orleans", location$location, ignore.case = T)] <- "New Orleans"

location$NC[grepl("NC", location$location, ignore.case = F) | 
              grepl("north carolina", location$location, ignore.case = T)] <- "North Carolina"

location$Toronto[grepl("toronto", location$location, ignore.case = T)] <- "Toronto"
location$Montreal[grepl("montreal", location$location, ignore.case = T) | 
                    grepl("montréal", location$location, ignore.case = T)] <- "Montreal"
location$Vancouver[grepl("vancouver", location$location, ignore.case = T)] <- "Vancouver"
location$Canada[!is.na(location$Toronto) | !is.na(location$Montreal) | !is.na(location$Vancouver) | 
                  grepl("canada", location$location, ignore.case = T)] <- "Canada"

location$London[grepl("london", location$location, ignore.case = T)] <- "London"
location$UK[!is.na(location$London) | grepl("UK", location$location, ignore.case = F) | 
              grepl("united kingdom", location$location, ignore.case = T) | grepl("engl", location$location, ignore.case = T) | 
              grepl("manchester", location$location, ignore.case = T) | grepl("scotland", location$location, ignore.case = T) | 
              grepl("oxford", location$location, ignore.case = T)] <- "UK"

location$GEN[grepl("geneva", location$location, ignore.case = T) | 
               grepl("Genève", location$location, ignore.case = T)] <- "Geneva"
location$Switz[!is.na(location$GEN) | grepl("zurich", location$location, ignore.case = T) | 
              grepl("swit", location$location, ignore.case = T)] <- "Switzerland"

location$Paris[grepl("paris", location$location, ignore.case = T)] <- "Paris"
location$France[!is.na(location$Paris) | grepl("france", location$location, ignore.case = T)] <- "France"

location$Stockholm[grepl("stockholm", location$location, ignore.case = T)] <- "Stockholm"

location$IP[grepl("israel", location$location, ignore.case = T) | grepl("tel aviv", location$location, ignore.case = T) | 
              grepl("palestine", location$location, ignore.case = T)] <- "Israel/Palestine"

location$Berlin[grepl("berlin", location$location, ignore.case = T)] <- "Berlin"
location$Germany[grepl("berlin", location$location, ignore.case = T) | 
                   grepl("germany", location$location, ignore.case = T) | 
                   grepl("deutsch", location$location, ignore.case = T)] <- "Germany"

location$Brussels[grepl("brussels", location$location, ignore.case = T)] <- "Brussels"

location$Delhi[grepl("delhi", location$location, ignore.case = T)] <- "Delhi"
location$India[!is.na(location$Delhi) | grepl("india$", location$location, ignore.case = T) | 
                 grepl("mumbai", location$location, ignore.case = T) | 
                 grepl("bengaluru", location$location, ignore.case = T)] <- "India"

location$unknown <- NA

location$unknown[which(apply(is.na(location[, 3:dim(location)[2]]), 1, all))] <- "unknown"

unique <- location[match(unique(location$location), location$location), 2:dim(location)[2]]

t(as.data.frame(rbind(lapply(location[, 3:dim(location)[2]], function(x) {
  length(x[!is.na(x)])
})))) -> locs

data.frame(location = rownames(locs), number = unlist(locs)) -> locs

locs$location <- as.character(locs$location)

major <- locs[-c(1, 2, 5, 7, 10, 12, 15, 16, 18, 20, 24, 26, 32, 34), ]
major$location[major$location == "LA"] <- "SoCal"
major$location[major$location == "GA"] <- "Atl"
major$location[major$location == "WA"] <- "Seattle"
major$location[major$location == "GEN"] <- "Geneva"
major$location[major$location == "IP"] <- "Israel-Palestine"
major$number[major$location == "CA"] <- 115 - 60 - 52
major$number[major$location == "TX"] <- 3
major$number[major$location == "PA"] <- 10
major$number[major$location == "Canada"] <- 19 - 5 - 3 - 3
major$number[major$location == "UK"] <- 79-47
major$number[major$location == "Switz"] <- 4
major$number[major$location == "France"] <- 1
major$number[major$location == "Germany"] <- 6
major$number[major$location == "India"] <- 6

# Generate treemap

library(treemap)

treemap(major, index = "location", vSize = "number", type = "index", 
        title = "Twitter Locations")


