### -----------------------------------------------
### Case Study: Mapping Breweries in Germany
### Simon Munzert
### -----------------------------------------------


##  goal

# 1. get list of breweries in Germany
# 2. import list in R
# 3. geolocate breweries
# 4. put them on a map


## load packages

library(tidyverse)
library(rvest)
library(stringr)
# devtools::install_github("hrbrmstr/nominatim")
#library(nominatim)
library(sf)
library(rnaturalearth)
library(RSelenium)






## inspect the source code in your browser ---------------

browseURL("http://www.biermap24.de/brauereiliste.php") 

browseURL("https://www.endsars.com")

## step 1: fetch list of cities with breweries
url <- "http://www.biermap24.de/brauereiliste.php"
url <-"https://www.endsars.com"
url <-"https://www.livescore.com"





## setup R + RSelenium -------------------------

# install current version of Java SE Runtime Environment
browseURL("https://duckduckgo.com/?q=java+download&va=z&t=hk&ia=web")

# set up connection via RSelenium package
# documentation: http://cran.r-project.org/web/packages/RSelenium/RSelenium.pdf


## example --------------------------
startServer()
?rsDriver
?wdman
browseURL("https://www.livescore.com/en/soccer/live/")


require 



# Specify the driver path
chromedriver_path = File.join(File.absolute_path('../..', 
           File.dirname( "C:/Program Files/Google/Chrome Beta/Application/chrome.exe")),
                  "browsers","chromedriver.exe")

Selenium::WebDriver::Chrome.driver_path = "C:\Program Files\Google\Chrome Beta\Application\chrome.exe"


?rsDriver

rD <- rsDriver(browser = "chrome", port= 4517L) 


#rD <- rsDriver(browser = "firefox") # if chrome is causing some issues on your system, try (a) using the current beta, which you then have to install first, or (b) another browser, such as firefox



# set up connection
remDr <- rD[["client"]]



remDr$navigate(url)

#remDr$open()

# start browser, navigate to page
url <- "https://www.iea.org/policies"


# open filter menu
xpath <- '//*[@id="content"]/div[2]/nav/div/div/div/div/button'
findElem <- remDr$findElement(using = 'xpath', value = xpath)
findElem$clickElement() # click on button

# open region menu
xpath <- '//*[@id="accordion-filter-region"]'
findElem <- remDr$findElement(using = 'xpath', value = xpath)
findElem$clickElement() # click on button

# select Europe
xpath <- '//*[@id="content"]/div[2]/div[1]/div/ul/li[2]/div/div/ul/li[1]/a/span[2]'
findElem <- remDr$findElement(using = 'xpath', value = xpath)
findElem$clickElement() # click on button

# specify text field
xpath <- '//*[@id="input-qs"]'
textfield <- remDr$findElement(using = 'xpath', value = xpath) 
textfield$clickElement() # click on text field
textfield$sendKeysToElement(list("battery", key = "enter")) # enter start year

# close filter menu
xpath <- '//*[@id="content"]/div[2]/div[1]/div/div/button'
findElem <- remDr$findElement(using = 'xpath', value = xpath) 
findElem$clickElement() 

# store index page
output <- remDr$getPageSource(header = TRUE)
write(output[[1]], file = "iea-renewables-1.html")

# close connection
remDr$closeServer()
remDr$close






cities <- html_text(anchors)
cities
cities <- str_trim(cities)
cities <- cities[str_detect(cities, "^[[:upper:]]+.")]
cities <- cities[6:length(cities)]
length(cities)
length(unique(cities))
sort(table(cities))
unique_cities <- unique(cities)

## step 2: geocode cities
# get free key for mapquest API at browseURL("https://developer.mapquest.com/")
#load("/Users/s.munzert/rkeys.RDa") # import API key (or paste it here in openstreetmap object)

pos <- data.frame(lon = NA, lat = NA)
if (!file.exists("geocodes_breweries.RData")){
  for (i in 1:length(unique_cities)) {
    pos[i,] <- try(nominatim::osm_search(unique_cities[i], country_codes = "de", key = openstreetmap) %>% dplyr::select(lon, lat))
  }
  pos$city <- unique_cities
  pos <- filter(pos, !str_detect(lon, "Error"))
  pos$lon <- as.numeric(pos$lon)
  pos$lat <- as.numeric(pos$lat)
  save(pos, file="geocodes_breweriers.RData")
} else {
  load("geocodes_breweries.RData")
}
head(pos)


## step 3: plot breweries of Germany
pos <- filter(pos, lon >= 6, lon <= 15, lat >= 47, lat <= 55)
worldmap <- rnaturalearth::ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
germany <- worldmap[worldmap$name == 'Germany',]
ggplot() + geom_sf(data = germany) + theme_bw() + geom_point(aes(lon,lat), data = pos, size = .5, color = "red") 

