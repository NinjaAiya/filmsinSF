library(RSocrata)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggmap)
library(httr)
library(tidytext)
library(jsonlite)

# Import SF films data -------------------------------------------------------
# read films data from DataSF website
dt_url <- "https://data.sfgov.org/resource/wwmu-gmzc.csv"
token <- read_json('~/Projects/filmsinSF/DataSF.json')$token
dt_raw <- read.socrata(url = "https://data.sfgov.org/resource/wwmu-gmzc.csv", app_token = token)

# replace blank values with NA
dt_raw[dt_raw ==''] <- NA
# check missing values in each column
sapply(dt_raw, function(x) sum(is.na(x)))
# remove 54 records with no location information and 229 records associated with TV shows
dt <- dt_raw %>%
  filter(!is.na(locations)) %>%
  filter(grepl('season',title, ignore.case=TRUE) == FALSE)
sapply(dt, function(x) sum(is.na(x)))

dt$locations <- iconv(dt$locations, to = "ASCII//TRANSLIT")

# Geocoding film locations ------------------------------------------------
## format locations to improve geocoding accuracy
locations <- gsub(".*\\((.*\\d+.*)\\).*", "\\1", dt$locations) %>%
  {gsub("(.+)\\s\\(.*","\\1",.)} %>%
  {gsub('\\@','at',.)} %>%
  {gsub(('/'),' and ',.)} %>%
  {gsub('(.+between.+)and.*','\\1',.)} %>%
  {gsub('between','and',.)} %>%
  {gsub('&','and',.)}

dt$locations_formatted <- paste0(locations, ', San Francisco, CA')

## define a function that will process googles server responses for us
### (source: https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/)
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply <- geocode(address, output='all', source = 'google', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA,formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- 'temp_geocoded.rds'
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(1, length(dt$locations_formatted))){
  print(paste("Working on index", ii, "of", length(dt$locations_formatted)))
  #query the google geocoder - this will pause here if we are over the limit.
  result <- getGeoDetails(dt$locations_formatted[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

# add the latitude and longitude to the main data
dt$index <- seq(1,nrow(dt))
dt_geocoded <- dt %>%
  left_join(geocoded, by = 'index') %>%
  select(title, release_year,locations, locations_formatted, formatted_address, lat, long)
# uncomment below to export data to csv for manual processing
# write.csv(dt_geocoded,'movies_geocoded.csv')


# Enrich movie data -------------------------------------------------------
# read cleaned geocoded data
dt <- read.csv('movies_geocoded.csv', header = TRUE, as.is = TRUE)

movies <- dt %>%
  filter(!is.na(lat)) %>%
  select(title, release_year) %>%
  unique()

# format title format for API calls
movies$title_formatted <- gsub(' ','+',movies$title)

# extract imdbID from movie titles using omdb api
movies$imdbID <- NA
movies$rating_imdb <- NA
movies$poster <- NA
movies$type <- NA
movies$plot <- NA
movies$genre <- NA

# extract imdbID, ratings, posters and other movie attributes using omdb api
apikey <- read_json('~/Projects/filmsinSF/omdb_api.json')$apikey
for (row in 1:nrow(movies)) {
  title <- movies[row,'title_formatted']
  u <- paste0('http://www.omdbapi.com/?t=',title,'&plot=full&r=json&apikey=',apikey)
  r <- GET(url = u)
  c <- content(r)
  if (c$Response == "True") {
    movies$imdbID[row] <- c$imdbID
    movies$rating_imdb[row] <- c$imdbRating
    if (!is.null(c$Poster)) {
      movies$poster[row] <- c$Poster
      }
    movies$type[row] <- c$Type
    movies$plot[row] <- c$Plot
    movies$genre[row] <- c$Genre
    }
  }

# check na values
sapply(movies, function(x) sum(is.na(x)))

movies <- movies %>%
  filter(type == 'movie')


# Movie plots sentiment analysis ------------------------------------------
movie_sentiments <- movies %>%
  select(title, plot) %>%
  unnest_tokens(word,plot) %>%
  na.omit() %>%
  inner_join(get_sentiments('afinn')) %>%
  group_by(title) %>%
  summarise(sentiment = round(mean(score), digits = 2))

head(united_DF)
movies <- left_join(movies, movie_sentiments, by = 'title') %>%
  separate(genre, c('drama','crime'),',')

movie_map <- left_join(dt, movies, by = 'title') %>%
  select(title, release_year.x, locations, lat, long, imdbID, rating_imdb, poster, genre, sentiment) %>%
  rename(release_year = release_year.x)

# transform movie genres from long to wide format
movie_map$genre <- gsub(' ','',movie_map$genre)
genres <- levels(factor(unlist(strsplit(movie_map$genre,','))))
temp <- as.data.frame(do.call(rbind, lapply(lapply(strsplit(movie_map$genre,','), factor, genres), table)))
movie_map <- cbind(movie_map, temp)

# save data for shiny app
write.csv(movie_map,'movie_map.csv')
