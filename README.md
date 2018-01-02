# A visualization of movies filmed in San Francisco

This is my first Shiny App project plotting movies filmed in San Francisco since 1924 using data from [DataSF](https://data.sfgov.org/Culture-and-Recreation/Film-Locations-in-San-Francisco/yitu-d5am). The project contains four main components:
  - Geocoding
  - Movie feature enrichment
  - Movie plot sentiment analysis
  - Visualization with R Shiny

The final result is [this interactive Shiny App](https://wendy-lu.shinyapps.io/films_in_san_francisco/) that lets you view grouped or individual movie on a map based on where they were filmed. Each movie is supplemented with additional information such as movie poster, release year, genre, and a sentiment score calculated based on the movie plots.
