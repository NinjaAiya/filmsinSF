library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

# import cleaned movie data
dt <- read.csv('~/Projects/filmsinSF/movie_map.csv', header = TRUE, as.is = TRUE)

# make leaflet icons for movie sentiment
sentimentIcons <- iconList(
  negative = makeIcon("~/Projects/filmsinSF/images/emoji-sad.svg",iconWidth = 15, iconHeight = 15),
  neutral = makeIcon("~/Projects/filmsinSF/images/emoji-unsure.svg",iconWidth = 15, iconHeight = 15),
  positive = makeIcon("~/Projects/filmsinSF/images/emoji-smile.svg",iconWidth = 15, iconHeight = 15)
)

# replace NA sentiment values with zero
dt$sentiment[is.na(dt$sentiment)] <- 0 

# break down sentiment scores to negative, neutral, positive groups
dt <- dt %>%
  mutate(sentiment_level = cut(sentiment,c(-3,-0.5,0.5,Inf), 
                               labels = c('negative','neutral', 'positive')))

# format movie information for leaflet map pop-up display
dt$poster_html <- paste0("<img src = ", dt$poster,"width='64' height='128'",">")
dt$title_html <- paste0("<h3>",dt$title,"</h3>")
dt$sentiment_html <- paste0("</p><b>Plot sentiment score: </b>", dt$sentiment, "</p>")
dt$location_html <- paste0("<p><b>Filming location: </b>",dt$locations,"</p>")
genres <- c('All',colnames(dt[11:33]))

# Shiny App -------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Roboto');
                    
                    h2 {
                    font-family: 'Roboto', sans-serif;
                    font-weight: 400;
                    line-height: 1;
                    color: #000000;
                    text-align: center;
                    }
                    
                    "))
    ),
  
  titlePanel('A Visualization of Movies Filmed in San Francisco since 1924'),
  
  
  
  mainPanel(
    tags$style(type = "text/css", "#my_map {height: calc(100vh - 80px) !important;}"),
    width = "100%",
    fluidRow(
      inputPanel(
        selectInput("select_genre","Choose a genre:", genres, selected = "Drama")
      )),
    fluidRow(
      column(width = 7, leafletOutput("my_map")),
      column(width = 5, plotOutput('hist_sentiment'))
      )
  )
)

server <- function(input, output, session){
  subsetData <- reactive({
    if (input$select_genre == 'All') {
      return(dt)
    }
    new_data <- dt[dt[input$select_genre]==1,]
    return(new_data)
  })
  # display the data in real time to identify if the subsetting
  # is occurring as expected.
  # output$viewData <- renderTable({
  #   subsetData()
  # })
  output$hist_sentiment <- renderPlot({
    # hist(subsetData()$sentiment,
    #      xlab = "Sentiment scores based on movie plots",
    #      main = paste0("Distribution of movie sentiments in the ", input$select_genre, " genre"))
    ggplot(subsetData()) + 
      theme_minimal() +
      geom_histogram(aes(x = sentiment, y = ..density..),
                     binwidth = 0.25,
                     fill = "#2877bf",
                     alpha = 0.7) +
      geom_density(aes(x = sentiment),
                   color = "#1c5589") +
      labs(title = paste0("Distribution of Movie Sentiments in the ", input$select_genre, " Genre"))
  })
  # Create a palette that maps sentiment levels to colors
  pal <- colorFactor(c("red", "orange","yellow"), domain = c("negative", "neutral","positive"))
  output$my_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -122.431297, lat = 37.773972, zoom = 12) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        data = subsetData(),
        lng= ~long, 
        lat=~lat,
        popup = paste0(subsetData()$title_html, subsetData()$location_html, subsetData()$sentiment_html, subsetData()$poster_html),
        color = ~pal(sentiment_level),
        stroke = FALSE, 
        fillOpacity = 0.5,
        clusterOptions = markerClusterOptions(),
        group = "cluster") %>%
      addCircleMarkers(
        data = subsetData(),
        lng= ~long, 
        lat=~lat,
        popup = paste0(subsetData()$title_html, subsetData()$location_html, subsetData()$sentiment_html, subsetData()$poster_html),
        color = ~pal(sentiment_level),
        stroke = FALSE, 
        fillOpacity = 0.5,
        group = "individual") %>%
      addLayersControl(
        baseGroups = c("individual","cluster"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  observe({
    leafletProxy('my_map') %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data = subsetData(),
        lng= ~long, 
        lat=~lat,
        popup = paste0(subsetData()$title_html, subsetData()$location_html, subsetData()$sentiment_html, subsetData()$poster_html),
        color = ~pal(sentiment_level),
        stroke = FALSE, 
        fillOpacity = 0.5,
        clusterOptions = markerClusterOptions(),
        group = "cluster") %>%
      addCircleMarkers(
        data = subsetData(),
        lng= ~long, 
        lat=~lat,
        popup = paste0(subsetData()$title_html, subsetData()$location_html, subsetData()$sentiment_html, subsetData()$poster_html),
        color = ~pal(sentiment_level),
        stroke = FALSE, 
        fillOpacity = 0.5,
        group = "individual") %>%
      addLayersControl(
        baseGroups = c("individual","cluster"),
        options = layersControlOptions(collapsed = FALSE))
  })
}
shinyApp(ui, server)


