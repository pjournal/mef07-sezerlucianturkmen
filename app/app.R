#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
pti <- c("shiny","tidyverse","ggplot2movies")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
  install.packages(pti)
}
### Shiny starter code
library(shiny)
library(tidyverse)
library(ggplot2movies)

# Prepare data
# Use Google Drive to connect Dataset with library of 'googleDrive'
  id <- "1nM0Cr-gVFF9UjvxSZXHNfFnrDWIUoTHi"
total_list <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))

# Filter out rows with non-numeric values in the 'streams' column
filtered_spotify_data <- total_list %>%
  filter(!grepl("[a-zA-Z]", streams))

# Convert character to numeric , original r
filtered_spotify_data$streams <- as.numeric(filtered_spotify_data$streams)

# Original column names
original_names <- colnames(filtered_spotify_data)

# New column names without "_."
new_names <- gsub("_\\.", "", original_names)

# Rename the columns
colnames(filtered_spotify_data) <- new_names

#data
shiny_spotify_set <- filtered_spotify_data

# Get music type list dynamically from column names
music_types <- c( "danceability", "valence", "energy", "acousticness", "instrumentalness", "liveness","speechiness")

# Define UI for application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("released_year", "Year Filter", min = min(shiny_spotify_set$released_year), max = max(shiny_spotify_set$released_year), value = c(2000, 2023), step = 1, sep = ""),
      sliderInput("streams", "Stream filter", min = 0, max = max(shiny_spotify_set$streams), value = 0),
      selectInput("music_type", "Music Features", choices = c("All", music_types), selected = c("danceability","energy"), multiple = TRUE)
    ),
    mainPanel(plotOutput("spotify_plot"))
  )
)

server <- function(input, output) {
  
  output$spotify_plot <- renderPlot({
    my_df <- shiny_spotify_set %>% 
      filter(released_year >= input$released_year[1] & released_year <= input$released_year[2] & streams >= input$streams)
    
    if (!("All" %in% input$music_type)){
      my_df <- my_df %>% 
        select(released_year, input$music_type)
    }else{
      my_df <- my_df %>% 
        select(released_year, music_types)
    }
    
    # Group by release year and calculate the mean for each music type
    my_df <- my_df %>% 
      group_by(released_year) %>% 
      summarise(across(everything(), mean, na.rm = TRUE))
    
    # Reshape data for plotting
    my_df <- pivot_longer(my_df, cols = -released_year, names_to = "music_type", values_to = "mean_value")
    
    ggplot(my_df, aes(x = released_year, y = mean_value, color = music_type, size = mean_value)) + 
      geom_point() +
      scale_size_continuous(range = c(3, 10)) +  # Adjust dot size
      labs(title = "Spotify Data",
           x = "Released Year",                  # X-axis label
           y = "Mean Value",                     # Y-axis label
           color = "Music Feature",                 # Legend title for color
           size = "Mean Value")                  # Legend title for size
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
