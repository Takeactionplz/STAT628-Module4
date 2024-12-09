library(shiny)
library(httr)
library(dplyr)
library(purrr)
library(reticulate)
library(fmsb)
library(scales)

virtualenv_create("python3_env")
virtualenv_install("python3_env", packages = c("pandas","numpy", "gensim", "nltk", "scikit-learn", "wheel", "setuptools"))  # Install required Python packages
use_virtualenv("python3_env", required = TRUE)

# Import Python script
score_calculator <- import_from_path("score_calculator")
similarity_df <- read.csv("Similarity.csv")

find_nearest_episode <- function(selected_scores, similarity_df) {
  # Convert input scores into a vector
  selected_vector <- as.numeric(c(selected_scores$Science, 
                                  selected_scores$Finance, 
                                  selected_scores$Thrilling))
  
  # Calculate distance
  similarity_df <- similarity_df %>%
    rowwise() %>%
    mutate(
      Distance = (as.numeric(similarity_science) - selected_vector[1])^2 +
        (as.numeric(similarity_finance) - selected_vector[2])^2 +
        (as.numeric(similarity_thrilling) - selected_vector[3])^2
    ) %>%
    ungroup()
  
  nearest_episode <- similarity_df %>%
    filter(Distance == min(Distance)) %>%
    select(name, similarity_science, similarity_finance, similarity_thrilling, Distance) %>%
    mutate(
      similarity_science = round(similarity_science, 4),
      similarity_finance = round(similarity_finance, 4),
      similarity_thrilling = round(similarity_thrilling, 4),
      Distance = round(Distance, 4)  # Optionally round distance to 4 decimal places
    )
  
  return(nearest_episode)
}

# Define radar chart drawing function
create_beautiful_radarchart <- function(data, color = c("#0000FF", "#FC4E07"), caxislabels = NULL) {
  radarchart(
    data,
    axistype = 1,
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    axislabcol = "grey",
    vlcex = 1.0,  # Font size
    vlabels = c(
      expression(bold("Science")), 
      expression(bold("Finance")), 
      expression(bold("Thrilling"))
    ),
    caxislabels = caxislabels
  )
  title(
    main = "Radar Chart of Scores",
    cex.main = 1.5,  # Font size
    font.main = 2    # Bold font
  )
}

# Function to fetch podcast information, returns all results
get_podcasts_by_name <- function(podcast_name, token) {
  res <- GET('https://api.spotify.com/v1/search', 
             query = list(q = podcast_name, 
                          type = 'show', 
                          access_token = token)) %>% 
    content() %>% .$shows %>% .$items
  
  if (length(res) > 0) {
    podcasts <- map_dfr(res, ~data.frame(
      podcast_name = .x$name,
      podcast_uri = .x$uri,
      publisher = .x$publisher,
      image = ifelse(length(.x$images) > 0, .x$images[[1]]$url, NA),
      stringsAsFactors = FALSE
    ))
    return(podcasts)
  }
  
  return(NULL)
}

# Function to fetch podcast episodes (supports pagination)
get_all_episodes_by_podcast <- function(podcast_uri, token) {
  show_id <- sub("spotify:show:", "", podcast_uri)  # Extract podcast ID
  episodes <- list()
  offset <- 0
  limit <- 50  # Maximum number of items per request
  
  repeat {
    res <- GET(paste0('https://api.spotify.com/v1/shows/', show_id, '/episodes'),
               query = list(access_token = token, limit = limit, offset = offset)) %>%
      content()
    
    if (is.null(res$items) || length(res$items) == 0) break
    
    episodes <- append(episodes, res$items)
    offset <- offset + limit
    
    # Stop if the number of returned items is less than the limit
    if (length(res$items) < limit) break
  }
  
  if (length(episodes) > 0) {
    episodes_df <- map_dfr(episodes, ~data.frame(
      episode_name = .x$name,
      episode_uri = .x$uri,
      description = .x$description,  # Add description field
      release_date = .x$release_date,
      stringsAsFactors = FALSE
    ))
    return(episodes_df)
  }
  
  return(NULL)
}

# Spotify API Access Token
get_spotify_token <- function(client_id, client_secret) {
  POST('https://accounts.spotify.com/api/token',
       accept_json(), authenticate(client_id, client_secret),
       body = list(grant_type = 'client_credentials'),
       encode = 'form', httr::config(http_version = 2)) %>% 
    content() %>% .$access_token
}

# Shiny UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tags$div(class = "title", "Spotify Podcast Search"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      textInput("podcast_name", "Podcast Name:", ""),
      actionButton("search", "Search Podcast"),
      hr(),
      uiOutput("podcast_selector"),
      tableOutput("podcast_table"),
      uiOutput("podcast_image"),
      hr(),
      textInput("episode_name", "Episode Name:", ""),
      actionButton("search_episode", "Search Episode"),
      hr(),
      uiOutput("episode_selector"),
      tableOutput("episode_result")
    ),
    mainPanel(
      div(
        p("Click 'Methodology' to learn how the scores are calculated!", 
          style = "font-size: 18px; color: #444; font-weight: bold; text-align: center; margin-bottom: 20px;"),
        tabsetPanel(
          # Data Visualization tab
          tabPanel(
            "Data Visualization",
            fluidRow(
              uiOutput("main_content"),  # Dynamic content display
              tags$br(),
              plotOutput("radar_chart", height = "400px")  # Radar chart
            ),
            style = "background-color: #f5deb3; color: #000; padding: 15px;"
          ),
          # Methodology tab
          tabPanel(
            "Methodology",
            fluidRow(
              column(
                width = 12,
                # Add title
                h3("Methodology", 
                   style = "font-weight: bold; color: #000; text-align: center;"),
                # Algorithm description
                h4("1. Word Embeddings",
                   style = "font-weight: bold; color: #2a7b9b; margin-top: 15px;"),
                p(HTML("The <b>Word2Vec</b> model is used to transform input descriptions into numerical vectors."),
                  style = "font-size: 16px; color: #444; line-height: 1.6;"),
                # Example table
                h4("2. Scoring and Similarity Calculation",
                   style = "font-weight: bold; color: #2a7b9b; margin-top: 15px;"),
                p(HTML("The <b>cosine similarity</b> is computed between the input text and predefined topic vectors (e.g., Science, Finance, Thrilling)."),
                  style = "font-size: 16px; color: #444; line-height: 1.6;"),
                h4("3. Score Transformation",
                   style = "font-weight: bold; color: #2a7b9b; margin-top: 15px;"),
                p(HTML("To handle the skewed distribution of scores, <b>Boxcox transformation</b> and <b>MinMax scaling</b> are applied to normalize the scores."),
                  style = "font-size: 16px; color: #444; line-height: 1.6;"),
                # Add images
                h4("4. Before and After Boxcox Transformation",
                   style = "font-weight: bold; color: #2a7b9b; margin-top: 15px;"),
                tags$img(src = "before_boxcox.png", 
                         alt = "Before Boxcox Transformation", 
                         style = "display: block; margin: 20px auto; max-width: 80%;"),
                tags$img(src = "after_boxcox.png", 
                         alt = "After Boxcox Transformation", 
                         style = "display: block; margin: 20px auto; max-width: 80%;")
              )
            ),
            style = "background-color: #f5deb3; color: #000; padding: 15px;"
          )
        ),
        style = "background-color: #f5deb3; padding: 20px; border-radius: 5px;"
      )
    )
  ),
  # Modify footer information
  tags$div(
    class = "footer",
    tags$p("App Maintainer: xtang254@wisc.edu"),
    tags$p("Contributors: Xupeng Tang, Xiangsen Dong")
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Your server logic remains unchanged
}

shinyApp(ui = ui, server = server)
