library(shiny)
library(httr)
library(dplyr)
library(purrr)
library(reticulate)
library(fmsb)
library(scales)


virtualenv_create("python3_env")
virtualenv_install("python3_env", packages = c("pandas","numpy", "gensim", "nltk", "scikit-learn", "wheel", "setuptools"))  # 安装所需的 Python 包
use_virtualenv("python3_env", required = TRUE)


# 导入 Python 脚本
score_calculator <- import_from_path("score_calculator")
similarity_df <- read.csv("Similarity.csv")

find_nearest_episode <- function(selected_scores, similarity_df) {
  # 将输入分数向量化
  selected_vector <- as.numeric(c(selected_scores$Science, 
                                  selected_scores$Finance, 
                                  selected_scores$Thrilling))
  
  # 计算距离
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
      Distance = round(Distance, 4)  # 可选：距离也保留 4 位小数
    )
  
  return(nearest_episode)
}

# 定义绘制雷达图函数
create_beautiful_radarchart <- function(data, color = c("#0000FF", "#FC4E07"), caxislabels = NULL) {
  radarchart(
    data,
    axistype = 1,
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    axislabcol = "grey",
    vlcex = 1.0,  # 字体大小
    vlabels = c(
      expression(bold("Science")), 
      expression(bold("Finance")), 
      expression(bold("Thrilling"))
    ),
    caxislabels = caxislabels
  )
  title(
    main = "Radar Chart of Scores",
    cex.main = 1.5,  # 字体大小
    font.main = 2    # 字体加粗
  )
  
}

# 获取播客信息的函数，返回所有结果
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

# 获取播客单集的函数（支持分页）
get_all_episodes_by_podcast <- function(podcast_uri, token) {
  show_id <- sub("spotify:show:", "", podcast_uri)  # 提取播客 ID
  episodes <- list()
  offset <- 0
  limit <- 50  # 每次请求的最大数量
  
  repeat {
    res <- GET(paste0('https://api.spotify.com/v1/shows/', show_id, '/episodes'),
               query = list(access_token = token, limit = limit, offset = offset)) %>%
      content()
    
    if (is.null(res$items) || length(res$items) == 0) break
    
    episodes <- append(episodes, res$items)
    offset <- offset + limit
    
    # 如果返回的结果少于限制数量，说明已经到最后一页
    if (length(res$items) < limit) break
  }
  
  if (length(episodes) > 0) {
    episodes_df <- map_dfr(episodes, ~data.frame(
      episode_name = .x$name,
      episode_uri = .x$uri,
      description = .x$description,  # 添加 description 字段
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
    #mainPanel(
     # div(class = "main-panel", 
      #    uiOutput("main_content"), # 提示内容动态显示
       #   plotOutput("radar_chart")
      #)
    #)
    mainPanel(
      div(
        p("Click 'Methodology' to learn how the scores are calculated !", 
          style = "font-size: 18px; color: #444; font-weight: bold; text-align: center; margin-bottom: 20px;"),
        tabsetPanel(
          # 数据可视化选项卡
          tabPanel(
            "Data Visualization",
            fluidRow(
              uiOutput("main_content"),  # 提示内容动态显示
              tags$br(),
              plotOutput("radar_chart", height = "400px")  # 输出雷达图
            ),
            style = "background-color: #f5deb3; color: #000; padding: 15px;"
          ),
          # 方法论选项卡
          tabPanel(
            "Methodology",
            fluidRow(
              column(
                width = 12,
                # 添加标题
                h3("Methodology", 
                   style = "font-weight: bold; color: #000; text-align: center;"),
                # 添加算法描述
                h4("1. Word Embeddings",
                   style = "font-weight: bold; color: #2a7b9b; margin-top: 15px;"),
                p(HTML("The <b>Word2Vec</b> model is used to transform input descriptions into numerical vectors."),
                  style = "font-size: 16px; color: #444; line-height: 1.6;"),
                # 添加表格示例
                h4("2. Scoring and Similarity Calculation",
                   style = "font-weight: bold; color: #2a7b9b; margin-top: 15px;"),
                p(HTML("The <b>cosine similarity</b> is computed between the input text and predefined topic vectors (e.g., Science, Finance, Thrilling)."),
                  style = "font-size: 16px; color: #444; line-height: 1.6;"),
                h4("3. Score Transformation",
                   style = "font-weight: bold; color: #2a7b9b; margin-top: 15px;"),
                p(HTML("To handle the skewed distribution of scores, <b>Boxcox transformation</b> and <b>MinMax scaling</b> are applied to normalize the scores."),
                  style = "font-size: 16px; color: #444; line-height: 1.6;"),
                # 添加图片
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
  # 修改底部维护者信息
  tags$div(
    class = "footer",
    tags$p("App Maintainer: xtang254@wisc.edu"),
    tags$p("Contributors: Xupeng Tang, Xiangsen Dong")
  )
  
)


# Shiny Server
server <- function(input, output, session) {
  client_id <- "73a6c4010cfe43f0aec4381483fbc3da"
  client_secret <- "5c483e45edf84faa944dbaca1d96f51f"
  
  token <- reactive({
    get_spotify_token(client_id, client_secret)
  })
  
  podcast_data <- reactiveVal(NULL)
  episode_data <- reactiveVal(NULL)
  matched_episodes <- reactiveVal(NULL)
  selected_episode <- reactiveVal(NULL)
  episode_scores <- reactiveVal(NULL)  # 新增用于存储分数的 reactiveVal
  nearest_episode <- reactiveVal(NULL) # 存储最相似的 episode
  user_input_status <- reactiveVal("initial") # 跟踪用户输入状态
  
  # 搜索播客
  observeEvent(input$search, {
    req(input$podcast_name)
    podcasts <- get_podcasts_by_name(input$podcast_name, token())
    podcast_data(podcasts)
    matched_episodes(NULL)
    selected_episode(NULL)
    user_input_status("podcast_selected") # 更新状态
  })
  
  # 搜索单集并获取单集信息
  observeEvent(input$search_episode, {
    req(input$episode_name, input$selected_podcast)
    selected_podcast <- podcast_data() %>%
      filter(podcast_name == input$selected_podcast)
    episodes <- get_all_episodes_by_podcast(selected_podcast$podcast_uri, token())
    episode_data(episodes)
    
    # 匹配单集
    all_episodes <- episode_data()
    matches <- all_episodes %>% filter(grepl(input$episode_name, episode_name, ignore.case = TRUE))
    matched_episodes(matches)
    if (nrow(matches) > 0) {
      selected_episode(matches[1, ]) # 默认选择第一个匹配的单集
    }
    episode_scores(NULL)  # 清空旧的分数
    nearest_episode(NULL) # 清空最相似 episode
    user_input_status("episode_selected") # 更新状态
  })
  
  # 对选定单集评分
  observeEvent(input$selected_episode_name, {
    req(selected_episode())
    selected <- selected_episode()
    print(selected$description)
    # 获取描述并计算分数
    desc <- selected$description
    if (!is.null(desc) && desc != "") {
      # 调用 Python 计算得分
      score <- score_calculator$calculate_similarity_scores(desc)
      print(paste("Python returned scores:", score))  # 确认 Python 返回分数
      
      # 更新分数到 reactiveVal
      episode_scores(list(
        Science = as.numeric(score[1]),
        Finance = as.numeric(score[2]),
        Thrilling = as.numeric(score[3])
      ))
      
      # 找到最相似的 episode
      nearest <- find_nearest_episode(episode_scores(), similarity_df)
      nearest_episode(nearest)
      
    } else {
      episode_scores(list(
        Science = NA,
        Finance = NA,
        Thrilling = NA
      ))
      nearest_episode(NULL)
    }
  
    
    user_input_status("episode_scored")
  })
  
  # 动态显示主面板内容
  output$main_content <- renderUI({
    status <- user_input_status()
    if (status == "initial") {
      # 状态为初始状态时的提示
      tags$p(
        "Please enter podcast and episode name.",
        style = "font-size: 20px; font-weight: bold; color: #444444; font-family: 'CourierPrime';"
      )
    } else if (status == "podcast_selected") {
      # 播客已选择但未输入单集名称时的提示
      tags$p(
        "Podcast selected. Please enter episode name.",
        style = "font-size: 20px; font-weight: bold; color: #444444; font-family: 'CourierPrime';"
      )
    } else if (status == "episode_selected" || status == "episode_scored") {
      req(selected_episode())
      req(episode_scores())  # 确保分数已生成
      req(nearest_episode)
      scores <- episode_scores()
 
      # 确保分数是数值类型并格式化为 4 位小数
      science_score <- formatC(as.numeric(scores$Science), format = "f", digits = 4)
      finance_score <- formatC(as.numeric(scores$Finance), format = "f", digits = 4)
      thrilling_score <- formatC(as.numeric(scores$Thrilling), format = "f", digits = 4)
      
      nearest <- nearest_episode()
      nearest_name <- nearest$name
      nearest_science <- nearest$similarity_science
      nearest_finance <- nearest$similarity_finance
      nearest_thrilling <- nearest$similarity_thrilling
      
      tagList(
        h3("Scores"),
        tags$ul(
          tags$li(HTML(paste0("<span style='font-size:18px; font-weight:bold;'>Science Score: ", science_score, "</span>"))),
          tags$li(HTML(paste0("<span style='font-size:18px; font-weight:bold;'>Finance Score: ", finance_score, "</span>"))),
          tags$li(HTML(paste0("<span style='font-size:18px; font-weight:bold;'>Thrilling Score: ", thrilling_score, "</span>")))
        ),
        h3("The Most Similar Episode"),
        tags$ul(
          tags$li(HTML(paste0("<span style='font-size:18px; font-weight:bold;'>Name: ", nearest_name, "</span>"))),
          tags$li(HTML(paste0("<span style='font-size:18px; font-weight:bold;'>Science Score: ", nearest_science, "</span>"))),
          tags$li(HTML(paste0("<span style='font-size:18px; font-weight:bold;'>Finance Score: ", nearest_finance, "</span>"))),
          tags$li(HTML(paste0("<span style='font-size:18px; font-weight:bold;'>Thrilling Score: ", nearest_thrilling, "</span>")))
        )
      )
    }
  })
  
  
  output$radar_chart <- renderPlot({
    req(episode_scores(), nearest_episode())
    
    radar_data <- data.frame(
      Science = c(1, 0, episode_scores()$Science, nearest_episode()$similarity_science),
      Finance = c(1, 0, episode_scores()$Finance, nearest_episode()$similarity_finance),
      Thrilling = c(1, 0, episode_scores()$Thrilling, nearest_episode()$similarity_thrilling)
    )
    rownames(radar_data) <- c("Max", "Min", "Selected Episode", "Nearest Neighbor")
    
    op <- par(mar = c(1, 0.8, 0.8, 0.8))
    create_beautiful_radarchart(data = radar_data, caxislabels = c(0, 0.25, 0.5, 0.75, 1))
    legend(
      x = "bottom", legend = rownames(radar_data[-c(1, 2),]), horiz = TRUE,
      bty = "n", pch = 20, col = c("#0000FF", "#FC4E07"),
      text.col = "black", cex = 1.2, pt.cex = 1.5
    )
    par(op)
  })
  
  # 显示单集详情表格
  output$episode_details <- renderTable({
    req(selected_episode())
    selected_episode() %>%
      select(`Episode Name` = episode_name, 
             `Release Date` = release_date,
             Description = description)
  })
  
  # 渲染播客选择器
  output$podcast_selector <- renderUI({
    req(podcast_data())
    selectInput("selected_podcast", "Select a Podcast:", 
                choices = podcast_data()$podcast_name, 
                selected = podcast_data()$podcast_name[1])
  })
  
  # 显示播客信息表格
  output$podcast_table <- renderTable({
    req(input$selected_podcast, podcast_data())
    podcast_data() %>%
      filter(podcast_name == input$selected_podcast) %>%
      select(`Podcast Name` = podcast_name, Publisher = publisher)
  })
  
  # 显示播客封面
  output$podcast_image <- renderUI({
    req(input$selected_podcast, podcast_data())
    selected_podcast <- podcast_data() %>%
      filter(podcast_name == input$selected_podcast)
    
    img_url <- selected_podcast$image
    if (!is.na(img_url)) {
      tags$img(src = img_url, height = "150px", style = "margin: 5px;")
    }
  })
  
  # 显示匹配的单集选择器
  output$episode_selector <- renderUI({
    req(matched_episodes())
    selectInput("selected_episode_name", "Select a Matching Episode:", 
                choices = matched_episodes()$episode_name,
                selected = matched_episodes()$episode_name[1])
  })
  
  # 更新用户选择的单集
  observeEvent(input$selected_episode_name, {
    req(matched_episodes())
    matches <- matched_episodes()
    selected_episode(matches %>% filter(episode_name == input$selected_episode_name))
  })
  
  # 显示单集信息表格
  output$episode_result <- renderTable({
    req(selected_episode())
    selected_episode() %>%
      select(`Episode Name` = episode_name, `Release Date` = release_date)
  })
}



shinyApp(ui = ui, server = server)
