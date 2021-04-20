## server.R ##
#library(shiny)
#library(shinydashboard)



server <- function(input, output, session) {
  
  # ---- ABOUT PAGE ----
  output$resid_plot = renderImage({
    list(src = "model_res_plot.png",
         alt = "Sentiment Model Residual Plot")
  }, deleteFile = FALSE)
  
  # ---- DATA PAGE ----
  output$sent_dataframe = renderDataTable(data[, !(colnames(data) %in% c("cleaned_text", "doc_labels",
                                                                         "doc_label_weights"))],
                                          options = list(
                                            pageLength = 3,
                                            scrollX = TRUE))
  output$freq_dataframe = renderDataTable(freq_data,
                                          options = list(
                                            pageLength = 3,
                                            scrollX = TRUE))
  output$topic_dataframe = renderDataTable(topic_data,
                                           options = list(
                                             pageLength = 3,
                                             scrollX = TRUE))
  
  
  # ----- PAGE 1, SENTIMENT ------
  
  subdata = reactive({
    subset(data, Player == input$sent_player, select = c("Player",
                                                         "emot_anger",
                                                         "emot_disgust",
                                                         "emot_fear",
                                                         "emot_joy",
                                                         "emot_sadness",
                                                         "season_begin",
                                                         "season"))
  })
  
  observe({
    updateSelectInput(session, "sent_season", choices = c("Average",sort(data[data$Player==input$sent_player,"season"])))
  })
  
  subscores = reactive({
    if (input$sent_season=="Average") {
      s = colMeans(subdata()[,c("emot_anger", 
                                "emot_disgust", 
                                "emot_fear", 
                                "emot_joy", 
                                "emot_sadness")])
    } else {
      s = subdata()[subdata()$season==input$sent_season,c("emot_anger", 
                                                          "emot_disgust", 
                                                          "emot_fear", 
                                                          "emot_joy", 
                                                          "emot_sadness")]
    }
    return(s)
  })
  
  output$sent_plot = renderPlotly({
    s = unlist(subscores(), use.names=FALSE)
    
    # Add all scores
    all_scores = colMeans(data[,c("emot_anger", 
                                  "emot_disgust", 
                                  "emot_fear", 
                                  "emot_joy", 
                                  "emot_sadness")])
    all_s = unlist(all_scores, use.names=FALSE)
    
    plot_ly(type = "scatterpolar",
            mode="markers",
            fill="toself") %>% 
      add_trace(r = all_s,
                theta = c("Anger", 
                          "Disgust", 
                          "Fear", 
                          "Joy", 
                          "Sadness"),
                mode = "lines",
                name = "NBA Average Sentiment",
                showlegend = T,
                opacity = avg_opcty,
                fillcolor = avg_clr,
                line = list(color = avg_clr)) %>%
      add_trace(r = s,
                theta = c("Anger", 
                          "Disgust", 
                          "Fear", 
                          "Joy", 
                          "Sadness"),
                mode="markers",
                name = paste(input$sent_player,"Sentiment"),
                showlegend = T,
                opacity = plyr_opcty,
                fillcolor = plyr_clr,
                marker = list(color = plyr_clr)) %>%
      layout(polar = list(radialaxis = list(visible = T,
                                            range = c(0.0,1.0),
                                            title = list(text="Percentile",
                                                         font = list(size=8)))),
             showlegend = T)
  })
  
  
  output$sent_pred_box = renderInfoBox({
    pred = pmScores(input$sent_player, pm)[2]
    perc = pmScores(input$sent_player, pm)[3]
    if (perc > 0.5) {
      clr = "green"
      icn = "plus"
    } else {
      clr = "red"
      icn = "minus"
    }
    score_str = as.character(round(pred,4))
    if (pred > 0) {
      score_str = paste0("+",score_str)
    }
    infoBox(
      "SBPM", score_str, icon = icon(icn), fill = TRUE,
      color = clr
    )
  })
  
  output$sent_perc_box = renderInfoBox({
    pred = pmScores(input$sent_player, pm)[2]
    perc = pmScores(input$sent_player, pm)[3]
    if (perc > 0.5) {
      clr = "green"
      icn = "plus"
    } else {
      clr = "red"
      icn = "minus"
    }
    score_str = paste0(as.character(round(perc*100,2)),"%")
    infoBox(
      "SBPM%", score_str, icon = icon(icn), fill = TRUE,
      color = clr
    )
  })
  
  
  # ---- Page 2, FREQUENCY ----
  
  freq_subdata = reactive({
    subset(freq_data, Player == input$freq_player, select = c("Player",
                                                              "text_length",
                                                              "num_tweets",
                                                              "num_mentions",
                                                              "season",
                                                              "avg_tweet_length"))
  })
  
  observe({
    updateSelectInput(session, "freq_season", choices = c("Average",sort(freq_data[freq_data$Player==input$freq_player,"season"])))
  })
  
  freq_subscores = reactive({
    if (input$freq_season=="Average") {
      s = colMeans(freq_subdata()[,c("text_length",
                                     "num_tweets",
                                     "num_mentions",
                                     "avg_tweet_length")])
    } else {
      s = freq_subdata()[freq_subdata()$season==input$freq_season,c("text_length",
                                                                    "num_tweets",
                                                                    "num_mentions",
                                                                    "avg_tweet_length")]
    }
    return(s)
  })
  
  output$freq_plot = renderPlotly({
    s = unlist(freq_subscores(), use.names=FALSE)
    
    x1 = list(title = "Average Tweet Length")
    y1 = list(title = "# of NBA Players")
    x2 = list(title = "Tweets/Year")
    y2 = list(title = "# of NBA Players")
    
    fig1 =  plot_ly()
    fig1 = fig1 %>% add_histogram(x = freq_data$avg_tweet_length, 
                                  type = "histogram",
                                  opacity = avg_opcty,
                                  marker = list(color = avg_clr),
                                  name = "NBA Avg Tweet Length")
    fig1 = fig1 %>% add_segments(x=s[4], 
                                 y=0, 
                                 xend=s[4], 
                                 yend=285, 
                                 line=list(color=plyr_clr, width = 5),
                                 mode="lines",
                                 name = input$freq_player)
    fig1 = fig1 %>% layout(xaxis = x1, yaxis = y1, showlegend = TRUE)
    
    fig2 = plot_ly(alpha = 0.6)
    fig2 = fig2 %>% add_histogram(x = freq_data$num_tweets, 
                                  type = "histogram",
                                  opacity = avg_opcty,
                                  marker = list(color = avg_clr),
                                  name = "NBA Tweets/Year")
    fig2 = fig2 %>% add_segments(x=s[2], 
                                 y=0, 
                                 xend=s[2], 
                                 yend=800,
                                 line=list(color=plyr_clr, width = 5),
                                 mode="lines",
                                 name = input$freq_player)
    fig2 = fig2 %>% layout(xaxis = x2, yaxis = y2, showlegend = FALSE)
    fig =  subplot(fig1, fig2, nrows = 2, margin = 0.2, titleX = TRUE, titleY = TRUE, shareY = TRUE)
    fig
    
  })
  
  output$freq_pred_box = renderInfoBox({
    pred = pmScores(input$freq_player, pm)[4]
    perc = pmScores(input$freq_player, pm)[5]
    if (perc > 0.5) {
      clr = "green"
      icn = "plus"
    } else {
      clr = "red"
      icn = "minus"
    }
    score_str = as.character(round(pred,4))
    if (pred > 0) {
      score_str = paste0("+",score_str)
    }
    infoBox(
      "UBPM", score_str, icon = icon(icn), fill = TRUE,
      color = clr
    )
  })
  
  output$freq_perc_box = renderInfoBox({
    pred = pmScores(input$freq_player, pm)[4]
    perc = pmScores(input$freq_player, pm)[5]
    if (perc > 0.5) {
      clr = "green"
      icn = "plus"
    } else {
      clr = "red"
      icn = "minus"
    }
    score_str = paste0(as.character(round(perc*100,2)),"%")
    infoBox(
      "UBPM%", score_str, icon = icon(icn), fill = TRUE,
      color = clr
    )
  })
  
  
  
  
  # ---- PAGE 3, TOPIC ----
  
  topic_subdata = reactive({
    subset(topic_data, Player == input$topic_player)
  })
  
  observe({
    updateSelectInput(session, "topic_season", choices = c("Average",sort(data[data$Player==input$topic_player,"season"])))
  })
  
  
  topic_subscores = reactive({
    drops = c("Player","twitter_handle","season")
    ts = topic_subdata()[ , !(names(topic_subdata()) %in% drops)]
    if (input$topic_season=="Average") {
      s = as.data.frame.list(colMeans(ts))
    } else {
      temp = topic_subdata()[topic_subdata()$season==input$topic_season,]
      s = temp[ , !(names(temp) %in% drops)]
    }
    s = s[,order(-s[nrow(s),])]
    return(s)
  })
  
  output$topic_plot = renderPlotly({
    s = topic_subscores()
    top_topics = unlist(lapply(colnames(s)[1:5],clean_topic_names))
    top_scores = as.numeric(s[1,1:5])
    top_data = data.frame(top_topics, top_scores, stringsAsFactors = FALSE)
    top_data$top_topics <- factor(top_data$top_topics, levels = unique(top_data$top_topics)[order(top_data$top_scores, decreasing = TRUE)])
    fig1 = plot_ly(top_data,
                   x = ~top_topics,
                   y = ~top_scores,
                   type = "bar",
                   marker=list(color=plyr_clr),
                   name = paste(input$topic_player,"Tweet Topics"))
    x = list(title = "Top 5 Tweet Topics")
    y = list(title = "Tweet Topic Score*")
    fig1 = fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
    fig1
  })
  
  output$topic_plot_average = renderPlotly({
    drops = c("Player","twitter_handle","season")
    ts = topic_data[ , !(names(topic_data) %in% drops)]
    s = as.data.frame.list(colMeans(ts))
    s = s[,order(-s[nrow(s),])]
    top_topics = unlist(lapply(colnames(s)[1:5],clean_topic_names))
    top_scores = as.numeric(s[1,1:5])
    top_data = data.frame(top_topics, top_scores, stringsAsFactors = FALSE)
    top_data$top_topics <- factor(top_data$top_topics, levels = unique(top_data$top_topics)[order(top_data$top_scores, decreasing = TRUE)])
    fig1 = plot_ly(top_data,
                   x = ~top_topics,
                   y = ~top_scores,
                   type = "bar",
                   marker=list(color=avg_clr),
                   name = paste(input$topic_player,"Tweet Topics"))
    x = list(title = "Top 5 Tweet Topics - NBA Average")
    y = list(title = "Tweet Topic Score*")
    fig1 = fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
    fig1
  })
  
  output$topic_pred_box = renderInfoBox({
    pred = pmScores(input$topic_player, pm)[6]
    perc = pmScores(input$topic_player, pm)[7]
    if (perc > 0.5) {
      clr = "green"
      icn = "plus"
    } else {
      clr = "red"
      icn = "minus"
    }
    score_str = as.character(round(pred,4))
    if (pred > 0) {
      score_str = paste0("+",score_str)
    }
    infoBox(
      "TBPM", score_str, icon = icon(icn), fill = TRUE,
      color = clr
    )
  })
  
  output$topic_perc_box = renderInfoBox({
    pred = pmScores(input$topic_player, pm)[6]
    perc = pmScores(input$topic_player, pm)[7]
    if (perc > 0.5) {
      clr = "green"
      icn = "plus"
    } else {
      clr = "red"
      icn = "minus"
    }
    score_str = paste0(as.character(round(perc*100,2)),"%")
    infoBox(
      "TBPM%", score_str, icon = icon(icn), fill = TRUE,
      color = clr
    )
  })
  
  
  # ---- PROJECTIONS PAGE ----
  
  
  observeEvent(input$proj_submit, {
    if (input$proj_handle != "@twitter_handle") {
      
      proj_list = getTwitterInfo(input$proj_handle)
      
      output$rendering_text = renderText({ paste0("Scores gathered for ",input$proj_handle) })
      
      # 3 graphs
      
      output$proj_sent_plot = renderPlotly({
        s = unlist(proj_list$proj_sent_scores[,1:5], use.names=FALSE)
        all_scores = colMeans(data[,c("emot_anger", 
                                      "emot_disgust", 
                                      "emot_fear", 
                                      "emot_joy", 
                                      "emot_sadness")])
        all_s = unlist(all_scores, use.names=FALSE)
        
        plot_ly(type = "scatterpolar",
                mode="markers",
                fill="toself") %>% 
          add_trace(r = all_s,
                    theta = c("Anger", 
                              "Disgust", 
                              "Fear", 
                              "Joy", 
                              "Sadness"),
                    mode = "lines",
                    name = "NBA Average Sentiment",
                    showlegend = T,
                    opacity = avg_opcty,
                    fillcolor = avg_clr,
                    line = list(color = avg_clr)) %>%
          add_trace(r = s,
                    theta = c("Anger", 
                              "Disgust", 
                              "Fear", 
                              "Joy", 
                              "Sadness"),
                    mode="markers",
                    name = paste(input$proj_handle,"Sentiment"),
                    showlegend = T,
                    opacity = plyr_opcty,
                    fillcolor = plyr_clr,
                    marker = list(color = plyr_clr)) %>%
          layout(polar = list(radialaxis = list(visible = T,
                                                range = c(0.0,1.0),
                                                title = list(text="Percentile",
                                                             font = list(size=8)))),
                 showlegend = T)
      })
      
      output$proj_freq_plot = renderPlotly({
        s = proj_list$proj_freq_scores
        
        x1 = list(title = "Average Tweet Length")
        y1 = list(title = "# of NBA Players")
        x2 = list(title = "Tweets/Year")
        y2 = list(title = "# of NBA Players")
        
        fig1 =  plot_ly()
        fig1 = fig1 %>% add_histogram(x = freq_data$avg_tweet_length, 
                                      type = "histogram",
                                      opacity = avg_opcty,
                                      marker = list(color = avg_clr),
                                      name = "NBA Avg Tweet Length")
        fig1 = fig1 %>% add_segments(x=s$avg_tweet_length, 
                                     y=0, 
                                     xend=s$avg_tweet_length, 
                                     yend=285, 
                                     line=list(color=plyr_clr, width = 5),
                                     mode="lines",
                                     name = input$proj_handle)
        fig1 = fig1 %>% layout(xaxis = x1, yaxis = y1, showlegend = TRUE)
        
        fig2 = plot_ly(alpha = 0.6)
        fig2 = fig2 %>% add_histogram(x = freq_data$num_tweets, 
                                      type = "histogram",
                                      opacity = avg_opcty,
                                      marker = list(color = avg_clr),
                                      name = "NBA Tweets/Year")
        fig2 = fig2 %>% add_segments(x=s$num_tweets, 
                                     y=0, 
                                     xend=s$num_tweets, 
                                     yend=800,
                                     line=list(color=plyr_clr, width = 5),
                                     mode="lines",
                                     name = input$proj_handle)
        fig2 = fig2 %>% layout(xaxis = x2, yaxis = y2, showlegend = FALSE)
        fig =  subplot(fig1, fig2, nrows = 2, margin = 0.2, titleX = TRUE, titleY = TRUE, shareY = TRUE)
        fig
      })
      
      
      output$proj_topic_plot = renderPlotly({
        s = proj_list$proj_topic_scores
        s = s[,order(-s[nrow(s),])]
        top_topics = unlist(lapply(colnames(s)[1:5],clean_topic_names))
        top_scores = as.numeric(s[1,1:5])
        top_data = data.frame(top_topics, top_scores, stringsAsFactors = FALSE)
        top_data$top_topics <- factor(top_data$top_topics, levels = unique(top_data$top_topics)[order(top_data$top_scores, decreasing = TRUE)])
        fig1 = plot_ly(top_data,
                       x = ~top_topics,
                       y = ~top_scores,
                       type = "bar",
                       marker=list(color=plyr_clr),
                       name = paste(input$proj_handle,"Tweet Topics"))
        x = list(title = "Top 5 Tweet Topics")
        y = list(title = "Tweet Topic Score*")
        fig1 = fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
        fig1
      })
      
      output$proj_scores_table = renderDataTable(updateProjTable(proj_list))
      
    }
  })
  
  updateProjTable = function(proj_list) {
    # trained in models.R
    sent_xgb_fit = readRDS("./sent_model.rds")
    sent_all_predicts = readRDS("./sent_full_preds.rds")
    sent_percentile = ecdf(sent_all_predicts)
    
    freq_xgb_fit = readRDS("./freq_model.rds")
    freq_all_predicts = readRDS("./freq_full_preds.rds")
    freq_percentile = ecdf(freq_all_predicts)
    
    topic_xgb_fit = readRDS("./topc_model.rds")
    topic_all_predicts = readRDS("./topc_full_preds.rds")
    topic_percentile = ecdf(topic_all_predicts)
    
    get_pred_perc = function(scores_list, xgb_fit, topic_percentile) {
      pred = predict(xgb_fit, scores_list)
      perc = topic_percentile(pred)
      proj__list = list(pred, perc)
      names(proj__list) = c("pred", "perc")
      return(proj__list)
    }
    
    
    # Get projections
    # 1. Sent
    sent_scores = proj_list$proj_sent_scores
    colnames(sent_scores) = c("emot_anger", 
                           "emot_fear",
                           "emot_disgust",
                           "emot_joy",
                           "emot_sadness",
                           "sent_score")
    proj_sent_list = get_pred_perc(sent_scores, sent_xgb_fit, sent_percentile)

    # 2. Freq
    freq_scores = data.frame(c(proj_list$proj_freq_scores$num_tweets),
                             c(proj_list$proj_freq_scores$num_mentions),
                             c(proj_list$proj_freq_scores$avg_tweet_length))
    colnames(freq_scores) = c("num_tweets",
                              "num_mentions",
                              "avg_tweet_length")
    proj_freq_list = get_pred_perc(freq_scores, freq_xgb_fit, freq_percentile)
    
    
    # 3. Topic
    topic_scores = proj_list$proj_topic_scores
    proj_topic_list = get_pred_perc(topic_scores, topic_xgb_fit, topic_percentile) 
    
    score = c(proj_sent_list$pred, proj_freq_list$pred, proj_topic_list$pred)
    percentile = c(proj_sent_list$perc, proj_freq_list$perc, proj_topic_list$perc)
    category = c("Sentiment (SBPM)", "Usage (UPBM)", "Topic (TBPM)")
    proj_df = data.frame(category, score, percentile)
    colnames(proj_df) = c("Category", "Projected BPM", "Percentile (in NBA)")
    return(proj_df)
  }
  
}







