# ---- Packages ----
library(shiny)
library(stringr)
library(ggplot2)
library(plotly)
library(dplyr)
library(knitr)
library(shinydashboard)
library(rtweet)
library(httr)
library(jsonlite)
library(reshape)
library(lubridate)
library(caret)
library(mlbench)
library(xgboost)



# ---- Page 1, SENTIMENT ---- 

data = read.csv("nlu_scores_final.csv")
clean_season = function(s) {
  return(as.numeric(stringr::str_split(s,"-")[[1]][[1]]))
}
data$season_begin = lapply(data$season,clean_season)
data$season_begin = as.numeric(data$season_begin)


# ---- Page 2, FREQUENCY ----

freq_data = read.csv("frequency_scores.csv")
freq_data = unique(merge(x = freq_data, y = data[,c("Player","twitter_handle")], by = "twitter_handle", all.x = TRUE))
freq_data$avg_tweet_length = freq_data$text_length / freq_data$num_tweets


# ---- Page 3, TOPICS ----
clean_topic_names = function(s) {
  return(toupper(str_replace_all(s, "[.]", " ")))
}
topic_data = read.csv("nlu_topic_scores_general.csv")


# ---- Colors ----
avg_opcty = 0.5
plyr_opcty = 0.5
avg_clr = "#808080"
plyr_clr = "#F0A202"


# ---- Plus Minus Scores ----
pm = read.csv("plus_minus_scores.csv")
pm = pm[ , !(names(pm) %in% c("X"))]
pm = unique(merge(x = pm, y = data[,c("Player","twitter_handle")], by = "twitter_handle", all.x = TRUE))
pmScores = function(player_name, pm = pm) {
  scores = subset(pm, 
                  Player == player_name,
                  select=c(Player,
                           sent_pred,
                           sent_perc,
                           freq_pred,
                           freq_perc,
                           topc_pred,
                           topc_perc))
  return(scores)
}





# ---- PROJECTIONS PAGE ----

# twitter keys
api_key = "oixmKPW8n8t2lc36fPdl4mSAF"
api_secret_key = "32UMo5DcM8dMrgBIzYn46TAHw4PWdg73FLx5sCLmBRl3H10V8K"
access_token = "1352296820634910720-PgHGnS2ruMNHaJqgV6D0JvWxbsRqph"
access_token_secret = "f9tnbr6H8ZKLKPxXxtgznK22O7n2GbFZnG7rVXGQgAqad"
ibm_key = "DxXZG9G24IFMBznqOQcI3SYa7MW6kU-hj3Hb3cWa-CoI"

## authenticate via web browser
token = create_token(
  app = "NBA Tweet Draft Prospect",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

clean = function(text) {
  # Set the text to lowercase
  text = tolower(text)
  text = gsub("&amp", " ", text)
  text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
  text = gsub("@\\w+",  "", text)
  text = gsub("[[:punct:]]", "", text)
  text = gsub("[[:digit:]]", " ", text)
  text = gsub("http\\w+", " ", text)
  text = gsub("[ \t]{2,}", " ", text)
  text = gsub("^\\s+|\\s+$", " ", text) 
  text = gsub("\\n", " ", text) 
  return(text)
}


# FIXME should have error for empty dataframes
watsonNLUtoDF = function(data, username, verbose = F, language = 'en') {
  
  ## Url for Watson NLU service on Bluemix
  base_url = "https://api.us-south.natural-language-understanding.watson.cloud.ibm.com/instances/60e2504b-0a72-434f-b0a6-606bca97a695/v1/analyze?version=2020-08-01"
  
  ## Initialize Empty Dataframes
  conceptsDF = data.frame()
  keywordsDF = data.frame()
  emotionDF = data.frame()
  sentimentDF = data.frame()
  categoriesDF = data.frame()
  analyzedTextDF = data.frame()
  
  ## Loop over each id, identify the type and send the value to Watson
  for (i in 1:nrow(data)){
    try({
      
      id = data$screen_name[i]
      value = data$text[i]
      
      ## Define the JSON payload for NLU
      body = list(api_endpoint = value, 
                  features = list(
                    categories = {},
                    concepts = {},
                    keywords = {},
                    emotion = {},
                    sentiment = {}),
                  language = language,
                  return_analyzed_text = TRUE)
      
      ## Provide the correct type for each id
      names(body)[1] = "text"
      
      if(verbose == T){
        ##print(paste("Sending", "text", "for", id, "to Watson NLU..."))
      }
      
      ## Hit the API and return JSON
      watsonResponse = POST(base_url,
                            content_type_json(),
                            authenticate("apikey",username),
                            body = toJSON(body, auto_unbox = T)) 
      
      ## Parse JSON into dataframes
      concepts = data.frame(id = id, 
                            fromJSON(toJSON(content(watsonResponse), pretty = T), flatten = T)$concepts,
                            stringsAsFactors = F)
      
      keywords = data.frame(id = id, 
                            fromJSON(toJSON(content(watsonResponse), pretty = T), flatten = T)$keywords,
                            stringsAsFactors = F)
      
      emotion = data.frame(id = id, 
                           fromJSON(toJSON(content(watsonResponse), pretty = T), flatten = T)$emotion,
                           stringsAsFactors = F)
      
      sentiment = data.frame(id = id, 
                             fromJSON(toJSON(content(watsonResponse), pretty = T), flatten = T)$sentiment,
                             stringsAsFactors = F)
      
      categories = data.frame(id = id,
                              fromJSON(toJSON(content(watsonResponse), pretty = T), flatten = T)$categories,
                              stringsAsFactors = F)
      
      analyzedText = data.frame(id = id,
                                fromJSON(toJSON(content(watsonResponse), pretty = T), flatten = T)$analyzed_text,
                                stringsAsFactors = F)   
      
      ## Append results to output dataframes
      conceptsDF = rbind(conceptsDF, concepts)
      keywordsDF = rbind(keywordsDF, keywords)
      emotionDF = rbind(emotionDF, emotion)
      sentimentDF = rbind(sentimentDF, sentiment)
      categoriesDF = rbind(categoriesDF, categories)
      analyzedTextDF = rbind(analyzedTextDF, analyzedText)
      
      if(verbose == T) {
        ##print(paste("Iteration", i, "of", nrow(data), "complete."))
      }
    })
  }
  resultsList = list(conceptsDF, keywordsDF, emotionDF, sentimentDF, categoriesDF, analyzedTextDF, watsonResponse)
  names(resultsList) = c("conceptsDF", "keywordsDF", "emotionDF", "sentimentDF", "categoriesDF", "analyzedTextDF", "response")
  return(resultsList)
}


get_text_length = function(s) {
  return(length(str_split(clean(s)," ")[[1]]))
}

get_label = function(s) {
  return(str_replace_all(str_split(s,'/')[[1]][2]," ","."))
}


getTwitterInfo = function(handle) {
  # GET HANDLE
  h = substr(handle, start = 1, stop = 1)
  if (h == "@") {
    handle = substring(handle, first = 2)
  }
  
  # GET TIMELINE
  #print(paste0("Getting timeline for ",handle))
  timeline = get_timelines(handle, n = 3200)
  #print(paste0(handle, " had ", as.character(nrow(timeline))," tweets gathered."))
  
  # CLEAN TEXT
  proj_text_dirty = paste(sapply(timeline$text, paste, collapse=" "), collapse=" ")
  proj_text = clean(proj_text_dirty)
  #print("Got the text cleaned and stuff")
  
  # CHECK TEXT LENGTH FIXME
  if (length(str_split(proj_text," ")) < 5) {
    # FIXME should return an error
  }
  
  # CALL IBM API
  text = c(proj_text)
  screen_name = c(handle)
  my_df = data.frame(text, screen_name)
  #print(paste0("Pinging the IBM API for user ",handle))
  responseList = watsonNLUtoDF(my_df, ibm_key, verbose = T)
  #print("Finished!")
  
  # 1. nlu_scores
  anger = c(responseList$emotionDF$document.emotion.anger)
  disgust = c(responseList$emotionDF$document.emotion.disgust)
  fear = c(responseList$emotionDF$document.emotion.fear)
  joy = c(responseList$emotionDF$document.emotion.joy)
  sadness = c(responseList$emotionDF$document.emotion.sadness)
  sentiment = c(responseList$sentimentDF$document.score)
  proj_sent_scores = data.frame(anger,disgust, fear, joy, sadness, sentiment)
  
  
  # 2. frequency scores
  avg_tweet_length = mean(unlist(lapply(timeline$text, get_text_length)))
  month_counts = timeline %>% count(month=floor_date(created_at, "month"))
  if (nrow(month_counts) > 3) {
    num_tweets = mean(month_counts[3:nrow(month_counts)-1,,drop=F]$n) * 12
  } else {
    #cat("This happened\n")
    num_tweets = mean(month_counts$n) * 12
  }
  has_mention = function(s) {
    if (grepl("@",s,fixed=TRUE)) {
      return(1)
    } else {
      return(0)
    }
  }
  num_mentions = sum(unlist(lapply(timeline$text, has_mention))) / nrow(timeline) * num_tweets
  proj_freq_scores = data.frame(avg_tweet_length, num_tweets, num_mentions)

  # 3. Topic Scores
  t_scores_names = lapply(responseList$categoriesDF$label,get_label)
  t_scores = responseList$categoriesDF$score
  t_list = as.list(rep(0, 24))
  names(t_list) <- colnames(topic_data)
  proj_topic_scores = data.frame(t_list)
  proj_topic_scores = proj_topic_scores[ , !(colnames(proj_topic_scores) %in% c("Player"))]
  for (i in 1:length(t_scores_names)) {
    name = t_scores_names[i][[1]]
    if (!(name %in% colnames(proj_topic_scores))) {
      next
    }
    score = t_scores[i][[1]]
    proj_topic_scores[name][1] = proj_topic_scores[name][1] + score
  }
  
  
  resultsList = list(proj_sent_scores,proj_freq_scores,proj_topic_scores)
  names(resultsList) = c("proj_sent_scores", "proj_freq_scores", "proj_topic_scores")
  return(resultsList)
}











