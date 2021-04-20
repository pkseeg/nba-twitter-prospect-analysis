library(rtweet)


## store api keys (these are fake example values; replace with your own keys)
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

timeline = get_timelines("andyblarsen", n = 3200)



# steps
# 1. concatenate all tweets into a string
proj_text_dirty = paste(sapply(timeline$text, paste, collapse=" "), collapse=" ")
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
proj_text = clean(proj_text_dirty)

# 2. check the length of the string

# 3. call nlu API to get scores



library(httr)
library(jsonlite)
library(reshape)
library(dplyr)


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
      
      ## In my particular data set, "Company_Source" is a concatenated field of Company and social media Source
      ## (e.g., "Southwest (Facebook)"), and "Content" represents the actual Facebook post or Tweet
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
        print(paste("Sending", "text", "for", id, "to Watson NLU..."))
      }
      
      ## Hit the API and return JSON
      watsonResponse = POST(base_url,
                             content_type_json(),
                             authenticate("apikey",username),
                             body = toJSON(body, auto_unbox = T)) 
      #print(watsonResponse)
      #print(fromJSON(toJSON(content(watsonResponse), pretty = T), flatten = T)$concepts)
      
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
        print(paste("Iteration", i, "of", nrow(data), "complete."))
      }
    })
  }
  resultsList = list(conceptsDF, keywordsDF, emotionDF, sentimentDF, categoriesDF, analyzedTextDF, watsonResponse)
  names(resultsList) = c("conceptsDF", "keywordsDF", "emotionDF", "sentimentDF", "categoriesDF", "analyzedTextDF", "response")
  return(resultsList)
}
#The last part of this code walks you through next steps of sending and collecting data to/from Watson NLU.

text = c(proj_text)
screen_name = c("andyblarsen")
my_df = data.frame(text, screen_name)
responseList = watsonNLUtoDF(my_df, ibm_key, verbose = T)

# now I just need to build 3 dfs from my responseList
# 1. nlu_scores
#  - anger
anger = c(responseList$emotionDF$document.emotion.anger)
#  - disgust
disgust = c(responseList$emotionDF$document.emotion.disgust)
#  - fear
fear = c(responseList$emotionDF$document.emotion.fear)
#  - joy
joy = c(responseList$emotionDF$document.emotion.joy)
#  - sadness
sadness = c(responseList$emotionDF$document.emotion.sadness)
#  - sentiment
sentiment = c(responseList$sentimentDF$document.score)
proj_sent_scores = data.frame(anger,disgust, fear, joy, sadness, sentiment)


# 2. frequency scores
get_text_length = function(s) {
  return(length(str_split(clean(s)," ")[[1]]))
}
#  - text_length
avg_tweet_length = mean(unlist(lapply(timeline$text, get_text_length)))
#  - num_tweets
library(dplyr)
library(lubridate)
month_counts = timeline %>% count(month=floor_date(created_at, "month"))
if (nrow(month_counts) > 3) {
  num_tweets = mean(month_counts[3:nrow(month_counts)-1,,drop=F]$n) * 12
} else {
  num_tweets = mean(month_counts$n) * 12
}
#  - num_mentions
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
get_label = function(s) {
  return(str_replace_all(str_split(s,'/')[[1]][2]," ","."))
}
t_scores_names = lapply(responseList$categoriesDF$label,get_label)
t_scores = responseList$categoriesDF$score
t_list = as.list(rep(0, 24))
names(t_list) <- colnames(topic_data)
proj_topic_scores = data.frame(t_list)
for (i in 1:length(t_scores_names)) {
  name = t_scores_names[i][[1]]
  score = t_scores[i][[1]]
  proj_topic_scores[name][1] = proj_topic_scores[name][1] + score
}



