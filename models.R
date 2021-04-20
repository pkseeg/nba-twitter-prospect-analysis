library(caret)
library(mlbench)
library(Metrics)
# ---- MODEL 1, SENTIMENT ----

# Explanation:
#   - Filled all the NA values with the 5th percentile of BPM and VORP
#   - Averaged scores across all seasons -> i.e. each player had one set of scores
#   - Created XGBoost model
#       - Predicted BPM using sentiment scores
#       - Not super predictive (see scores of fit)
#   - Ran XGBoost model on scores from each player to create distribution
#   - Used distribution of predictions to give each player
#     percentile scores, then converted those to a +/- score
#     where -10 is a bad score, +10 is a good score

sent_data = read.csv("nlu_scores_final.csv")
care_data = read.csv("all_drafts.csv")
sent_data = unique(merge(x = sent_data, y = care_data[,c("twitter_handle","BPM","VORP")], by = "twitter_handle", all.x = TRUE))

# Fill in NA BPM, VORP with the 5th percentile scores (there are some very low outliers, but not playing is obviously bad)
bpm_5 = quantile(sent_data$BPM, 0.05, na.rm = TRUE) 
sent_data[is.na(sent_data$BPM),"BPM"] = bpm_5
vorp_5 = quantile(sent_data$VORP, 0.05, na.rm = TRUE)
sent_data[is.na(sent_data$VORP),"VORP"] = vorp_5

# get the average scores
sent_data = aggregate(sent_data[, c("emot_anger", 
                                    "emot_fear",
                                    "emot_disgust",
                                    "emot_joy",
                                    "emot_sadness",
                                    "sent_score",
                                    "BPM",
                                    "VORP")], list(sent_data$twitter_handle), mean)

# create 80%/20% for training and validation datasets
bpm_data = subset(sent_data,select=c(emot_anger, 
                                     emot_fear,
                                     emot_disgust,
                                     emot_joy,
                                     emot_sadness,
                                     sent_score,
                                     BPM))
set.seed(33)
val_index = createDataPartition(bpm_data$BPM, p=0.80, list=FALSE)
val = bpm_data[-val_index,]
val$player = sent_data[-val_index, ]$Group.1
trn = bpm_data[val_index,]

# train a model and summarize model
set.seed(33)
trctrl = trainControl(method = "cv", number = 5)
tune_grid = expand.grid(nrounds = 200,
                       max_depth = 5,
                       eta = 0.05,
                       gamma = 0.01,
                       colsample_bytree = 0.75,
                       min_child_weight = 0,
                       subsample = 0.5)

xgb_fit = train(BPM ~ ., data = trn, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

# Testing
# estimate skill on validation dataset
set.seed(33)
test_predict = predict(xgb_fit, val)
set.seed(33)
all_predicts = predict(xgb_fit, bpm_data)

# RMSE
rmse(val$BPM, test_predict)


# RESIDUALS PLOT
library(ggplot2)
predicted = test_predict
actual = val$BPM
twitter = val$player
res_data = data.frame(predicted, actual, twitter)
colnames(res_data) = c("Predicted", "Actual", "Handle")
ggplot(res_data, aes(x=Actual, y=Predicted)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlim(-9, 9) +
  ylim(-9, 9) +
  labs(title = "Predicted vs Actual BPM (Sentiment)",
       x = "Actual BPM",
       y = "Predicted BPM (Sent Scores)") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8)) + 
  annotate(geom="text", x=8, y=-2.5, label="Luka Doncic", color="red") +
  geom_point(aes(x=6.3, y=-3.824676), colour="red") + 
  annotate(geom="text", x=-7.5, y=0, label="Tyshawn Taylor", color="red") + 
  geom_point(aes(x=-8.1, y=-1.0140809), colour="red") + 
  annotate(geom="text", x=-7, y=-6, label="Marcus Denmon", color="darkgreen") + 
  geom_point(aes(x=-7.0, y=-6.8420448), colour="darkgreen") +
  annotate(geom="text", x=0.2, y=1, label="Jonathan Isaac", color="darkgreen") + 
  geom_point(aes(x=0.2, y=-0.1548419), colour="darkgreen")

#res_data[abs(res_data$Predicted - res_data$Actual) < 0.4, ]
#res_data[res_data$Actual < -5, ]

# Saving the Model and Distribution of all scores
#saveRDS(xgb_fit, "./sent_model.rds")
#saveRDS(all_predicts, "./sent_full_preds.rds")


# Loading the saved data
# BELOW IS ALL THE CODE YOU NEED TO MAKE IT RUN IN THE SERVER 
#    (once you've trained and saved, obviously)
xgb_fit = readRDS("./sent_model.rds")
all_predicts = readRDS("./sent_full_preds.rds")

sent_percentile = ecdf(all_predicts)
get_sent_percentile = function(scores_list, xgb_fit, sent_percentile) {
  pred = predict(xgb_fit, scores_list)
  perc = sent_percentile(pred)
  # turn that into a +/- score
  plusminus = (perc - .5) * 20
  return(plusminus)
}


# EXAMPLE DYNAMIC SENT SCORE
my_scores = list("emot_anger" = 0.1, 
                 "emot_fear" = 0.1,
                 "emot_disgust" = 0.1,
                 "emot_joy" = 0.9,
                 "emot_sadness" = 0.1,
                 "sent_score" = 0.9)
get_sent_percentile(my_scores, xgb_fit, sent_percentile)






# ---- MODEL 2, USAGE ----

# FIXME: Fix the explanation
# Explanation:
#   - Filled all the NA values with the 5th percentile of BPM and VORP
#   - Averaged scores across all seasons -> i.e. each player had one set of scores
#   - Created XGBoost model
#       - Predicted BPM using sentiment scores
#       - Not super predictive (see scores of fit)
#   - Ran XGBoost model on scores from each player to create distribution
#   - Used distribution of predictions to give each player
#     percentile scores, then converted those to a +/- score
#     where -10 is a bad score, +10 is a good score

freq_data = read.csv("frequency_scores.csv")
care_data = read.csv("all_drafts.csv")
freq_data = unique(merge(x = freq_data, y = care_data[,c("twitter_handle","BPM","VORP")], by = "twitter_handle", all.x = TRUE))
freq_data$avg_tweet_length = freq_data$text_length / freq_data$num_tweets

# Fill in NA BPM, VORP with the 5th percentile scores (there are some very low outliers, but not playing is obviously bad)
bpm_5 = quantile(freq_data$BPM, 0.05, na.rm = TRUE) 
freq_data[is.na(freq_data$BPM),"BPM"] = bpm_5
vorp_5 = quantile(freq_data$VORP, 0.05, na.rm = TRUE)
freq_data[is.na(freq_data$VORP),"VORP"] = vorp_5

# get the average scores
freq_data = aggregate(freq_data[, c("num_tweets",
                                    "num_mentions",
                                    "avg_tweet_length",
                                    "BPM",
                                    "VORP")], list(freq_data$twitter_handle), mean)

# create 80%/20% for training and validation datasets
bpm_data = subset(freq_data,select=c(num_tweets,
                                     num_mentions,
                                     avg_tweet_length,
                                     BPM))
set.seed(33)
val_index = createDataPartition(bpm_data$BPM, p=0.80, list=FALSE)
val = bpm_data[-val_index,]
trn = bpm_data[val_index,]

# train a model and summarize model
set.seed(33)
trctrl = trainControl(method = "cv", number = 5)
tune_grid = expand.grid(nrounds = 200,
                        max_depth = 5,
                        eta = 0.05,
                        gamma = 0.01,
                        colsample_bytree = 0.75,
                        min_child_weight = 0,
                        subsample = 0.5)

xgb_fit = train(BPM ~ ., data = trn, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

# Testing
# estimate skill on validation dataset
set.seed(33)
test_predict = predict(xgb_fit, val)
set.seed(33)
all_predicts = predict(xgb_fit, bpm_data)

# RMSE
rmse(val$BPM, test_predict)

# Saving the Model and Distribution of all scores
saveRDS(xgb_fit, "./freq_model.rds")
saveRDS(all_predicts, "./freq_full_preds.rds")


# Loading the saved data
# BELOW IS ALL THE CODE YOU NEED TO MAKE IT RUN IN THE SERVER 
#    (once you've trained and saved, obviously)
xgb_fit = readRDS("./freq_model.rds")
all_predicts = readRDS("./freq_full_preds.rds")

freq_percentile = ecdf(all_predicts)
get_freq_percentile = function(scores_list, xgb_fit, freq_percentile) {
  pred = predict(xgb_fit, scores_list)
  perc = freq_percentile(pred)
  # turn that into a +/- score
  plusminus = (perc - .5) * 20
  return(plusminus)
}


# EXAMPLE DYNAMIC FREQ SCORE
my_scores = list("num_tweets" = 100,
                 "num_mentions" = 80,
                 "avg_tweet_length" = 13 
                 )
get_freq_percentile(my_scores, xgb_fit, freq_percentile)






# ---- MODEL 3, TOPICS ----

# FIXME: fix explanation to fit topics
# Explanation:
#   - Filled all the NA values with the 5th percentile of BPM and VORP
#   - Averaged scores across all seasons -> i.e. each player had one set of scores
#   - Created XGBoost model
#       - Predicted BPM using sentiment scores
#       - Not super predictive (see scores of fit)
#   - Ran XGBoost model on scores from each player to create distribution
#   - Used distribution of predictions to give each player
#     percentile scores, then converted those to a +/- score
#     where -10 is a bad score, +10 is a good score

topc_data = read.csv("nlu_topic_scores_general.csv")
care_data = read.csv("all_drafts.csv")
topc_data = unique(merge(x = topc_data, y = care_data[,c("twitter_handle","BPM","VORP")], by = "twitter_handle", all.x = TRUE))

# Fill in NA BPM, VORP with the 5th percentile scores (there are some very low outliers, but not playing is obviously bad)
bpm_5 = quantile(topc_data$BPM, 0.05, na.rm = TRUE) 
topc_data[is.na(topc_data$BPM),"BPM"] = bpm_5
vorp_5 = quantile(topc_data$VORP, 0.05, na.rm = TRUE)
topc_data[is.na(topc_data$VORP),"VORP"] = vorp_5

# get the average scores
topc_data = aggregate(subset(topc_data,select=-c(twitter_handle, 
                                                 Player,
                                                 season,
                                                 VORP)), list(topc_data$twitter_handle), mean)
bpm_data = subset(topc_data, select = -c(Group.1))

set.seed(33)
val_index = createDataPartition(bpm_data$BPM, p=0.80, list=FALSE)
val = bpm_data[-val_index,]
trn = bpm_data[val_index,]

# train a model and summarize model
set.seed(33)
trctrl = trainControl(method = "cv", number = 5)
tune_grid = expand.grid(nrounds = 200,
                        max_depth = 5,
                        eta = 0.05,
                        gamma = 0.01,
                        colsample_bytree = 0.75,
                        min_child_weight = 0,
                        subsample = 0.5)

xgb_fit = train(BPM ~ ., data = trn, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

# Testing
# estimate skill on validation dataset
set.seed(33)
test_predict = predict(xgb_fit, val)
set.seed(33)
all_predicts = predict(xgb_fit, bpm_data)

# RMSE
rmse(val$BPM, test_predict)

# DELETEME
hist(all_predicts)

# Saving the Model and Distribution of all scores
saveRDS(xgb_fit, "./topc_model.rds")
saveRDS(all_predicts, "./topc_full_preds.rds")


# Loading the saved data
# BELOW IS ALL THE CODE YOU NEED TO MAKE IT RUN IN THE SERVER 
#    (once you've trained and saved, obviously)
xgb_fit = readRDS("./topc_model.rds")
all_predicts = readRDS("./topc_full_preds.rds")

topc_percentile = ecdf(all_predicts)
get_topc_percentile = function(scores_list, xgb_fit, topc_percentile) {
  pred = predict(xgb_fit, scores_list)
  perc = topc_percentile(pred)
  # turn that into a +/- score
  plusminus = (perc - .5) * 20
  return(plusminus)
}


# EXAMPLE DYNAMIC SENT SCORE
my_scores = list("emot_anger" = 0.1, 
                 "emot_fear" = 0.1,
                 "emot_disgust" = 0.1,
                 "emot_joy" = 0.9,
                 "emot_sadness" = 0.1,
                 "sent_score" = 0.9)
get_topc_percentile(my_scores, xgb_fit, topc_percentile)





# ---- BUILD TABLE WITH ALL SCORES OF PLAYERS ----
sent_xgb_fit = readRDS("./sent_model.rds")
sent_predicts = readRDS("./sent_full_preds.rds")
sent_percentile = ecdf(sent_predicts)

freq_xgb_fit = readRDS("./freq_model.rds")
freq_predicts = readRDS("./freq_full_preds.rds")
freq_percentile = ecdf(freq_predicts)

topc_xgb_fit = readRDS("./topc_model.rds")
topc_predicts = readRDS("./topc_full_preds.rds")
topc_percentile = ecdf(topc_predicts)

get_pred_perc = function(scores_list, xgb_fit, topic_percentile) {
  pred = predict(xgb_fit, scores_list)
  perc = topic_percentile(pred)
  proj__list = list(pred, perc)
  names(proj__list) = c("pred", "perc")
  return(proj__list)
}

plusminus_scores = data.frame(matrix(ncol = 7, nrow = 0))
cols = c("twitter_handle", "sent_pred", "sent_perc", "freq_pred", "freq_perc", "topc_pred", "topc_perc")
colnames(plusminus_scores) = cols

handles = unique(sent_data$Group.1)
for (i in 1:length(handles)) {
  handle = handles[i]
  sent_scores = subset(sent_data,
                       Group.1 == handle,
                       select=c(emot_anger, 
                               emot_fear,
                               emot_disgust,
                               emot_joy,
                               emot_sadness,
                               sent_score,
                               BPM))
  sent_list = get_pred_perc(sent_scores, sent_xgb_fit, sent_percentile)
  
  freq_scores = subset(freq_data,
                       Group.1 == handle,
                       select=c(num_tweets,
                                num_mentions,
                                avg_tweet_length,
                                BPM))

  freq_list = get_pred_perc(freq_scores, freq_xgb_fit, freq_percentile)
  
  topc_scores = subset(topc_data,
                       Group.1 == handle,
                       select=-c(Group.1))
  topc_list = get_pred_perc(topc_scores, topc_xgb_fit, topc_percentile)
  plusminus_scores[i, ] = list(handle, sent_list$pred, sent_list$perc, freq_list$pred, freq_list$perc, topc_list$pred, topc_list$perc)
}

write.csv(plusminus_scores, "plus_minus_scores.csv")

