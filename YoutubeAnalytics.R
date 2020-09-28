rm(list = ls())
setwd("D:/A DSDA/Semester 3/Social Media Analytics/")

library(tuber)
library(tidyverse)
library(lubridate)
library(stringi)
library(wordcloud)
library(gridExtra)
library(plyr)
library(dplyr)
library(tm)

app_id <- "Your Client ID"
app_secret <- "Your Client Secret ID"

yt_oauth(app_id=app_id,app_secret=app_secret,token ="")

# Extract the data against the YouTube channel using Youtube Reporting API

# Channel statistics
chstat = get_channel_stats("UCjOXlKzzxdZG6BhKCQtWKWQ")
View(chstat)


# Channel video statistics
channel_videos_stats = get_all_channel_video_stats(channel_id = "UCjOXlKzzxdZG6BhKCQtWKWQ")
View(channel_videos_stats)
write.csv(channel_videos_stats, 
          "D:/A DSDA/Semester 3/Social Media Analytics/tripoto_channel_stats.csv")

# Videos
videos = yt_search(term="", type="video", channel_id = "UCjOXlKzzxdZG6BhKCQtWKWQ")
videos = videos %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2016-03-03") %>%
  arrange(date)

# Extracting all the comments from the entire channel 
comments = lapply(as.character(videos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})
View(comments)

#Creating the dataframe from the extracted comments
df <- ldply(comments, data.frame)
View(df)
write.csv(df, "D:/A DSDA/Semester 3/Social Media Analytics/tripoto_channel_comments.csv")

comments = read.csv("D:/A DSDA/Semester 3/Social Media Analytics/channel_comments.csv")
View(comments)

################# Extracting Comments for ################
# Got the IDs for the following videos from the Channel Video Statistics

# Most viewed video
all_comments = get_all_comments(video_id="JQr1-VchonU")
write.csv(all_comments, "D:/A DSDA/Semester 3/Social Media Analytics/tripoto_most_viewed.csv")
# most liked video
all_comments1 = get_all_comments(video_id="DtTztc61b5U")
write.csv(all_comments1, "D:/A DSDA/Semester 3/Social Media Analytics/tripoto_most_liked.csv")
# most disliked video
all_comments2 = get_all_comments(video_id="K18nEQf8i9c")
write.csv(all_comments2, "D:/A DSDA/Semester 3/Social Media Analytics/tripoto_most_disliked.csv")


#############################################################################

# Creating wordclouds after cleaning the data in Python
# Did the following for all the comments, comments on most viewed/liked/disliked/commented videos

comments = read.csv("D:/A DSDA/Semester 3/Social Media Analytics/channel_comments.csv")
text = comments$textOriginal

pal <- brewer.pal(8, "Dark2")
wordcloud(text, 
          min.freq = 3,  
          max.words = 500, width=500, height =500,  
          random.order = FALSE, color=pal)


# Calculating the sentiment analysis

library(syuzhet)
mysentiment <- get_nrc_sentiment(comments$textOriginal) # 7:42
sentiment_values = get_nrc_values(comments1$textOriginal)
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL

# Plotting the sentiments
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none") +
    xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Comments")
