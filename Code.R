###################################################################################################
### AUTOMATED CONTENT & SENTIMENT ANALYSIS                                                                                       
### (c) Patrick Cichy, Berner Fachhochschule BFH
###################################################################################################

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
### STEP 1: LOAD PACKAGES AND SET WORKING DIRECTORY
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#install.packages("stringr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("scales")
library (syuzhet)
library (stringr)
library (tidyverse)
library (ggplot2)
library(scales)
options(scipen=999)

#working directory
setwd ("")
getwd()


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 2: LOAD AND SELECT DATA (E.G. AMAZON REVIEWS) 
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# load data
load("ChatGPT.rda")

tweets_orig <- tweets



# OPTIONAL: Specify minimum text length (number of characters)
#tweets <- subset(tweets, tweets$text_length > 100)

# OPTIONAL: Further selection (select variable to filter)
#tweets<- subset(tweets, tweets$product == "Fitbit Charge 2")




# Ex 1 - say about user
#Average/ median of Hour when to tweet, Nr of Retweets, Likes,Followers, Freinds, verified
# take unique users
Users <- tweets[4:10]
Users = Users[!duplicated(Users$User),]

#create Histogramm for Tweettime
plot_dataHour <- tweets %>% 
  group_by (timeofday_hour) %>%
  count()

ggplot (plot_dataHour, 
        aes (x=timeofday_hour, y=n)) +
  geom_bar(stat = "identity")+
  theme_minimal () +
  ggtitle("Number of tweets over time (per hour)") +
  xlab("Hour") +
  ylab("Number of tweets")



#create median
retweets_median = median(Users$Retweets)
retweets_mean = mean(Users$Retweets)

likes_median = median(Users$Likes)
likes_mean = mean(Users$Likes)

Friends_median = median(Users$UserFriends)
Friends_mean = mean(Users$UserFriends)

Followers_median = median(Users$UserFollowers)
Followers_mean = mean(Users$UserFollowers)

verified_median = median(Users$UserVerified)
verified_mean = mean(Users$UserVerified)

#ex 2 what ar the tweets about? (Industries, specific applications and emotions)







#ex 3 how did the exitment cahnge over time
#number of tweets over time
plot_data <- tweets %>% 
  group_by (tweet_date) %>%
  count()

ggplot (plot_data, 
        aes (x=tweet_date, y=n)) +
  geom_bar(stat = "identity")+
  theme_minimal () +
  ggtitle("Number of reviews over time (per day)") +
  xlab("Date 22/23") +
  ylab("Number of tweets")



#ex 4 





### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 3: PERFORM AUTOMATED CONTENT ANALYSIS
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Select text column and create your custom dictionary
reviews$dic1 <- "NA"
reviews$dic1 <-str_count(reviews$reviewText, "design|color|black|looks")
reviews$dic2 <-str_count(reviews$reviewText, "bad|dislike|waste|don't")
reviews$dic3 <-str_count(reviews$reviewText, "bad|dislike|waste|dont")
reviews$dic2_occurence<-ifelse(reviews$dic2>=2,1,0)
reviews$dic3_occurence<-ifelse(reviews$dic3>=2,1,0)
reviews$dic1_occurence<- "NA"
#select threshold for occurence of the topic
reviews$dic1_occurence<-ifelse(reviews$dic1>=2,1,0)
#number of reviews that cover topic
sum (reviews$dic1_occurence)
sum(reviews$dic2_occurence)

## VISUALIZE RESULTS
#sum of reviews that cover topic per day
plot_data <- reviews %>% 
  group_by (reviewDate_month) %>%
  summarise(n=sum(dic3_occurence))

ggplot (plot_data, aes (x=reviewDate_month, y=n)) + geom_bar(stat = "identity")+ theme_minimal () + ggtitle("Number of reviews covering the topic (per month)")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 4: PERFORM SENTIMENT ANALYSIS
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Select text column and calculate sentiment scores. You can change the method (e.g."syuzhet", "bing", "nrc")
reviews$sentiment <- "NA"
reviews$sentiment <- get_sentiment(reviews$reviewText, method="syuzhet", lang="english")

## VISUALIZE RESULTS
# mean over time
plot_data <- reviews %>% 
  group_by (reviewDate_month) %>%
  summarise(n=mean(sentiment))

ggplot (plot_data, aes (x=reviewDate_month, y=n)) + geom_line()+ theme_minimal () + ggtitle("Sentiment scores over time (mean per month)")



### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 5: PERFORM ADDITIONAL ANALYSIS
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#correlation of sentiment with custom dictionary
cor(reviews$dic1, reviews$reviewRating, method = "pearson")
cor(reviews$dic1, reviews$sentiment, method = "pearson")
cor(reviews$dic2, reviews$reviewRating, method = "pearson")
cor(reviews$dic2, reviews$sentiment, method = "pearson")
cor(reviews$dic3, reviews$reviewRating, method = "pearson")
cor(reviews$dic3, reviews$sentiment, method = "pearson")
