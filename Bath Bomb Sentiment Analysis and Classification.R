##################################################################
##################################################################
##              STEP ONE - CREATING DATASET                    ##
##################################################################
##################################################################

install.packages("rtweet")
library (rtweet)

###################################################################
#       Creating a token to access tweets from Twitter            #
###################################################################

twitter_token <- create_token(
  app = "lush get_tweets script",
  consumer_key = "",
  consumer_secret = "",
  access_token = '',
  access_secret = '',
  set_renv = FALSE)

# FOR TESTING PURPOSES : TIMELINES OF LUSH ACCOUNTS IN UK AND NORTH AMERICA
tweets <- get_timelines("lushltd", 
                        n = 10000, 
                        language = 'en',
                        token = twitter_token)

tweets2 <- get_timeline("lushcosmetics",
                        n=2000,
                        language = 'en',
                        since = '2019-01-01', 
                        until = '2020-3-31',
                        token = twitter_token)

##################################################################
#                extracting tweets from twitter                  #
##################################################################

lushbathbomb <- search_tweets("lush || bath || bomb", n=3000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

lushbathbomb2 <- search_tweets("lushbathbomb", n=3000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

lushcommunity <- search_tweets("lush||community", n=3000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

lushltd <- search_tweets("LushLtd", n=3000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

lushcosmetics <- search_tweets("lushcosmetics", n=3000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

lushAusNZ <- search_tweets("Lush_AusNZ", n=3000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

lushSingapore <- search_tweets("LushSingapore", n=3000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

lushMalaysia <- search_tweets("MalaysiaLush", n=3000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

lushKitchen <- search_tweets("LushKitchen", n=3000, include_rts=FALSE, retryonratelimit=TRUE, lang="en")

#################################################################
#                 Data creation                                 #
#################################################################
lushRData <- rbind(lushbathbomb, lushbathbomb2, lushcommunity, lushltd, lushcosmetics, lushAusNZ, lushMalaysia, lushSingapore, lushKitchen)

#################################################################
#                   feature selection                           #
#################################################################
 
lushdata <- lushRData[c("user_id", "status_id", "created_at", "screen_name",
                        "text", "favorite_count", "retweet_count","location", "verified")]

################################################################
#                    storing dataset                           #  
################################################################

setwd("C:\\Users\\Zuha Ansari\\Documents")
write.csv(lushdata, file = "LushData.csv")

###################################################################
###################################################################
##                       STEP TWO                                ##
###################################################################
###################################################################

#install package and run library
install.packages("syuzhet")
library(syuzhet)
install.packages("ggplot2")
library(ggplot2)

#############################################
#             cleaning data                 #
#############################################

# Converting tweets to ASCII to trackle strange characters
lushdata <- iconv(lushdata, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
lushdata <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",lushdata)
# removing mentions, in case needed
lushdata <-gsub("@\\w+","",lushdata)

###############################################################
#                   Sentiment Analysis                        #
###############################################################

#using nrc setiment to classify the words used according to sentiments
ew_sentiment<-get_nrc_sentiment((lushdata))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

##############################################
#     sentiment analysis histogram          #
#############################################

ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

summary(sentimentscores$Score)
summary(ew_sentiment)

###################################################################
###################################################################
##        STEP THREE - Words, Popularity, Location               ##
##               Classification modelling                        ##
###################################################################
###################################################################

#install all packages and run all libraries

install.packages("rtweet")
install.packages("SnowballC")
install.packages('devtools')
install.packages("slam")
install.packages("wordcloud")
install.packages('tm')
install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("forestmangr")
library (rtweet)
library(SnowballC)
library(devtools)
library(slam)
library(wordcloud)
library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(forestmangr)

# Remove retweets
lush_tweets_organic <- lushRData[lushRData$is_retweet==FALSE, ] 
# Remove replies
lush_tweets_organic <- subset(lush_tweets_organic, is.na(lush_tweets_organic$reply_to_status_id)) 

lush_tweets_organic <- lush_tweets_organic %>% arrange(-favorite_count)
lush_tweets_organic[1,5]
lush_tweets_organic <- lush_tweets_organic %>% arrange(-retweet_count)
lush_tweets_organic[1,5]

#Keeping only the retweets
lush_retweets <- lushRData[lushRData$is_retweet==TRUE,]
# Keeping only the replies
lush_replies <- subset(lushRData, !is.na(lushRData$reply_to_status_id))

lush_tweets_organic$text <-  gsub("https\\S*", "", lush_tweets_organic$text)
lush_tweets_organic$text <-  gsub("@\\S*", "", lush_tweets_organic$text) 
lush_tweets_organic$text  <-  gsub("amp", "", lush_tweets_organic$text) 
lush_tweets_organic$text  <-  gsub("[\r\n]", "", lush_tweets_organic$text)
lush_tweets_organic$text  <-  gsub("[[:punct:]]", "", lush_tweets_organic$text)


##############################################################################
#               The most frequently used words in tweets                     #
##############################################################################

tweets <- lush_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words) %>%
  filter(!word =="lush" ) %>%
  filter(!word =="bath" ) %>%
  filter(!word =="lushcosmetics") %>%
  filter(!word =="bombs" ) %>%
  filter(!word =="bomb" ) %>%
  filter(!word =="im" ) %>%
  filter(!word =="it's" ) %>%
  filter(!word =="i'm " )

# bar chart of the most frequent words found in the tweets
tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets",
       subtitle = "Stop words removed from the list")


#most used word cloud
tweets <- as.character(tweets)
tweets <- gsub("c\\(", " ", tweets)
set.seed(1234)
wordcloud(tweets, min.freq=4, scale=c(5, .1), random.order=FALSE, rot.per=.1, 
          colors=brewer.pal(9, "Set2"))


#################################################################################
#                             Location based modelling                          #
#################################################################################

# different unique locations are represented
unique(lushdata$location)


#[1] 510 (this indicates there are about 510 locations which people have given)
length(unique(lushdata$location))

# location word cloud to identify the different
# locations as they are inputted by users as strings
lush_tweets_organic$location <- as.character(lush_tweets_organic$location)
lush_tweets_organic$location <- gsub("c\\(", " ", lush_tweets_organic$location)
set.seed(1234)
wordcloud(lush_tweets_organic$location, min.freq=4, scale=c(5, .5), random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))

#graph displaying the 510 unique locations which is overcrowded and cannot be read
lushdata %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ")

#graph has added the function to show n number of locations, in this case 20
lushdata %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Top 20 unique locations of Twitter users")


#############################################################################
#                    list of tweets by verified users                       #
#############################################################################

lushVerified <- lushdata %>% 
  select(verified) %>% 
  group_by(verified) %>%
  summarize(count=n())
lushVerified <- subset(lushVerified, count > 11)

data <- data.frame(
  category=lushVerified$verified,
  count=lushVerified$count
)


# donut graph to illustrate what percentage of the tweets are from 
# Verified users (True = Verified at 7%/  False= Not Verified at 93%)

data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
data <- round_df(data, 2)
Verified <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Verified)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


verifiedTweets <- cbind(lushdata$verified, lushdata$text, lushdata$location, lushdata$favorite_count)
View(verifiedTweets)


#######################################################################
#         list of tweets with the most amount of favourites          #
######################################################################

View(lushdata$favorite_count >45,"TRUE")

sum(lushdata$favorite_count >10, TRUE)

summary(lushdata$favorite_count)
boxplot(lushdata$favorite_count)
qqplot(lushdata$retweet_count, lushdata$favorite_count)

chisq.test(lushdata$retweet_count, lushdata$favorite_count)
##  Result
#	          Pearson's Chi-squared test
#           data:  lushdata$retweet_count and lushdata$favorite_count
#           X-squared = 8303.5, df = 407, p-value < 2.2e-16

sum(lushdata$verified)
#[1] 83

chisq.test(lushdata$verified, lushdata$favorite_count)
#   Result
#	           Pearson's Chi-squared test
#            data:  lushdata$verified and lushdata$favorite_count
#            X-squared = 80.18, df = 37, p-value = 5.044e-05

################### THE END ###############################


