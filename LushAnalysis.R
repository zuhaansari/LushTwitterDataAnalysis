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


twitter_token <- create_token(
  app = "lush get_tweets script",
  consumer_key = "8YwJxvEx0Ddl4zNjqkG2bdH7l",
  consumer_secret = "ue5TrgX7WBrMSXlieODGgQV86F9vSmuy8Jm0GWmEsGJ9FcWWdM",
  access_token = '1059567061880455168-PBGpLflNajjnJUv485BVz2pJQaQps9',
  access_secret = 'bzWidmK0CLdodtzLXv07vD2L4PEJlik3Aam9zLGOREhLP',
  set_renv = FALSE)


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

# Creating a data frame
data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2856, 192, 120)
)


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

###############


verifiedTweets <- cbind(lushdata$verified, lushdata$text, lushdata$location, lushdata$favorite_count)
View(verifiedTweets)



test1 <- lushdata[which(lushdata$verified == "TRUE"), names(verified) %in% c("text","location","favorite_count")]

head(lushdata$verified)

lushdata %>%
  count(verified, sort = TRUE) %>%
  mutate(verified = reorder(verified, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = verified, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Favorited",
       y = "Count",
       title = "Top 20 favourited tweets")



#######################################################################
#         list of tweets with the most amount of favourites          #
######################################################################

View(lushdata$favorite_count >45,"TRUE")

sort(lushdata$favorite_count)

View(lushdata$favorite_count >200, TRUE)

sum(lushdata$favorite_count >200, TRUE)

lushdata %>%
  count(favorite_count, sort.default(TRUE)) %>%
  mutate(favorite_count = reorder(favorite_count, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = favorite_count, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Tweet",
       title = "Twitter fav count ")


lushdata %>%
  count(favorite_count, sort = TRUE) %>%
  mutate(favorite_count = reorder(favorite_count, n)) %>%
  ggplot(aes(x = favorite_count, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Favorited",
       y = "Count",
       title = "Top 20 favourited tweets")

#number/count of favorite tweets
#[1]   0   1   3   2 276   4   5  17   9  20  15  35   6  47   7  19  22 341  11   8  31  49  21 117  1
#3  16  43  36  52  53  12  71  25  14  29 
#[36]  10  27  18
unique(lushdata$favorite_count)
summary(lushdata$favorite_count)
boxplot(lushdata$favorite_count)
qqplot(lushdata$retweet_count, lushdata$favorite_count)
qqline(lushdata$retweet_count, lushdata$favorite_count)

wilcox.test(lushdata$retweet_count, lushdata$favorite_count)
##  Result
#          Wilcoxon rank sum test with continuity correction
#          data:  lushdata$retweet_count and lushdata$favorite_count
#          W = 413105, p-value < 2.2e-16
#          alternative hypothesis: true location shift is not equal to 0

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

