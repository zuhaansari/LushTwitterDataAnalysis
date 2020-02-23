install.packages("rtweet")
library (rtweet)

twitter_token <- create_token(
  app = 'lush get_tweets script',
  consumer_key = '8YwJxvEx0Ddl4zNjqkG2bdH7l',
  consumer_secret = 'ue5TrgX7WBrMSXlieODGgQV86F9vSmuy8Jm0GWmEsGJ9FcWWdM',
  set_renv = TRUE)
#tracking the tag
lushbathbomb <- search_tweets("#lushbathbomb", n=2000, include_rts=FALSE, lang="en")
lushbathbomb


lushbathbomb$location

library(mice)

lushbathbomb
#just account
#lush <- get_timeline("@LushLtd", n= 3200)
#lush

#retweets and organic tweets
# Remove retweets
#lush_tweets_organic <- lush[lush$is_retweet==FALSE, ] 
# Remove replies
#lush_tweets_organic <- subset(lush, is.na(lush$reply_to_status_id)) 

#
#favorite count (i.e. the number of likes) or retweet_count 
#lush_tweets_organic <- lush_tweets_organic %>% arrange(-favorite_count)
#lush_tweets_organic[1,5]
#lush_tweets_organic <- lush_tweets_organic %>% arrange(-retweet_count)
#lush_tweets_organic[1,5]
