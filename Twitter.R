install.packages("rtweet")
library (rtweet)

twitter_token <- create_token(
  app = "lush get_tweets script",
  consumer_key = "8YwJxvEx0Ddl4zNjqkG2bdH7l",
  consumer_secret = "ue5TrgX7WBrMSXlieODGgQV86F9vSmuy8Jm0GWmEsGJ9FcWWdM",
  access_token = '1059567061880455168-PBGpLflNajjnJUv485BVz2pJQaQps9',
  access_secret = 'bzWidmK0CLdodtzLXv07vD2L4PEJlik3Aam9zLGOREhLP',
  set_renv = FALSE)

##TIMELINES OF LUSH ACCOUNTS IN UK AND North America as testing
tweets <- get_timelines("lushltd", 
                        n = 10000, 
                        language = 'en',
                        token = twitter_token)

tweets2 <- get_timeline("lushcosmetics",
                        n=10000,
                        language = 'en',
                        since = '2019-01-01', 
                        until = '2020-3-31',
                        token = twitter_token)

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


lushRData <- rbind(lushbathbomb, lushbathbomb2, lushcommunity, lushltd, lushcosmetics, lushAusNZ, lushMalaysia, lushSingapore, lushKitchen)


lushdata <- lushRData[c("user_id", "status_id", "created_at", "screen_name",
                        "text", "favorite_count", "retweet_count","location", "verified")]

write.csv(lushdata, file = "LushData.csv")

####################################################################
