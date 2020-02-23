install.packages("syuzhet")
#
library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
lushbathbomb <- iconv(lushbathbomb, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
lushbathbomb <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",lushbathbomb)
# removing mentions, in case needed
lushbathbomb <-gsub("@\\w+","",lushbathbomb)
ew_sentiment<-get_nrc_sentiment((lushbathbomb))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

#analysis on the tweets with the tag #lushcommunity plot
install.packages("ggplot2")
library(ggplot2)
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

##plot time series (if ggplot2 is installed)
ts_plot(lushbathbomb)
