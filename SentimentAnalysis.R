install.packages("syuzhet")
#
library(syuzhet)

setwd('C:/Users/Zuha Ansari/Desktop/College/Final Year Project')
mydata <- read.csv (file = "LushData.csv") 


# Converting tweets to ASCII to trackle strange characters
lushdata <- iconv(lushdata, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
lushdata <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",lushdata)
# removing mentions, in case needed
lushdata <-gsub("@\\w+","",lushdata)
ew_sentiment<-get_nrc_sentiment((lushdata))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

#analysis on the tweets with the tag #lush plot
install.packages("ggplot2")
library(ggplot2)
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

summary(sentimentscores$Score)
summary(ew_sentiment)

####################################################
#                    Naive Byes                    #
###################################################

install.packages("quanteda")
install.packages("quanteda.textmodels")
install.packages("quanteda.corpora")
install.packages("caret")
