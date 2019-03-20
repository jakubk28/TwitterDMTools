## ----setup, include = FALSE,results='hide'-------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  
)

## ----eval=FALSE----------------------------------------------------------
#  #load in devtools package
#  library(devtools)
#  
#  #install TwitterDMTools package from Github repository
#  install_github("jakubk28/TwitterDMTools")
#  
#  #load in TwitterDMTools package
#  library(TwitterDMTools)

## ----message=FALSE,results='hide',cache=TRUE,echo=FALSE------------------
#load in devtools package
library(devtools)

#install TwitterDMTools package from Github repository
#install_github("jakubk28/TwitterDMTools")

#load in TwitterDMTools package
library(TwitterDMTools)

## ----eval=FALSE,message=FALSE,results='hide',cache=TRUE------------------
#  #load in devtools package
#  library(devtools)
#  
#  #install TwitterDMTools package from Github repository
#  install.packages("TwitterDMTools")
#  
#  #load in TwitterDMTools package
#  library(TwitterDMTools)

## ----message=FALSE,eval=FALSE--------------------------------------------
#  #search for tweets mentioning "plastic AND pollution" tweets for 60 seconds
#  streamTweets("plastic pollution",60,"conservationTweets")

## ----message=FALSE,eval=FALSE--------------------------------------------
#  #load in json data file and save as a data frame object
#  tweetsDF <- loadTweets("conservationTweets.json")

## ----message=FALSE,cache=TRUE--------------------------------------------
#load in included tweet data file
tweetsDF <- ConservationTweets

## ----cache=TRUE----------------------------------------------------------
#tokenize tweets into singular words
tweetsDFtoken1<-tokenizeTweets(tweetsDF,1)

#view the top entries in the tokenized data frame
head(tweetsDFtoken1)

## ----cache=TRUE----------------------------------------------------------
#pairs of words
#tokenize tweets into pairs of words
tweetsDFtoken2<-tokenizeTweets(tweetsDF,2)

#view the top entries in the pair tokenized data frame
head(tweetsDFtoken2)

#triples of words
#tokenize tweets into pairs of words
tweetsDFtoken3<-tokenizeTweets(tweetsDF,3)

#view the top entries in the triples tokenized data frame
head(tweetsDFtoken3)

## ----cache=TRUE----------------------------------------------------------
#display most frequent words
head(tokenFreq(tweetsDFtoken1))

## ----cache=TRUE----------------------------------------------------------
#remove stop words from the dataframe
Token1<-removeStopWords(tweetsDFtoken1)

#most frequent words after removal of basic stop words
head(tokenFreq(Token1))

## ----cache=TRUE----------------------------------------------------------
#find the context of the word "chester"
SpecificWordContext<-tweetContext(tweetsDF,"chester")
SpecificWordContext

## ----cache=TRUE----------------------------------------------------------
#remove stop words from the dataframe
Token1<-removeStopWords(tweetsDFtoken1,c("palm","oil","palmoil"))

#most frequent words after removal of basic stop words
head(tokenFreq(Token1))

## ----warning=FALSE,fig.align="center",fig.width=5,fig.height=4,cache=TRUE----
#plot frequency bar chart of most frequently occuring words
plotFreqBar(Token1,5)

## ----echo=FALSE----------------------------------------------------------
Token2<-removeStopWords(tweetsDFtoken2,c("palm","oil","palmoil"))

## ----message=FALSE,warning=FALSE,fig.align="center",fig.width=5,fig.height=4,cache=TRUE----
#plot frequency bar chart of most frequently occuring pairs of words
plotFreqBar(Token2,5)

## ----message=FALSE,warning=FALSE,fig.align="center",fig.width=4,fig.height=4,cache=TRUE----
#plot frequency cloud of at most 30 words, occuring at least 2 times
plotFreqCloud(Token1,maxWords = 30,minWordFreq = 2)

## ----message=FALSE,warning=FALSE,fig.align="center",fig.width=6,fig.height=6,cache=TRUE----
#plot term network using bigrams (tokenization of 2)
plotBigrams(Token2,minWordFreq = 50)

## ----message=FALSE,fig.align="center",fig.width=5,fig.height=4,cache=TRUE----
#comparison bar chart of most used positive and negative words
plotCompBar(Token1,maxWords = 5,minWordFreq = 2)

## ----message=FALSE,warning=FALSE,fig.align="center",fig.width=4,fig.height=4,cache=TRUE----
#comparison bar chart of most used positive and negative words
plotCompCloud(Token1,maxWords = 20,minWordFreq = 5)

## ----message=FALSE,results=FALSE,cache=TRUE,fig.align="center",fig.width=6,fig.height=6----
#estimate number of topics in topic model
estimatedTopicNumber <- estTopics(Token1,2,16,1)

## ----message=FALSE,cache=TRUE--------------------------------------------
#create LDA model with 9 topics
ExampleLDA<-tweetLDA(Token1,9)

## ----message=FALSE,warning=FALSE,fig.align="center",fig.width=7,fig.height=6,cache=TRUE----
#plot words for 9 topics of model
plotTopicTerms(ExampleLDA)

## ----message=FALSE,warning=FALSE,fig.align="center",fig.width=7,fig.height=6,cache=TRUE----
#plot distribution of documents within topics
plotTopicDist(ExampleLDA)

