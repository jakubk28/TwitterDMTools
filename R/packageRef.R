#rtweet
#tidytext

#library(dplyr)
#library(tidytext)
#library(tidyr)
#library(ggplot2)
#library(wordcloud)
#library(stringr)?
#library(rtweet)
#library(reshape2)
#library(ggraph)
#library(igraph)
#library(widyr)?
#dont need devtools unless for installing packages
#library(devtools)
#only need for complining but not needed in acutal package
#library(roxygen2)

#########################################
##DATA COLLECTION AND TIDYING FUNCTIONS##
#########################################

#FUNCTION 1 - STREAM TWEETS
#' streamTweets
#'
#' Search for Tweets containing entered words/phrases over designated time period and save with desired file name in file directory.
#' @param searchWords Words or phrases to be included in tweets, format: "word1,word2,a phrase"
#' @param searchPeriod Number of seconds that Tweets will be collected for
#' @param fileName File name of the results of the function, format: "myfilename.json"
#' @return File in local directory with Tweets containing desired words and/or phrases.
#' @export
#' @examples streamTweets("tree,bushes,apple orchard",60,"treeTweets.json")
streamTweets <- function(searchWords,searchPeriod,fileName = "streamedTweets.json"){
  #list of words or phrases that will be searched for
  rtweet::stream_tweets(searchWords,
                        #search time period in seconds
                        timeout = searchPeriod,
                        #name of file with results
                        file_name = fileName,
                        #do not return results on screen
                        parse = FALSE
  )
}


#FUNCTION 2 - LOAD IN TWEETS AND TURN INTO DATAFRAME OBJECT
#' loadTweets
#'
#' Load .json file containing streamed tweets into a data frame object.
#' @param fileName File name containing streamed Tweets. Format: "myfilename.json"
#' @return Data frame object containing streamed tweets and relevant information.
#' @export
#' @examples streamTweets() or streamTweets("myfilename.json")
loadTweets <- function(fileName){
  #read in tweet data (.json file)
  tweetsDf <- rtweet::parse_stream(fileName)
  return(tweetsDf)
}



#FUNCTION 3 - TOKENIZE TWEETS INTO WORDS
#' tokenizeTweets
#'
#' Splits contents of tweets into words/pairs of words/ triples of words depending on the tokenization number.
#' @param tidyTweetVarName data frame variable containing imported streamed tweets, leave blank if name was not changed.
#' @param tokenNum 1 - splits into singular words, 2 - splits into pairs of words, 3 - splits into triples of words.
#' @return Data frame object containing all Tweets split into n number of words.
#' @export
#' @examples tokenizeTweets(Tweets,3) tokenizeTweets(,2) tokenizeTweets()
tokenizeTweets <- function(tweetDf,tokenNum=1){
  #convert array into df and keep only tweet text
  tweetTextDf <- as.data.frame(tweetDf[,5],drop=FALSE)
  #tokenize based on input: 1,2,3
  if(tokenNum==1){
    tweetsTokenized <- tweetTextDf %>%
      tidytext::unnest_tokens(word,text)
  }
  else if(tokenNum==2){
    #seperate into bigrams
    tweetsTokenized <- tweetTextDf %>%
      #tokenize into tokenNum of words
      tidytext::unnest_tokens(bigram,text,token = "ngrams",n = tokenNum) %>%
      #split each word into its individual cell
      tidyr::separate(bigram, c("word1","word2"), sep = " ")
  }
  else if(tokenNum==3){
    #seperate into bigrams
    tweetsTokenized <- tweetTextDf %>%
      #tokenize into tokenNum of words
      tidytext::unnest_tokens(bigram,text,token = "ngrams",n = tokenNum) %>%
      #split each word into its individual cell
      tidyr::separate(bigram, c("word1","word2","word3"), sep = " ")
  }
  else{
    stop('Set tokenization number to 1, 2, or 3.')
  }
  return(tweetsTokenized)
}


#FUNCTION 4 - DISPLAY MOST FREQUENT WORDS
#' dispFreq
#'
#' Displays the most common words/pair/triplesof words.
#' @param tokenizedTweetVarName Leave blank unless the name of the tokenized Tweets data frame was changed
#' @return Displays the most frequent words used.
#' @export
#' @examples dispFreq() dispFreq(TweetsTokenized)
tokenFreq <- function(tokenizedTweetsDf){
  #check if tokenized by word or bigram
  if(ncol(tokenizedTweetsDf)==1){
    #sort by frequency
    tweetFreq <- tokenizedTweetsDf %>%
      plyr::count(word,sort = TRUE)
  }
  else if(ncol(tokenizedTweetsDf)==2){
    tweetFreq <-tokenizedTweetsDf %>%
      plyr::count(word1,word2,sort=TRUE)
  }
  else if(ncol(tokenizedTweetsDf)==3){
    tweetFreq <- tokenizedTweetsDf %>%
      plyr::count(word1,word2,word3,sort=TRUE)
  }
  return(tweetFreq)
}


#FUNCTION 5 - LOOK UP WORD FOR CONTEXT
#' tweetContext
#'
#' If uncertain about the context of a popular word, the full length of the tweet will be shown to give context.
#' @param WordReq The word or phrase wanted for context, format: "word1"
#' @param TidyTweetsVarName Name of the variable containing the imported tweets, if unchanged then leave blank.
#' @return Full length Tweet containing the word or phrase searched for.
#' @export
#' @examples tweetContext("pineapple") tweetContext("pineapple tree") tweetContext("pineapple",myImportedTweets)
#look up anomalies for context
tweetContext <- function(tweetDataFrame,wordReq){
  tweetsContaining <- tweetDataFrame %>%
    dplyr::filter(str_detect(text, wordReq)) %>%
    dplyr::select(text)
  return(tweetsContaining)
}


#FUNCTION 6 - STOP WORDS, extra words entered as "c("word1","word2",...)"
#' removeStopWords
#'
#' Call and remove stop words from the tokenized tweet data set, stop words include common words that are not interesting for analysis (the,and, a, ...)
#' @param tidyTweetsTokenizedVarName the variable name of the data frame containing the tokenized tweets. Leave blank if unchanged
#' @param extraStopWords If there are extra words to exclude from the analysis enter them here, format: "c("word1","word2")
#' @return Tokenized Tweets data frame excluding stop words.
#' @export
#' @examples removeStopWords() removeStopWords(,c("tree","garden")) removeStopWords(tokenizedTweetsNew,"pear")
removeStopWords <- function(tweetDfTokenized,extraStopWords=NULL){
  #call stop words dictionary
  data(stop_words)
  #check whether extra stop words are entered, if so:
  stop_words <- dplyr::bind_rows(data_frame(word = c("t.co","https"),lexicon = c("custom")),stop_words)
  if(!is.null(extraStopWords)){
    #add custom words to stop-words list
    stop_words <- dplyr::bind_rows(data_frame(word = extraStopWords,lexicon = c("custom")),stop_words)
  }
  #remove stop words from dataframe
  if(ncol(tweetDfTokenized)==1){
    tweetDfTokenClean <- tweetDfTokenized %>%
      dplyr::anti_join(stop_words)
  }
  else if(ncol(tweetDfTokenized)==2){
    tweetDfTokenClean <- tweetDfTokenized %>%
      #anti_join(stop_words)
      dplyr::filter(!word1 %in% stop_words$word) %>%
      dplyr::filter(!word2 %in% stop_words$word)
  }
  return(tweetDfTokenClean)
}



###########################
##VISUALISATION FUNCTIONS##
###########################

#FUNCTION 7 - PLOT OF MOST COMMON WORDS ==> BAR GRAPH FORMAT
#' plotFreqBar
#'
#' Plot a bar chart of the n most frequent words in the Tweet data set
#' @param mostFreqWords Name of variable containing the frequency of words used, leave blank if unchanged
#' @param numOfWords Top n words to be plotted, default is 10.
#' @return Top n most frequent words used in the Tweet dataset.
#' @export
#' @examples plotFreqBar(,100) plotFreqBar() plotFreqBar(mostFrequentlyUsedWords,5)
plotFreqBar <- function(tokenFreqs,numOfWords=10){
  if(ncol(tokenFreqs)==2){
    TweetFreqTemp <- tokenFreqs
  }
  else if(ncol(tokenFreqs)==3){
    TweetFreqTemp <- mutate(tokenFreqs,word = paste(word1,word2))
    TweetFreqTemp %>%
      dplyr::select(-c(word1,word2))
  }
  else if(ncol(tokenFreqs)==4){
    TweetFreqTemp <- mutate(tokenFreqs,word = paste(word1,word2,word3))
    TweetFreqTemp %>%
      dplyr::select(-c(word1,word2,word3))
  }
  TweetFreqTemp %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::top_n(numOfWords,n) %>%
    dplyr::mutate(word = reorder(word, n)) %>%
    ggplot2::ggplot(aes(word, n)) +
    geom_col(show.legend = FALSE) +
    labs(y = "Word count",x = NULL) +
    coord_flip()
}

#FUNCTION 8 - PLOT OF MOST COMMON WORDS ==> WORD CLOUD FORMAT
#' plotFreqCloud
#'
#' Plot a cloud plot containing the most frequently used words within the Tweet data set
#' @param mostFreqWords Name of variable containing the frequency of words used, leave blank if unchanged
#' @param maxWords n number of most commonly used words to be displayed
#' @param minWordFreq Minimum frequency of word to appear on the cloud plot
#' @return Cloud plot containing n number of most frequent words within the Tweet dataset
#' @export
#' @examples plotFreqCloud() plotFreqCloud(,10,5) plotFreqCloud(mostFrequentlyUsedWords,100,100)
plotFreqCloud <- function(tokenFreqs, maxWords=70, minWordFreq=100){
  if(ncol(tokenFreqs)==2){
    tokenFreqs %>%
      wordcloud::with(wordcloud(word,n,max.words = maxWords,min.freq = minWordFreq,random.order = FALSE,
                                rot.per=0.35,colors = RColorBrewer::brewer.pal(8,"Dark2")))
  }
  else{
    stop('Cannot plot word cloud with tokenization >1')
  }
}

#FUNCTION 9 - PLOT OF MOST COMMON POS AND NEG WORDS ==> WORD CLOUD FORMAT
#' plotCompMap
#'
#' Plot a comparison cloud of the n most negative and positive words in the Tweet data set
#' @param mostFreqWords Name of variable containing the frequency of words used, leave blank if unchanged
#' @param maxWords n number of most commonly used words to be displayed
#' @param minWordFreq Minimum frequency of word to appear on the cloud plot
#' @return Cloud plot containing n most frequent positive and negative words in the Tweet data set
#' @export
#' @examples plotCompMap() plotCompMap(,10,5) plotCompMap(mostFrequentlyUsedWords,100,100)
#create comparisons map against pos and neg
plotCompCloud <- function(tokenFreqs, maxWords = 50, minWordFreq = 1){
  if(ncol(tokenFreqs)==2){
    tokenFreqs %>%
      dplyr::inner_join(get_sentiments("bing")) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::filter(row_number() <= maxWords & n >= minWordFreq) %>%
      reshape2::acast(word~sentiment,value.var = "n",fill=0) %>%
      wordcloud::comparison.cloud(colors = RColorBrewer::brewer.pal(8,"Dark2"), max.words = maxWords,match.colors = TRUE,random.order = FALSE)
  }
  else{
    stop('Cannot plot word cloud with tokenization >1')
  }
}

#FUNCTION 10 - PLOT OF MOST COMMON POS AND NEG WORDS ==> BAR CHART FORMAT
#' plotCompBar
#'
#' Plot a bar chart of the n most negative and positive words in the Tweet data set
#' @param mostFreqWords Name of variable containing the frequency of words used, leave blank if unchanged
#' @param maxWords n number of most commonly used words to be displayed
#' @param minWordFreq Minimum frequency of word to appear on the cloud plot
#' @return Bar chart containing n most frequent positive and negative words in the Tweet data set
#' @export
#' @examples plotCompBar() plotCompBar(,10,5) plotCompBar(mostFrequentlyUsedWords,100,100)
#create comparisons map against pos and neg
plotCompBar <- function(tokenFreqs, maxWords = 10, minWordFreq = 1){
  tokenFreqs %>%
    plyr::inner_join(get_sentiments("bing")) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::group_by(sentiment) %>%
    dplyr::filter(row_number() <= maxWords & n >= minWordFreq) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(word = reorder(word, n)) %>%
    ggplot2::ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Frequency",x = NULL) + coord_flip()
}

#FUNCTION 11 - PLOT BIGRAMS
#' plotBigrams
#'
#' Plot a cloud plot showing how bigrams connect together and the frequency of bigrams
#' @param mostFreqWords Name of variable containing the frequency of words used, leave blank if unchanged
#' @param minWordFreq Minimum frequency Bigram to appear on the cloud plot
#' @return Directional cloud plot displaying connectivity between Bigrams
#' @export
#' @examples plotBigrams(,10) plotBigrams() plotBigrams(mostFrequentlyUsedWords,)
plotBigrams <- function(tokenFreqs, minWordFreq = 5){
  arrowDesign <- grid::arrow(type="closed",length=unit(.15,"inches"))
  tokenFreqs %>%
    dplyr::filter(n >= minWordFreq) %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha=n),show.legend = TRUE,
                   arrow=arrowDesign,end_cap=circle(.07,'inches'))+
    geom_node_point(color="lightblue",size=5) +
    geom_node_text(aes(label=name),vjust=1,hjust=1) +
    theme_void()
}
