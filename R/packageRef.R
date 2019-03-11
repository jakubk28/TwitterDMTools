
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
#' @examples tweetDataFrame <- loadTweets("myfilename.json")
loadTweets <- function(fileName){
  #read in tweet data (.json file)
  tweetsDf <- rtweet::parse_stream(fileName)
  return(tweetsDf)
}



#FUNCTION 3 - TOKENIZE TWEETS INTO WORDS
#' tokenizeTweets
#'
#' Splits contents of tweets into words/pairs of words/ triples of words depending on the tokenization number.
#' @param tweetDataFrame Name of the dataframe object containing all tweets (output of loadTweets() )
#' @param tokenNum 1 - splits into singular words, 2 - splits into pairs of words, 3 - splits into triples of words.
#' @return Data frame object containing all Tweets split into n number of words.
#' @export
#' @examples tweetDataFrameTokenized <- tokenizeTweets(tweetDataFrame,1)
tokenizeTweets <- function(tweetDataFrame,tokenNum=1){
  #convert array into df and keep only tweet text
  tweetTextDf <- as.data.frame(mutate(tweetDataFrame[,5],1:nrow(tweetDataFrame)))
  names(tweetTextDf)[2] <- "document"
  tweetTextDf$text <-gsub('[[:punct:]]','',tweetTextDf$text)
  #tweetTextDf <- as.data.frame(tweetDataFrame[,5])
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
#' tokenFreq
#'
#' Displays the most common words/pair/triplesof words.
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets() )
#' @return Displays the most frequent words used within Tweet data.
#' @export
#' @examples mostCommonTokens <- tokenFreq(tweetDataFrameTokenized)
tokenFreq <- function(tweetDataFrameTokenized){
  #check if tokenized by word or bigram
  if(ncol(tweetDataFrameTokenized) == 2){
    #sort by frequency
    tweetFreq <- tweetDataFrameTokenized %>%
      dplyr::count(word,sort = TRUE)
  }
  else if(ncol(tweetDataFrameTokenized) == 3){
    tweetFreq <-tweetDataFrameTokenized %>%
      dplyr::count(word1,word2,sort = TRUE)
  }
  else if(ncol(tweetDataFrameTokenized) == 4){
    tweetFreq <- tweetDataFrameTokenized %>%
      dplyr::count(word1,word2,word3,sort = TRUE)
  }
  return(tweetFreq)
}


#FUNCTION 5 - LOOK UP WORD FOR CONTEXT
#' tweetContext
#'
#' If uncertain about the context of a popular word, the full length of the tweet will be shown to give context.
#' @param tweetDataFrame Name of the dataframe object containing all tweets (output of loadTweets() )
#' @param WordReq The word or phrase wanted for context, format: "word1"
#' @return Full length Tweet containing the word or phrase searched for.
#' @export
#' @examples tweetContext(tweetDataFrame,"pineapple") tweetContext(tweetDataFrame,"pineapple tree")
#look up anomalies for context
tweetContext <- function(tweetDataFrame,wordReq){
  tweetsContaining <- tweetDataFrame %>%
    dplyr::filter(stringr::str_detect(text, wordReq)) %>%
    dplyr::select(text)
  return(tweetsContaining)
}


#FUNCTION 6 - STOP WORDS, extra words entered as "c("word1","word2",...)"
#' removeStopWords
#'
#' Call and remove stop words from the tokenized tweet data set, stop words include common words that are not interesting for analysis (the,and, a, ...)
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets() )
#' @param extraStopWords If there are extra words to exclude from the analysis enter them here, format: "c("word1","word2")
#' @return Tokenized Tweets data frame excluding stop words.
#' @export
#' @examples removeStopWords(tweetDataFrameTokenized) removeStopWords(tweetDataFrameTokenized,c("tree","garden"))
removeStopWords <- function(tweetDataFrameTokenized,extraStopWords=NULL){
  #call stop words dictionary
  data(stop_words)
  #check whether extra stop words are entered, if so:
  stop_words <- dplyr::bind_rows(data_frame(word = c("t.co","https","amp"),lexicon = c("custom")),stop_words)
  if(!is.null(extraStopWords)){
    #add custom words to stop-words list
    stop_words <- dplyr::bind_rows(data_frame(word = extraStopWords,lexicon = c("custom")),stop_words)
  }
  #remove stop words from dataframe
  if(ncol(tweetDataFrameTokenized)==2){
    tweetDfTokenClean <- tweetDataFrameTokenized %>%
      #dplyr::anti_join(stop_words)
      dplyr::filter(!word %in% stop_words$word)

  }
  else if(ncol(tweetDataFrameTokenized)==3){
    tweetDfTokenClean <- tweetDataFrameTokenized %>%
      #anti_join(stop_words)
      dplyr::filter(!word1 %in% stop_words$word) %>%
      dplyr::filter(!word2 %in% stop_words$word)
  }
  else if(ncol(tweetDataFrameTokenized)==4){
    tweetDfTokenClean <- tweetDataFrameTokenized %>%
      #anti_join(stop_words)
      dplyr::filter(!word1 %in% stop_words$word) %>%
      dplyr::filter(!word2 %in% stop_words$word) %>%
      dplyr::filter(!word3 %in% stop_words$word)
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
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets() )
#' @param numOfWords Top n words to be plotted, default is 10.
#' @return Top n most frequent words used in the Tweet dataset.
#' @export
#' @examples plotFreqBar(tweetDataFrameTokenized,5) plotFreqBar(tweetDataFrameTokenized)
plotFreqBar <- function(tweetDataFrameTokenized,numOfWords=10){
  tokenFreqs <- tokenFreq(tweetDataFrameTokenized)
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
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets() )
#' @param maxWords Maximum number of words to appear on the plot.
#' @param minWordFreq Minimum frequency of word to appear on the plot
#' @return Cloud plot containing n number of most frequent words within the Tweet dataset
#' @export
#' @examples plotFreqCloud(tweetDataFrameTokenized) plotFreqCloud(tweetDataFrameTokenized,10,5)
plotFreqCloud <- function(tweetDataFrameTokenized, maxWords=70, minWordFreq=100){
  tokenFreqs <- tokenFreq(tweetDataFrameTokenized)
  if(ncol(tokenFreqs)==2){
    tokenFreqs %>%
     with(wordcloud::wordcloud(word,n,max.words = maxWords,min.freq = minWordFreq,random.order = FALSE,
                                rot.per=0.35,colors = RColorBrewer::brewer.pal(8,"Dark2")))
  }
  else{
    stop('Cannot plot word cloud with tokenization >1')
  }
}

#FUNCTION 9 - PLOT OF MOST COMMON POS AND NEG WORDS ==> WORD CLOUD FORMAT
#' plotCompCloud
#'
#' Plot a comparison cloud of the n most negative and positive words in the Tweet data set
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets() )
#' @param maxWords Maximum number of words to appear on the plot.
#' @param minWordFreq Minimum frequency of word to appear on the plot
#' @return Cloud plot containing n most frequent positive and negative words in the Tweet data set
#' @export
#' @examples plotCompMap(tweetDataFrameTokenized) plotCompMap(tweetDataFrameTokenized,10,5)
#create comparisons map against pos and neg
plotCompCloud <- function(tweetDataFrameTokenized, maxWords = 50, minWordFreq = 1){
  tokenFreqs <- tokenFreq(tweetDataFrameTokenized)
  if(ncol(tokenFreqs)==2){
    temp<-tokenFreqs %>%
      dplyr::inner_join(get_sentiments("bing")) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::filter(row_number() <= maxWords & n >= minWordFreq) %>%
      reshape2::acast(word~sentiment,value.var = "n",fill=0)
    temp<-temp[,c(2,1)] %>%
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
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets() )
#' @param maxWords Maximum number of words to appear on the plot.
#' @param minWordFreq Minimum frequency of word to appear on the plot.
#' @return Bar chart containing n most frequent positive and negative words in the Tweet data set
#' @export
#' @examples plotCompBar(tweetDataFrameTokenized) plotCompBar(tweetDataFrameTokenized,10,5)
#create comparisons map against pos and neg
plotCompBar <- function(tweetDataFrameTokenized, maxWords = 10, minWordFreq = 1){
  tokenFreq(tweetDataFrameTokenized) %>%
    dplyr::inner_join(get_sentiments("bing")) %>%
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
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets() )
#' @param minWordFreq Minimum frequency of word to appear on the plot.
#' @return Directional cloud plot displaying connectivity between Bigrams
#' @export
#' @examples plotBigrams(tweetDataFrameTokenized,10) plotBigrams(tweetDataFrameTokenized)
plotBigrams <- function(tweetDataFrameTokenized, minWordFreq = 5){
  tokenFreqs <- tokenFreq(tweetDataFrameTokenized)
  arrowDesign <- grid::arrow(type="closed",length=unit(.15,"inches"))
  tokenFreqs %>%
    dplyr::filter(n >= minWordFreq) %>%
    igraph::graph_from_data_frame() %>%
    ggraph::ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha=n),show.legend = TRUE,
                   arrow=arrowDesign,end_cap=circle(.07,'inches'))+
    geom_node_point(color='cyan4',size=5) +
    geom_node_text(aes(label=name),vjust=1,hjust=1) +
    theme_void()
}
