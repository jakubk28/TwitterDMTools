
#########################################
##DATA COLLECTION AND TIDYING FUNCTIONS##
#########################################

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

  #checks whether final 5 characters of the string contain .json file extension
  if(stringr::str_sub(fileName,-5,-1) %in% '.json'== FALSE){
    #if false append .json file extention to file name
    fileName<-paste0(fileName,'.json')
  }

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


#' loadTweets
#'
#' Load .json file containing streamed tweets into a data frame object.
#' @param fileName File name containing streamed Tweets. Format: "myfilename.json"
#' @return Data frame object containing streamed tweets and relevant information.
#' @export
#' @examples tweetDataFrame <- loadTweets("myfilename.json")
loadTweets <- function(fileName){

  #checks whether final 5 characters of the string contain .json file extension
  if(stringr::str_sub(fileName,-5,-1) %in% '.json'== FALSE){
    #if false append .json file extention to file name
    fileName<-paste0(fileName,'.json')
  }

  #read in tweet data (.json file)
  tweetsDf <- rtweet::parse_stream(fileName)
  return(tweetsDf)
}


#' tokenizeTweets
#'
#' Splits contents of tweets into words/pairs of words/ triples of words depending on the tokenization number.
#' @param tweetDataFrame Name of the dataframe object containing all tweets (output of loadTweets())
#' @param tokenNum 1 - splits into singular words, 2 - splits into pairs of words, 3 - splits into triples of words.
#' @return Data frame object containing all Tweets split into n number of words.
#' @export
#' @examples tweetDataFrameTokenized <- tokenizeTweets(tweetDataFrame,1)
tokenizeTweets <- function(tweetDataFrame,tokenNum=1){

  #convert array into dataframe and keep only tweet text
  tweetTextDf <- as.data.frame(mutate(tweetDataFrame[,5],1:nrow(tweetDataFrame)))
  #rename column 2 to document
  names(tweetTextDf)[2] <- "document"
  #remove punctuation from text
  tweetTextDf$text <-gsub('[[:punct:]]','',tweetTextDf$text)

  #tokenize into single words
  if(tokenNum==1){
    #tokenize into single words
    tweetsTokenized <- tweetTextDf %>%
      tidytext::unnest_tokens(word,text)
  }

  #tokenize into n bigrams
  else if(tokenNum>=2){
    tweetsTokenized <- tweetTextDf %>%
      #tokenize into tokenNum of words
      tidytext::unnest_tokens(bigram,text,token = "ngrams",n = tokenNum) %>%
      #split each word into its individual cell
      tidyr::separate(bigram, paste0('word',seq(tokenNum)), sep = " ")
    #if tokenization is set to greater than 3 produce warning
    if(tokenNum>3){
      warning('recommend using token of 1,2,3')
    }
  }
  #return tokenized df
  return(tweetsTokenized)
}


#' tokenFreq
#'
#' Displays the most common words/pair/triplesof words.
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
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


#' tweetContext
#'
#' If uncertain about the context of a popular word, the full length of the tweet will be shown to give context.
#' @param tweetDataFrame Name of the dataframe object containing all tweets (output of loadTweets())
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


#' removeStopWords
#'
#' Call and remove stop words from the tokenized tweet data set, stop words include common words that are not interesting for analysis (the,and, a, ...)
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
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
