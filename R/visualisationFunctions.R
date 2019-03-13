###########################
##VISUALISATION FUNCTIONS##
###########################

#' plotFreqBar
#'
#' Plot a bar chart of the n most frequent words in the Tweet data set
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
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

#' plotFreqCloud
#'
#' Plot a cloud plot containing the most frequently used words within the Tweet data set
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
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

#' plotCompCloud
#'
#' Plot a comparison cloud of the n most negative and positive words in the Tweet data set
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
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

#' plotCompBar
#'
#' Plot a bar chart of the n most negative and positive words in the Tweet data set
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
#' @param maxWords Maximum number of words to appear on the plot.
#' @param minWordFreq Minimum frequency of word to appear on the plot.
#' @return Bar chart containing n most frequent positive and negative words in the Tweet data set
#' @export
#' @examples plotCompBar(tweetDataFrameTokenized) plotCompBar(tweetDataFrameTokenized,10,5)
#create comparisons map against pos and neg
plotCompBar <- function(tweetDataFrameTokenized, maxWords = 10, minWordFreq = 1){
  tokenFreq(tweetDataFrameTokenized) %>%
    #concatinate setniment dictionary with words in data set
    dplyr::inner_join(get_sentiments("bing")) %>%
    #append total number of pos and neg words
    dplyr::inner_join(
      #group by sentiment class
      dplyr::group_by(.,sentiment) %>%
        #total frequency of pos and neg words
        dplyr::summarise(n=sum(n)) %>%
        #ungroup
        dplyr::ungroup() %>%
        #ovreall frequency of pos AND eng words
        dplyr::mutate(ntot=sum(n)) %>%
        #calculate proportion of pos and neg words in data set
        dplyr::mutate(prop=n/ntot),"sentiment") %>%
    #append proportions to groups for plotting
    dplyr::mutate(sentiment = paste0(sentiment, " (p = ", round(prop, 2), ")")) %>%
    dplyr::rename(n=n.x) %>%
    #sort by frequency of words
    dplyr::arrange(desc(n)) %>%
    #group by sentiment again
    dplyr::group_by(sentiment) %>%
    #impose boundary conditions
    dplyr::filter(row_number() <= maxWords & n >= minWordFreq) %>%
    #ungroup positive and negative classes
    dplyr::ungroup() %>%
    dplyr::mutate(word = reorder(word, n)) %>%
    #plot frequency of words by group
    ggplot2::ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Frequency",x = NULL) + coord_flip()
}


#' plotBigrams
#'
#' Plot a cloud plot showing how bigrams connect together and the frequency of bigrams
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
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
