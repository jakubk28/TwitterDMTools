
#############################
##TOPIC MODELLING FUNCTIONS##
#############################


#' ldaForm
#'
#' Convert data frame object into document term matrix for use with lda clustering
#'
#' @keywords internal
#' @noRd
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
#' @return Document term matrix, suitable for clustering using lda algorithm
#' @examples ldaForm(tweetDataFrameTokenized)
ldaForm <- function(tweetDataFrameTokenized){
  ldaInputForm <- tweetDataFrameTokenized %>%
    #count unique words per document
    dplyr::group_by(document) %>%
    dplyr::count(word) %>%
    dplyr::ungroup() %>%
    #cast into document term matrix
    tidytext::cast_dtm(document,word,n)
  return(ldaInputForm)
}

#' estTopics
#'
#' Produce a plot with using 4 optimisationg techniques to determine range of optimal topic numbers for the LDA model.
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
#' @param minTopics Lower bound of topics to be optimised over.
#' @param maxTopics Upper bound of topics to be optimised over.
#' @param stepNum Optimisation step amount. (1 will take longer but be more accurate)
#' @return Optimised range of topics for specific data frame variable.
#' @export
#' @examples estTopics(tweetDataFrameTokenized,2,30,1
estTopics <- function(tweetDataFrameTokenized,minTopics=2,maxTopics=30,stepNum=1){

  #optimise topic number over range specified
  result <- ldatuning::FindTopicsNumber(
    ldaForm(tweetDataFrameTokenized),
    topics = seq(from = minTopics, to = maxTopics, by = stepNum),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 2L,
    verbose = TRUE
  )
  #plot results
  ldatuning::FindTopicsNumber_plot(result)
  #return table form of results
  return(result)

}

#' tweetLDA
#'
#' Create a latent Dirichlet allocation cluster model for a tweet data frame.
#' @param tweetDataFrameTokenized Dataframe object containing word tokens from tweets (Output of tokenizeTweets())
#' @param numberOfTopics Number of topics associated with the corpus, this is the amount of groups the clustering
#' algorithm will split the model into. If number of topics is unknown, use (guess function) first to estimate.
#' @return Model type object.
#' @export
#' @examples tweetLDA(tweetDataFrameTokenized,8)
tweetLDA <- function(tweetDataFrameTokenized, numberOfTopics){

  #create lda cluster model using Gibbs sampling
  temp_LDA <- topicmodels::LDA(
    ldaForm(tweetDataFrameTokenized),
    k=numberOfTopics,
    method = "Gibbs",
    control = list(seed=1234)
  )
  return(temp_LDA)
}


#' plotTopicTerms
#'
#' Plot most terms associated with each topic
#' @param ldaModel lda model with numerous topics, output of tweetLDA().
#' @param topN number of top terms to be plotted for each topic
#' @return Plot of most common terms within each topic
#' @export
#' @examples plotTopicTerms(tweetDataFrameTokenized,10)
plotTopicTerms <- function(ldaModel,topN=10){

  #extract top 10 terms from each topic
  top_terms <- tidytext::tidy(ldaModel) %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(topN,beta) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(topic,-beta)

  #plot extracted beta terms
  top_terms %>%
    dplyr::mutate(term = reorder(term, beta)) %>%
    dplyr::group_by(topic, term) %>%
    dplyr::arrange(desc(beta)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(term = factor(paste(term, topic, sep = "__"),
                         levels = rev(paste(term, topic, sep = "__")))) %>%
    ggplot2::ggplot(aes(term, beta, fill = as.factor(topic))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
      labs(title = "Top 10 terms in each LDA topic",
      x = NULL, y = expression(beta)) +
      facet_wrap(~ topic, ncol = 4, scales = "free")

}


#' plotTopicDist
#'
#' Plot distribution of gamma values for specific topics
#' @param ldaModel lda model with numerous topics, output of tweetLDA().
#' @return Plot of distribution of gamma for each topic
#' @export
#' @examples plotTopicDist(tweetDataFrameTokenized)
plotTopicDist <- function(ldaModel){

  #distrbution of probabilty for each topic
  ggplot2::ggplot(tidytext::tidy(ldaModel,matrix="gamma"), aes(gamma, fill = as.factor(topic))) +
    geom_histogram(show.legend = FALSE) +
    facet_wrap(~ topic, ncol = 4) +
    scale_y_log10() +
    labs(title = "Distribution of probability for each topic",
    y = "Number of documents", x = expression(gamma))

}
