% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataFunctions.R
\name{removeStopWords}
\alias{removeStopWords}
\title{removeStopWords}
\usage{
removeStopWords(tweetDataFrameTokenized, extraStopWords = NULL)
}
\arguments{
\item{tweetDataFrameTokenized}{Dataframe object containing word tokens from tweets (Output of tokenizeTweets())}

\item{extraStopWords}{If there are extra words to exclude from the analysis enter them here, format: "c("word1","word2")}
}
\value{
Tokenized Tweets data frame excluding stop words.
}
\description{
Call and remove stop words from the tokenized tweet data set, stop words include common words that are not interesting for analysis (the,and, a, ...)
}
\examples{
removeStopWords(tweetDataFrameTokenized) removeStopWords(tweetDataFrameTokenized,c("tree","garden"))
}
