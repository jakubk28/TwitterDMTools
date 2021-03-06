% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topicmodellingFunctions.R
\name{tweetLDA}
\alias{tweetLDA}
\title{tweetLDA}
\usage{
tweetLDA(tweetDataFrameTokenized, numberOfTopics)
}
\arguments{
\item{tweetDataFrameTokenized}{Dataframe object containing word tokens from tweets (Output of tokenizeTweets())}

\item{numberOfTopics}{Number of topics associated with the corpus, this is the amount of groups the clustering
algorithm will split the model into. If number of topics is unknown, use (guess function) first to estimate.}
}
\value{
Model type object.
}
\description{
Create a latent Dirichlet allocation cluster model for a tweet data frame.
}
\examples{
tweetLDA(tweetDataFrameTokenized,8)
}
