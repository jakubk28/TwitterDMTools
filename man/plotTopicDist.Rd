% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topicmodellingFunctions.R
\name{plotTopicDist}
\alias{plotTopicDist}
\title{plotTopicDist}
\usage{
plotTopicDist(ldaModel)
}
\arguments{
\item{ldaModel}{lda model with numerous topics, output of tweetLDA().}
}
\value{
Plot of distribution of gamma for each topic
}
\description{
Plot distribution of gamma values for specific topics
}
\examples{
plotTopicDist(tweetDataFrameTokenized)
}
