% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packageRef.R
\name{loadTweets}
\alias{loadTweets}
\title{loadTweets}
\usage{
loadTweets(fileName = "streamedTweets.json")
}
\arguments{
\item{fileName}{File name containing streamed Tweets, can be left empty if streamTweets function fileName was left empty. Format: format: "myfilename.json"}
}
\value{
Data frame object containing streamed tweets and relevant information.
}
\description{
Load .json file containing streamed tweets into a data frame object.
}
\examples{
streamTweets() or streamTweets("myfilename.json")
}