% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mentionees.R
\name{get_mentionees}
\alias{get_mentionees}
\title{get_mentionees}
\usage{
get_mentionees(tweets)
}
\value{
a tibble with two columns noting the user ID and how many times they were mentioned
}
\description{
This function takes a tibble of tweets and records which users were mentioned, 
and how many times. Mentions include replies and retweets, but not quotes.
}
\examples{
get_mentionees(tweets)
}
