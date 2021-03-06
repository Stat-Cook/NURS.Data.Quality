% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_mining.R
\name{prepare_data}
\alias{prepare_data}
\title{Dummy data and impute missing values for supplied data set, subdividing into test and train set.}
\usage{
prepare_data(data, outcome)
}
\arguments{
\item{data}{Data frame of values to be divided.}

\item{outcome}{The column of data frame to be treated as the outcome state.}
}
\value{
list(train_data, test_data)
}
\description{
Dummy data and impute missing values for supplied data set, subdividing into test and train set.
}
