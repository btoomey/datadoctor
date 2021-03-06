% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropy.R
\name{entropy}
\alias{entropy}
\title{Entropy measure}
\usage{
entropy(x, base = exp(1))
}
\arguments{
\item{x}{numeric or integer vector}

\item{base}{of the logarithm, default is exp(1), the natural logarithm}
}
\description{
\code{entropy} calculates the Shannon entropy measure via maximum likelihood
for a numeric or integer variable. The user may specify the base,
\code{exp(1)} for the natrual base, \code{2} for bits, and so on.
}

