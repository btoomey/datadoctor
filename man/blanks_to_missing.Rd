% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cols_missing_blank.R
\name{blanks_to_missing}
\alias{blanks_to_missing}
\title{Convert any blank values to missing (NA) values}
\usage{
blanks_to_missing(df, cols.to.use = names(df))
}
\arguments{
\item{df}{The data.frame}

\item{cols.to.use}{The columns for which blanks are to be convert to missing}
}
\value{
The modified data.frame.
}
\description{
Convert any blank values to missing (NA) values
}

