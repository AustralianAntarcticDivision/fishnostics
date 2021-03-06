% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_outputs.R
\name{process_single_series}
\alias{process_single_series}
\title{Process single series of Monte Carlo outputs}
\usage{
process_single_series(res, vars, ref, col_names = NULL, FUN)
}
\arguments{
\item{res}{dataframe of MCMC output}

\item{vars}{names of columns to select}

\item{ref}{a column name to calculate a comparison statistic}

\item{col_names}{column names for the output (default=NULL will when
implemented generate these automatically)}

\item{FUN}{function to apply "prop" = proportion use for status}
}
\description{
Process single series of Monte Carlo outputs
}

