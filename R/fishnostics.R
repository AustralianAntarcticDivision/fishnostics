#' fishnostics: Diagnostics for fisheries assessment models, specifically CASAL,
#' however, applicable to other fisheries assessments
#'
#' This package currently includes plots used to present model outputs along
#' with diagnostic plots.
#'
#' The intention is that this package will be implemented in completely in
#' ggplot2, however, currently base and lattice are used.
#'
#' Need to generate some example datsets to use for package examples
#'
#' @docType package
#' @name fishnostics
NULL
#> NULL

#' Generic parameters for plotting
#'
#' @param probs quantile probabilities for plotting
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param main main label
#' @param save logical to specify whether the plot should be saved (if TRUE a
#' file_name must be supplied) if TRUE calls \code{\link{save_plot}}
#' @param file_name valid file name list to store model results passed to
#'  \code{\link{save_plot}}
#' @param file_type default = png, passed to \code{\link{save_plot}}
#' @param graph_pars graphical parameters passed to par
#' @name plotting
NULL
#> NULL
