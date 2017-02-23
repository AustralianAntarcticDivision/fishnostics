## functions to process MCMC outputs (from planetfish)
## this may be better placed in planetfish

#' Process two Monte Carlo outputs
#'
#' Process two Monte Carlo outputs
#' @param res dataframe of outputs from simulations
#' @param true_vars names of the columns that represent the true or known
#'  parameters
#' @param est_vars names of columns that represent estimated parameters
#' @param col_names column names for the output (default=NULL will when
#' implemented generate these automatically)
#' @param FUN function to apply "rel_err" calculate the relative error
#' @export
process_two_series <- function(res, true_vars, est_vars, col_names=NULL, FUN){
  ## define valid FUNs, remember to add new FUNs below
  valid_FUNs <- c("rel_err")
  ## do some checks on res
  if(!FUN %in% valid_FUNs)
    stop(paste0("supplied FUN must be one of ", valid_FUNs))
  if(length(true_vars) != length(est_vars))
    stop("true_vars and est_vars are different lengths")
  if(!any(true_vars) %in% names(d))
    stop("one or more true_vars are not present in res")
  if(!any(est_vars) %in% names(d))
    stop("one or more est_vars are not present in res")
  if(is.null(col_names))
    ##* still need to add a pattern check to the column names
    stop("automatic column names using pattern matching not yet implemented")
  if(length(col_names) != length(true_vars))
    stop("col_names and true_vars have different lengths")
  ## extract the separate data
  true <- res[,c(true_vars)]
  est <- res[,c(est_vars)]
  ## calculate the new values based on
  switch(FUN,
         rel_err = obj <- rel_err(est, true)
  )
  ## add column names
  names(obj) <- col_names
  ## return the object
  obj
}

#' Process single series of Monte Carlo outputs
#'
#' Process single series of Monte Carlo outputs
#' @param res dataframe of MCMC output
#' @param vars names of columns to select
#' @param ref a column name to calculate a comparison statistic
#' @param col_names column names for the output (default=NULL will when
#' implemented generate these automatically)
#' @param FUN function to apply "prop" = proportion use for status
#' @export
process_single_series <- function(res, vars, ref, col_names=NULL, FUN){
  ## define valid FUNs, remember to add new FUNs below
  valid_FUNs <- c("prop")
  ## do some checks on d
  if(!FUN %in% valid_FUNs)
    stop(paste0("supplied FUN must be one of ", valid_FUNs))
  if(!any(vars) %in% names(d))
    stop("one or more vars are not present in res")
  if(!(comp) %in% names(d))
    stop("comp is not present in res")
  if(is.null(col_names))
    ##* still need to add a pattern check to the column names
    stop("automatic column names using pattern matching not yet implemented")
  if(length(col_names) != length(vars))
    stop("col_names and vars have different lengths")
  ## extract the separate data
  est <- res[,c(vars)]
  comp <- res[,c(ref)]
  ## calculate the new values based on
  switch(FUN,
         prop = obj <- est/ref
  )
  ## add column names
  names(obj) <- col_names
  ## return the object
  obj
}



