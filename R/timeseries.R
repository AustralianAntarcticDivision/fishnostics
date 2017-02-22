## this file contains function for plotting data over time (eg biomass, year
## classes)

#' Plot a timeseries with uncertainty
#'
#' Plot a timeseries with uncertainty represented as a grey polygon with the
#' option of adding a second series to the plot
#' @param d dataframe with columns representing each of the ys (could rename d to be the main series?)
#' @param xs vector of xs corresponding with the columns of d (if NULL, names of
#' d are used)
#' @param d2 a dataframe with the same number of columns as d used to add a
#' second series as a line plot (same xs are used)
#' @inheritParams plotting
#' @export
plot_ts_uncertainty <- function(d,
                                xs=NULL,
                                d2=NULL,
                                probs=c("lwr"=0.025, "mid"=0.5, "upr"=0.975),
                                xlab="", ylab="",
                                save=FALSE, file_name=NULL, file_type="png", path=NULL,
                                graph_pars=list(mfrow=c(1,1), mar=c(4,4,1,1),
                                                las=2)
){
  ## set the graphical parameters for the plot
  ##* I've removed 'windows' from graph_pars because it is a function
  graph_pars <- par(graph_pars)
  ## check the probabilities
  if(length(probs) != 3) stop("probs must be a vector of length 3")
  if(any(probs>1) | any(probs<0)) stop("all probs must be in [0,1]")
  ## calculate the midpoints and quantiles to plot
  mid <- apply(d, 2, quantile, probs[["mid"]])
  lwr <- apply(d, 2, quantile, probs[["lwr"]])
  upr <- apply(d, 2, quantile, probs[["upr"]])
  ## if a second data set is provided plot it as well
  if(!is.null(d2)){
    if(ncol(d) != ncol(d2)) stop("d and d2 have different numbers of columns")
    ## compare the same probabilities
    mid2 <- apply(d2, 2, quantile, probs[["mid"]])
    lwr2 <- apply(d2, 2, quantile, probs[["lwr"]])
    upr2 <- apply(d2, 2, quantile, probs[["upr"]])
  }
  ## extract the xs
  if(is.null(xs)) xs <- as.numeric(names(d))
  ## can we asign a plot to an object then print it? Nope, think ggplot does though
  ## if we save the plot do this
  if(save){
    ## if save and no file name return an error
    if(is.null(file_name))
      stop("file name must be specified is plot is to be saved")
    if(!file_type %in% c("png", "pdf"))
      stop(paste0("specified file type not implemented, please use ",
                  c("png", "pdf")))
    ## if no path specified use the working dir
    if(is.null(path)) path <- getwd()
    ## save plot based on specified file_type
    switch(
      ## this turns the graphics device on
      ##* add additional options
      png = {png(filename=paste0(path,"/",file_name))
      },
      pdf = {pdf(filename=paste0(path,"/",file_name))
      }
    )
  }
  ## now we create the plot
  #* increase the number of tick marks
  ## this gets overwritten by the polygon
  plot(mid ~ xs, type="l",ylab=ylab,xlab=xlab,ylim=c(0,1.05*max(upr)), las=2)
  ## how does the polygon function work
  polygon(x=c(xs,rev(xs)),y=c(upr, rev(lwr)),col="lightgrey",border="lightgrey")
  #abline(h=1,col="darkgrey",lty=3)
  ## points adds the
  points(mid ~ xs, type="l", pch=21,col="blue",bg="blue",lwd=2,lty="solid")
  #for (ii in 1:length(ycs_y_hist)) segments(ycs_y_hist[ii],lwr[ii],ycs_y_hist[ii],upr[ii])
  ## now add the line for the true values
  if(!is.null(d2)){
    points(mid2 ~ xs, type="l", pch=21,col="red",bg="red",lwd=1.8,lty=2)
    points(lwr2 ~ xs, type="l", pch=21,col="red",bg="red",lwd=1.8,lty=3)
    points(upr2 ~ xs, type="l", pch=21,col="red",bg="red",lwd=1.8,lty=3)
  }
  ## if save turn off graphics device
  if(save) dev.off()
  ## at the end of plotting we reset the graphical parameters
  par(graph_pars)
}

