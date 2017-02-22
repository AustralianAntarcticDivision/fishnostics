#' Save a plot
#'
#' Save a plot
#' @inheritParams plotting
#' @export
save_plot <- function(file_name, file_type, path){
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
    png = {png(filename=paste0(path,"/", file_name))
    },
    pdf = {pdf(filename=paste0(path,"/", file_name))
    }
  )
  ## should we return something?
}
