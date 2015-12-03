#' @title Multicore implementation of the raster::\code{\link{calc}} function.
#' 
#' @description Allows functions to be applied to raster objects, with multicore support, controlling the chunksize with minrows.
#' 
#' @param x Raster* object
#' @param minrows Integer
#' @param fun Function to be applied to the raster object.
#' @param mc.cores Integer Numvber of corrse, see \code{\link{mclapply}}.
#' @param ... Arguments to be passed to \code{\link{writeRaster}}.
#' @details For further help, see \code{\link{calc}}. Warnings of the parallel package (see \code{\link{mclapply}} for instance) apply to this function.
#' @return a Raster* object 
#' @author Johannes Eberenz, adapted from Loic Dutrieux
#' @seealso \code{\link{calc}}
#' @import raster
#' @import parallel
#' @export
#' 

# Author: Loic Dutrieux
# June 2013
# loic.dutrieux@wur.nl

mcCalc <- function(x, minrows=1, fun, mc.cores=1, logfile=NULL, ...) {
  
  dots <- list(...)
  
  if(mc.cores == 1) { # Normal calc
    out <- calc(x=x, fun=fun, ...)
    return(out)
  } else {
    
    s <- blockSize(x, minblocks=mc.cores, minrows = minrows)
    blocs <- seq(1, s$n)
    
    
    # Create blocks and run the function on that bloc
    fun2 <- function(i) {
      e <- extent(x, r1=s$row[i], r2=s$row[i]+s$nrows[i]-1)
      m_free <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=T))
      # logging:
      if(!is.null(logfile)){
      #  sink(logfile, append = T)
        log_msg <- sprintf("\n%s: processing bloc %d of %d, mfree:%d, listOut:%s\n",
                           as.character(Sys.time()), i, s$n, m_free, object.size(listOut))
        cat(log_msg, file = logfile, append = T)
      #  sink()
      }
      
      if (m_free < 1000000){
        tmp <- rasterTmpFile(prefix = "mcCalc_")
        b <- crop(x, e, filename=tmp)
      } else {
        b <- crop(x, e)
      }
  
      out <- calc(x=b, fun=fun) # Does this line need an elipsis
      rm(b)
      gc()
      
      return(out)
    }
    

    if(!is.null(logfile)) cat("\nStart processing...",  file = logfile, append = T)
    
    listOut <- mclapply(X=blocs, FUN=fun2, mc.cores=mc.cores)
    
    # Add ALL arguments passed in the ellipsis in the listOut object
    if(!is.null(logfile)) cat("\nStart mosaic...",  file = logfile, append = T)
    listOut <- c(listOut, dots)
    listOut$fun <- max
    
    out <- do.call(mosaic, listOut)
    print(out)
    
    return(out)
  }
  
}