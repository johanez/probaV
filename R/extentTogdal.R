#' @title Convert raster extnet to gdal extent
#' @description For use with \code{\link{gdalUtils}}
#'
#' @param x ratser* or raster::extent object
#'
#' @return numeric vector with corner coordinates (xmin, ymin, max, ymax).
#' 
#' @import raster
#' @export 

extentTogdal <- function(x){
  if (class(x) %in% c("RasterBrick", "Rasterlayer", "RasterStack")) {
    x <- extent(x)
  }
  if (class(x)=="Extent"){
    return(c(x@xmin, x@ymin, x@xmax, x@ymax))
  } 
  else stop("Not an raster or extent object!")
}