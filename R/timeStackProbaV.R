#' @title Build time stack from probav images
#'
#' @description Stacks Prova-V layers.
#' @author J Eberenz
#' @param x Character. Directory of a Proba-V geotiffs or list of filenames.
#' @param patttern Character. As in \code{\link{list.files}}
#' @param orderChrono Logical. Wether to oder the stack chronologically. Defatult \code{TRUE}.
#' @param tile Character. Whcih tile to process. Format: "X00Y00".
#' @param quick Logical. See \code{raster::stack}
#' @param end_date Date. Last date to process.
#' @param ... Additional arguments to \code{raster::writeRaster}
#'
#' @return a RasterStack or rasterBrick.
#'
#' @export
#'
#' @import raster

timeStackProbaV <- function(x, pattern, order_chrono=TRUE, tile=NULL, quick=FALSE, end_date=NULL, ...){

  df_info <- getProbaVinfo(x, pattern)
  if(order_chrono){
    df_info <- df_info[order(df_info$date),]
  }

  if (!is.null(tile)) df_info  <- df_info[df_info$tile==tile, ]
  if (!is.null(end_date)) df_info  <- df_info[as.numeric(df_info$date) <= end_date,]


  s <- raster::stack(file.path(fdir, df_info$fpath), quick=quick)

  #cat("build brick ... ")
  #s <- brick(s)
  names(s) <- row.names(df_info)

  s <- setZ(x=s, z=format(df_info$date, "%Y%j"))

  if(hasArg(filename)) {
    cat("writing...")
    out <- writeRaster(s, progress="bar", ... )
    return(out)
  }

  return(s)
}
