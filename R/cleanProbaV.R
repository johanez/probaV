#' Clean Proba-V
#'
#' @description Uses Proba-V quality layer to perform cleaning over a single data layer.
#'
#' @param f_data Character. Filename (full path) of a .gif the containing data
#' @param QC_val Numeric. Quality control values to \bold{keep} in the data.
#' @param fill (vector of) Numeric or NULL. Fill values in the data layer to be filtered out. (important to do before reprojection with resampling method other than Nearest Neighbours)
#' @param as.is Logical. As in \code{\link{read.gdal}}.
#' @param ... Arguments to be passed to \code{\link{writeRaster}}.
#'
#' @return A single rasterLayer, or a stack of the bands for RADIOMETRY input
#' @export
#' @author J Eberenz, adapted from Loic Dutrieux
#'
#' @seealso \code{\link{processProbaVbatch}} for batcher
#'
#'
#'
#' @import raster
#' @import rgdal
#'
#'

cleanProbaV <- function(f_data, QC_val, fill=255, as.is=FALSE, ...){

  # derive QC filename from data file name, check
  f_QC <- gsub("_[A-Z]+\\.tif", "_SM.tif", f_data)
  if(!file.exists(f_QC)) {
    warning(f_QC, "not processed, no quality flag (SM) file found!")
  } else {
    cat("reading data...")

    # as.is = T for datasets without scaling factor
    data <- brick(readGDAL(f_data, as.is=as.is, silent=TRUE))
    cat(inMemory(data))
    cat("reading SM...")
    QC <- raster(f_QC)

    clean <- function(x, y) {
      x[!y %in% QC_val] <- NA
      if(!is.null(fill)) {
        x[x %in% fill] <- NA
      }
      return(x)
    }
    cat("start overlay...")
    if(nlayers(data)>1) {

      b <- raster::overlay(x = data, y = QC, fun = clean, unstack=T, progress="bar", ...)

      if (hasArg(filename)) {
        cat("writing...")
        writeRaster(b, ... , bylayer=T, suffix=c("RED0_sm", "NIR0_sm", "BLUE_sm", "SWIR_sm"), progress="bar")
        file.remove(filename)
      }
    } else {
      # single leyer, e.g. NDVI
      raster::overlay(x = data, y = QC, fun = clean, ...)
    }
  }
}


