#' Wrapper/batcher to pre-process Proba-V in batch mode
#' 
#' @description Processes Proba-V data for subsequentent use in time-series analysis. Performs Proba-V cleaning and operations with parallel support using foreach
#' 
#' @param x Character. Directory where the data is located. Or list of file names.
#' @param pattern Character. Only useful if x is of length 1. See \link{list.files} for more details
#' @param tiles Character. Tiles to process, format "X10Y06", 
#' @param start_d Date. Starting date.
#' @param QC_val Numeric. See \code{\link{cleanProbaV}}
#' @param fill Numeric. See \code{\link{cleanProbaV}}
#' @param as.is Logical. See \code{\link{cleanProbaV}}
#' @param ncores Numeric. Number of cores to use for processing. See \link{registerDoParallel}
#' @param outdir Character. Directory where the output should be written.
#' @param overwritw Logical. Wether to overwrite existing files using the same naming convention in outdir. ault is \code{FALSE})
#' 
#' @seealso \code{\link{cleanProbaV}}, and \code{\link{timeStackProbaV}} for stacking.
#' 
#' @return This function is used for its side effect of producing cleaned data, hence written to disk at \code{outdir} location
#' 
#' @import raster
#' @import doParallel
#' @import foreach

processProbaVbatch <- function(x, pattern = "NDVI.tif$", tiles=NULL, start_d=NULL, QC_val, fill=NULL, as.is=FALSE, outdir, ncores=1, overwrite=FALSE) {
  if (!is.character(x)) {
    stop('x needs to be of class character')
  }
  
  if(length(x) == 1) {
    info <- getProbaVinfo(x, pattern=pattern)

    x <- list.files(path=x, pattern=pattern, full.names=TRUE,  recursive = T, include.dirs = F, no.. = T)
  }
  if (!is.null(tiles)) {
    x <- x[info$tile %in% tiles]
  }
  if (!is.null(start_d)) {
    x <- x[info$date <= start_d]
  }
  
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  type <- dataType(raster(x[1]))
  
  outnames <- file.path(outdir, gsub("\\.tif", "_sm.tif", basename(x)))
    # for unstacking radiometry image, sm will be in band suffix, for checking we use the red band
  outnames <- gsub("RADIOMETRY_sm\\.tif", "RED0_sm.tif", outnames)
  if(!overwrite){
    x <- x[!file.exists(outnames)]
    outnames <- outnames[!file.exists(outnames)]
  }
  outnames <- gsub("_RED0_sm\\.tif", ".tif", outnames)

 
  cat("Processing", length(x), "files. Names: ", length(outnames), "\n")
  
  
  if (ncores > 1){
    registerDoParallel(ncores)
  } else registerDoSEQ()

  xprocessed <- foreach(i=x, o=outnames, .combine = c, .multicombine = T, .inorder = F, .packages = c("raster", "rgdal"), .verbose = T ) %dopar% {
    cat("...out:", o)
    r <- cleanProbaV(i, filename=o, QC_val = QC_val, fill=fill, datatype = type, as.is = as.is )
    print(r)
    o
  }
  
  registerDoSEQ()
  cat(length(xprocessed), " files processed")
  return(xprocessed)
  
# old...
#     mcmapply(FUN=cleanProbaV, f_data=x, filename = outnames, 
#          MoreArgs = list(QC_val = QC_val, fill=fill, datatype = type, read_gdal=read_gdal), mc.cores=mc.cores)

}