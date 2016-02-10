#' @title Multicore implementation of the raster::\code{\link{calc}} function.
#'
#' @description
#' Allows functions to be applied to raster objects, with multicore support,
#' controlling the chunksize with minrows. In parallel mode, intermediate blocks will always be written to disk.
#' Mosaicing depends on \code{\link{gdalUtils::mosaic_rasters}}
#' @author Loic Dutrieux, adaption by Johannes Eberenz
#' @param x Raster* object
#' @param minrows Integer
#' @param fun Function to be applied to the raster object.
#' @param mc.cores Integer Numvber of cores, see \code{\link{mclapply}}. 0 will call normal \code{\link{calc}}.
#' @param logfile Character Logfile for debugging parallel execution.
#' @param out_name Character fielname for output brick.
#' @param datatype Character  \code{\link{raster::dataType}} for output. Will be converted to gdal \code{ot}.
#' @param ... Arguments to be passed to \code{\link{raster::calc}}, if single core is used.
#' @details For further help, see \code{\link{calc}}. Warnings of the parallel package (see \code{\link{mclapply}} for instance) apply to this function.
#' @return a Raster* object, (always written to disk in multicore case!)
#' @author Johannes Eberenz, adapted from Loic Dutrieux
#' @seealso \code{\link{calc}}
#' @import raster
#' @import parallel
#' @import gdalUtils
#' @export
#'

mcCalc <- function(x, minrows=1, fun, mc.cores=1, logfile=NULL, out_name, datatype="FLT4S", of="ENVI", ...) {

  print(list(...))
  print(out_name)


  if(mc.cores == 0) { # Normal calc
    out <- calc(x=x, fun=fun, ...)
    return(out)
  } else {

    s <- blockSize(x, minblocks=mc.cores, minrows = minrows)
    blocs <- seq(1, s$n)


    # Create blocks and run the function on that bloc
    fun2 <- function(i) {

      # sleep a bit in the beginning, to desync cycles
      if (i <= mc.cores) {Sys.sleep(i * 30)}
      # check mem
      m_free <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=T))

      # get block extent
      e <- extent(x, r1=s$row[i], r2=s$row[i]+s$nrows[i]-1)
      tmp_out <- rasterTmpFile(prefix = sprintf("mcCalc_o_%04i_%07i_%07i_", i, s$row[i], r2=s$row[i]+s$nrows[i]-1))

      # logging:
      if(!is.null(logfile)){
        log_msg <- sprintf("\n%s: processing block %d of %d, mfree:%d, file:%s",
                           as.character(Sys.time()), i, s$n, m_free, tmp_out)
        cat(log_msg, file = logfile, append = T)
      }

      # cropping
      if (m_free < 2.5e+06){
        tmp_b <- rasterTmpFile(prefix = sprintf("mcCalc_b_%04i_", i))
        b <- crop(x, e, filename=tmp_b)
      } else {
        b <- crop(x, e)
      }

      # calc (needs dataype!)
      out <- calc(x=b, fun=fun, filename=tmp_out, datatype=datatype, format="GTiff")
      rm(b)
      gc()

      return(filename(out))
    }


    if(!is.null(logfile)) cat("\nStart processing...",  file = logfile, append = T)

    listOut <- mclapply(X=blocs, FUN=fun2, mc.cores=mc.cores)
    listOut <- unlist(listOut)

    if(!is.null(logfile)) {
      cat("\nlistOut n:",  length(listOut), "\n files:", file = logfile, append = T)
      cat("\n", listOut, file = logfile, append = T)
      cat("\nStart mosaic... ",  file = logfile, append = T)

      #cat(sapply(listOut, FUN = filename), file = logfile, append = T)
    }


    # gdalUtils mosaic
    b_metrics <- gdalUtils::mosaic_rasters(gdalfile=listOut, dst_dataset = out_name,
                                           output_Raster = T, verbose = T,
                                           of=of, ot=datatype2GDALot(datatype))

    return(b_metrics)
  }

}
