#' Title
#'
#' @param x RasterBrick.
#' @param QC_band 
#' @param yday Numeric. See getHarmMetrics
#' @param n_years 
#' @param order 
#' @param robust 
#' @param threshold 
#' @param span 
#' @param mc.cores 
#' @param ... 
#'
#' @return
#' @import bfastSpatial
#'
#' @examples
#' 
#' 
mapDistance2Loess <- function(x, QC_band=NULL, span=0.3,  res_type=c("distance", "sd_distance"), mc.cores=1, ...) {

  s_info <- getProbaVinfo(names(x))
  bands <- s_info[s_info$date == s_info$date[1], 'band']
  dates <- s_info[s_info$band == bands[1], 'date']
  #ydays <- s_info[s_info$band == bands[1], 'yday']
  if (nlayers(x) != length(bands) * length(dates)) {
    stop("Stack layers don't fit meta data in layer names!")
  }


  fun <- function(x){
    # smooth loess and getHarmMetrics
    m <- matrix(x, nrow= length(bands), ncol=length(dates))
    if (!is.null(QC_band)){
      qc <- m[QC_band,]
      qc <- qc %in% getProbaVQClist()$clear_all
      m <- m[-QC_band,]
    } else qc <- NULL
    
    if (!all(is.na(m[1,]))) {
      res <- try({
        d <- apply(m, MARGIN = 1, smoothLoess, QC_good=qc, dates = dates, res_type = res_type, span=span, threshold=NULL)
        as.vector(t(d))
      })
      if(class(res) == 'try-error') {
        res <- rep(NA, length(x))
      }
    } else {
      res <- rep(NA, length(x))
    }
    # no names, because they get lsot in mc.calc anyway
    ### names(res) <- paste0(rep(bands, nrow(coefs)), "_", rownames(coefs))
    return(res)
  }
  
  out <- mc.calc(x=x, fun=fun, mc.cores=mc.cores, ...)

  return(out)
}
