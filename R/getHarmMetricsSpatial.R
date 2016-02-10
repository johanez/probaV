#' Calculate time series metrics for a tile
#' @description Spatial version of \code{\link{getHarmMetrics}}, using \code{\link{mcCalc}}.
#' @param x RasterBrick or RasterStack Input time series. Layer names must follow proba_v pattern.
#' @param n_years Integer. See \code{\link{getHarmMetrics}}
#' @param order Integer. See \code{\link{getHarmMetrics}}
#' @param robust Logical. See \code{\link{getHarmMetrics}}
#' @param cf_bands Integer. Which bands to use for temporal cloud filter (See \code{\link{smoothLoess}})
#' @param thresholds threshold Numeric vector of length 2. See \code{\link{smoothLoess}}.
#' @param scale_f Numeric of length bands. Sclaing factor per band.
#' @param minrows Numeric. Min blcok size fore  \code{\link{mcCalc}}
#' @param mc.cores Numeric. Processign cores, see\code{\link{mcCalc}}
#' @param logfile Character. See \code{\link{mcCalc}}
#' @param ... additional arguments to \code{\link{mcCalc}}.
#'
#' @return rasterBrick with metrics as bands.
#' @export
#'

getHarmMetricsSpatial <- function(x, n_years=NULL, order=2, robust=FALSE,
                                  cf_bands, thresholds=c(-80, Inf, -120, 120) , span=0.3, scale_f=NULL, minrows=1, mc.cores=1, logfile, ...) {

  s_info <- getProbaVinfo(names(x))
  bands <- s_info[s_info$date == s_info$date[1], 'band']
  dates <- s_info[s_info$band == bands[1], 'date']
  ydays <- s_info[s_info$band == bands[1], 'yday']
  if (nlayers(x) != length(bands) * length(dates)) {
    stop("Inputstack nlayers doesn't fit meta data in layer names!")
  }
  thresholds <- matrix(thresholds, nrow=2)
  len_res <- (3 + (order*2)) * length(bands)
  cat("\nOutputlayers:", len_res, "\n")


  fun <- function(x){
    # smooth loess and getHarmMetrics
    m <- matrix(x, nrow= length(bands), ncol=length(dates))

    if (!all(is.na(m[1,]))) {
      res <- try({
        # smooth loess on all cf bands, then combine
        qc <- foreach(bn = 1:length(cf_bands), .combine='&') %do% {
          qcb <-   smoothLoess(m[cf_bands[bn],], dates = dates, threshold = thresholds[,bn],
                               res_type = "QC", span=span)
          qcb == 1
        }

        #get metrics
        coefs <- apply(m, 1, FUN=getHarmMetrics, yday=ydays, QC_good=qc, order=order, robust=robust)

        if (!is.null(scale_f)){
          # scaling
          res_1 <- as.integer(t(round((scale_f) * t(coefs))))
        } else res_1 <- c(coefs)

        if (length(res_1) != len_res) {
          res_1 <- rep(-9999, len_res)
        }
        res_1
      })

      if(class(res) == 'try-error') {
        res <- rep(NA_integer_, len_res)
      }

    } else {
      res <- rep(NA_integer_, len_res)
    }

    # no names, because they get lsot in mc.calc anyway
    return(res)
  }

  # use mcCalc ratehr than mc.calc (controll minrows)
  out <- mcCalc(x=x, fun=fun, minrows = minrows, mc.cores = mc.cores, logfile=logfile, ...)

  return(out)
}
