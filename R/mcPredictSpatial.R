#' Precdict from raster Brick in parallel
#'
#' @param model Object of class \code{ranger}, see \link{\code{ranger::ranger}}.
#' Other models are currently not supported!
#' @param b_metrics RasterBrick of covariates
#' @param b_clumps RasterLayer (optional) with spatial segments
#' @param df_clumps data.frame (optional) with additional covarates linked to spatial segments.
#' @param type Character What should eb returned, "response" or "probs" (scaled from 0..254).
#' @param ranger_threads Integer. Numebr of cores threads for  \link{\code{ranger::predict.ranger}}
#' @param minrows Integer. See \code{\link{mcCalc}}.
#' @param mc.cores Integer. See \code{\link{mcCalc}}.
#' @param logfile Character, see \code{\link{mcCalc}}.
#' @param ... additional args to \code{\link{mcCalc}}.
#'
#' @return RasterLayer with predictions.
#' @export

mcPredictSpatial <- function(model, b_metrics, b_clumps=NULL, df_clumps=NULL, type=c("response", "probs"), ranger_threads=1, minrows=1, mc.cores=1, logfile, ...) {

  if(!is.null(b_clumps)){
    if(is.null(df_clumps)) stop("df_clumps missing!")
    s_metrics <- stack(b_metrics, b_clumps)
  } else {
    s_metrics <- b_metrics
  }

  n_metrics <- nlayers(b_metrics)

  # fucntion to pass to calc
  fun <- function(x){

    if(!is.null(b_clumps)){
      x <- cbind(x[,1:n_metrics], df_clumps[x[,ncol(x)],])
    }

    # Store nrow
    nrow_x <- nrow(x)
    # find cases where not everything is NA
    complete_r <- !is.na(x[,1])
    x <- x[complete_r,]
    # fill inbetween NAs
    x[is.na(x)] <- 0
    #cat(x[1:5,1:22], file = logfile, append = T)

    if (any(complete_r)) {

      res <- try({
        # precit using the supplied model
        pred <- rep(NA, nrow_x)
        if (type == 'response') {
          pred[complete_r] <- predict(model, data=x, num.threads= ranger_threads)$predictions
        } else if( type == 'probs' ){
          pred[complete_r] <- floor(predict(model, data=x, num.threads= ranger_threads)$predictions[,2] * 254)
        }
        cat(pred[1:10], file = logfile, append = T)
        pred
      })

      if(class(res) == 'try-error') {
        res <- rep(NA, nrow_x)
      }

    } else {
      res <- rep(NA, nrow_x)
    }
    return(res)
  }

  # use mcCalc ratehr than mc.calc (controll minrows)
  out <- mcCalc(s_metrics, fun=fun, minrows = minrows, mc.cores = mc.cores, logfile=logfile, ...)

  return(out)
}
