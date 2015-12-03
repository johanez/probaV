#' @title Smooth time series using loess
#' 
#' @description Temporal outliers are marked or removed based on their distanced from a smoothing model derived with \code{\link{loess}}.
#' 
#' @param tsx ts or numeric vector. Time series data.
#' @param QC_good Integer or Logical vector. Optional time series of ts quality: 0=bad, 1=good  
#' @param dates Dates or Numeric. Vector of observation times. Overwrites index of tsx.
#' @param threshold Numeric vector of length 2. Upper and lower threshold for non-outlier distances from smoothed time series. 
#' @param res_type Character. Determins the returned object , see \link[=dest]{details}.
#' @param ... additional arguments to \code{\link{loess}}
#'
#' @return a ts, numeric vector or data.frame, see \link[=dest]{details}
#' @details 
#' "distance" gives the distance, "sd_distance" the distance devided by the sd,  
#' "filled", a ts with outleirs replaced by smoothed values, "omit" the input ts 
#' with outliers removed,  
#' "QC" the status of observations: 0=Missing/band input QC, 1=good, 2=temporal outlier. 
#' "all" returns a data.frame with all these vectors.
#' @export
#'
#' @examples

smoothLoess <- function(tsx, QC_good=NULL, dates=NULL, threshold=c(-50, Inf), res_type=c("distance", "sd_distance", "all", "filled", "omit", "QC"), ...) {
  if(is.null(QC_good)) {
    QC_good <- as.numeric(!is.na(tsx))
  } else {
    QC_good <- as.numeric(QC_good)
  }
  
  x <- as.numeric(tsx)
  x[QC_good==0] <- NA
  
  if (is.null(dates)){
    dates <- index(tsx) 
  }
  dates <- as.numeric(dates)
  loe <-  loess(formula = x ~ dates, na.action = "na.omit", ...)
  #loe <-  loess(formula = x ~ dates, na.action = "na.omit")#, span=0.3)
  loe_pred <- predict(loe, dates)
#   if (class(x)=="integer") {
#     loe_pred <- round(loe_pred, 0)
#   }
  
  distance <-  (loe_pred - x)

  if (!is.null(threshold)){
    QC_good[distance < threshold[1] & !is.na(distance)] <- 2
    QC_good[distance > threshold[2] & !is.na(distance)] <- 2
  }
  # prepare output
  if(class(tsx)=="zoo") {
    
    tsx <- zoo(cbind(x = as.numeric(tsx), QC_good, filled=loe_pred), index(tsx))
    # names(tsx) <- c(name_x, "QC_good", paste0(name_x, "_filled"))
    return(tsx)
    
  } else {
    x_omit <- x
    x_omit[QC_good != 1] <- NA
    res <- switch(res_type,
                  all = data.frame( x=as.numeric(tsx), QC_good=QC_good, filled=loe_pred, distance=round(distance)),
                  filled = loe_pred,
                  omit = x_omit,
                  QC = QC_good,
                  distance = distance,
                  sd_distance = (distance / sd(x, na.rm = T)))
    return(res)
  }
}


#plot(dates,loe_pred, type='l')
#points(dates, x)
