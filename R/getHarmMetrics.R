#' fits robust linear harmonic model to ts and retunrs the parameters
#'
#' @description Processes Proba-V data for subsequentent use in time-series analysis. Performs Proba-V cleaning and operations with parallel support.
#'
#' @param x Numeric or ts Time series
#' @param dates Dates oberservation times if no time series is supplied
#' @param QC_good Integer or Logical. If supplied, values of x that are not 1 or \code{TRUE} in QC_good are omitted. Must have same length as x and dd.
#' @param sig Numeric If not NA, coeficients with p-value > sig will be set to 0.
#' @param n_years Integer. If not \code{NULL}, additional coeff. for n intra-yearly variation.
#' @param lin_trend Logical. Should a linear trend be modelled?
#' @param order Numeric. First or second order harmonics
#' @param robust Logical Use robust regression from \code{\link{robustbase:lmrob}}?
#' @param probs Numeric. Vector of two probabilities to compute quantiles.
#' @param ... additional args. non implemented.
#'
#' @seealso \code{\link{removeDips}}, and \code{\link{robustbase::lmrob}}
#'
#' @return Numeric vector of the models coefficients and percentiles (length depending on arguments)
#'
#' @importFrom robustbase lmrob
#' @importFrom lubridate decimal_date
#'
#' @export
#'

getHarmMetrics <- function(x, dates=NULL, QC_good=NULL, sig=0.95, n_years=NA, lin_trend=T, order=c(1,2,3), robust=F, return_model=F, probs = c(0.01, 0.99), ...){

  # helper to eg p values from model objects
  lmp <- function (modelobject) {
    if (!inherits(modelobject, "lm")) stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- unname(pf(f[1],f[2],f[3],lower.tail=F))
    attributes(p) <- NULL
    return(p)
  }

  if(is.null(dates)){
    if (inherits(x, c("ts", "zoo"))){
      dd <- index(x)
    } else stop("Dates or ts must be given!")
  } else {
    if (inherits(dates, c("POSIXt", "Date"))) {
      dd <- lubridate::decimal_date(dates)
    } else if (is.numeric(dates)){
      dd <- dates
    } else stop("Dates must be given!")
  }
  x <- as.numeric(x)

  k <- (2*pi)

  if (!is.null(QC_good)) x[QC_good != 1] <- NA
  if (length(na.omit(x)) < 10) warning("Less than 10 samples!")

  co <- cos(dd*k)
  si <- sin(dd*k)

  if(order ==1) {
    form <- x ~ (co + si)
  } else {
    co2 <- cos(2*dd*k)
    si2 <- sin(2*dd*k)
    if (order == 2){
      form <- x ~  co + si + co2 + si2
    } else if (order ==3){
      co3 <- cos(3*dd*k)
      si3 <- sin(3*dd*k)
      form <- (x ~ ( co + si + co2 + si2 + co3 + si3))
    } else stop("order must be between 1 or and 3")
  }

  if(!is.na(n_years)){
    co0 <- cos(dd * k / n_years)
    si0 <- sin(dd * k / n_years)
    form <- update.formula(form, ~ . + co0 + si0)
  }

  if(lin_trend){
    trend <- dd
    form <- update.formula(form, ~ . + trend)
  }

  if (robust) {
    lmh <- robustbase::lmrob(form, setting = "KS2014")
  } else {
    lmh <- lm(form)
  }
  print(summary(lmh))
  if (return_model) return(lmh)

  metrics <- c(quantile(x, probs = c(0.01, 0.99), na.rm = T), lmh$coefficients)

  if (!is.na(sig)) {
    p_values <- anova(lmh)$"Pr(>F)"[1:(length(lmh$coefficients)-1)]
    metrics[-c(1:3)][p_values > sig] <- 0
  }
  names(metrics) <- c("min", "max", "intercept", names(lmh$coefficients)[-1])
  return(metrics)
}

