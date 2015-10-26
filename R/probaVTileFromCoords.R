#' Proba-V tile from coordinates
#'
#' @description Get Proba-V tile from coordinates
#'
#' @param x Numeric. Longitude
#' @param y Numeric. Lattitude
#'
#' @return A character vector with the Proba-V tiles, format \code{"X00Y00"}
#'
#' @author J Eberenz
#'
#'
#' @examples
#'
#' probaVTileFromCoords(10, -5)
#'

probaVTileFromCoords <- function(x, y){

  X <- floor((x)/10) + 18
  Y <- 5 - floor((y-15)/10)

  if ((X >= 0 || X < 36) && (Y >= 0 || Y < 14)){
    sprintf("X%02dY%02d", X, Y)
  } else{
    NA
  }
}

