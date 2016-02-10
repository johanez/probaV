#' Convert raster::dataType to GDAL ot
#' @description Helper function to use gdal command line tools.
#' @param datatype Character \code{\link{raster::dataType}}
#'
#' @return Character GDAL datatype.
#' @export
#' @details
#' Mapping: In:"LOG1S", "INT1S", "INT1U", "INT2S", "INT2U", "INT4S", "INT4U", "FLT4S", "FLT8S",
#' Out: "Byte", "Int16", "Byte", "Int16", "UInt16", "Int32", "UInt32", "Float32", "Float64"
#' @examples
#' datatype2GDALot("INT2S")

datatype2GDALot <- function(datatype){
  ots <-        c("Byte", "Int16", "Byte", "Int16", "UInt16", "Int32", "UInt32", "Float32", "Float64")
  names(ots) <- c("LOG1S", "INT1S", "INT1U", "INT2S", "INT2U", "INT4S", "INT4U", "FLT4S", "FLT8S")
  ot <- ots[datatype]
  if (is.na(ot)) ot <- "Float32"
  return(ot)
}
