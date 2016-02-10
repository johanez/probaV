#' @title Build GDAL vrt from probav images
#'
#' @description
#' Stacks Proba-V layers into a GDAl virtual file,
#' using \code{\link{gdalbuildvrt}}. GDAL vrt was found to query faster
#' than \code{raster::stack}.
#'
#' @param x Character. Directory of a Proba-V geotiffs or list of well formated filenames.
#' @param patttern Character. As in \code{\link{list.files}}.
#' @param stacked_bands Character. Which abnds are stacked (typically RADIOMETRY)
#' @param vrt_name Character. Output filename.
#' @param orderChrono Logical. Wether to oder the stack chronologically.
#' Default \code{TRUE}.
#' @param tile Character. Which tile to process. Format: "X00Y00".
#' @param end_date Date. Last date to process.
#' @param retunr_raster Logical. Weather a RasterBrick should eb returned.
#' Else a info data.frame witht is returned (see \code{\link{getProbaVInfo}})
#'
#' @return a rasterBrick pointing to the vrt or a data.frame with meta data.
#' @details
#' If stacked bands are present, a nested vrt is build.
#'
#' @import raster
#' @import gdalUtils
#' @export
#'
#' @examples
#'
#'
timeVrtProbaV <- function(x, pattern, stacked_bands=NULL, vrt_name, order_chrono=TRUE, tile=NULL, end_date=NULL, return_raster=TRUE){

  df_info <- getProbaVinfo(x, pattern)
  if(order_chrono){
    df_info <- df_info[order(df_info$date),]
  }

  if (!is.null(tile)) df_info  <- df_info[df_info$tile==tile, ]
  if (!is.null(end_date)) df_info  <- df_info[as.numeric(df_info$date) <= end_date,]


  if(!is.null(stacked_bands))  {
    cat("\n Builng sub vrts, layers:", length( df_info$fpath[df_info$band %in% stacked_bands]), "\n")
    for (s in df_info$fpath[df_info$band %in% stacked_bands]){

      for (b in 1:4){
        s_out <- gsub(s, pattern = "RADIOMETRY", replacement = c("RED0", "NIR0", "BLUE", "SWIR")[b])
        gdalUtils::gdalbuildvrt(s, extension(s_out, "vrt"), b=b, overwrite = T, verbose=F)
      }

    }

    df_info <- getProbaVinfo(x, "(SM.tif$|NDVI.tif$|RED0|NIR0|BLUE|SWIR)")
    if (!is.null(tile)) df_info  <- df_info[df_info$tile==tile, ]
    if (!is.null(end_date)) df_info  <- df_info[as.numeric(df_info$date) <= end_date,]
  }

  cat("\n building main vrt, layers:", nrow(df_info))
  gdalUtils::gdalbuildvrt(df_info$fpath, vrt_name, separate = T, overwrite = T, verbose=F)


  if(return_raster) {
    b <- brick(vrt_name)
    names(b) <- rownames(df_info)
    b <- setZ(x=b, z=format(df_info$date, "%Y%j"))
    return(b)
  }

  return(df_info)
}
