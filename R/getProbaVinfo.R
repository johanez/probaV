
#' @title Retrieve Proba-v info from filenames
#'
#' @description Parses through typical filenames and retrieves information on product, tile and acquisition date.
#'
#'
#' @param x Character. Directory of a Proba-V geotiffs or list of filenames.
#' @param patttern Character. As in \code{\link{list.files}}
#' @param tiles. Character. Which Proba-V tiles to include. Format: "X00Y00". \code{NULL} for all.
#' @param ... Additional arguments to pass to \code{\link{write.csv}}.
#'
#' @return A data.frame with tile meta data.
#'
#'

getProbaVinfo <- function(x, pattern, tiles=NULL, ...){
  if (!is.character(x)) {
    stop('x needs to be of class character')
  }
  if (length(x) ==1) {
    fpathlist <- list.files(x, pattern = pattern, recursive = T, include.dirs = F, no.. = T, full.names=F)
    flist <- basename(fpathlist)
    fsize <- file.size(file.path(x, fpathlist))

  } else {
    flist <- x # should check for pattern!
    fsize <- file.size(x)
  }

  product <- substr(flist, 8, 13)
  X <- as.numeric(substr(flist, 16, 17))
  Y <- as.numeric(substr(flist, 19, 20))
  tile <- substr(flist, 15, 20)
  date <- as.Date(substr(flist, 22, 29), format="%Y%m%d")
  year <- as.numeric(substr(flist, 22, 25))
  yday <- as.POSIXlt(date)$yday
  band <- substr(flist, 41, nchar(flist)-4)

  info <- data.frame(product = product, band = band, X = X, Y = Y, tile=tile,
                     date = date, year=year, yday=yday,
                     fpath=fpathlist, fsize=fsize,
                     stringsAsFactors = FALSE)
  row.names(info) <- flist

  # return only info for specific tile
  if(!is.null(tiles)){
    info <- info[info$tile %in% tiles, ]
  }

  # optional: print to .csv for future reference
  if(hasArg(file))
    write.csv(info, ...)

  return(info)

}
