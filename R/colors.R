# Color utilities
# 
# Author: Renaud Gaujoux
# Created: 30 Nov 2012
###############################################################################

#' Colour utilities
#' 
#' \code{alphacol} adds an alpha value to a colour specification and convert to 
#' a hexadecimal colour string.
#'  
#' @inheritParams grDevices::col2rgb
#' 
#' @return a character vector of color specifications.
#' @export
#' @examples
#' 
#' # Alphas
#' alphacol('red') # do nothing
#' alphacol('red', 10)
#' alphacol('#aabbcc', 5)
#' alphacol(4, 5)
#' 
alphacol <- function(col, alpha = FALSE){
    col <- as.hexmode(col2rgb(col, alpha))
    storage.mode(col) <- "character"
    if( !is.logical(alpha) ){
        if( alpha < 1 ) alpha <- alpha * 255
        alpha <- round(alpha)
        col['alpha', ] <- as.character(as.hexmode(alpha))
    }
	apply(col, 2, function(x) paste("#", paste(x, collapse=''), sep=''))
}


