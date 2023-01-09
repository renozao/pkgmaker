# Data related functions 
# 
# Author: Renaud Gaujoux
# Creation: 18 Jun 2012
###############################################################################

#' Generating Names
#' 
#' Generates names or dimnames for objects.
#' 
#' @param x object whose names are generated.
#' 
#' @return the input object updated with names. 
#' @export
#' @rdname addnames
addnames <- function(x, ...){
	UseMethod('addnames')
}

#' @export
#' @rdname addnames
addnames.default <- function(x, ...){
	if( is.vector(x) ) addnames.vector(x, ...)
	else 
		stop("no applicable method for 'addnames' applied to an object of class ", class(x))
}

#' @param prefix prefix string to use. A vector can be used to specify a prefix 
#' for each dimension of \code{x}. 
#' Names are build as \code{<prefix><sep><index>}.
#' @param sep separator used between the prefix and the numeric index. 
#' @param ... extra arguments to allow extension and passed to the next method.
#' 
#' @export
#' @rdname addnames
addnames.vector <- function(x, prefix='x', sep='', ...){
	names(x) <- paste(prefix, 1:length(x), sep=sep) 
	x
} 


#' @export
#' @rdname addnames
addnames.array <- function(x, prefix=letters[1:length(dim(x))], sep='', ...){
	d <- dim(x)
	# recycle prefix if necessary
	if( length(prefix) != length(d) )
		prefix <- rep(prefix, length(d))
	
	dimnames(x) <- 
			lapply(seq_along(d), function(i) paste(prefix[i], 1:d[i], sep=sep))
	x
} 

#' @export
#' @rdname addnames
addnames.matrix <- function(x, prefix=c('row', 'col'), ...){
	addnames.array(x, prefix=prefix, ...)
}


#' Flattens All List Levels Using Separated Names
#' 
#' @param x a list object, usually containing other lists -- of lists.
#' @param sep character string used to separate each component of the final element names.
#' @param use.names logical that indicates if the original names of each the sucessive
#' nested list elements should be used to build the final names of the result list.
#' @param depth maximum number of levels to unlist. 
#' Root level is `1L`. 
#' 
#' @return a vector of the same type as the inner vector elements of the input list.
#' @export
#' @examples 
#' 
#' x <- list(X = list(a = 1
#'                    , b = list(b.1 = 2
#'                               , b.2 = list(b.2.1 = 4, b.2.2 = data.frame())
#'                               , b.3 = 3)
#'                    , c = matrix()))
#' unlist_with_sep(x)
#' unlist_with_sep(x, '###')
#' 
unlist_with_sep <- function(x, sep = '/', use.names = TRUE, depth = Inf){
  
  # early exit for non-list input
  if( !is.list(x) ) return(x)
  
  .local <- function(y, n = 1L){
    if( is.null(names(y)) || !use.names ) names(y) <- seq_along(y)
    if( anyDuplicated(names(y)) ){
      stop(sprintf("Invalid names at level %i: foudn duplucated names %s", n, str_out(unique(names(y)[duplicated(names(y))]), Inf)))
    }
    
    leaves <- c()
    res <- lapply(seq_along(y), function(i){
          m <- names(y)[i]
          e <- y[[i]]
          if( is.list(e) && !is.data.frame(e) && n < depth){
            e <- .local(e, n+1L)
            names(e) <- paste(m, names(e), sep = sep)
            
          }else{
            e <- y[i]
            leaves <<- c(leaves, i)
          }
          
          e
        })
    
#    # use original names for leaves
#    names(res) <- rep('', length(res))
#    names(res)[leaves] <- names(y)[leaves]
    
    # concatenate result
    do.call(c, res)
    
  }
  
  .local(x)
  
}
