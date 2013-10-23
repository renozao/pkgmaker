# Project: pkgmaker
# 
# Author: renaud gaujoux
# Created: Oct 23, 2013
###############################################################################


#' Knitr Extensions
#' 
#' \code{knit_ex} is a utility function for running small knitr examples, 
#' e.g., to illustrate functionnalities or issues. 
#' 
#' @param x text to knit as a character vector
#' @param ... arguments passed to \code{\link[knitr]{knit2html}} or \code{\link[knitr]{knit}}
#' @param quiet logical that indicates if knitting should be quiet (no progress bars etc..).
#' 
#' @export
#' @examples
#' 
#' library(knitr)
#' knit_ex("1 + 1")
#' 
knit_ex <- function(x, ..., quiet = TRUE){
    
    library(knitr)
    hk <- knit_hooks$get()
    saveRDS(hk, 'knit_hooks.rds')
    if( !(html_chunks <- any(grepl("```{", x, fixed = TRUE))) ){
        if( all(!grepl(">>=", x, fixed = TRUE)) ){
            x <- c("```{r}", x, "```")
            html_chunks <- TRUE   
        }
    }
    x <- paste0(x, collapse = "\n")
    if( any(html_chunks) ){
        res <- knit2html(text = x, ..., fragment.only = TRUE, quiet = quiet)
    }else{
        res <- knit(text = x, ..., quiet = quiet)
    }
    cat(res)
}

try_message <- function(expr){
    tryCatch(expr, error = function(e){ message(e); invisible()})
}

#' \code{hook_try} is a knitr hook to enable showing error 
#' messages thrown by \code{\link{try}}.
#' The function is not meant to be called directly, but only registered 
#' using \code{\link{knit_hooks}} (see details on this dedicated man page).
#' 
#' \code{hook_try} simply defines a function \code{try} in \code{envir} that prints 
#' the error message if any, and is called instead of base \code{\link{try}}. 
#' 
#' @param before logical that indicates when the hook is being called: 
#' before or after the chunk is processed.
#' @param options list of current knitr chunk options 
#' @param envir environment where the chunk is evaluated
#' 
#' @rdname knit_ex
#' @export
#' @examples
#' 
#' library(knitr)
#' 
#' # standard error message is caught
#' knit_ex("stop('ah ah')")
#' 
#' # with try the error is output on stderr but not caughted by knitr
#' knit_ex("try( stop('ah ah') )")
#' 
#' # no message caught
#' knit_ex("
#' ```{r, include = FALSE}
#' knit_hooks$set(try = pkgmaker::hook_try)
#' ```
#' 
#' ```{r, try=TRUE}
#' try( stop('ah ah') )
#' ```")
#' 
hook_try <- local({
    .try_defined <- FALSE
    function(before, options, envir){
    
        # remove hacked version of try
        if( !before ){
            if( .try_defined && exists('try', envir = envir, inherits = FALSE) ){
                remove(list = 'try', envir = envir)
            }
            .try_defined <<- FALSE
            return(invisible())
        }
        if( isTRUE(options$try) ){
        # define hacked version of try()
                .try <- try_message
                environment(.try) <- envir
                envir$try <- .try
                .try_defined <<- TRUE
        }
    }
})
