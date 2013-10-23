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


#' Knitr Hook for Handling Backspace Character
#' 
#' \code{hook_backspace} is a chunk hook that enables the use of backspace
#' characters in the output (e.g., as used in progress bars), and still 
#' obtain a final output as in the console.
#' 
#' @rdname knitr_ex
#' @export 
hook_backspace <- local({
    .output_fun <- NULL
    function(before, options, envir){
        # do nothing if the option is not ON
        if( !isTRUE(options$backspace) ) return()
        
        # set/unset hook
        if( before ){
            # store current output function
            if( is.null(.output_fun) ).output_fun <<- knit_hooks$get('output')
            
            # define extra output hook layer
            ohook <- function(x, options){
                        res <- .output_fun(x, options)
                        str_bs(res)
            }
            
            knit_hooks$set(output = ohook)
        }else{
            knit_hooks$set(output = .output_fun)
            .output_fun <<- NULL
        }
    }
})

str_bs <- function(x){
    s <- strsplit(x, "\b", fixed = TRUE)[[1]]
    if( !length(s) || !nzchar(s) ) return('')
    .nb <- 0L
    .s <- NULL
    fn <- function(s){
        if( !nzchar(s) ){
            if( !is.null(.s) ) .nb <<- .nb + 1L
        }else if( is.null(.s) ) .s <<- s
        else if( .nb ){
            .s <<- paste0(substr(.s, 1, nchar(.s) - .nb-1), s)
            .nb <<- 0L
        }
        NULL
    }
    sapply(s, fn)
    last <- tail(s, 1L)
    if( isTRUE(grepl("\b$", x)) ){
        .s <- substr(.s, 1, nchar(.s) - .nb-1)   
    } else if( length(s) == 1L ) return( .s )
    else if( .nb && nzchar(last) ) .s <- paste0(substr(.s, 1, nchar(.s) - .nb-1), last)
    .s
}

#' Code Toggle Feature for Markdown Documents
#' 
#' \code{md_toggleCode} outputs javascript code into .md documents so that R code chunks
#' are hidden and can be toggled in a click.
#' 
#' @rdname knit_ex
#' @export
md_toggleCode <- function(){
    cat(
"
<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js\"></script>
<script type=\"text/javascript\">
// toggle visibility of R source blocks in R Markdown output
function toggle_R(what) {
  var x = document.getElementsByClassName('r');
  if (x.length == 0) return;
  function toggle_vis(o) {
    var d = o.style.display;
    o.style.display = (d == 'block' || d == '') ? 'none':'block';
  }

  for (i = 0; i < x.length; i++) {
    var y = x[i];
    switch (y.tagName.toLowerCase()) {
      case 'pre':
        toggle_vis(y);
        if( what == 'setup' ){
            y.id = \"Rcode_\" + i;
        }
        break;
      case 'code':
        var z = y.parentNode;
        // pandoc uses the class 'sourceCode r' on both pre and code
        if (z.tagName.toLowerCase() == 'pre' && z.className != 'sourceCode r') {
          toggle_vis(z);
          if( what == 'setup' ){
              z.id = \"Rcode_\" + i;
              var newContent = $(\"<a href=\\\"\\\" onclick=\\\"$('#\" + z.id + \"').toggle(); return false;\\\">Show/Hide R code</a>\");
              newContent.insertBefore(z);
          }
        }
        break;
    }
  }
}

$( document ).ready(function(){
    toggle_R('setup');
});
</script>"
    )
}