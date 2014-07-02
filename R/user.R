# Project: pkgmaker
# 
# Author: renaud
# Created: Jul 2, 2014
###############################################################################

#' User Queries
#' 
#' This function is an improved version of \code{\link[Biobase]{userQuery}} and ask 
#' the user about some task that needs her intervention to proceed, 
#' e.g., ask if one should perform a computation, install a package, etc.. 
#' 
#' @inheritParams Biobase::userQuery
#' @param idefault default response in interactive mode.
#' This answer will be in upper case in the question and will be the one returned if the 
#' user simply hits return.
#' 
#' @return the character string typed/agreed by the user or directly the default answer in 
#' non-interactive mode.  
#' 
#' @export
askUser <- function (msg, allowed = c("y", "n"), idefault = "n", default = "n", case.sensitive = FALSE) 
{
    if ( !interactive() )  return(default)
    fallowed <- allowed
    # add extra info on answer options
    if( !is.null(nm <- names(allowed)) ){
        allowed[nm != ''] <- nm[nm != '']  
    }
    if( !isFALSE(idefault) )
        fallowed[fallowed == idefault] <- toupper(idefault)
    repeat {
        allowMsg <- paste("[", paste(fallowed, collapse = "/"), 
                "]: ", sep = "")
        outMsg <- paste(msg, allowMsg)
        cat("\n", outMsg, sep='')
        if (case.sensitive) 
            ans <- readLines(n = 1)
        else ans <- tolower(readLines(n = 1))
        if( !isFALSE(idefault) && !nchar(ans) ) 
            ans <- idefault
        if (ans %in% allowed) 
            break
        else cat(paste(ans, "is not a valid response, try again.\n"))
    }
    # return answer
    ans
}


#' User Data Directory
#'  
#' \code{userData} returns the path to a local directory/file where package-related user data can be stored.
#' Note that a base directory is \strong{always} created if necessary (see details).
#' 
#' The package-specific user data base directory is the sub-directory \emph{R-data/}, 
#' located in the user's home.   
#'  
#' If in interactive mode, and the base directory does not exist yet, 
#' the user is asked if it should be created in his home directory. 
#' Otherwise, or if the user does not allow the creation in his home, this directory is created 
#' in the current R session's temporary directory.
#' 
#' @param ... path parts passed to \code{\link{file.path}} to be appended to 
#' the main path.
#' @param create logical that indicates if the \strong{base} directory should be 
#' created if it does not exists.
#'  
#' Note that directories -- and files -- under the base directory are not automatically 
#' created. The user should therefore care of it in the caller function if necessary.
#' 
#' If \code{create=TRUE}, then the base directory is forced to be created in the user's home directory.  
#' If \code{create=FALSE}, then the base directory is never created.
#' 
#' See also section \emph{Details}.
#'   
#' @param package name of the package associated with the user data path.
#' It is used to prefix the path, within the user R data directory. 
#' 
#' @seealso \code{\link{tempdir}}
#' @export
userData <- local({
    function(..., create=NULL, package = topenv(parent.frame())){
        
        if( is.environment(package) ) package <- utils::packageName(package)
        
        p <- file.path(Sys.getenv('HOME'), 'R-data', package)
        
        # ask the user about creating the directory
        if( !file.exists(p) && (is.null(create) || isTRUE(create))  ){
            if( is.null(create) ){
                ans <- askUser(str_c("The ", package, " user data directory '", p, "' doen't exist. Do you want to create it?")
                    	, idefault='y', default='n')
                if( ans == 'n' ){
                    p <- file.path(tempdir(), 'R-data', package)
                }
            }
            
            if( !file.exists(p) ){
                message("Creating user data directory '", p, "'")
                dir.create(p, recursive=TRUE)
            }
        }
        file.path(p, ...)
    }
})
