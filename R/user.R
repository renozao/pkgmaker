# Project: pkgmaker
# 
# Author: renaud
# Created: Jul 2, 2014
###############################################################################

#' @include packages.R
NULL

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
#' @param default default response in non-interactive mode.
#' 
#' If \code{NA}, then the user is forced to provide an answer, even in non-interactive mode 
#' (e.g., when run through \code{Rscript}).  
#' 
#' @return the character string typed/agreed by the user or directly the default answer in 
#' non-interactive mode.  
#' 
#' @export
askUser <- function (msg, allowed = c("y", "n"), idefault = "n", default = "n", case.sensitive = FALSE) 
{
    ucon <- stdin()
    if ( !interactive() ) {
        if( is_NA(default) ) ucon <- 'stdin'
        else return(default)
    }
    
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
            ans <- readLines(ucon, n = 1)
        else ans <- tolower(readLines(ucon, n = 1))
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
userData <- function(..., create=NULL, package = topenv(parent.frame())){
        
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

#' Require a Package with User Interaction
#'
#' Like base \code{\link{require}}, \code{irequire} tries to find and load a package, 
#' but in an interactive way, i.e. offering the user to install it if not found.
#' 
#' @param package name of the package
#' @param lib path to the directory (library) where the package is to be
#' looked for and installed if agreed by the user.
#' @param ... extra arguments passed to \code{\link{install.packages}}.
#' @param load a logical that indicates if the package should be loaded, 
#' possibly after installation.
#' @param msg message to display in case the package is not found when first 
#' trying to load/find it.
#' This message is appended to the string \dQuote{Package '<packagename>' is required}.
#' @param quiet logical that indicates if loading a package should be done quietly 
#' with \code{\link{require.quiet}} or normally with \code{\link{require}}.
#' @param prependLF logical that indicates if the message should start at a new line.
#' @param ptype type of package: from CRAN-like repositories, Bioconductor, Bioconductor software, Bioconductor annotation.
#' Bioconductor packages are installed using \code{\link[repotools]{install.pkgs}} from the 
#' \pkg{repotools} package.
#' @param autoinstall logical that indicates if missing packages should just be installed 
#' without asking with the user, which is the default in non-interactive sessions.
#' 
#' @return \code{TRUE} if the package was successfully loaded/found (installed), 
#' \code{FALSE} otherwise.  
#'  
#' @family require
#' @export
irequire <- function(package, lib=NULL, ..., load=TRUE, msg=NULL, quiet=TRUE, prependLF=FALSE
        , ptype=c('CRAN-like', 'BioC', 'BioCsoft', 'BioCann')
        , autoinstall = !interactive() ){
    
    .reqpkg <- if( quiet ) qrequire else{
                if( prependLF ) message()
                require
            }
    reqpkg <- function(...){
        .reqpkg(..., lib=lib, character.only=TRUE)
    }
    
    # vectorized version
    if( length(package) >1L ){
        return( all(sapply(package, irequire, lib = lib, ...
                                , load = load, msg = msg, quiet = quiet
                                , prependLF = prependLF, autoinstall = autoinstall)) )
    }
    # try loading it
    if( load && reqpkg(package) ) return( TRUE )
    # try finding it without loading
    else if( length(find.package(package, lib.loc=lib, quiet=TRUE)) ) return( TRUE )
    
    # package was not found: ask to install
    msg <- paste0("Package '", package, "' is required",
            if( is.null(msg) ) '.' else msg)
    
    # stop if not auto-install and not interactive
    if( !interactive() && !autoinstall ) stop(msg)
    
    # non-interactive mode: force CRAN mirror if not already set
    if( !interactive() && length(iCRAN <- grep("@CRAN@", getOption('repos'))) ){
        repos <- getOption('repos')
        repos[iCRAN] <- 'http://cran.rstudio.com'
        op <- options(repos = repos)
        on.exit(options(op), add = TRUE)
    }
    
    # detect annotation packages
    if( missing(ptype) && grepl("\\.db$", package) ) ptype <- 'BioCann'
    ptype <- match.arg(ptype)
    
    if( !autoinstall ){
        msg <- paste0(msg, "\nDo you want to install it from known repositories [", ptype, "]?\n"
                , " Package(s) will be installed in '", if(is.null(lib) ) .libPaths()[1L] else lib, "'")
        if( quiet && prependLF ) message()
        repeat{
            ans <- askUser(msg, allowed = c('y', 'n', r='(r)etry'), idefault='y', default = 'y')
            if( ans == 'n' ) return( FALSE )
            if( ans == 'y' ) break
            if( ans == 'r' && reqpkg(package) ) return(TRUE)
        }
    }
    
    ## install
    # check Bioconductor repositories
    hasRepo <- function(p){ any(grepl(p, getOption('repos'))) }
    install_type <- ptype
    if( ptype == 'CRAN-like' 
            || ( ptype == 'BioC' && hasRepo('/((bioc)|(data/annotation))/?$') )
            || ( ptype == 'BioCsoft' && hasRepo('/bioc/?$') )
            || ( ptype == 'BioCann' && hasRepo('/data/annotation/?$') )
            ){
        install_type <- 'CRAN'
    }
    
    if( install_type == 'CRAN' ){
        pkginstall <- install.packages
    }else{ # Bioconductor 
        # use enhanced installer from repotools
        if( !reqpkg('repotools') ){
            sourceURL("http://tx.technion.ac.il/~renaud/GRAN/repotools.R")
        }
        pkginstall <- ns_get('install.pkgs', 'repotools')
    }
    message()
    pkginstall(package, lib=lib, ...)
    #
    
    # try  reloading
    if( load ) reqpkg(package)
    else length(find.package(package, lib.loc=lib, quiet=TRUE))
}
