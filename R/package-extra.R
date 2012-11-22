# TODO: Add comment
# 
# Author: renaud
###############################################################################

#' Install/Run Extra Things After Standard Package Installation
#' 
#' These functions define a framework to register actions for which default sets of arguments
#' can be defined when (lazy-)loading a package, and run later on, e.g., after the package 
#' is installed using dedicated commands.
#' 
#' \code{packageExtraHandler} defines main action handler functions, for which 
#' default arguments are registered using \code{packageExtra}. 
#' 
#' @details
#' For example, calling \code{packageExtra('install', pkgs='non_CRAN_pkg', repos='http://non-standard-repo')}
#' in a source file of package 'myPkg' registers the call 
#' \code{install.packages('non_CRAN_pkg', repos='http://non-standard-repo', ...)}
#' in a registry internal to the package. 
#' All calls to \code{packageExtra('install', ...)} can then be run by the user, as
#' a post installation step via \code{install.extra('myPkg', ..)}.
#' 
#' @param handler name of a handler, e.g, \code{'install'}.
#' It must be unique across all handlers registered by any other packages.  
#' @param fun handler function that will be called with the arguments registered
#' with \code{packageExtra(name, ...)}
#' @param package package name where to store/look for the internal registries.
#' End users should not need to use this argument.
#'  
#' @rdname packageExtra
#' @export
packageExtraHandler <- function(handler, fun, package=NULL){
	
	# load handler registry
	if( missing(package) || is.null(package) ) where <- topns(FALSE)
	else if( is.character(package) ) where <- asNamespace(package)
	else stop("Invalid argument `package`: must be missing or a package name.")
	handlers <- simpleRegistry('.__extraHandlers__', envir=where)
	
	# return whole registry if no name is provided
	if( missing(handler) ) return( handlers )
	# return handler function if handler is missing
	if( missing(fun) ) return( handlers$get(handler) )
	
	# set handler in package internal registry
	handlers$set(handler, fun)
	# wrap setter for postponing action 
	f <- function(...){
		handlers <- packageExtraHandler(package='pkgmaker')
		handlers$set(handler, fun)
	}
	f()
	
	# do nothing if
	# - not loading a real namespace
	# - loading the pkgmaker namespace
	if( !isLoadingNamespace(nodev=TRUE) || isLoadingNamespace('pkgmaker') ){
		return()
	}
	ns_name <- getLoadingNamespace()
	# postpone registration
	postponeAction(f, str_c(ns_name, ':', handler), group='extraHandlers')
}
#' \code{packageExtra} registers extra actions for a given handler.
#' 
#' @param extra name of the extra action. 
#' When registering an action with \code{}, it should be unique within the package, 
#' or it will overwrite a previously registered action.
#' @param ... extra arguments passed to the actual call to the handler.
#' In \code{packageExtra}, these define default arguments for the call, which can 
#' be overwritten by arguments passed to the runner function. 
#' @param .wrap logical that indicates if a function that runs the extra action should
#' be returned or only the default arguments
#' 
#' @rdname packageExtra
#' @export
packageExtra <- function(handler, extra=NULL, ..., package=NULL, .wrap=FALSE){
	
	# load extra registry
	if( missing(package) || is.null(package) ) where <- topns(FALSE)
	else if( is.character(package) ) where <- asNamespace(package)
	else stop("Invalid argument `package`: must be missing or a package name.")
	extras <- simpleRegistry('.__extraArguments__', envir=where)
	
	args <- list(...)
	# return whole registry if no other argument is provided
	if( missing(handler) && !length(args) ) return( extras )
	# return handler function if handler is missing
	doSet <- TRUE 
	if( grepl(":", handler) ){
		if( is.null(extra) ){
			doSet <- FALSE
			extra <- sub("^[^:]+:", "", handler)
			handler <- sub("^([^:]+):.*", "\\1", handler)
		}
	}
	if( is.null(extra) ) return( grep(str_c("^", handler), extras$names(), value=TRUE) )
	
	key <- str_c(handler, ':', extra)
	if( !doSet ){
		args <- extras$get(key)
		if( .wrap ){
			f <- function(...){
				fhandler <- packageExtraHandler(handler, package='pkgmaker')
				if( is.null(fhandler) )
					stop("Could not find action handler '", handler, "'")
				args <- expand_list(list(...), args, .exact=FALSE)
				print(args)
				do.call(fhandler, args)
			}
			f
		}else args
		
	}else{ # set the arguments in the package internal registry
		if( !isRunningPostponedAction() )
			message("# Adding extra action '", key, "' in ", environmentName(where))
		extras$set(key, args)
	}
}
#' \code{packageExtraRunner} defines a function to run all or some of the actions registered 
#' for a given handler in a given package.
#' For example, \code{install.extras} is the runner defined for the extra handler \code{'install'} 
#' via \code{packageExtraRunner('install')}.
#' 
#' @rdname packageExtra
#' @export
packageExtraRunner <- function(handler){
	
	function(package, extra=NULL, ...){
	
		.local <- function(p, ...){
			# load package namespace
			extras <- packageExtra(handler, package=p)
			if( !is.null(extra) )
				extras <- extras[extras %in% extra]
			
			# execute extras
			sapply(extras, 
				function(e, ...){
					f <- packageExtra(e, extra=NULL, package=p, .wrap=TRUE)
					f(...)
				}
			, ...)
		}
		invisible(sapply(package, .local, ...))
	}
}

packageExtraHandler('install', install.packages)
#' \code{install.extras} runner for actions registered for the extra handler \code{'install'}.
#' 
#' @rdname packageExtra
#' @export
install.extras <- packageExtraRunner('install')
