# TODO: Add comment
# 
# Author: renaud
###############################################################################

.getExtraEnv <- function(package){
	if( missing(package) || is.null(package) ) where <- topns(FALSE)
	else if( isString(package) ) {
		package <- sub("^package:", "", package)
		if( package == 'R_GlobalEnv') where <- .GlobalEnv
		else where <- asNamespace(package)
	}
	else stop("Invalid argument `package`: must be missing or a package name.")
	where
}

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
#' @return the runner function associated with the newly registered handler,
#' as built by \code{packageExtraRunner}.  
#'  
#' @rdname packageExtra
#' @export
packageExtraHandler <- function(handler, fun, package=NULL){
	
	# load handler registry
	where <- .getExtraEnv(package=package)
	handlers <- simpleRegistry('.__extraHandlers__', envir=where)
	
	# return whole registry if no name is provided
	if( missing(handler) ) return( handlers )
	# return handler function if handler is missing
	if( missing(fun) ) return( handlers$get(handler) )
	
	# set handler in package internal registry
	handlers$set(handler, fun)
	# wrap setter for recalling postponing action 
	f <- function(...){
		handlers <- packageExtraHandler(package='pkgmaker')
		handlers$set(handler, fun)
	}
	f()
	# build associated runner
	runner <- packageExtraRunner(handler)
	# set postpone action to register the handler in global pkgmaker registry
	if( isLoadingNamespace(nodev=TRUE) && !isLoadingNamespace('pkgmaker') ){
		ns_name <- getLoadingNamespace()
		postponeAction(f, str_c(ns_name, ':', handler), group='extraHandlers')
	}
	invisible(runner)
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
	where <- .getExtraEnv(package=package)
	extras <- simpleRegistry('.__extraArguments__', envir=where)
	
	args <- list(...)
	# return whole registry if no other argument is provided
	if( missing(handler) ) return( extras )
	if( is.null(handler) ) return( extras$names() )
	# return handler function if handler is missing
	doSet <- TRUE
	if( grepl(":", handler) ){
		if( is.null(extra) ){
			doSet <- FALSE
			extra <- sub("^[^:]+:", "", handler)
			handler <- sub("^([^:]+):.*", "\\1", handler)
		}
	}
	if( is.null(extra) ){
		if( length(args)){
			stop("Missing argument `extra`: a name must be specified when registering an extra.")
		}
		return( grep(str_c("^", handler), extras$names(), value=TRUE) )
	}
	
	# build key
	key <- str_c(handler, ':', extra)
	if( .wrap || !doSet ){ # retrieve registered arguments or action function
		args <- extras$get(key)
		if( .wrap ){
			fhandler <- packageExtraHandler(handler, package='pkgmaker')
			if( is.null(fhandler) ){
				handlers <- packageExtraHandler(package='pkgmaker')
				stop("Could not find action handler '", handler, "' in pkgmaker global handler registry.\n"
					, "  Available handlers are: ", str_out(handlers$names(), Inf))
			}
			# define wrapper function
			f <- function(...){
				cl <- match.call()
				cl[[1L]] <- as.name('fhandler')
				# add default arguments
				lapply(names(args), function(a){
					if( !a %in% names(cl) )
						cl[[a]] <<- as.name(substitute(a, list(a=a)))
				})
				eval(cl)
			}
			# set registered arguments as default arguments
			formals(f) <- c(args, formals(f))
			f
		}else args
		
	}else{ # set the arguments in the package internal registry
		message("Registering extra action '", key, "' in ", environmentName(where), ' ... ', appendLF=FALSE)
		extras$set(key, args)
		message('OK')
	}
}
#' \code{packageExtraRunner} defines a function to run all or some of the actions registered 
#' for a given handler in a given package.
#' For example, the function \code{install.extrapackages} is the runner defined for the extra handler \code{'install'} 
#' via \code{packageExtraRunner('install')}.
#' 
#' @param .verbose logical that indicates if verbose messages about the extra actions being
#' run should be displayed.
#' 
#' @rdname packageExtra
#' @export
packageExtraRunner <- function(handler){
	
	function(package, extra=NULL, ..., .verbose=getOption('verbose')){
	
		.local <- function(p, ...){
			# load package namespace
			extras <- packageExtra(handler=handler, package=p)
			if( !is.null(extra) )
				extras <- extras[extras %in% extra]
			# execute extras
			sapply(extras, 
				function(e, ...){
					f <- packageExtra(e, package=p, .wrap=TRUE)
					if( .verbose ){
						message("# Running extra action '", e, "' ...")
						message("# Action: ", str_fun(f))
						on.exit( message("# ERROR [", e, "]\n") )
					}
					res <- f(...)
					if( .verbose ){
						on.exit()
						message("# OK [", e, "]\n")
					}
					res
				}
			, ...)
		}
		invisible(sapply(package, .local, ...))
	}
}

#' \code{install.extras} runs all extra actions registered for a given package.
#' 
#' @rdname packageExtra
#' @export
install.extras <- packageExtraRunner(NULL)
#' \code{install.extrapackages} runs all extra \code{'install'} actions, 
#' i.e. those registered for handler \code{'install'}.
#' 
#' @rdname packageExtra
#' @export
install.extrapackages <- packageExtraHandler('install', install.packages)
