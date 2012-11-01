# Development utility functions
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################

#' @include namespace.R
#' @include unitTests.R
#' @include logging.R
NULL

#' Executing R Commands
#' 
#' \code{R.exec} executes a single R command via \code{\link{system2}}.
#' 
#' @param ... extra arguments that are concatenated and appended to 
#' the command. 
#' 
#' @export
R.exec <- function(...){
	cmd <- paste(file.path(R.home(), 'bin', 'R'), ' ', ..., sep='', collapse='')
	message(cmd)
	system(cmd)
}

#' \code{R.CMD} executes R CMD commands.
#' 
#' @param cmd command to run, e.g. \sQuote{check} or \sQuote{INSTALL}.
#' 
#' @export
#' @rdname R.exec
R.CMD <- function(cmd, ...){
	R.exec('CMD ', cmd, ' ', ...)
}

#' \code{R.SHLIB} executes R CMD SHLIB commands.
#' 
#' @param libname name of the output compiled library
#' 
#' @export
#' @rdname R.exec
R.SHLIB <- function(libname, ...){
	R.CMD('SHLIB', '-o ', libname, .Platform$dynlib.ext, ...)
}

#' Quick Installation of a Source Package
#' 
#' Builds and install a minimal version of a package from its 
#' source directory.
#' 
#' @param path path to the package source directory
#' @param lib installation directory. Defaults to the user's R 
#' default installation directory. 
#' 
#' @export
#' 
quickinstall <- function(path, lib=NULL){
	
	npath <- normalizePath(path)
	nlib <- if( !is.null(lib) ) normalizePath(lib)
	pkg <- as.package(path)
	
	owd <- setwd(tempdir())
	on.exit( setwd(owd) )
	# build
	message("# Building package `", pkg$package, "` in '", getwd(), "'")
	R.CMD('build', '--no-manual --no-resave-data --no-vignettes ', npath)
	spkg <- paste(pkg$package, '_', pkg$version, '.tar.gz', sep='')
	if( !file.exists(spkg) ) stop('Error in building package `', pkg$package,'`')
	# install
	message("# Installing package `", pkg$package, "`", if( !is.null(lib) ) str_c("in '", nlib, "'"))
	R.CMD('INSTALL', if( !is.null(lib) ) paste('-l', nlib), ' --no-docs --no-multiarch --no-demo --with-keep.source ', spkg)
}

#' Compile Source Files from a Development Package
#' 
#' @param pkg the name of the package to compile
#' @param load a logical indicating whether the compiled library should be loaded
#' after the compilation (default) or not.
#' 
#' @return None
#' @export
compile_src <- function(pkg=NULL, load=TRUE){
	
	if( !is.null(pkg) ){
		library(devtools)
		p <- as.package(pkg)
		path <- p$path
	}else{
		pkg <- packageName()
		path <- packagePath(lib=NA) # do not look installed packages
	}
	
	owd <- getwd()
	on.exit(setwd(owd))
	
	# Compile code in /src
	srcdir <- file.path(path, 'src')
	message("# Checking '", srcdir, "' ... ", appendLF=FALSE)
	if( !file.exists(srcdir) ){
		message("NO")
	} else {
		message("YES")
		message("## Compiling '",srcdir,"' ##")
		setwd(srcdir)
		Sys.setenv(R_PACKAGE_DIR=path)
		R.SHLIB(pkg, " *.cpp ")
		message("## DONE")
		if( load ){
			if( existsFunction('load_dll', where='package:devtools') ){ # post 0.8
				f <- getFunction('load_dll', where='package:devtools')
				f(pkg)
			}else{ # prior 0.8
				f <- getFunction('load_c', where='package:devtools')
				f(pkg)
			}
		}
	}
}

#' Package Development Utilities
#' 
#' \code{packageEnv} is a slight modification from \code{\link{topenv}}, which 
#' returns the top environment, which in the case of development 
#' packages is the environment into which the source files are loaded by 
#' \code{\link[devtools]{load_all}}.
#' 
#' @param pkg package name. If missing the environment of the runtime caller 
#' package is returned.
#' @param skip a logical that indicates if the calling namespace should be 
#' skipped.
#' @param verbose logical that toggles verbosity
#' 
#' @rdname devutils
#' @return \code{packageEnv} returns an environment
#' @export
packageEnv <- function(pkg, skip=FALSE, verbose=FALSE){
	
	# return package namespace
	if( !missing(pkg) ){
		# - if the package is loaded: use asNamespace because as.environment does not
		# return a correct environment (don't know why)
		# - as.environment('package:*') will return the correct environment
		# in dev mode.
		env <- 
		if( !is.null(path.package(pkg, quiet=TRUE)) ) asNamespace(pkg)
		else if( isLoadingNamespace(pkg) ) getLoadingNamespace(env=TRUE)
		else if( pkg %in% search() ) as.environment(pkg)
		else as.environment(str_c('package:', pkg)) # dev mode
		return(env)
	}
	
	envir = parent.frame()
#	message("parent.frame: ", str_ns(envir))
	pkgmakerEnv <- topenv()
#	message("pkgmaker ns: ", str_ns(pkgmakerEnv))

	n <- 1
	skipEnv <- pkgmakerEnv
	while( identical(e <- topenv(envir), skipEnv) 
			&& !identical(e, emptyenv()) 
			&& !identical(e, .GlobalEnv) ){
		if( verbose > 1 ) print(e)
		n <- n + 1
		envir <- parent.frame(n)
	}
	
	if( !skip ){
		if( identical(e, .BaseNamespaceEnv) ){
			if( verbose ) message("packageEnv - Inferred ", str_ns(skipEnv))
			return( skipEnv )
		}
		if( verbose ) message("packageEnv - Detected ", str_ns(e))
		return(e)
	}
	if( verbose > 1 ) message("Skipping ", str_ns(skipEnv))
	# go up one extra namespace
	skipEnv <- e
	while( identical(e <- topenv(envir), skipEnv) 
			&& !identical(e, emptyenv()) 
			&& !identical(e, .GlobalEnv) ){
		if( verbose > 1 ) print(e)
		n <- n + 1
		envir <- parent.frame(n)
	}
	if( identical(e, .BaseNamespaceEnv) ){
		if( verbose ) message("packageEnv - Inferred ", str_ns(skipEnv))
		return( skipEnv )
	}
	if( verbose ) message("packageEnv - Detected ", str_ns(e))
	return(e)
}

#' \code{topns_name} returns the name of the runtime sequence of top namespace(s), 
#' i.e. the name of the top calling package(s), from top to bottom.
#' 
#' \code{topns_name}: the top namespace is is not necessarily the namespace where \code{topns_name} 
#' is effectively called.
#' This is useful for packages that define functions that need to access the 
#' calling namespace, even from calls nested into calls to another function from
#' the same package -- in which case \code{topenv} would not give the desired 
#' environment.   
#' 
#' @param n number of namespaces to return
#' @param strict a logicical that indicates if the global environment should 
#' be considered as a valid namespace.
#' @param unique logical that indicates if the result should be reduced
#' to contain only one occurence of each namespace. 
#'  
#' @rdname devutils
#' @export
topns_name <- function(n=1L, strict=TRUE, unique=TRUE){
	
	nf <- sys.nframe()
	i <- 0
	res <- character()
	while( i <= nf && length(res) < n ){
		e <- sys.frame(i)
		if( !strict || !identical(e, .GlobalEnv) ){
			pkg <- getPackageName(e, create=FALSE)
			if( pkg != '' ){
				res <- c(res, pkg)
			}
		}
		i <- i + 1
	}
	
	if( !length(res) ){# try with packageEnv
		e <- packageEnv(skip=TRUE)
		if( isNamespace(e) ){
			res <- getPackageName(e)
#			print(res)
		}else{
			warning('Could not find top namespace.', immediate.=TRUE)
			return('')
		}
	}
	
	if( unique || n==1L ) res <- match.fun('unique')(res)
	if( length(res) || n>1L ) res else ''
}

#' \code{topns} returns the runtime top namespace, i.e. the namespace of 
#' the top calling package, possibly skipping the namespace where \code{topns} 
#' is effectively called.
#' This is useful for packages that define functions that need to access the 
#' calling namespace, even from calls nested into calls to another function from
#' the same package -- in which case \code{topenv} would not give the desired 
#' environment.
#'  
#' @rdname devutils
#' @export
topns <- function(strict=TRUE){
	asNamespace(topns_name(n=1L, strict=strict))
	#packageEnv(skip=TRUE, verbose=verbose)
}

#' \code{packageName} returns the current package's name.
#' 
#' @param envir environment where to start looking for a package name.
#' The default is to use the \strong{runtime} calling package environment.
#' @param .Global a logical that indicates if calls from the global 
#' environment should throw an error (\code{FALSE}: default) or the string
#' \code{'R_GlobalEnv'}.
#' @param rm.prefix logical that indicates if an eventual prefix 'package:' 
#' should be removed from the returned string.
#' 
#' @export
#' @rdname devutils
#' @return a character string
packageName <- function(envir=packageEnv(), .Global=FALSE, rm.prefix=TRUE){
	
	if( is.null(envir) ) envir <- packageEnv() 
	if( is.character(envir) ){
		return( sub("^package:", "", envir) )
	}
	
	# retrieve package environment
	e <- envir
	
	# try with name from environment
	nm <- environmentName(e)
	if( identical(e, .GlobalEnv) && .Global ) return(nm)
	else if( isNamespace(e) || identical(e, baseenv()) ) return(nm)
	else if( grepl("^package:", nm) ){# should work for devtools packages
		if( rm.prefix ) 
			nm <- sub("^package:", "", nm)
		return(nm)
	}
	
	# try to find the name from the package's environment (namespace) 
	if( exists('.packageName', e) && .packageName != 'datasets' ){
		if( .packageName != '' )
			return(.packageName)
	}
	# get the info from the loadingNamespace
	info <- getLoadingNamespace(info=TRUE)
	if( !is.null(info) ) # check whether we are loading the namespace 
		info$pkgname
	else{# error
		stop("Could not reliably determine package name [", nm, "]")
	}
}

#' \code{str_ns} formats a package environment/namespace for log/info messages.
#' 
#' @rdname devutils
#' @export
str_ns <- function(envir=packageEnv()){
	if( !is.environment(envir) )
		stop("Invalid argument: must be an environment [", class(envir), ']')
	str_c(if( isNamespace(envir) ) 'namespace' else 'environment',
			" '", packageName(envir, rm.prefix=FALSE), "'")
}


#' \code{packagePath} returns the current package's root directory, which is 
#' its installation/loading directory in the case of an installed package, or
#' its source directory served by devtools. 
#' 
#' @param package optional name of an installed package 
#' @param lib path to a package library where to look. If \code{NA}, then only 
#' development packages are looked up.
#' @param ... arguments passed to \code{\link{file.path}}.
#' 
#' @rdname devutils
#' @return a character string
#' @export
packagePath <- function(..., package=NULL, lib=NULL){
	
	# try to find the path from the package's environment (namespace)
	pname <- packageName(package)
	# try installed package
	path <- if( !isNA(lib) ) system.file(package=pname, lib.loc=lib)		

	# somehow this fails when loading an installed package but is works 
	# when loading a package during the post-install check
	if( is.null(path) || path == '' ){
		# get the info from the loadingNamespace
		info <- getLoadingNamespace(info=TRUE)
		path <- 
			if( !is.null(info) ) # check whether we are loading the namespace 
				file.path(info$libname, info$pkgname)
			else{# we are in dev mode: use devtools
				library(devtools)
				p <- as.package(pname)
				
				# handle special sub-directories of the package's root directory
				dots <- list(...)
				Rdirs <- c('data', 'R', 'src', 'exec', 'tests', 'demo'
							, 'exec', 'libs', 'man', 'help', 'html'
							, 'Meta')
				if( length(dots) == 0L || sub("^/?([^/]+).*", "\\1", ..1) %in%  Rdirs)
					p$path
				else file.path(p$path,'inst')
				
			}
	}	
	stopifnot( !is.null(path) && path != '' )
	
	# add other part of the path
	file.path(path, ...)	
}

#' Tests if a package is installed
#' 
#' @param lib.loc path to a library of R packages where to search the package
#' 
#' @rdname devutils
#' @export
isPackageInstalled <- function(..., lib.loc=NULL){
	
	inst <- utils::installed.packages(lib.loc=lib.loc)
	pattern <- '^([a-zA-Z.]+)(_([0-9.]+)?)?$';
	res <- sapply(list(...), function(p){
				vers <- gsub(pattern, '\\3', p)
				print(vers)
				pkg <- gsub(pattern, '\\1', p)
				print(pkg)
				if( !(pkg %in% rownames(inst)) ) return(FALSE);
				p.desc <- inst[pkg,]
				if( (vers != '') && compareVersion(vers, p.desc['Version']) > 0 ) return(FALSE);
				TRUE
			})
	all(res)
}

#stripLatex <- function(x){
#	gsub("\\\\.\\{(.)\\}", "\\1", x)
#}

#' \code{as.package} is enhanced version of \code{\link[devtools]{as.package}}, 
#' that is not exported not to mask the original function.
#' It could eventually be incorporated into \code{devtools} itself.
#' Extra arguments in \code{...} are passed to \code{\link{find.package}}. 
#' 
#' @param x package specified by its installation/development path or its name
#' as \code{'package:*'}.
#' @param quiet a logical that indicate if an error should be thrown if a 
#' package is not found. It is also passed to \code{\link{find.package}}.
#' 
#' 
#' @rdname devutils
as.package <- function(x, ..., quiet=FALSE){
	
	# check for 'package:*'
	if( is.character(x) ){
		i <- grep('^package:', x)
		if( length(i) > 0L ){
			x[i] <- sapply(sub('^package:', '', x[i]), find.package, ..., quiet=quiet)
		}
	}
	res <- devtools::as.package(x)
	if( !devtools::is.package(res) ) return()
	res	
}

NotImplemented <- function(msg){
	stop("Not implemented - ", msg)
}

