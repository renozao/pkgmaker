# Development utility functions
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################

#' @include utils.R
#' @include namespace.R
NULL

# taken from AnnotationDbi
make.name.tree <- function (x, recursive, what.names) 
{
  if (!is.character(what.names) || length(what.names) != 1) 
    stop("'what.names' must be a single string")
  what.names <- match.arg(what.names, c("inherited", "full"))
  .make.name.tree.rec <- function(x, parent_name, depth) {
    if (length(x) == 0) 
      return(character(0))
    x_names <- names(x)
    if (is.null(x_names)) 
      x_names <- rep.int(parent_name, length(x))
    else if (what.names == "full") 
      x_names <- paste0(parent_name, x_names)
    else x_names[x_names == ""] <- parent_name
    if (!is.list(x) || (!recursive && depth >= 1L)) 
      return(x_names)
    if (what.names == "full") 
      x_names <- paste0(x_names, ".")
    lapply(seq_len(length(x)), function(i) .make.name.tree.rec(x[[i]], 
              x_names[i], depth + 1L))
  }
  .make.name.tree.rec(x, "", 0L)
}

.unlist2 <- function (x, recursive = TRUE, use.names = TRUE, what.names = "inherited") 
{
  ans <- unlist(x, recursive, FALSE)
  if (!use.names) 
    return(ans)
  if (!is.character(what.names) || length(what.names) != 1) 
    stop("'what.names' must be a single string")
  what.names <- match.arg(what.names, c("inherited", "full"))
  names(ans) <- unlist(make.name.tree(x, recursive, what.names), 
      recursive, FALSE)
  ans
}
unlist2 <- ns_get('AnnotationDbi::unlist2') %||% .unlist2

set_libPaths <- function(lib.loc=NULL){
  ol <- Sys.getenv('R_LIBS')
  olib <- .libPaths()
  res <- list(R_LIBS=ol, .libPaths=olib)
  if( is_NA(lib.loc) ) return(res)
  
  # add lib path
  if( is.null(lib.loc) ) lib.loc <- .libPaths()
  if( is.character(lib.loc) ){
    # current .libPaths
    .libPaths(lib.loc)
    # R_LIBS env variable
    rlibs <- paste(lib.loc, collapse=.Platform$path.sep)
    Sys.setenv(R_LIBS=rlibs)
  }else if( is.list(lib.loc) ){
    Sys.setenv(R_LIBS=lib.loc$R_LIBS)
    .libPaths(lib.loc$.libPaths)
  }
  
  res
}

#' Executing R Commands
#' 
#' Functions to execute R commands.
#' @name R.exec
NULL

#' @describeIn R.exec executes a single R command via \code{\link{system2}}.
#' 
#' @param ... extra arguments that are concatenated and appended to 
#' the command. 
#' @param lib.loc logical that indicates if the current library locations
#' should be used.
#' If a character vector, then it is used as the library path specification.
#' 
#' @export
R.exec <- function(..., lib.loc=NULL){
	cmd <- paste(file.path(R.home('bin'), 'R'), ' ', ..., sep='', collapse='')
	# add lib path
	ol <- set_libPaths(lib.loc)
	on.exit(set_libPaths(ol))

	message(cmd)
	system(cmd, intern=interactive())
}

#' @describeIn R.exec executes R CMD commands.
#' 
#' @param cmd command to run, e.g. \sQuote{check} or \sQuote{INSTALL}.
#' 
#' @export
R.CMD <- function(cmd, ...){
	R.exec('CMD ', cmd, ' ', ...)
}

#' @describeIn R.exec executes R CMD SHLIB commands.
#' 
#' @param libname name of the output compiled library
#' 
#' @export
R.SHLIB <- function(libname, ...){
	R.CMD('SHLIB', '-o ', libname, .Platform$dynlib.ext, ...)
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
	if( !missing(pkg) && !is.null(pkg) ){
		# - if the package is loaded: use asNamespace because as.environment does not
		# return a correct environment (don't know why)
		# - as.environment('package:*') will return the correct environment
		# in dev mode.
		env <- if( is.environment(pkg) ) topenv(pkg)
        else if( isLoadingNamespace(pkg) ) getLoadingNamespace(env=TRUE)
		else if( !is.null(path.package(pkg, quiet=TRUE)) ) asNamespace(pkg)
		else if( isNamespaceLoaded(pkg) ) asNamespace(pkg)
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

#' @describeIn devutils returns the name of the runtime sequence of top namespace(s), 
#' i.e. the name of the top calling package(s), from top to bottom.
#' 
#' The top namespace is is not necessarily the namespace where \code{topns_name} 
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
#' @export
topns_name <- function(n=1L, strict=TRUE, unique=TRUE){
	
	if( n==1L && !is.null(ns <- getLoadingNamespace()) ){
		return(ns)
	}
	nf <- sys.nframe()
	i <- 0
	res <- character()
	while( i <= nf && length(res) < n ){
		e <- sys.frame(i)
		if( !strict || !identical(e, .GlobalEnv) ){
			pkg <- methods::getPackageName(e, create=FALSE)
			if( pkg != '' ){
				res <- c(res, pkg)
			}
		}
		i <- i + 1
	}
	
	if( !length(res) ){# try with packageEnv
		e <- packageEnv(skip=TRUE)
		if( isNamespace(e) ){
			res <- methods::getPackageName(e)
#			print(res)
		}else{
			#warning('Could not find top namespace.', immediate.=TRUE)
			return('')
		}
	}
	
	if( unique || n==1L ) res <- match.fun('unique')(res)
	if( length(res) || n>1L ) res else ''
}

#' @describeIn devutils returns the runtime top namespace, i.e. the namespace of 
#' the top calling package, possibly skipping the namespace where \code{topns} 
#' is effectively called.
#' This is useful for packages that define functions that need to access the 
#' calling namespace, even from calls nested into calls to another function from
#' the same package -- in which case \code{topenv} would not give the desired 
#' environment.
#'  
#' @export
topns <- function(strict=TRUE){
	ns <- topns_name(n=1L, strict=strict)
	if( ns == '.GlobalEnv' ) return( .GlobalEnv )
	else if( nchar(ns) ) asNamespace(ns)
	#packageEnv(skip=TRUE, verbose=verbose)
}

#' @describeIn devutils returns the current package's name.
#' It was made internal from version 0.16, since the package \pkg{utils} 
#' exported its own \code{\link[utils]{packageName}} function in R-3.0.0. 
#' 
#' @param envir environment where to start looking for a package name.
#' The default is to use the \strong{runtime} calling package environment.
#' @param .Global a logical that indicates if calls from the global 
#' environment should throw an error (\code{FALSE}: default) or the string
#' \code{'R_GlobalEnv'}.
#' @param rm.prefix logical that indicates if an eventual prefix 'package:' 
#' should be removed from the returned string.
#' 
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

#' @describeIn devutils formats a package environment/namespace for log/info messages.
#' 
#' @export
str_ns <- function(envir=packageEnv()){
	if( !is.environment(envir) )
		stop("Invalid argument: must be an environment [", class(envir), ']')
	str_c(if( isNamespace(envir) ) 'namespace' else 'environment',
			" '", packageName(envir, rm.prefix=FALSE), "'")
}


#' @describeIn devutils returns the current package's root directory, which is 
#' its installation/loading directory in the case of an installed package, or
#' its source directory served by devtools. 
#' 
#' @param package optional name of an installed package 
#' @param lib.loc path to a library of R packages where to search the package
#' @param ... arguments passed to \code{\link{file.path}}.
#' @param check logical that indicates if an error should be thrown if the path to the 
#' package root directory cannot be found. 
#' If this is the case and `check = FALSE`, then the function returns `NULL`. 
#' 
#' @return a character string
#' @export
packagePath <- function(..., package=NULL, lib.loc=NULL, check = TRUE){
	
	# try to find the path from the package's environment (namespace)
	pname <- packageName(package)
	
	# check if one is currently loading the namespace
	path <- NULL
	if( !is.null(info <- getLoadingNamespace(info=TRUE)) && info$pkgname == pname ){
		path <- info$path
	}else {
		# try loaded/installed package
		path <- find.package(package=pname, lib.loc=lib.loc, quiet=TRUE)		
	}
	# somehow this fails when loading an installed package but is works 
	# when loading a package during the post-install check
	if( !length(path) || path == '' ){
		# get the info from the loadingNamespace
		if( !is.null(info <- getLoadingNamespace(info=TRUE)) ){
			path <- info$path
		}
	}
  
  # check if the path was found
  if( !length(path) || !nzchar(path) ){
    if( check ) stop("Could not find path to package ", package)
    return(NULL)
  }
	
	# for development packages: add inst prefix if necessary
	if( isDevNamespace(pname) ){
		# handle special sub-directories of the package's root directory
		dots <- list(...)
		Rdirs <- c('data', 'R', 'src', 'exec', 'tests', 'demo'
				, 'exec', 'libs', 'man', 'help', 'html'
				, 'Meta')
		if( length(dots) && !sub("^/?([^/]+).*", "\\1", ..1) %in%  Rdirs)
			path <- file.path(path,'inst')
	}
	
	# add other part of the path
	file.path(path, ...)	
}

#' @describeIn devutils checks if a package is installed.
#' 
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

#' @describeIn devutils an enhanced version of \code{\link[devtools]{as.package}}, 
#' that is not exported not to mask the original function.
#' It could eventually be incorporated into \code{devtools} itself.
#' Extra arguments in \code{...} are passed to \code{\link{find.package}}. 
#' 
#' @param x package specified by its installation/development path or its name
#' as \code{'package:*'}.
#' @param quiet a logical that indicate if an error should be thrown if a 
#' package is not found. It is also passed to \code{\link{find.package}}.
#' @param extract logical that indicates if DESCRIPTION of package 
#' source files should be extracted.
#' In this case there will be no valid path. 
#' 
#' @export
as_package <- function(x, ..., quiet=FALSE, extract=FALSE){
	
	if( !requireNamespace('devtools', quietly = TRUE) ) 
        stop("Package 'devtools' is required to load development packages")
	
	if( is.null(x) ) return( devtools::as.package() )
	if( devtools::is.package(x) ) return(x)
	
	if( extract && grepl("\\.tar\\.gz$", x) ){ # source file
		# extract in tempdir
		tmp <- tempfile(x)
		on.exit( unlink(tmp, recursive=TRUE) )
		pkg <- basename(sub("_[0-9.]+\\.tar\\.gz$", '', x))
		desc <- file.path(pkg, 'DESCRIPTION')
		untar(x, desc, exdir=tmp)
		return(devtools::as.package(file.path(tmp, pkg)))
	} else { # check for 'package:*'
		if( grepl('^package:', x) ){
			libs <- .libPaths()
			pkg <- sub('^package:', '', x)
			p <- lapply(libs, find.package, package=pkg, quiet=TRUE, verbose=FALSE)
			p <- unlist(p[sapply(p, length)>0])
			if( !length(p) ){
				if( !quiet )
					stop("Could not find installed package ", pkg)
				return()
			}
			x <- p[1L]
		}
	}
	# try development packages
	res <- try(devtools::as.package(x), silent=TRUE)
	if( !is(res, 'try-error') )
		return(res)
	# try loaded or installed packages
	if( length(res <- find.package(package=x, quiet=TRUE)) )
		return(devtools::as.package(res))
	if( quiet )
		stop("Could not find package ", x)
	NULL
}

# internal overload of as.package
as.package <- as_package

NotImplemented <- function(msg){
	stop("Not implemented - ", msg)
}

#' Loading Package Data
#' 
#' Loads package data using \code{\link[utils]{data}}, but allows the user to avoid
#' NOTEs for a \sQuote{non visible binding variable} to be thrown when checking a package.
#' This is possible because this function returns the loaded data.
#'  
#' @param list character vector containing the names of the data to load.
#' @inheritParams utils::data
#' @param ... other arguments eventually passed to \code{\link[utils]{data}}.
#' @param options list of R options to set before calling \code{\link[utils]{data}}.
#' This may be useful the data is shipped as an R script.
#' @param stringsAsFactors logical that indicates if character columns of tabular data should be 
#' converted into factors.
#' 
#' @return the loaded data.
#' 
#' @importFrom withr with_options
#' @export
#' @examples 
#' 
#' \dontrun{ mydata <- packageData('mydata') }
#' 
packageData <- function(list, envir = .GlobalEnv, ..., options = NULL, stringsAsFactors = getOption('stringsAsFactors')){
	
  # use default value based on R version
  if( is.null(stringsAsFactors) ){
    stringsAsFactors <- testRversion("<=3.6.3")
    
  }
  # workaround for when this function is called under batchtools
  # See https://github.com/mllg/batchtools/issues/195
  if( !length(options) ){
    options <- setNames(list(1), tempfile())
    
  }
  
	withr::with_options(options, {
		# same as utils::data if no 'list' argument
		if( missing(list) ) return( data(..., envir=envir) )
		# load into environment
		data(list=list, ..., envir = envir)
		# return the loaded data
		.get <- function(x, envir, ...){
			res <- get(x, ..., envir = envir)
			# force factors into character vectors
			if( !stringsAsFactors && is.data.frame(res) ){
				for(n in colnames(res)[sapply(res, is.factor)]){
					res[[n]] <- as.character(res[[n]])
				}
			}
			# update value in environment
			assign(x, value = res, envir = envir)
			res
		}
		if( length(list) == 1L ) .get(list, envir=envir)
		else sapply(list, .get, envir=envir, simplify=FALSE)
    
	})
	
}

#' @describeIn packageData loads a package data in the parent frame.
#' It is a shortcut for \code{packageData(list, ..., envir=parent.frame())}.
#' 
#' @param error a logical that indicates whether an error should be thrown if 
#' the requested data cannot be found.
#' @param simplify logical that indicates if queries of one object only (i.e. argument `list` 
#' is of length one) should return the data object itself. 
#'  
#' @importFrom assertthat assert_that is.scalar
#' @export
#' @examples
#' 
#' \dontrun{ 
#' # in a package' source => won't issue a NOTE
#' myfunction function(){
#' 	mydata <- ldata('mydata') 
#' }
#' }
#' 
ldata <- function(list, ..., package = NULL, error = TRUE, simplify = TRUE){
  # parameter check
  if( missing(list) ) list <- NULL
  assert_that(is.null(list) || (is.vector(list) && is.character(list))
                , msg = "Invalid argument 'list': value must be NULL or a character vector.")
  assert_that(is.null(package) || isNZString(package), msg = "Invalid argument 'package': value must be NULL or a non-empty string.")
  assert_that(is.scalar(error) && is.logical(error), msg = "Invalid argument 'error': value must be a single logical.")
  assert_that(is.scalar(simplify) && is.logical(simplify), msg = "Invalid argument 'simplify': value must be a single logical.")
  ##
  
  # load data.list
  if( is.null(package) || qrequire(package, character.only = TRUE) ){
    dlist <- list.data(package = package)[["data"]]
    
  }else{
    msg <- sprintf("Could not find data objects %s in package '%s': package not found.", str_out(list, Inf), package)
    if( error ) stop(msg)
    else warning(msg)
    dlist <- character()
    
  }
  # limit to requested data list
  if( !missing(list) || length(list) ){
    dlist <- dlist[dlist %in% list]
    
  }
  # pre-check that the requested data exist
  if( !error ){
    # initialize result list
    res <- sapply(list, function(x) NULL, simplify = FALSE)
    list <- intersect(list, dlist)
    
  }
  
  # fetch the data objects
	e <- parent.frame()
  res_data <- sapply(list, function(l) packageData(list=l, ..., envir=e, package = package), simplify = FALSE)
  
  # return 
  if( error ){
    # simplify if requested
    if( simplify && length(res_data) == 1L ) res_data <- res_data[[1L]]
    return(res_data)
    
  }else{
    if( length(miss <- setdiff(names(res), names(res_data))) ){
      pkg_str <- "in the currently loaded packages"
      if( !is.null(package) ) pkg_str <- paste0("in package ", package)
      warning(sprintf("Could not find data object(s) %s: %s", pkg_str, str_out(miss, Inf)))
      
    }
    #
    for(n in names(res_data)){
      res[[n]] <- res_data[[n]]
    }
    if( simplify && length(res) == 1L ) res <- res[[1L]]
    res
    
  }
  
}

#' List Package Data Objects
#' 
#' Lists data objects that are shipped within package(s).
#' 
#' @param package a single character string that specifies the name of a particular
#' package where to look for data objects.
#' 
#' @return a `data.frame` object with columns:
#' 
#'   * `package`: name of the package that holds the data object.
#'   * `data`: name of the key to use in [utils::data] or [ldata] to load the data object.
#'   * `object`: name of the (sub-)object that is contained in the data object.
#' 
#' @seealso [utils::data], [ldata]
#' @export
#' @examples
#' 
#' # list all data objects
#' head(list.data())
#' 
#' # list all data objects in package 'datasets'
#' subset(list.data("datasets"), data %in% "beavers")
#' 
list.data <- function(package = NULL){
  dlist <- data(package = package)[["results"]]
  m <- str_match(dlist[, "Item"], "^(([^(]+)|(([^(]+)\\(([^)]+)\\)))$")
  key <- ifelse(!is.na(m[, 3L]), m[, 3L], m[, 6L])
  stopifnot(!anyNA(key))
  obj_name <- ifelse(!is.na(m[, 3L]), m[, 3L], m[, 5L])
  stopifnot(!anyNA(obj_name))
  data.frame(package = dlist[, "Package"], data = key, object = obj_name, stringsAsFactors = FALSE)
  
}


#' Generate a Loading Script for Development Packages
#' 
#' Writes a script file that contains code that loads a given development package.
#' 
#' This is useful when we want to load a development package in `batchtools` registries:
#' 
#' ```
#' library(devtools)
#' library(batchtools)
#' 
#' load_all("path/to/pkgA")
#' makeRegistry(..., source = load_all_file("pkgA"))
#' ```
#' 
#' @param path a character string that contains the path to the development package.
#' @param package the name of the package for which the loading script must be generated.
#' It must be a package that has already been loaded with [devtools::load_all] in the current
#' session, so that its path can be retrieved.
#' @param dest the path to script file to create (as a character string).
#' If not provided, then the script is written in a temporary .R file with prefix 
#' `"load_all_<pkgname>_"`.
#' 
#' @return a character string that contains the path to the script file.
#' 
#' @export
load_all_file <- function(path = path.package(package), package, dest = NULL){
  
  if( !missing(path) && !missing(package)  ){
    stop("Arguments 'path' and 'package' are exclusive: only one of them can be provided.")
    
  }
  if( !requireNamespace('devtools', quietly = TRUE) ) 
    stop("Package 'devtools' is required to load development package information.")
  
  # load package object
  if( !missing(package) ) path <- path.package(package) 
  pkg <- devtools::as.package(path)
   
  # define destination script file
  if( is.null(dest) ) dest <- tempfile(paste0("load_all_", pkg[["package"]], "_"), fileext = ".R")
  # write file
  cat(sprintf("# package loader script (generated on: %s) \nmessage(\"* Loader script '%s'\")\nmessage(\"* Loading development package %s from: '%s'\")\ndevtools::load_all(\"%s\")\n"
          , date(), dest, pkg[["package"]], pkg[["path"]], pkg[["path"]])
      , file = dest)
  
  # return path to script
  dest
  
}
