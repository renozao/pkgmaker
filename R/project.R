# Utils for R package projects
# 
# Author: Renaud Gaujoux
# Created: May 1, 2013
###############################################################################


#' Test for Package Root Directory
#' 
#' Tells if a directory is a package directory, i.e. that it contains
#' a \code{DESCRIPTION} file.
#' 
#' @param x path to the directory to test
#' @param error logical that indicates if an error should be raised
#' if the directory is not a package directory.
#' 
is_package_path <- function(x, error = FALSE) {
	if (is.null(x)) return(FALSE)
	x <- normalizePath(x, mustWork = FALSE)
	x <- gsub("\\\\$", "", x)
	desc_path <- file.path(x, "DESCRIPTION")
	if( !error ){
		file.exists(x) && file.exists(desc_path)
	}else{
		if ( !file.exists(x) ) stop("Can't find directory ", x, call. = FALSE)
		if ( !file.info(x)$isdir ) stop(x, " is not a directory", call. = FALSE)
		if (!file.exists(desc_path)) stop("No DESCRIPTION file found in ", x, call. = FALSE)
		TRUE
	}
}


#' Find Path to Development Package Root Directory
#' 
#' Development packages are looked-up according to rules
#' defined in a file \code{.Rpackages} in the user's home directory. 
#' 
#' @param x name of the development package to lookup.
#' @export
find_devpackage <- function (x) 
{
	
	
	if (is_package_path(x)) {
		return(x)
	}
	
	config_path <- "~/.Rpackages"
	if (!file.exists(config_path)) {
		return(NULL)
	}
	config_path <- path.expand(config_path)
	lookup <- source(config_path)$value
	if (is_package_path(lookup[[x]])) {
		return(lookup[[x]])
	}
	if (!is.null(lookup$default)) {
		default_loc <- lookup$default(x)
		if (is_package_path(default_loc, error = TRUE)) {
			return(default_loc)
		}
	}
	NULL
}

#' Load Development Package
#' 
#' @param pkg name of the package/project to load.
#' @param reset logical that indicates if the package should be reloaded (passed to \code{\link[devtools]{load_all}}.
#' @param ... other arguments passed to \code{\link[devtools]{load_all}}.
#' @param utests logical that indicates if an environment containing the unit test functions should be created.
#' If \code{TRUE} this environment is accessible at \code{pkgname::UnitTests$test.filename.r$function.name}.
#' @param verbose logical that indicates if log messages should be printed.
#' @param addlib logical that indicates if the \code{lib/} sub-directory, if it exists, should be prepended 
#' to the library path. 
#' This enables to control the version of the loaded dependencies.
#' @param character.only logical that indicates if argument \var{pkg} should be evaluated or taken litteral. 
#' 
#' @export 
load_project <- function(pkg, reset = FALSE, ..., utests = TRUE, verbose=FALSE, addlib=TRUE, character.only = FALSE) {
	
	if( !requireNamespace('devtools') ){
		stop("Could not load package: required package 'devtools' is not installed.")
	}
	if( !character.only ){
		pkg <- deparse(substitute(pkg))
		pkg <- sub("^\"(.*)\"$", "\\1", pkg)
	}
	
	devpkg_path <- find_devpackage(pkg)
	if( !is.character(devpkg_path) ) pkg <- tolower(pkg)
	else pkg <- devpkg_path 
	
	# add ../lib to the path if necessary
	if( addlib && is.character(tp <- find_devpackage(pkg)) ){
		tp <- as.package(tp)
		pdir <- normalizePath(file.path(dirname(tp$path), "lib"), mustWork=FALSE)
		if( file_test('-d', pdir) && !is.element(pdir, .libPaths()) ){
			message("Adding to .libPaths: '", pdir, "'")
			olibs <- .libPaths()
			.libPaths(c(pdir, .libPaths()))
			on.exit( .libPaths(olibs), add=TRUE )
		}
	}
	devpkg <- as.package(pkg)
	
	# load package
	op <- options(verbose=verbose)
	on.exit(options(op), add=TRUE)  
	devtools::load_all(pkg, reset = reset, ...)
	#
	
	# source unit test files if required
	udir <- file.path(devpkg$path, 'inst', c('tests', 'unitTests'))
	if( utests && length(w <- which(file.exists(udir))) ){
		message("# Sourcing unit test directory ... ", appendLF = FALSE)
		f <- list.files(udir[w[1L]], pattern = "\\.[Rr]$", full.names=TRUE)
		if( length(f) ){
#			if( !requireNamespace('RUnit') ) stop("Missing required dependency 'RUnit' to load unit tests")
			# create unit test environment
			utest_env <- new.env(parent = devtools::ns_env(devpkg))
			assign('UnitTests', utest_env, devtools::ns_env(devpkg))
			# source test files in separate sub-environments
			sapply(f, function(f){
						e <- new.env(parent = utest_env)
						assign(basename(f), e, utest_env)
						sys.source(f, envir = e)
					})
		}
		message('OK [', length(f), ']')
		# reload to export the unit test environment
		devtools::load_all(pkg, reset = FALSE, ...)
	}
	#
	
	invisible(devpkg)
}


is_Mac <- function(check.gui=FALSE){
	is.mac <- (length(grep("darwin", R.version$platform)) > 0)
	# return TRUE is running on Mac (adn optionally through GUI)
	is.mac && (!check.gui || .Platform$GUI == 'AQUA')
}

R_OS <- function(){
	if( is_Mac() ) 'MacOS'
	else .Platform$OS.type
}

packageMakefile <- function(package=NULL, template=NULL, temp = FALSE, print = TRUE){
	
	capture.output(suppressMessages({
		library(pkgmaker)
        if( !requireNamespace('devtools', quietly = TRUE) ) 
                stop("Package 'devtools' is required to generate a package Makefile")
						
	}))
#	defMakeVar <- pkgmaker::defMakeVar
#	subMakeVar <- pkgmaker::subMakeVar
	
	project_path <- getwd()
	project_name <- basename(project_path)
	subproject_path_part <- ''
	if( is.null(package) || isString(package) ){
		if( isString(package) && !nzchar(package) ) package <- NULL
		lookup_dir <- c('pkg', '.')
		if( !is.null(package) ){
			lookup_dir <- c(lookup_dir, file.path('pkg', package))
			subproject_path_part <- file.path(package, '')
		}
		pdir <- file.path(lookup_dir, 'DESCRIPTION')
		if( !length(sd <- which(is.file(pdir))) ){
			stop("Could not detect package source directory")
		}
		package <- pdir[sd[1L]]
	}
	package <- normalizePath(package)
	p <- pkg <- as.package(dirname(package));
	pdir <- package_dir <- p[['path']];
	
	## create makefile from template
	# load template makefile
	if( is.null(template) ){
		template <- packagePath('package.mk', package='pkgmaker')
	}
	l <- paste(readLines(template), collapse="\n")
	
	# user
	cuser <- Sys.info()["user"]
	l <- defMakeVar('AUTHOR_USER', cuser, l)
	l <- defMakeVar('R_PACKAGE', pkg$package, l)
	# R_PACKAGE_PATH
	l <- defMakeVar('R_PACKAGE_PATH', package_dir, l)
	# R_PACKAGE_PROJECT
	l <- defMakeVar('R_PACKAGE_PROJECT', project_name, l)
	# R_PACKAGE_PROJECT_PATH
	l <- defMakeVar('R_PACKAGE_PROJECT_PATH', project_path, l)
	l <- defMakeVar('R_PACKAGE_SUBPROJECT_PATH_PART', subproject_path_part, l)
	# R_BIN
	l <- subMakeVar('R_BIN', R.home('bin'), l)
    # REPO_DIRS
    repo_dirs <- gsub("^\\./", "", sapply(c('source', 'win.binary', 'mac.binary'), contrib.url, repos = '.'))
    l <- defMakeVar('REPO_DIRS', paste0(repo_dirs, collapse = ' '), l)
    # BUILD_DIR
    l <- defMakeVar('BUILD_DIR', file.path(repo_dirs['source'], ''), l)
	# R_PACKAGE_TAR_GZ
	pkg_targz <- file.path(repo_dirs['source'], package_buildname(p, 'source'))
	l <- defMakeVar('R_PACKAGE_TAR_GZ', pkg_targz, l)
    # R_PACKAGE_ZIP
	pkg_zip <- file.path(repo_dirs['win.binary'], package_buildname(p, 'win.binary'))
	l <- defMakeVar('R_PACKAGE_ZIP', pkg_zip, l)
    # R_PACKAGE_TGZ
	pkg_mac <- file.path(repo_dirs['mac.binary'], package_buildname(p, 'mac.binary'))
	l <- defMakeVar('R_PACKAGE_TGZ', pkg_mac, l)
	# R_PACKAGE_TYPE	
	l <- defMakeVar('R_PACKAGE_OS', R_OS(), l)
	#

    # auto-conf variables
    init_var <- list(version = pkg$version)
    if( is.dir(file.path(package_dir, 'vignettes')) ) 
        init_var <- c(init_var, has_vignettes=TRUE)
    # dump variables
    if( length(init_var) ){
        init_var <- setNames(init_var, paste0('R_PACKAGE_', toupper(names(init_var))))
        init_var_str <- str_out(init_var, Inf, use.names = TRUE, sep = "\n")
        l <- subMakeVar('INIT_CHECKS', init_var_str, l)
    }
	
	# R_CMD_CHECK
	rlibs <- ''
    if( is.dir(devlib <- file.path(dirname(pdir), 'lib')) ){
		rlibs <- paste0("R_LIBS=", devlib, ' ')
	}
    l <- subMakeVar('R_LIBS', rlibs, l)
	#

	# create makefile
	mk <- if( temp ) tempfile('package_', tmpdir='.', fileext='.mk') else 'package.mk'
	cat(l, file=mk)
	if ( print ){
		cat(mk)
	}
	invisible(l)
}