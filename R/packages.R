# Package related functions
# 
# Author: Renaud Gaujoux
# Creation: 29 Jun 2012
###############################################################################

path.protect <- function(...){
  f <- file.path(...)
  if( .Platform$OS.type == 'windows' ){
    f <- gsub("\\\\", "/", f)
  }
  paste('"', f, '"', sep='')
}

#' Quick Installation of a Source Package
#' 
#' Builds and install a minimal version of a package from its 
#' source directory.
#' 
#' @param path path to the package source directory
#' @param destdir installation directory. 
#' If \code{NULL}, the package is installed in the default installation library.
#' If \code{NA}, the package is installed in a temporary directory, whose path is returned
#' as a value.
#' @param vignettes logical that indicates if the vignettes should be 
#' rebuilt and installed.
#' @param force logical that indicates if the package should be installed even if a previous
#' installation exists in the installation library.
#' @param ... extra arguments passed to \code{\link{R.CMD}}
#' @param lib.loc library specification.
#' If \code{TRUE} then the installation directory \code{destdir} is added to the default 
#' library paths.
#' This can be usefull if dependencies are installed in this directory.
#' If \code{NULL}, then the default library path is left unchanged.
#' 
#' @return The path of the library where the package was installed.
#' 
#' @export
#' 
quickinstall <- function(path, destdir=NULL, vignettes=FALSE, force=TRUE, ..., lib.loc=if(!is.null(destdir)) TRUE){
	
	npath <- normalizePath(path)
	pkg <- as.package(path)
  
  # define installation library
	nlib <- if( !is.null(destdir) ) destdir
  else if( is_NA(destdir) ) tempfile("pkglib_")
  
  # normalize path
  if( !is.null(nlib) ){
    # create direcory if needed
    if( !is.dir(nlib) ) dir.create(nlib, recursive=TRUE)
    nlib <- normalizePath(nlib)
    
    if( !is.dir(nlib) ){
      stop("Could not install package '", pkg$package, "': installation directory '", nlib, "' does not exist.")
    }
    
    # add destination directory to default libraries
    if( isTRUE(lib.loc) ) lib.loc <- unique(c(nlib, .libPaths()))
  }
  
  # setup result string
	res <- invisible(if( !is.null(destdir) ) nlib else .libPaths()[1L])
  
  # early exit if the package already exists in the library (and not forcing install)
	message("# Check for previous package installation ... ", appendLF=FALSE)
  if( !is.null(destdir) && is.dir(file.path(nlib, pkg$package)) ){
    if( !force ){
      message("YES (skip)")
      return(res)
    }
    message("YES (replace)")
  }else message("NO")
  
	# add lib path
	ol <- set_libPaths(lib.loc)
	on.exit(set_libPaths(ol), add=TRUE)
	message("Using R Libraries: ", str_out(.libPaths(), Inf))
	
	owd <- setwd(tempdir())
	on.exit( setwd(owd), add=TRUE)
  
	# build
	message("# Building package `", pkg$package, "` in '", getwd(), "'")
	opts <- '--no-manual --no-resave-data '
	if( !vignettes ) opts <- str_c(opts, '--no-vignettes ')
	R.CMD('build', opts, path.protect(npath), ...)
	spkg <- paste(pkg$package, '_', pkg$version, '.tar.gz', sep='')
	if( !file.exists(spkg) ) stop('Error in building package `', pkg$package,'`')
	# install
	message("# Installing package `", pkg$package, "`"
          , if( !is.null(destdir) ){
            tmp <- if( is_NA(destdir) ) 'temporary '
            str_c("in ", tmp, "'", nlib, "'")
          })
  opts_inst <- ' --no-multiarch --no-demo --with-keep.source '
	if( !vignettes ) opts_inst <- str_c(opts_inst, '--no-docs ')
	R.CMD('INSTALL', if( !is.null(destdir) ) paste('-l', path.protect(nlib)), opts_inst, path.protect(spkg), ...)
  
  # return installation library
	invisible(res)
}

#' Require a Package
#' 
#' Require a package with a custom error message
#' 
#' @param pkg package name as a character string
#' @param ... extra arguments concatenated to for the header of the 
#' error message 
#' 
#' @export
requirePackage <- function(pkg, ...){
	
	if( !require(pkg, character.only=TRUE) ){
		if( nargs() > 1L ) stop(..., " requires package(s) ", str_out(pkg))
		else stop("Could not find required package(s) ", str_out(pkg))
	}
}

parse_deps <- function (string) 
{
	if (is.null(string)) 
		return()
	string <- gsub("\\s*\\(.*?\\)", "", string)
	pieces <- strsplit(string, ",")[[1]]
	pieces <- gsub("^\\s+|\\s+$", "", pieces)
	pieces[pieces != "R"]
}

packageDependencies <- function(x, recursive=FALSE){
	x <- as.package(x)
	d <- lapply(x[c('depends', 'imports', 'linkingto', 'suggests')], parse_deps)
	unlist(d)
}

#' Installing All Package Dependencies
#' 
#' Install all dependencies from a package source directory or 
#' package source file. 
#' 
#' @param pkg package path or source file
#' @param all logical that indicates if 'Suggests' packages
#' should be installed.
#' @param ... extra arguments passed to \code{\link{install.packages}}.
#' @param dryrun logical that indicates if the packages should be 
#' effectively installed or only shown. 
#' 
#' @export
#' @examples 
#' 
#' try( install.dependencies('Matrix', dryrun=TRUE) )
#' \dontrun{
#' install.dependencies("mypackage_1.0.tar.gz", dryrun=TRUE)
#' }
#' 
install.dependencies <- function (pkg = NULL, all=FALSE, ..., dryrun=FALSE) 
{
	pkg <- as.package(pkg, extract=TRUE)
	#parse_deps <- devtools:::parse_deps
	deps <- c(parse_deps(pkg$depends)
			, parse_deps(pkg$imports) 
			, parse_deps(pkg$linkingto)
			, if( isTRUE(all) ) parse_deps(pkg$suggests) )
	not.installed <- function(x) length(find.package(x, quiet = TRUE)) == 0
	message("Package dependencies for ", pkg$package, ": ", str_out(deps, Inf))
	deps <- Filter(not.installed, deps)
	if (length(deps) == 0){
		message("Missing: none")
		return(invisible())
	}
	message("Missing: ", str_out(deps, Inf))
	message("Installing ", length(deps), " dependencies for ", pkg$package)
	if( !dryrun ) install.packages(deps, ...)
	invisible(deps)
}

#' Setting Mirrors and Repositories
#' 
#' \code{setBiocMirror} sets all Bioconductor repositories (software, data, 
#' annotation, etc.).
#' so that they are directly available to \code{\link{install.packages}}.
#' It differs from \code{\link{chooseBioCmirror}} in that it effectively enables 
#' the repositories.
#' 
#' @param url or Bioconductor mirror url
#' @param version version number
#' @param unique logical that indicate if duplicated urls or names should be 
#' removed.
#'
#' @rdname mirrors
#' @export 
setBiocMirror <- function(url='http://www.bioconductor.org', version=NULL, unique=TRUE){
	
    #get all bioconductor repos      
    biocRepos <- getBiocRepos(url, version)
	
	repos <- c(biocRepos, getOption('repos'))
	if( unique ){
		nam <- names(repos)
		repos <- repos[!duplicated(repos) & (!duplicated(nam) | nam=='')]
	}
    options(repos=repos)
}

#' \code{getBiocMirror} is a shortcut for \code{getOption('BioC_mirror')}, which 
#' returns the current Bioconductor mirror as used by \code{biocLite}.
#'  
#' @export
#' @rdname mirrors
getBiocMirror <- function(){
	getOption('BioC_mirror')
}
#' \code{getBiocRepos} returns urls to all Bioconductor repositories on a 
#' given mirror.
#' 
#' @export
#' @rdname mirrors
getBiocRepos <- function(url='http://www.bioconductor.org', version=NULL){
	
	if( is.null(url) ){
		url <- getBiocMirror()
		if( is.null(url) )
			stop("No Bioconductor mirror was setup. Use `setBiocMirror`.")
	}
	
	## BioConductor CRAN-style repositories.
	## The software repo (bioc) _must_ be the first element.
	biocParts <- c(
			bioc='bioc'
			, biocData='data/annotation'
			, biocExp='data/experiment'
			, biocExtra='extra'
    )
	
	# define version suffix for bioconductor repo
	if( is.null(version) ){
		assoc <- list(`2`=c(7L, 2L))
		Rv <- as.integer(sub("([0-9]+).*", "\\1", R.version$minor))
		offset <- assoc[[R.version$major]]
	    version <- paste(R.version$major, offset[2L] + Rv - offset[1L], sep='.')
	}
	
	#add version suffix for bioconductor repo
    setNames(paste(url, 'packages', version, biocParts, sep='/'), names(biocParts))
}

#' \code{setCRANMirror} sets the preferred CRAN mirror.
#' 
#' @rdname mirrors
#' @export
setCRANMirror <- function(url=CRAN, unique=TRUE){
	
	repos <- c(CRAN=url, getOption('repos'))
	if( unique ){
		nam <- names(repos)
		repos <- repos[!duplicated(repos) & (!duplicated(nam) | nam=='')]
	}
    options(repos=repos)
}

#' \code{CRAN} simply contains the url of CRAN main mirror 
#' (\url{http://cran.r-project.org}), and aims at simplifying its use, e.g., in 
#' calls to \code{\link{install.packages}}.
#' 
#' @rdname mirrors
#' @export
#' 
#' @examples
#' \dontrun{
#' install.packages('pkgmaker', repos=CRAN)
#' }
CRAN <- 'http://cran.r-project.org'


#' Adding Package Libraries
#' 
#' Prepend/append paths to the library path list, using \code{\link{.libPaths}}.
#' 
#' This function is meant to be more convenient than \code{.libPaths}, which requires 
#' more writing if one wants to:
#' \itemize{
#' \item sequentially add libraries;
#' \item append and not prepend new path(s);
#' \item keep the standard user library in the search path.
#' }
#' 
#' @param ... paths to add to .libPath
#' @param append logical that indicates that the paths should be appended
#' rather than prepended.
#' 
#' @export
#' 
#' @examples
#' ol <- .libPaths()
#' # called sequentially, .libPaths only add the last library
#' show( .libPaths('.') )
#' show( .libPaths(tempdir()) )
#' # restore
#' .libPaths(ol)
#' 
#' # .libPaths does not keep the standard user library
#' show( .libPaths() ) 
#' show( .libPaths('.') )
#' # restore
#' .libPaths(ol)
#' 
#' # with add_lib
#' show( add_lib('.') )
#' show( add_lib(tempdir()) )
#' show( add_lib('..', append=TRUE) )
#' 
#' # restore 
#' .libPaths(ol)
#' 
add_lib <- function(..., append=FALSE){
	
	p <- 
	if( append ) c(.libPaths(), ...)
	else c(..., .libPaths())
	.libPaths(p)
}


#' Package Check Utils
#' 
#' \code{isCRANcheck} tries to identify if one is running CRAN checks.
#' 
#' Currently \code{isCRANcheck} returns \code{TRUE} if the check is run with 
#' either environment variable \code{_R_CHECK_TIMINGS_} (as set by flag \code{'--timings'})
#' or \code{_R_CHECK_CRAN_INCOMINGS_} (as set by flag \code{'--as-cran'}).
#' 
#' Note that the checks performed on CRAN farms are not always run with such flags, 
#' so there is no guarantee this function identifies such runs, and one should 
#' rely custom environment variables to enable specific tests or examples.
#' 
#' @param ... each argument specifies a set of tests to do using an AND operator.
#' The final result tests if any of the test set is true.
#' Possible values are:
#' \describe{
#' \item{\code{'timing'}}{Check if the environment variable \code{_R_CHECK_TIMINGS_} is set, 
#' as with the flag \code{'--timing'} was set.}
#' \item{\code{'cran'}}{Check if the environment variable \code{_R_CHECK_CRAN_INCOMING_} is set, 
#' as with the flag \code{'--as-cran'} was set.}
#' }
#' 
#' @references Adapted from the function \code{CRAN}
#' in the \pkg{fda} package.
#' 
#' @export
isCRANcheck <- function(...){
  
  tests <- list(...)
  if( !length(tests) ){ #default tests
	  tests <- list('timing', 'cran')
  }
  test_sets <- c(timing="_R_CHECK_TIMINGS_", cran='_R_CHECK_CRAN_INCOMING_')
  tests <- sapply(tests, function(x){
			  # convert named tests
			  if( length(i <- which(x %in% names(test_sets))) ){
				  y <- test_sets[x[i]]
				  x <- x[-i]
				  x <- c(x, y)
			  }
			  # get environment variables
			  evar <- unlist(sapply(x, Sys.getenv))
			  all(nchar(as.character(evar)) > 0)
		  })
  
  any(tests)
}
#' \code{isCRAN_timing} tells if one is running CRAN check with flag \code{'--timing'}.
#' 
#' @export
#' @rdname isCRANcheck
isCRAN_timing <- function() isCRANcheck('timing')

.utestCheckEnvarname <- '_R_CHECK_RUNNING_UTESTS_'

#' \code{isCHECK} tries harder to test if running under \code{R CMD check}, at least  
#' for unit tests that use the unified unit test framework defined by \pkg{pkgmaker} 
#' (see \code{\link{utest}}).
#' 
#' \code{isCHECK} checks both CRAN expected flags and the value of environment variable
#' \code{_R_CHECK_RUNNING_UTESTS_}.
#' It will return \code{TRUE} if such variable is set to anything not equivalent 
#' to \code{FALSE}.
#' For example, the function \code{\link{utest}} sets it to the name of the package  
#' being checked (\code{_R_CHECK_RUNNING_UTESTS_=<pkgname>}), 
#' but unit tests run as part of unit tests vignettes are run with 
#' \code{_R_CHECK_RUNNING_UTESTS_=FALSE}, so that developers cann run all tests.
#' 
#' @rdname isCRANcheck
#' @export
#' 
#' @examples
#' 
#' isCHECK()
#' 
isCHECK <- function(){
	isCRANcheck() || !isFALSE(utestCheckMode(raw=FALSE))
}

utestCheckMode <- function(value, raw=TRUE){
	if( missing(value) ){
		val <- Sys.getenv()[.utestCheckEnvarname]
		# convert false values to FALSE if required
		if( !raw && (is.na(val) || !nchar(val) || tolower(val) == 'false' || val == '0') ){
			val <- FALSE
		}
		val
	}else{
		old <- utestCheckMode()
		if( is_NA(value) ) Sys.unsetenv(.utestCheckEnvarname) # unset
		else do.call(Sys.setenv, setNames(list(value), .utestCheckEnvarname)) # set value
		# return old value
		old	
	}	
}

