# Package related functions
# 
# Author: Renaud Gaujoux
# Creation: 29 Jun 2012
###############################################################################

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

# taken from devtools:::install_deps but add field Suggests
install_alldeps <- function (pkg = NULL, ...) 
{
	pkg <- as.package(pkg)
	#parse_deps <- devtools:::parse_deps
	deps <- c(parse_deps(pkg$depends), parse_deps(pkg$imports), 
			parse_deps(pkg$linkingto), parse_deps(pkg$suggests))
	not.installed <- function(x) length(find.package(x, quiet = TRUE)) == 
				0
	deps <- Filter(not.installed, deps)
	if (length(deps) == 0) 
		return(invisible())
	message("Installing dependencies for ", pkg$package, ":\n", 
			paste(deps, collapse = ", "))
	install.packages(deps, ...)
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
