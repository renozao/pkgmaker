# Package hooks
# 
# Author: renaud
# Creation: 26 Jun 2012
###############################################################################

#' @include utils.R
#' @include devutils.R
NULL

#' Default Load/Unload Functions
#' 
#' @inheritParams base::.onLoad
#' @inheritParams base::library.dynam
#' 
#' @export
#' @rdname load
#' 
#' @examples
#' 
#' # in a package namespace:
#' .onLoad <- function(libname=NULL, pkgname){
#' 
#' 	pkgmaker::onLoad(libname, pkgname)
#' 
#' }
onLoad <- function(libname=NULL, pkgname, chname=packageName()){
	
	# load compiled library normally or in devmode
	if( !is.null(libname) ){
		if( file.exists(packagePath('libs')) ){
			sapply(chname, library.dynam, package=pkgname, lib.loc=libname)
		}
	}else{
		compile_src() # compile source files and load
	}
		
}

#' @inheritParams base::.onUnload
#' @export
#' @rdname load
#' 
#' @examples
#' 
#' # in a package namespace:
#' .onUnload <- function(libpath){
#' 
#' 	pkgmaker::onUnload(libpath)
#' 
#' }
onUnload <- function(libpath) {
	
	# unload compiled library normally or in devmode
	dlls <- base::getLoadedDLLs()
	pname <- packageName()
	if ( pname %in%  names(dlls) ){
		if( !missing(libpath) )	library.dynam.unload(pname, libpath)
		else dyn.unload(dlls[[pname]][['path']])
	}
	
}
