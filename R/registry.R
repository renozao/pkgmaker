# Registry utility functions
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################

# fix some functions of registry objects
fix_registry <- function(regobj){
	
	# get private environment
	.REGENV <- environment(environment(regobj$delete_entry)$f)
	
	# dummy variables for R CMD check
	PERMISSIONS <- 
	.get_entry_indices <-  
	.get_entry_names <- 
	SEALED_ENTRIES <-
	DATA <- 
	.delete_entry <- 
	.get_entries <- 
	NULL
	
	# fix bug in delete_entry
	hook <- function(...){
		key <- list(...)
		if( length(key) == 1L && isString(key[[1L]]) ){
			key <- list(...)
			if (!PERMISSIONS["delete_entries"]) 
				stop("Deletion of entries not allowed.", call. = FALSE)
			entry_index <- .get_entry_indices(key)
			# fix
			if( key[[1L]] %in% .get_entry_names() )
				entry_index <- match(key[[1L]], .get_entry_names())
			#end_fix
			if (length(entry_index) != 1) 
				stop("Key specification must be unique.", call. = FALSE)
			if (entry_index %in% SEALED_ENTRIES) 
				stop(paste("Deletion of entry not allowed."), call. = FALSE)
			DATA[entry_index] <<- NULL
		} else .delete_entry(...)
	}
	environment(hook) <- .REGENV
	regobj$delete_entry <- hook
	#
	
	# fix bug in get_entry
	hook <- function(...){
		key <- list(...)
		if( length(key) == 1L && isString(key[[1L]]) ){
			res <- .get_entries(...)
			if( key[[1L]] %in% names(res) )
				res[[key[[1L]]]]
			else res[[1L]] 
		}else .get_entries(...)[[1]]
	}
	environment(hook) <- .REGENV
	regobj$get_entry <- hook
	#
	
	regobj
}

#' Package Registry
#' 
#' @param name Name of a sub-registry
#' @param error a logical that indicate whether an error is thrown if the 
#' sub-registry is not found (default) or just \code{NULL}.
#' @param package package where to look for the registry.
#' If \code{NULL} this will defaults to the top caller package. 
#' 
#' @return a \code{\link[registry:regobj]{registry}} object or \code{NULL} (see argument 
#' \code{error}).
#' 
#' @rdname registry
#' @export
packageRegistry <- function(name, error=TRUE, package=NULL){
	
	library(registry)
	
	# get calling package environment and name
	e <- 
	if( is.environment(package) ) package else packageEnv(package)
	nm <- packageName(e)
	
	# create registry environment if necessary
	if( !exists('.packageRegistry', e) ){
		# make registry object with special classes for the registry and entries
		meta <- registry(c(paste('package', nm, 'subregistry', sep='_'), 'package_subregistry')
						, c(paste('package', nm, 'metaregistry', sep='_'), 'package_metaregistry'))
		# set fields
		meta$set_field("key", type="character", is_key = TRUE, index_FUN = match_exact)
		meta$set_field("regobj", type="registry", is_mandatory = TRUE)
		meta$set_field("description", type="character", is_mandatory = TRUE)
		
		# fix registry
		meta <- fix_registry(meta)
		
		# store within the calling package environment
		e$.packageRegistry <- meta
	}
	
	if( missing(name) )	return(e$.packageRegistry)
	else{
		reg <- regfetch(e$.packageRegistry, name, exact=TRUE, error=FALSE)
		if( is.null(reg) ){
			if( error )
				stop("Could not find registry `", name, "` in package `", nm, "`")
			else return(NULL)
		}else{
			reg$regobj
		}
	}
}


#' @S3method format package_subregistry
format.package_subregistry <- function(x, ...){
	c(Key = x$key
	, Description = x$description
	, Entries = x$regobj$n_of_entries()) 	
}

#' @S3method format package_metaregistry
format.package_metaregistry <- function(x, ...){
	rec <- x$get_entries()
	data.frame(t(sapply(rec, base::format, ...))[,-1])	 
}

#' @S3method xtable package_metaregistry 
#' @importFrom xtable xtable
xtable.package_metaregistry <- function(x, ...){
	d <- format(x)
	xtable::xtable(d, ...)
}

#' @S3method print package_metaregistry
print.package_metaregistry <- function(x, ...){	
	registry:::print.registry(x)
	print(format(x))
}

#' Adds a registry to the package 
#' 
#' @param regobj a \code{\link[registry:regobj]{registry}} object or a single character 
#' string that indicates the class of the objects that are stored in the 
#' sub-registry.
#' @param description short description line about the registry.
#' It is recommended to provide such description as it makes clearer the purpose of the 
#' registry.
#' This description is listed  
#' @param ... named values used to set extra information about the new registry, that 
#' are stored in dedicated fields.
#' Currently not used, as no extra field other than \code{'description'} is defined.
#' @param overwrite a logical that indicate if an existing registry with the same 
#' should be overwritten if it exists.
#' 
#' @inheritParams packageRegistry
#' @rdname registry
#' @export
setPackageRegistry <- function(name, regobj, description='', ..., overwrite=FALSE){
	
	library(registry)
	
	nm <- packageName()
	ns_str <- str_ns()
	# get meta-registry
	regenv <- packageRegistry()
	
	# force overwrite in dev mode
	if( missing(overwrite) && isDevNamespace(nm) ){
		overwrite <- TRUE
	}
	
	oldreg <- packageRegistry(name, error=FALSE)
	if( !is.null(oldreg) ){
		if( !overwrite ){
			if( isLoadingNamespace() ){ # exit if loading a namespace
				message("NOTE: Did not create registry '", name,"' in ", ns_str, ": registry already exists.")
				return(oldreg)
			}
			stop("Could not create registry '", name,"' in ", ns_str, ": registry already exists")
		}else{
			message("Remove registry '", name,"' from ", ns_str)
			regenv$delete_entry(name)
		}
	}
	message("Create registry '", name,"' in ", ns_str)
	
	if( is.character(regobj) ){
		pref <- nm
		objtype <- regobj[1]
		regobj <- registry(entry_class = paste(pref, name, 'entry', sep='_')
						, registry_class = c(paste(pref, name, 'registry', sep='_'), 'object_subregistry'))
		# access key
		regobj$set_field("key", type="character", is_key = TRUE
				, index_FUN = match_partial_ignorecase)
		# object
		regobj$set_field("object", type=objtype, is_mandatory=TRUE, validity_FUN = validObject)
	}
	# fix registry
	regobj <- fix_registry(regobj)
	
	# create new meta entry
	regenv$set_entry(key=name, regobj=regobj, description=description, ...)
	# return newly created registry
	regenv$get_entry(name)$regobj
}

#' Finds an entry in a registry.
#' 
#' @param regobj a registry object
#' @param key a key to match
#' @param msg a header to use in case of error
#' @param all logical to indicate if hidden keys (starting with a '.') should be 
#' returned and output in message.
#' @param error a logical that indicates if an error should be thrown if the key has no match 
#' or multiple matches
#' @param exact a logical that indicates if matching should be exact or partial
#' @param verbose a logical that indicates if verbosity should be toggle on
#' 
#' @export
regfetch <- function(regobj, key=NULL, msg=NULL, all=FALSE, error=TRUE, exact=FALSE, verbose=FALSE){
	
	library(registry)
	# list -- all -- keys if no key is specified
	allkeys <- regobj$get_entry_names()
	if( !all ) allkeys <- grep("^[^.]", allkeys, value=TRUE)
	if( is.null(key) ){
		return(allkeys)
	}
	
	# set verbosity level
	if( !missing(verbose) ){
		ol <- lverbose(verbose)
		on.exit( lverbose(ol) )
	}
	
	if( !is.null(msg) ) msg <- str_c(msg, ' - ')
	
	if( regobj$n_of_entries() == 0L ){
		if( error )	stop(msg, "Registry is empty: no matching entry for key ", dQuote(key), ".")
		else return(NULL)
	}
	
	d <- regobj$get_entries(key)
	
	# no entry found
	if( is.null(d) ){
		if( error ){
			stop(msg, "No matching entry for key ", dQuote(key), " in the registry."
							, "\n  Use one of:", str_wrap(str_out(sort(allkeys), NA), exdent=2))
		}else return(NULL)
	}
	# multiple match
	if( length(d) > 1L ){
		i <- which(key == names(d))
		if( length(i) == 1L ) d <- d[i]
		else if( error ) stop(msg, "Multiple entries found for key ", dQuote(key), ": ", str_out(sort(names(d)), NA))
		else return(NA)
	}
	
	# check single match
	if( length(d) != 1L )
		stop("Unexpected error: more than one entry was selected.")
	
	# check it is an exact match
	if( exact && names(d) != key ){
		if( error ){
			stop(msg, "No exact match for key '", key, "' in the registry."
							, "\n  Use one of:", str_wrap(str_out(allkeys), exdent=2))
		}else return(NULL)
	}
	
	# return single match
	res <- d[[1]]
	
	# return registry object if the entry is an automatic sub-registry
	if( is(regobj, 'object_subregistry') ) return(res$object)
	else return(res)
}


#' Automatic S4 Class for Registry Entries
#' 
#' @param registry a registry object
#' @param Class name of the class to generate
#' @param ... extra arguments passed to \code{\link{setClass}}.
#' 
setClassRegistry <- function(registry, Class, ...){
	
#	setClass(Class, representation, prototype, contains=character(),
#			validity, access, where, version, sealed, package,
#			S3methods = FALSE)
	
	f <- registry$get_fields()
	slots <- sapply(f, '[[', 'type', simplify=FALSE)
	
	args <- list(Class, representation=do.call('representation', slots))
#	if( !hasArg(validity) ){
#		.validity <-
#		sapply(f, function(x){
#			if(x$is_mandatory)
#				function(object){
#					if()
#				}
#		})
#		args$validity <- function(object){
#			
#		}
#	}
	do.call('setClass', c(args, ...))
}

