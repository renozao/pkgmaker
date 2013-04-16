# Rd utility functions
# 
# Author: Renaud Gaujoux
# Created: Mar 25, 2013
###############################################################################

getRdFile <- function(topic, package=NULL){
	help_call <- substitute(help(topic, package = package, try.all.packages = TRUE), 
			list(topic = topic, package = package))
	
	eval(help_call)
}

getRdTag <- function(topic, tag, package=NULL){
	# extract topic
	rd <- utils:::.getHelpFile(file=getRdFile(topic, package=package))
	w <- which(tools:::RdTags(rd) == tag)
	rd[w]
}

RdSection2latex <- function(topic, i=1L, notitle=TRUE, ...){
	rdsec <- getRdTag(topic, tag="\\section", ...)
	if( !length(rdsec) ) return()
	ltx <- capture.output(tools::Rd2latex(rdsec[i], fragment=TRUE))
	if( notitle ){
		parts <- stringr::str_match(ltx, "\\{Section\\}")
		w <- which(!is.na(parts[, 1]))
		ltx <- ltx[seq(w[1]+1, tail(w, 1)-1)]
	}
	ltx <- paste(ltx, collapse="\n")
	# remove link commands
	ltx <- gsub("\\\\LinkA\\{([^}]+)\\}\\{([^}]+)\\}", "\\2", ltx)
	
	cat(ltx)
	invisible()
}

