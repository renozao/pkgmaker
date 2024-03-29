# Roxygen extensions
# 
# Author: Renaud Gaujoux
###############################################################################

#' Bibliography roclet
#' 
#' @rawNamespace if( require(roxygen2) ) import(roxygen2)
#' 
#' @return An S3 object of class `roclet_rd`.
#' @export
#' @rdname roclet
bib_roclet <- function() {
  roc <- roclet("bib")
  # Add roclet_rd class to inherit its methods
  class(roc) <- append(class(roc), 'roclet_rd', after = 1L)
  roc
}

#' @section Roxygen tags and commands:
#'   * @@cite: 
#'   * @@bibliography
#'   * \\cite: 
#' 
#' @rawNamespace if( require(roxygen2) ) S3method(roclet_tags,roclet_bib)
roclet_tags.roclet_bib <- function(x) {
  list(
    cite = function(x){
      x <- tag_words()(x)
      x$tag <- 'references'
      x$val <- sprintf("\\cite{%s}", x$val)
      x
    },
    bibliography = function(x){
      x <- tag_words()(x)
      bib <- RoxyBibObject()
      bib$add_bibfile(x$val)
      NULL
    },
    inline = tag_toggle
  )
  
}

#' @rawNamespace if( require(roxygen2) ) S3method(roclet_process,roclet_bib)
#' @rdname roclet
roclet_process.roclet_bib <- function(x, parsed, base_path, global_options = list()){
  
  # build inline set
  inline_set <- unlist(sapply(parsed$blocks, function(x){
        if( !is.null(x$inline) && class(x$object) == 's4generic' ) as.character(x$object$value@generic)
    }))
  
  # get bibfile cache object
  BIBS <- RoxyBibObject(package = packageName(parsed$env))
  
  # extract citations in tag values and add them as reference tags
  for (i in seq_along(parsed$blocks)) {
    block <- parsed$blocks[[i]]
    hash <- digest(block)
    
    if( class(block$object) == 's4method' && 
        (as.character(block$object$value@generic) %||% '') %in% inline_set && 
        is.null(block$describeIn) ){
      # build inline description from title and description
      descIn <- list(name = as.character(block$object$value@generic)
                      , description = paste(block$title, if( !identical(block$description, block$title) ) block$description, sep = "\n\n"))
      if( !length(descIn$description) ) block$rdname <- block$rdname %||% descIn$name 
      else{
        block$describeIn <- descIn
        block$title <- block$description <- NULL
      }
    }
    
    if (length(block) == 0)
      next
    
    # 1. process all tags that can have \cite commands
    tags_cite <- c('details')
    j_cite <- which(names(block) %in% tags_cite)
    if( length(j_cite) ){
      cite_res <- lapply(block[j_cite], process_cite, bibs = BIBS, short = TRUE, block = block)
      block[j_cite] <- lapply(cite_res, '[[', 'value')
      bibkeys <- unique(unlist(lapply(cite_res, '[[', 'bibkeys')))
      
      # 2. add keys to references
      if( length(bibkeys) )
        block <- append(block, list(references = sprintf('\\cite{%s}', paste(bibkeys, collapse = ';'))))
    }
    
    # 3. process references
    j_ref <- which(names(block) %in% 'references')
    if( length(j_ref) ){
      ref_res <- lapply(block[j_ref], process_cite, bibs = BIBS, short = FALSE, block = block)
      block[j_ref] <- lapply(ref_res, '[[', 'value')
    }
    
    # update in parsed block only if necessary
    if( digest(block) != hash ) parsed$blocks[[i]] <- block
  }
  
  # call roclet_rd process method to update the .Rd files
  NextMethod()
  
}

# find cite tags and resolve them against bibfiles
process_cite <- function(bibs, x, short = TRUE, block = NULL){
  # extract \cite tags
  cite_match <- str_match_all(x, "\\\\cite\\{([^}]+)\\}")
  # for each process citations
  res <- list(value = x, bibkeys = NULL)
  
  lapply(seq_along(cite_match), function(i){
    m <- cite_match[[i]]
    # no \cite command: return string untouched
    if( !length(m) ) return()
    
    # split into individual bibkeys
    keys <- strsplit(m[, 2L], ';')
    # process each command
    mapply(function(cite_s, key){
          key <- str_trim(key)
          res$bibkeys <<- union(res$bibkeys, key)
          fkey <- bibs$format_cite(key, short = short, block = block)
#          fkey[!nzchar(fkey)] <- key[!nzchar(fkey)]
          res$value[i] <<- gsub(cite_s, paste(fkey, collapse = if( short ) ', ' else "\n\n"), res$value[i], fixed = TRUE)
        }, m[, 1L], keys)
  })
  res
  
}

#' @rawNamespace if( require(roxygen2) ) S3method(roclet_clean,roclet_bib)
#' @rdname roclet
roclet_clean.roclet_bib <- function(x, base_path) {
  # reset bibliography object
  RoxyBibObject(reset = TRUE)
  # call roclet_rd clean method
  NextMethod()
}

RoxyBibObject <- local({
  .obj <- NULL
  function(package = NA, reset = FALSE){
    if( reset ) .obj <<- NULL
    # create or update instance
    if( is.null(.obj) ) .obj <<- RoxyBib$new(package)
    else .obj$set_package(package)
    .obj
  }
})

RoxyBib <- R6::R6Class("RoxyTopic", public = list(
  
  # data members
  package = NA,
  bibfiles = character(),
  bibs_loaded = character(),
  bibs = list(),
  bibentries = list(),
  
  # constructor
  initialize = function(package = NA) {
    self$set_package(package)
  },
  
  set_package = function(package = NA){
    self$package <- package
    self$add_bibfile(package = package)
  },
  
  add_bibfile = function(path, package = NA){
    if( missing(path) ){
      if( !is.na(package) ){
        refs <- packagePath('REFERENCES.bib', package = package)
        if( file.exists(refs) ) path <- refs
      }
      if( missing(path) ) return()
    }
    self$bibfiles <- union(self$bibfiles, normalizePath(path)) 
  },
  
  load_bib = function(next.only = TRUE){
    path <- setdiff(self$bibfiles, self$bibs_loaded)[1L]
    if( is.na(path) ) return(FALSE)
    library(bibtex)
    newbibs <- read.bib2(file = path)
    self$bibs <- if( !length(self$bibs) ) newbibs else c(self$bibs, newbibs)
    self$bibs_loaded <- c(self$bibs_loaded, path)
    TRUE
  },
  
  # fetch bibitem from key
  get_bib = function(key, block = NULL){
    
    hit <- setNames(rep(NA_integer_, length(key)), key)
    while( anyNA(hit) ){
#      bibkeys <- sapply(self$bibs, function(x) attr(x, 'key'))
      bibkeys <- names(self$bibs)
      hit[key] <- match(key, bibkeys)
      if( !self$load_bib() ) break
    }
    
    if( anyNA(hit) ){
      msg <- sprintf("Could not find bib entry for key(s) %s", paste(names(hit)[is.na(hit)], collapse = ', '))
      if( !is.null(block) ) block_warning(block, msg)
      else warning(msg)
    }
    
    self$bibs[names(hit)[!is.na(hit)]]
  },
  
  format_cite = function(key, short = TRUE, ...){
    # load bibitem
    res <- setNames(key, key)
    bibitems <- self$get_bib(key, ...)
    if( !length(bibitems) ) return(res)
    
    # add bibitems to set of used bibitems for final output in package REFERENCES.bib
    if( !length(self$bibitems) ) self$bibitems <- bibitems
    else self$bibitems <- c(self$bibitems, bibitems[setdiff(names(bibitems), names(self$bibitems))])
  
    # format accordingly
    if( !short ){
      res[names(bibitems)] <- format(bibitems)
      res
    }else{
      res[names(bibitems)] <- sapply(bibitems, function(x){
            if( length(x$author$family) <= 1L ) 
              paste(x$author$family, '(', x$year, ')', sep='')				
            else{
              paste(x$author$family[[1]], ' et al. (', x$year, ')', sep='')
            }
          })
      res
    }
  }
  
))

#format_cite <- memoise::memoise(function(x, short = TRUE, ...){
#  
#  bibs <- load_bibliography()
#  k <- sapply(bibs, function(x) x$key)
#  mk <- match(key, k)
#  sel <- mk[!is.na(mk)]
#  if( !length(sel) ) return("")
#  if( !short ){
#    paste(format(bibs[sel]), collapse="\n\n")
#  }else{
#    sapply(bibs[sel], function(x){
#          if( length(x$author$family) <= 1L ) 
#            paste(x$author$family, '(', x$year, ')', sep='')				
#          else{
#            paste(x$author$family[[1]], ' et al. (', x$year, ')', sep='')
#          }
#        })
#  } 
#})

read.bib2 <- function (file = findBibFile(package), package = "bibtex", encoding = "unknown", 
    header = if (length(preamble)) paste(preamble, sep = "\n") else "", 
    footer = "") 
{
  if (!is.character(file)) {
    stop("'read.bib' only supports reading from files, 'file' should be a character vector of length one")
  }
  srcfile <- switch(encoding, unknown = srcfile(file), srcfile(file, 
          encoding = encoding))
  out <- .External("do_read_bib", file = file, encoding = encoding, 
      srcfile = srcfile)
  at <- attributes(out)
  if ((typeof(out) != "integer") || (getRversion() < "3.0.0")) 
    out <- lapply(out, make.bib.entry)
  else out <- list()
  preamble <- at[["preamble"]]
  out <- make.citation.list(out, header, footer)
  attr(out, "strings") <- at[["strings"]]
  keys <- lapply(out, function(x) attr(x, "key"))
  names(out) <- keys
  out
}
environment(read.bib2) <- asNamespace('bibtex')
