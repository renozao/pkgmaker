# Logging system
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################

#' @include utils.R options.R
NULL

#' Logging Feature
#' 
#' \code{lverbose} returns/sets the current verbosity level.
#' 
#' @param val logical/numeric value that sets the verbosity level.
#' @param global logical that indicates if the verbose level of 
#' all log handlers should be set to \var{val}.
#' 
#' @return the old verbose level   
#' @export
#' @rdname logging
lverbose <- local({
      .val <- as.integer(FALSE)
      function(val, global = FALSE){
        if( missing(val) ) return( max(getOption('pkg_verbose_level', 0L), .val) )
        oval <- .val
        .val <<- max(as.integer(val), 0)
	# set value globally
	if( global ){
	  options(pkg_verbose_level = val)
	}
        invisible(oval)
      }
    })

#' \code{lsilent} tells if all verbose messages are silenced. 
#' @rdname logging
#' @export
lsilent <- function(){
	l <- lverbose()
	is.na(l) || l == 0L
}
#' \code{is.verbose} tells if verbosity is on, i.e. at level greater than 0.
#' @rdname logging
#' @export
is.verbose <- function(){
	l <- lverbose()
	!is.na(l) && l >= 0L
}

#' \code{lmessage} prints out a message (on sdtout) if the verbosity level is greater than a 
#' given value. 
#' 
#' @param ... parts of a character message that are concatenated and passed to 
#' the current logger's write function.
#' @param level verbosity level threshold (numeric value) above which the 
#' message should be printed out. 
#' This threshold is compared with the current verbosity level as returned by 
#' \code{lverbose}.
#' @param appendLF logical indicating if an endline character should be appended 
#' at the end of the message.
#' @param sep separation character, used when concatenating all arguments in \code{...}.
#' @param force logical that indicates if one should output messages or return a non 
#' null logger, even if the verbose mode is not high enough.
#' 
#' @rdname logging
#' @export 
lmessage <- function(level, ..., appendLF=TRUE, sep='', force=FALSE){
  getLogger(force=force)$lmessage(level, ..., appendLF=appendLF, sep=sep, force=force)
}

#' \code{vmessage} prints out a log message (at level 1) using the current logger, 
#' typically on stdout.
#' It is a shortcut for \code{lmessage(1L, ...)}.
#'  
#' @rdname logging  
#' @export
vmessage <- function(...){
  lmessage(1L, ...)
}

#' \code{log_append} directly appends some message to the current log line.
#' @rdname logging
#' @export 
log_append <- function(...){
  if( !is.null(w <- getLogger(new=FALSE)$write) ) w(...)
}

# return a list of data about a given frame (i.e. caller function)
callInfo <- function(n=-1L){
  if( n<0L ) n <- n-1 
  f <- sys.function(n)
  e <- sys.frame(n)
  sysn <- if( n<0L ) sys.nframe()-n else n
  list(fd=digest(f), env=capture.output(e), nframe=sysn)
}

log_lastLF <- sVariable(list(Inf, TRUE))

.LOG_OPTIONS <- setupPackageOptions(NAME='logger', RESET=TRUE,
    # should a the next logger be indented
    autoindent = TRUE
)
logger.options <- .LOG_OPTIONS$options
logger.getOption <- .LOG_OPTIONS$getOption

getLogger <- local({
      
      # store info for top call
      .topCall <- NULL
      # list of logger objects
      .lastLogger <- list()
      
      function(..., type='STDOUT', new=TRUE, force=FALSE){
        
        # return NULL logger if not in verbose mode
        if( !force && !lverbose() ) return( new_logger('NULL') )
        
        # top caller
        call1 <- sys.call(1)
        topCall <- callInfo(1L)
        caller <- callInfo(-1L)
        # reset all static variables
        if( !identical(.topCall, topCall) ){
          .topCall <<- topCall
          .lastLogger <<- list()
        }
        
        ## build logger object
        if( !length(.lastLogger) ){ # new stored for future calls
          logger <- new_logger(type, ...)
          logger$caller(caller)
          .lastLogger[[as.character(caller$nframe)]] <<- logger
        }else{ # increment indentation
          
          autonew <- missing(new)
          if( new ){ # get new logger object
            last <- getLogger(new=FALSE)
            if( !autonew || (!is.null(last$caller()) && last$caller()$nframe < caller$nframe) ){
              # instanciate a new logger object only if the current caller is lower in the stack
              logger <- new_logger(type, ...)
              logger$caller(caller)
              # auto indent if last logger is higher in the stack
              if( !logger.getOption('autoindent') ){
                # reset autoindent option on.exit
                on.exit( logger.options(autoindent=TRUE), add=TRUE)
                logger$nindent(last$nindent(), add=FALSE)
              } else if( last$caller()$nframe < caller$nframe ){
                logger$nindent(last$nindent() + 1L, add=FALSE)
              }
              .lastLogger[[as.character(caller$nframe)]] <<- logger
            }else logger <- last
            # add initial new line if the last log message was from a higher logger
            logger$breaknext(log_lastLF()[[1L]] < caller$nframe)
          }else{
            # get logger for exact frame number 
            if( is.null(logger <- .lastLogger[[as.character(caller$nframe)]]) ){
              # get first higher logger
              i <- which(as.numeric(names(.lastLogger)) <= caller$nframe)
#					str(.lastLogger)
              if( length(i) )	logger <- .lastLogger[[tail(i, 1L)]]
              else logger <- .lastLogger[[1L]]
              
            }
          }
        }
        
        # return logger
        logger
      }
    })


new_logger <- function(type, ..., nindent=0L, indent=' '){
  
  .data <- list(
      lastLF = TRUE
      , breaknext = FALSE
      , cindent = indent
      , nindent = nindent
      , indent = rep(indent, nindent)
      , caller = NULL
  )
  
  # init logger object
  .logger <- list()
  
  .logger$data <- function(value){
    if( missing(value) ) .data
    else .data <<- value
  }
  
  # init type-specific slots
  f <- match.fun(paste('new_logger', type, sep=''))
  .logger <- f(.logger, ...)
  
  # get last used appendLF value
  .logger$lastLF <- function(){ .data$lastLF }
  
  # get initial new line flag
  .logger$breaknext <- function(val){
    if( missing(val) ) .data$breaknext
    else .data$breaknext <<- val
  }
  
  # get caller data
  .logger$caller <- function(val){
    if( missing(val) ) .data$caller
    else .data$caller <<- val
  }
  
  # get/set indentation
  .logger$indent <- function(val){
    if( missing(val) ) .data$indent
    else{
      if( is.numeric(val) ) return( .logger$nindent(val) )
      old <- .data$indent
      .data$indent <<- val
      old
    }
  }
  # get/set number of indentation
  .logger$nindent <- function(val, add=TRUE){
    if( missing(val) ) .data$nindent
    else{
      old <- .data$nindent
      .data$nindent <<- val + if( add ) .data$nindent else 0L
      if( .data$nindent >= 0L )
        .data$indent <<- paste(rep(.data$cindent, .data$nindent), collapse='')
      old
    }
  }
  
  # new message with auto-indentation and breakline
  .logger$message <- function(..., appendLF=TRUE, sep=''){
    
    if( is.null(.logger$write) ) return()
    
    msg <- if( .data$breaknext || isTRUE(logger.getOption('breaknext')) ){
      logger.options(breaknext=NULL)
      .data$breaknext <<- FALSE
      "\n"
    }
    # add indentation if on a new line
    lastLFframe <- log_lastLF()
    callerFrame <- .logger$caller()$nframe
    if( .data$lastLF || lastLFframe[[2L]] || (is.finite(lastLFframe[[1L]]) && lastLFframe[[1L]] >= callerFrame) )
      msg <- paste(msg, .data$indent, sep='')
    msg <- paste(msg, paste(..., sep='', collapse=sep), if( appendLF ) "\n", sep='')
    
    # store in global variable the frame number if it the line is left open
    log_lastLF(list(if( !appendLF ) callerFrame else Inf, appendLF))
    .data$lastLF <<- appendLF
    
    # call logger write function
    .logger$write(msg)
  }
  
  # log message with specific level
  .logger$lmessage <- function(level, ..., force=FALSE){
    if( force || lverbose() >= level ) .logger$message(...)
  }
  
  # show info about the logger
  .logger$show <- function(){
    cat("<Type: ", class(.logger)[1L], ">\n", sep='')
    .logger$info()
  }
  
  # wrap into a logger object
  structure(.logger, class=c(paste('logger', type, sep=''), 'logger'))
}

# NULL logger
new_loggerNULL <- function(.logger, ...){
  
  # write to log
  .logger$write <- NULL
  # special info
  .logger$info <- function(){
    cat("Output: NULL\n")
  }
  # return logger object
  .logger
}

# Logger that writes on the standard output
new_loggerSTDOUT <- function(.logger, ...){
  
  # append to log
  .logger$write <- function(...){
    cat(..., sep='')
  }
  
  .logger$info <- function(){
    cat("Output: stdout\n")
  }
  
  # return logger object
  .logger
  
}

# Logger that writes on the standard error
new_loggerSTDERR <- function(.logger, ...){
  
  # append to log
  .logger$write <- function(...){
    message(..., appendLF=FALSE)
  }
  
  .logger$info <- function(){
    cat("Output: stderr\n")
  }
  
  # return logger object
  .logger
  
}

#' General Log Formating
#' 
#' Generate a formatted diagnostic message.
#' This function is a shortcut for \code{message(sprintf(...))}.
#' 
#' @inheritParams base::sprintf
#' @inheritParams base::message
#' 
#' @seealso \code{\link{sprintf}}, \code{\link{message}}
#' @export
#' @examples 
#' 
#' messagef("Hello %s number %i", 'world', 4)
#' 
messagef <- function(fmt, ..., domain = NULL, appendLF = TRUE) message(sprintf(fmt, ...), domain = domain, appendLF = appendLF)

#' @describeIn messagef throws a simple note as an immediate warning.
#' It is a shorcut for `warning(..., immediate. = TRUE, call. = FALSE)`.
#' 
#' @inheritParams base::warning
#' @export
wnote <- function(..., immediate. = TRUE){
  warning(..., immediate. = immediate., call. = FALSE)
}


#' Simple Text Iteration Counter
#' 
#' @param n number of total steps
#' @param i0 starting step
#' @param title character string to use as title
#' @param extra character vector providing extra text to add at each step
#' @param verbose logical that toggles the counter
#' 
#' @export
#' @examples 
#' 
#' progress <- iterCount(LETTERS)
#' sapply(LETTERS, function(x){
#'  sleep(.1)
#' 	progress()
#' })
#' # terminate counter
#' progress(NULL) 
#' 
iterCount <- function(n=100, i0=0, title='Iterations', extra = NULL, verbose = TRUE){
  
  if( !verbose ) return( function(...) NULL )
  
  if( length(n) > 1L ){
    extra <- extra %||% n
    n <- length(n)
  }
  
  # setup
  size_i <- nchar(as.character(n))
  if( nzchar(title) ) title <- paste0(title, ': ')
  .msg_fmt <- sprintf("%s%%%ii/%i", title, size_i, n)
  
  inc_msg <- function(i, addon = extra[i]){
    if( is.null(addon) || is.na(addon) ) sprintf(.msg_fmt, i)
    else paste0(sprintf(.msg_fmt, i, addon))
  }
  
  .cat_msg <- function(...){
    cat(...)
    flush.console()
  } 
  
  if( !is.null(extra) ){
    .msg_fmt <- paste0(.msg_fmt, " (%s)")
    .msg_size <- inc_msg(n, extra[which.max(nchar(extra))])
  }else .msg_size <- inc_msg(n)
  
  .msg_size <- nchar(.msg_size)
  .back <- paste0(rep("\b", .msg_size), collapse ='')
  .i <- i0
  .cat_msg(inc_msg(i0))
  
  # increment function
  function(i, appendLF = i == n){
    if( missing(i) ) i <- .i
    else if( is.null(i) ){
      if( .i < n ) cat("\n")
      return()
    }
    .cat_msg(paste0(.back, inc_msg(i)))
    if( appendLF ) cat("\n")
    .i <<- i+1
  }
}
