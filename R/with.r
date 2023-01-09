# Copied from devtools to make these functions independent of devtools
# Function names were simply changed to using_* to avoid conflict
#
# Copyright Hadley Wickam 2015

#' Execute code in temporarily altered environment.
#' 
#' These functions were extracted from the \pkg{devtools} package 
#' to make them available without a dependency to \pkg{devtools}.
#'
#' \itemize{
#'   \item \code{using_dir}: working directory
#'   \item \code{using_collate}: collation order
#'   \item \code{using_envvar}: environmental variables
#'   \item \code{using_libpaths}: library paths, replacing current libpaths
#'   \item \code{using_lib}: library paths, prepending to current libpaths
#'   \item \code{using_locale}: any locale setting
#'   \item \code{using_options}: options
#'   \item \code{using_path}: PATH environment variable
#'   \item \code{using_par}: graphics parameters
#' }
#' @section Deprecation:
#' \code{using_env} will be deprecated in devtools 1.2 and removed in
#' devtools 1.3
#'
#' @param new values for setting
#' @param code code to execute in that environment
#'
#' @return Nothing, used for side effect.
#' 
#' @author Hadley Wickham
#' @name using_something
#' @examples
#' getwd()
#' using_dir(tempdir(), getwd())
#' getwd()
#'
#' Sys.getenv("HADLEY")
#' using_envvar(c("HADLEY" = 2), Sys.getenv("HADLEY"))
#' Sys.getenv("HADLEY")
#'
#' using_envvar(c("A" = 1),
#'   using_envvar(c("A" = 2), action = "suffix", Sys.getenv("A"))
#' )
NULL

using_something <- function(set) {
  function(new, code) {
    old <- set(new)
    on.exit(set(old))
    force(code)
  }
}
is.named <- function(x) {
  !is.null(names(x)) && all(names(x) != "")
}

# env ------------------------------------------------------------------------

set_envvar <- function(envs, action = "replace") {
  if (length(envs) == 0) return()

  stopifnot(is.named(envs))
  stopifnot(is.character(action), length(action) == 1)
  action <- match.arg(action, c("replace", "prefix", "suffix"))

  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)

  both_set <- set & !is.na(old)
  if (any(both_set)) {
    if (action == "prefix") {
      envs[both_set] <- paste(envs[both_set], old[both_set])
    } else if (action == "suffix") {
      envs[both_set] <- paste(old[both_set], envs[both_set])
    }
  }

  if (any(set))  do.call("Sys.setenv", as.list(envs[set]))
  if (any(!set)) Sys.unsetenv(names(envs)[!set])

  invisible(old)
}
#' @rdname using_something
#' @param action (for \code{using_envvar} only): should new values
#'    \code{"replace"}, \code{"suffix"}, \code{"prefix"} existing environmental
#'    variables with the same name.
#' @export
using_envvar <- function(new, code, action = "replace") {
  old <- set_envvar(new, action)
  on.exit(set_envvar(old, "replace"))
  force(code)
}

#' @rdname using_something
#' @export
using_env <- function(new, code) {
  message(
    "using_env() will be deprecated in devtools 1.3.\n",
    "Please use using_envvar() instead")
  using_envvar(new, code)
}

# locale ---------------------------------------------------------------------

set_locale <- function(cats) {
  stopifnot(is.named(cats), is.character(cats))

  old <- vapply(names(cats), Sys.getlocale, character(1))

  mapply(Sys.setlocale, names(cats), cats)
  invisible(old)
}

#' @rdname using_something
#' @export
using_locale <- using_something(set_locale)

# collate --------------------------------------------------------------------

set_collate <- function(locale) set_locale(c(LC_COLLATE = locale))[[1]]
#' @rdname using_something
#' @export
using_collate <- using_something(set_collate)

# working directory ----------------------------------------------------------

#' @rdname using_something
#' @export
using_dir <- using_something(setwd)

# libpaths -------------------------------------------------------------------

set_libpaths <- function(paths) {
  libpath <- normalizePath(paths, mustWork = TRUE)

  old <- .libPaths()
  .libPaths(paths)
  invisible(old)
}

#' @rdname using_something
#' @export
using_libpaths <- using_something(set_libpaths)

# lib ------------------------------------------------------------------------

set_lib <- function(paths) {
  libpath <- normalizePath(paths %||% .libPaths()[1L], mustWork = TRUE)

  old <- .libPaths()
  .libPaths(c(libpath, .libPaths()))
  invisible(old)
}

#' @rdname using_something
#' @export
using_lib <- using_something(set_lib)

# options --------------------------------------------------------------------

set_options <- function(new_options) {
  do.call(options, as.list(new_options))
}

#' @rdname using_something
#' @export
using_options <- using_something(set_options)

# par ------------------------------------------------------------------------

#' @rdname using_something
#' @export
using_par <- using_something(par)

# path -----------------------------------------------------------------------

#' @rdname using_something
#' @export
#' @param add Combine with existing values? Currently for
#'   \code{\link{using_path}} only. If \code{FALSE} all existing
#'   paths are overwritten, which you don't usually want.
#' @param prepend logical that indicates if the new paths should
#' be added in front of the current ones.
using_path <- function(new, code, add = TRUE, prepend = FALSE) {
  if (add){
      if( prepend ) new <- c(new, get_path())
      else new <- c(get_path(), new)
  }
  old <- set_path(new)
  on.exit(set_path(old))
  force(code)
}
    
# define local versions of devtools::set/get_path
get_path <- function(){
    strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]]
}

set_path <- function (path){
    path <- normalizePath(path, mustWork = FALSE)
    old <- get_path()
    path <- paste(path, collapse = .Platform$path.sep)
    Sys.setenv(PATH = path)
    invisible(old)
}

