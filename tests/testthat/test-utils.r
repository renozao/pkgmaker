# Unit test for utils
# 
# Author: Renaud Gaujoux
###############################################################################

context('Utilities')

library(stringr)

test_that('errorCheck', {
	
	f <- function(err=''){
		success <- exitCheck()
		on.exit( if(success()) cat("no error\n") else cat("with error\n") )
		
		if( err=='error' ) stop('There is an error')
		if( err=='try' ) try(stop('Catched error'), silent=TRUE)
		if( err=='tryCatch' ) tryCatch(stop('Catched error'), error = function(e){})
		
		success(1+1)
	}
	
	# without error
	out <- capture.output(res <- f())
	expect_identical(res, 2, 'If no error: return result')
	expect_identical(out, 'no error', 'If no error: correctly detected no error')
	
	# with error
	out <- capture.output(res <- try(f('error'), silent=TRUE))
	expect_true( is(res, 'try-error'), 'If error: effectively throws an error')
	expect_identical(out, 'with error', 'If error: correctly detected the error')
	
	# with try-caught error 
	out <- capture.output(res <- f('try'))
	expect_identical( res, 2, 'If try-catched error: return result')
	expect_identical(out, 'no error', 'If try-catched error: correctly detected no error')
	
	# with tryCatch-caught error 
	out <- capture.output(res <- f('tryCatch'))
	expect_identical( res, 2, 'If tryCatch-catched error: return result')
	expect_identical(out, 'no error', 'If tryCatch-catched error: correctly detected no error')
})


test_that('ExposeAttribute', {
	
	
	x <- 1:10
	expect_identical(ExposeAttribute(x), {attr_mode(x) <- 'rw'; x}
		, "Using ExposeAttribute() and attr_mode <- 'rw' is equivalent")
	x <- 1:10
	expect_identical(capture.output(print(ExposeAttribute(x, a='r', b='rw'))), capture.output(print(x))
		, "Printing object with exposed attribute is identical to plain print")

	checkSet <- function(x, name, msg, ...){
		attr(x, name) <- 1
		y <- ExposeAttribute(x, ...)
		eval(parse(text=str_c('y$', name, ' <- 1')))
		attr_mode(y) <- NULL 
		expect_identical(x, y, msg)
	}
	checkSetException <- function(x, name, msg, regexp, ...){
		y <- ExposeAttribute(x, ...)
		expect_error(eval(parse(text=str_c('y$', name, ' <- 1'))), regexp, info = msg)
	}
	
	checkSet(x, 'a', "Set works if default")
	checkSet(x, 'a', .MODE='rw', "Set works if all args are 'rw'")
	checkSet(x, 'a', a='rw', "Set works if specified arg is 'rw'")
	checkSet(x, 'a', a='w', "Set works if specified arg is 'w'")
	checkSet(x, 'a', a='rw', b='r', "Set works if specified arg is 'rw', even if others are not")
	checkSet(x, 'ab', ab='rw', `a.*`='r', "Set works if specified arg is 'rw', even if another match is not")
	checkSetException(x, 'a', .MODE='r', "Set throws an error if access right is 'r'", "Could not write attribute 'a'.*permission denied.*mode='r'")
	checkSetException(x, 'a', a='r', "Set throws an error if specific access right is 'r'", "Could not write attribute 'a'.*permission denied.*mode='r'")
	checkSetException(x, 'a', a='', "Set throws an error if specific access right is ''", "Could not write attribute 'a'.*permission denied.*mode=''")
	
	checkGet <- function(x, name, msg, ...){
		attr(x, name) <- 1
		y <- ExposeAttribute(x, ...)
		a <- eval(parse(text=str_c('y$', name)))
		expect_identical(attr(x, name), a, msg)
	}
	checkGetException <- function(x, name, msg, regexp, ...){
    attr(x, name) <- 1
		y <- ExposeAttribute(x, ...)
		expect_error(eval(parse(text=str_c('y$', name))), regexp, info = msg)
	}
	
	checkGet(x, 'a', "Get works if default")
	checkGet(x, 'a', .MODE='rw', "Get works if all args are 'rw'")
	checkGet(x, 'a', a='rw', "Get works if specified arg is 'rw'")
	checkGet(x, 'a', a='r', "Get works if specified arg is 'r'")
	checkGet(x, 'a', a='rw', b='w', "Get works if specified arg is 'rw', even if others are not")
	checkGet(x, 'ab', ab='r', `a.*`='w', "Get works if specified arg is 'rw', even if another match is not")
	checkGetException(x, 'a', .MODE='w', "Get throws an error if access right is only 'w'", "Could not access exposed attribute 'a'.*permission denied.*mode='w'")
	checkGetException(x, 'a', a='w', "Get throws an error if specific access right is only 'w'", "Could not access exposed attribute 'a'.*permission denied.*mode='w'")
	checkGetException(x, 'a', a='', "Get throws an error if specific access right is ''", "Could not access exposed attribute 'a'.*permission denied.*mode=''")
	
	
})


test_that('Sys.getenv_value', {
    
    on.exit( Sys.unsetenv('TOTO') )
    
    # undefined returns FALSE
    expect_identical(Sys.getenv_value('TOTO'), FALSE, 'undefined returns FALSE')
    # raw undefined returns NA
    expect_identical(Sys.getenv_value('TOTO', raw = TRUE), as.character(NA), 'raw undefined returns NA')
    
    Sys.setenv(TOTO='bla')
    expect_identical(Sys.getenv_value('TOTO'), 'bla', 'defined returns value')
    
    # anything false-like returns FALSE
    Sys.setenv(TOTO='false');
    expect_identical(Sys.getenv_value('TOTO'), FALSE, '"false" returns FALSE')
    Sys.setenv(TOTO='FALSE');
    expect_identical(Sys.getenv_value('TOTO'), FALSE, '"FALSE" returns FALSE')
    Sys.setenv(TOTO='0');
    expect_identical(Sys.getenv_value('TOTO'), FALSE, '"0" returns FALSE')
    
})


test_that('.str_bs', {
    
    expect_identical(str_bs("abcd"), "abcd", "No backspace returns string unchanged")
    expect_identical(str_bs("abcd\b"), "abc", "One backspace at the end is OK")
    expect_identical(str_bs("\babcd"), "abcd", "One leading backspace is OK")
    expect_identical(str_bs("abcd\b\b"), "ab", "Two backspaces at the end is OK")
    expect_identical(str_bs("abcd\b\b\b"), "a", "Three backspaces at the end is OK")
    expect_identical(str_bs("abcd\b\b\b\b"), "", "As many backspaces as characters at the end is OK")
    expect_identical(str_bs("abcd\b\be"), "abe", "Backspace in the middle is OK")
})

test_that("ldata", {
      
  # parameter check error
  expect_error(ldata(NA), "Invalid argument 'list':.* NULL .* character vector")
  expect_error(ldata(1L), "Invalid argument 'list':.* NULL .* character vector")
  for(v in list(1L, "", character()))
    expect_error(ldata(package = v), "Invalid argument 'package':.* NULL .* non-empty string")
  for(v in list(1L, c(TRUE, FALSE), logical()))
    expect_error(ldata(error = v), "Invalid argument 'error':.* single logical")
  for(v in list(1L, c(TRUE, FALSE), logical()))
    expect_error(ldata(simplify = v), "Invalid argument 'simplify':.* single logical")
  
  # load single data
  a <- ldata("iris", package = "datasets")
  expect_true(is.data.frame(a))
  e <- environment()
  expect_true(exists("iris", envir = e, inherit = FALSE), "Dataset is loaded in caller environment")
  expect_identical(e[["iris"]], a)
  expect_true(!exists("iris", envir = .GlobalEnv, inherit = FALSE), "Dataset is not loaded in .GlobalEnv")
  expect_error(ldata("blabla", package = "datasets"), "object 'blabla' not found")
  expect_identical(ldata("iris", package = "datasets", error = FALSE), a, "Data found returns correct data if error = FALSE")
  expect_identical(ldata("blabla", package = "datasets", error = FALSE), NULL, "Data not found returns NULL if error = FALSE")
  expect_identical(ldata("blabla", package = "datasets", error = FALSE, simplify = FALSE)
                      , list(blabla = NULL), "Data not found returns named list with NULL element if error = FALSE and not simplifying")
  expect_identical(ldata(c("blabla", "iris"), package = "datasets", error = FALSE)
                    , list(blabla = NULL, iris = a), "Some data not found with error = FALSE returns partially filled list")
  expect_identical(ldata(c("iris", "blabla"), package = "datasets", error = FALSE)
                    , list(iris = a, blabla = NULL), "Some data not found with error = FALSE returns partially filled list (order is honored)")
  
  # check that argument stringsAsFactors is honoured
  expect_true(is.factor(ldata("iris", package = "datasets", stringsAsFactors = TRUE)[["Species"]]) && 
                is.factor(e[["iris"]][["Species"]]))
  expect_true(is.character(ldata("iris", package = "datasets", stringsAsFactors = FALSE)[["Species"]]) && 
                is.character(e[["iris"]][["Species"]]))
})

