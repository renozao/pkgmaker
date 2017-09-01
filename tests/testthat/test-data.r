# Unit test for data utilities
# 
# Author: Renaud Gaujoux
###############################################################################

context('Data utilities')

test_that('unlist_with_sep', {
      
  l1 <- list(X = list(a = 1, b = list(b.1 = 2, b.2 = list(b.2.1 = 4, b.2.2 = data.frame()), b.3 = 3), c = matrix()))
  ul1 <- list(`X/a` = 1
      , `X/b/b.1` = 2, `X/b/b.2/b.2.1` = 4, `X/b/b.2/b.2.2` = data.frame()
      , `X/b/b.3` = 3
      , `X/c` = matrix())
  
  # defaults
  expect_identical( unlist_with_sep(l1), ul1, "List is correctly flatten")
  expect_identical( unlist_with_sep(matrix()), matrix(), "Result is identical to input for non list objects")
  # depth
  expect_identical( unlist_with_sep(l1, depth = 0L), l1, "Result is identical to input for depth = 0L")
  expect_identical( unlist_with_sep(l1, depth = 1L), l1, "Result is identical to input for depth = 1L")
  ul_depth <- list(`X/a` = 1, `X/b` = list(b.1 = 2, b.2 = list(b.2.1 = 4, b.2.2 = data.frame()), b.3 = 3), `X/c` = matrix())
  expect_identical( unlist_with_sep(l1, depth = 2L), ul_depth, "Result is correct for depth = 2L")
  ul_depth2 <- list(`X/a` = 1, `X/b/b.1` = 2, `X/b/b.2` = list(b.2.1 = 4, b.2.2 = data.frame()), `X/b/b.3` = 3, `X/c` = matrix())
  expect_identical( unlist_with_sep(l1, depth = 3L), ul_depth2, "Result is correct for depth = 2L")
  # sep
  ul2 <- ul1
  names(ul2) <- gsub("/", "#", names(ul2))
  expect_identical( unlist_with_sep(l1, sep = "#"), ul2, "Separator argument is honoured")
  # use.names
  ul_numbered <- ul1
  names(ul_numbered) <- c('1/1', '1/2/1', '1/2/2/1', '1/2/2/2', '1/2/3', '1/3')
  expect_identical( unlist_with_sep(l1, use.names = FALSE), ul_numbered, "Argument use.names works correctly")
  # levels with empty names are corrected
  l_empty <- l1
  names(l_empty$X$b) <- NULL
  ul_empty <- ul1
  names(ul_empty) <- c('X/a', 'X/b/1', 'X/b/2/b.2.1', 'X/b/2/b.2.2', 'X/b/3', 'X/c')
  expect_identical( unlist_with_sep(l_empty), ul_empty, "Empty intermediate names are filled with incremental numbers")
  # check error on duplicated levels
  l2 <- l1
  names(l2$X)[2] <- 'c'
  expect_error( unlist_with_sep(l2), "Invalid names at level 2.*'c'") 

})

