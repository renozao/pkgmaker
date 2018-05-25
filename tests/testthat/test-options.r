# Unit tests for options
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################

context('Package specific options')

test_that('setupPackageOptions', {
	
	opt <- setupPackageOptions(a=1,b=2,c=option_symlink('a'),d=4, RESET=TRUE)
	
	.test <- function(msg){
		expect_identical(names(opt$options('a')), 'a', paste(msg, " - options: name of target is ok"))
    expect_identical(names(opt$options('c')), 'c', paste(msg, " - options: name of link is ok"))
    expect_identical(opt$options('c'), setNames(opt$options('a'), 'c'), paste(msg, " - options: link ok"))
    expect_identical(opt$getOption('a'), opt$getOption('c'), paste(msg, " - getOption: link ok"))
	}
	
	.test('Default')
	opt$options(a=100)
	.test('After setting target')
	opt$options(c=50)
	.test('After setting link')

})

test_that('resetOptions', {
	opt <- setupPackageOptions(a=1,b=2,c=option_symlink('a'),d=4, RESET=TRUE)
	
	.checkOptions <- function(y, msg) expect_identical(opt$options(), y, msg)
	ref <- opt$options()
	# simple set
	opt$options(a=10)
	x <- ref
	x$a <- 10
	.checkOptions(x, 'change default option works')
	opt$resetOptions()
	.checkOptions(ref, 'default options are reset after resetOptions()')
	
	# new option
	opt$options(aaa=10)
	x <- ref
	x$aaa <- 10
	.checkOptions(x, 'add new option works')
	opt$resetOptions()
	.checkOptions(c(ref, aaa=10), 'new option kept after resetOptions()')
	opt$resetOptions(ALL=TRUE)
	.checkOptions(ref, 'all options are reset to default and new options are removed when ALL=TRUE')
	opt$options(a=20, b='c', d='toto', aaa=25)
	x <- ref
	x$a <- 20
	x$b <- 'c'
	x$d <- 'toto'
	x$aaa <- 25
	.checkOptions(x, '2 default options + 1 new are set correctly')
	opt$resetOptions('a', 'b')
	x$a <- 1
	x$b <- 2
	.checkOptions(x, 'only the specified options are reset to default when passed in argument')
	opt$resetOptions('bbb')
	.checkOptions(x, 'if some options are not present it does not affect the result')
	opt$resetOptions('aaa')
	x$aaa <- NULL
	.checkOptions(x, 'new options get removed')
	
})
