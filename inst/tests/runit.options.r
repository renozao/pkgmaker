# Unit tests for options
# 
# Author: Renaud Gaujoux
# Creation: 25 Apr 2012
###############################################################################

test.option_link <- function(){
	
	opt <- setupPackageOptions(a=1,b=2,c=option_symlink('a'),d=4)
	
	.test <- function(msg){
		checkIdentical(names(opt$options('a')), 'a', paste(msg, " - options: name of target is ok"))
		checkIdentical(names(opt$options('c')), 'c', paste(msg, " - options: name of link is ok"))
		checkIdentical(opt$options('c'), setNames(opt$options('a'), 'c'), paste(msg, " - options: link ok"))
		checkIdentical(opt$getOption('a'), opt$getOption('c'), paste(msg, " - getOption: link ok"))
	}
	
	.test('Default')
	opt$options(a=100)
	.test('After setting target')
	opt$options(c=50)
	.test('After setting link')

}
