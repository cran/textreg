

# simple tests for textreg package
library( testthat )
library( textreg )


context( "test binary.features" )

test_that("binary.features works (intercept)", {
	data( testCorpora )
	testI = testCorpora$testI
	
	res = textreg( testI$corpus, testI$labelI, c("frog","goat","bat"), C=100, binary.features=TRUE, verbosity=0 )	
	res = res$model
	expect_equal( res$ngram[[1]], "*intercept*" )
	expect_equal( res$support, 14 )
	expect_equal( res$totalDocs, 14 )
	expect_equal( res$totalDocs, 14 )
	expect_equal( res$posCount, 4 )
	expect_equal( res$negCount, 10 )
#	expect_equal( res$negWordCount, 10 )
#	expect_equal( res$posWordCount, 4 )
	expect_equal( res$beta, -12/28 )
	expect_equal( res$Z, 1 )
} )

test_that( "binary zero intercept included", {
	testIII = rep( "frog", 100 )
	res = textreg( testIII, rep( c(1,-1), 50 ), c(), binary.features=TRUE, verbosity = 0 )
	res = res$model
	expect_equal( res$beta[[1]], 0 )
	expect_output( res, "*intercept*" )
} )

test_that( "binary all positive frogs", {
	testIII = rep( "frog", 100 )
	res = textreg( testIII, rep( c(1), 100 ), c(), binary.features=TRUE, verbosity = 0 )
	res = res$model
	#expect_equal( nrow(res), 1 )
	expect_true( res$beta[[1]] >= 1.0  )  # intercept should be at least 1
} )


test_that( "binary negative betas", {
	data( testCorpora )
	test = testCorpora$testI
	test
	
	res = textreg( test$corpus, test$labelI, c(), C=0.5, a=1, maxIter=2145, verbosity=0, binary.features=TRUE )
	res
	expect_equal( res$model$Z, c(1,2 ) )
	expect_equal( res$model$totalDocs, c(14,4) )
	
	
	res = textreg( c( "A", "C A A", "A B A", "A A B A A C", "A A B A C A C A" ), c(1,1,1,-1,-1), c(), binary.features=TRUE, C=0, verbosity=0 )
	expect_equal( predict( res ), c(1,1,1,-1,-1), tolerance=0.00001 )
	expect_equal( as.numeric( calc.loss( res ) ), c(0,0,0) )
	expect_equal( res$model$Z, c( 1, sqrt(2) ), tolerance=0.00001 )
	expect_equal( res$model$totalDocs, c(5, 2 ) )
	expect_equal( res$model$support, c(5, 3 ) )

} )
