

# simple tests for textreg package
library( testthat )
library( textreg )
library( tm )

context( "making word lists and displays of word lists" )

test_that("making word tables works", {

	data( bathtub )
	mth.lab = meta(bathtub)$meth.chl

	rs = textreg( bathtub, mth.lab, c("methylene","chloride"), C = 3, a=1, gap=1, min.support = 5, verbosity=0, convergence.threshold=0.00001, maxIter=100 )
	rs

	rs2 = textreg( bathtub, mth.lab, c("methylene","chloride"), C = 4, a=1, gap=1, min.support = 5, verbosity=0, convergence.threshold=0.00001, maxIter=100 )
	rs2

	rs3 = textreg(  bathtub, mth.lab, c("methylene","chloride"), C = 3, a=1, gap=1, min.support = 5, verbosity=0, convergence.threshold=0.00001, maxIter=100, Lq=5 )
	rs3

	rs4 = textreg(  bathtub, mth.lab, c("methylene","chloride"), C = 50, a=1, gap=1, min.support = 5, verbosity=0, convergence.threshold=0.00001, maxIter=100, Lq=5 )
	rs4

	lst = list( rs, rs2, rs3, rs4 )
	
	tbl <- make.list.table( lst, model.names=c("C=3","C=4","Lq=5","Null"), topic="Testing Topic" )
	tbl
	expect_equal( attr(tbl,"topic"), "Testing Topic" )
	expect_equal( class(tbl), "data.frame" )
	expect_equal( names(tbl)[1:5], c("phrase","C=3","C=4","Lq=5","Null") )

	list.table.chart( tbl )


	tbl <- make.list.table( lst, annotate=FALSE)
	tbl
	expect_equal( ncol(tbl), 4 )
	expect_equal( nrow(tbl), nrow(lst[[3]]$model)-1 )
	
	# check that plotting doesn't crash when called
	list.table.chart( tbl )

	tbl <- make.list.table( lst, annotate=FALSE, method="word")
	tbl
	expect_equal( ncol(tbl), 4 )
	expect_equal( class(tbl[1,1]), "character" )
	
	tbl <- make.list.table( lst, annotate=TRUE, method="word")
	tbl
	expect_equal( nrow(tbl), nrow(lst[[3]]$model)-1 )

	expect_error( make.list.table( rs ) )

	# single table
	lst1 = list( rs2 )
	expect_error( make.list.table( lst1 ) )

	# weight method get right weights.
	tbl <- make.list.table( lst, annotate=FALSE, method="weight")
	tbl
	expect_equal( class(tbl), "matrix" )
	
	corrs = lst[[2]]$model$beta / lst[[2]]$model$Z
	names(corrs) = lst[[2]]$model$ngram
	corrs = corrs[-1] 
	expect_equal( sort( tbl[,2] ), sort(corrs) )

} )




