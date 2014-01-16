#' Convert tm corpus to vector of strings.
#'
#' @description
#' A utility function useful for testing and some dirty hacks.
#' This is because the tm package doesn't leave vector corpora of 
#' strings alone anymore.  
#' 
#" The textreg package really only works for lists of strings at this time,
#' and so sometimes you need to convert your tm object to a string vector
#' for various reasons, the 
#' main one being handing it to the C++ method.  It is ugly,
#' but so it goes.
#'
#' It is therefore a possibly better decision to pass a filename to a plain-text file
#' to the textreg call to be loaded by C++ directly.
#' See \code{\link{textreg}}.
#'
#' @import tm
#' @import NLP
#' @param corpus The tm corpus to convert.
#' @return vector of character.
#' @export
convert.tm.to.character = function( corpus ) {
	cp = sapply( corpus, function(x) { content(x) } )
	cp
}



#' Driver function for the C++ function.
#' 
#' Given a labeling and a corpus, find phrases that predict this labeling.
#' Generally you should use the textreg method, not this method.
#'
#' Warning: do not call directly.  Use textreg instead
#'
#' @param corpus A list of strings or a corpus from the \code{tm} package.
#' @param labeling A vector of +1/-1 or TRUE/FALSE indicating which documents are considered relevant and
#'     which are baseline.  The +1/-1 can contain 0 whcih means drop the document.
#' @param banned  List of words that should be dropped from consideration.
#' @param params List of parameters to pass to the call.
#' @seealso textreg, find_C_threshold
cpp_textreg <- function(corpus, labeling, banned=NULL, params ) {


	params["token.type"] = params["token.type"] == "word"
	params["traversal.strategy"] = params["traversal.strategy"] == "BFS"
	params$corpus = NULL
	params$labeling = NULL
	params$banned = NULL
	params$traversal.strategy = "BFS"
	
	if ( is.logical( labeling ) ) {
		labeling = 2 * labeling - 1
	}
	
	if ( sum( labeling == -1 ) == 0 && sum( labeling == 0 ) > 0 ) {
		warning( "Changing labeling to +1/-1 from 1/0.  Try passing logical vector instead." )
		labeling = 2 * labeling - 1
	}
	
	if ( length(banned) == 0 ) {
		banned = NULL
	}
	# if ( is.null( banned ) ) {
		# banned = c() #c("xxxxzzzxxx")
	# }
	
	if ( "Corpus" %in% class(corpus) ) {
		corpus = convert.tm.to.character( corpus )
	}

	stopifnot( is.character( corpus ) )
		
	stopifnot( is.numeric( labeling ) )
			
    ## Make the call...
    val <- .Call("textreg", corpus, labeling, banned, params, PACKAGE="textreg")
    
    val
}





#' Sparse regression of labeling vector onto all phrases in a corpus.
#' 
#' Given a labeling and a corpus, find phrases that predict this labeling.  This function
#' calls a C++ function that builds a tree of phrases and searches it using greedy coordinate
#' descent to solve the optimization problem associated with the associated sparse regression.
#'
#' See the bathtub vignette for more complete discussion of this method and the options 
#' you might pass to it.
#'
#' @export
#' @param corpus A list of strings or a corpus from the \code{tm} package.
#' @param labeling A vector of +1/-1 or TRUE/FALSE indicating which documents are considered relevant and
#'     which are baseline.  The +1/-1 can contain 0 whcih means drop the document.
#' @param banned  List of words that should be dropped from consideration.
#' @param C  The regularization term.  0 is no regularization.
#' @param a  What percent of regularization should be L1 loss (a=1) vs L2 loss (a=0)
#' @param maxIter Number of gradient descent steps to take (not including intercept adjustments)
#' @param verbosity Level of output.  0 is no printed output.
#' @param step.verbosity Level of output for line searches.  0 is no printed output.
#' @param positive.only Disallow negative features if true
#' @param positive.weight Scale weight pf all positively marked documents by this value.  (1, i.e., no scaling) is default)   NOT FULLY IMPLEMENTED
#' @param binary.features Just code presence/absence of a feature in a document rather than count of feature in document.
#' @param Lq Rescaling to put on the features (2 is standard).  Can be from 1 up.  Values above 10 invoke an infinity-norm.
#' @param min.support Only consider phrases that appear this many times or more.
#' @param min.pattern Only consider phrases this long or longer
#' @param max.pattern Only consider phrases this short or shorter
#' @param convergence.threshold How to decide if descent has converged.  (Will go for three steps at this threshold to check for flatness.)
#' @param objective.function 2 is hinge loss.  0 is something.  1 is something else.
#' @param gap Allow phrases that have wildcard words in them.  Number is how many wildcards in a row.
#' @param token.type  "word" or "character" as tokens.
#' @return A \code{\link{textreg.result}} object.
#' @examples
#' data( testCorpora )
#' textreg( testCorpora$testI$corpus, testCorpora$testI$labelI, c(), C=1, verbosity=1 )
# [[Rcpp::export]]
textreg <- function(corpus, labeling, banned=NULL, 
				objective.function = 2,
				C = 1.0,
				a = 1.0,
				maxIter = 40,
				verbosity = 1,
				step.verbosity = verbosity,
				positive.only = FALSE,
				binary.features = FALSE,
				positive.weight = 1,
				Lq = 2,
				min.support = 1,
				min.pattern = 1,
				max.pattern = 100,
				gap = 0,
#				traversal.strategy="BFS",
				token.type="word",
				convergence.threshold=0.0001 ) {


	params = as.list(environment())
	params["findC"] = FALSE
	params["findCIter"] = 0
	
	val <- cpp_textreg( corpus, labeling, banned, params )
    
    val$model$ngram = as.character( val$model$ngram )
    
    rownames( val$model ) = val$model$ngram
    
    # if ( length(val$banlist) == 1 && val$banlist[[1]] == "xxxxzzzxxx" ) {
    	# val$banlist = c()
    # }
    
    class( val ) = "textreg.result"
    val
}








#' Conduct permutation test on labeling to get null distribution of regularization parameter.
#' 
#' First determines what regularization will give null model on labeling.  Then permutes labeling
#' repeatidly, recording what regularization will give null model for permuted labeling.
#' This allows for permutation-style inference on the relationship of the labeling to the text, and
#' allows for appropriate selection of the tuning parameter.
#'
#' Important: use the same parameter values as used with the original textreg call!
#'
#' @inheritParams textreg
#'
#' @export
#' @param R  Number of times to scramble labling.  0 means use given labeling and find single C value.
#'
#' @return A list of numbers (the Cs) R+1 long.  The first number is always the C used for the _passed_ labeling.  The remainder are shuffles.
#' @examples
#' data( testCorpora )
#' find.threshold.C( testCorpora$testI$corpus, testCorpora$testI$labelI, c(), R=5, verbosity=1 )
find.threshold.C <- function(corpus, labeling, banned=NULL, 
				R = 0,
				objective.function = 2,
				a = 1.0,
				verbosity = 0,
				step.verbosity = verbosity,
				positive.only = FALSE,
				binary.features = FALSE,
				positive.weight = 1,				
				Lq = 2,
				min.support = 1,
				min.pattern = 1,
				max.pattern = 100,
				gap = 0,
				token.type="word",
				convergence.threshold=0.0001) {

	params = as.list(environment())
	params["C"] = 0
	params["findC"] = TRUE
	params["findCIter"] = R
	params["maxIter" ] = 0
	
	val <- cpp_textreg( corpus, labeling, banned, params )
				    
    as.numeric( val )
}









#' Pretty print results of textreg regression.
#' 
#' You can also reformat an textreg.result to get simpler diagnostics via \code{\link{reformat.textreg.model}}.
#'
#' @export
#' @param x A textreg.result object.
#' @param ... No extra options passed.
#' @param simple TRUE means print out simpler results.  False includes some ugly detail.
#'
#' @seealso reformat.textreg.model
#' @family textreg.result
print.textreg.result = function( x, simple=FALSE, ... ) {
	
	cat( "textreg Results\n" )
	with( x$notes, {
		cat( "    C = ", C, " a = ", a, " Lq = ", Lq, "\n", sep="" )
		cat( "    min support = ", min.support, "  phrase range = ", min.pattern, "-", max.pattern, " with up to ", gap, " gaps.", sep="" )
		if ( binary.features ) {
			cat( "  (binary features)" )
		}
		if ( positive.only ) {
			cat( "  (positive terms only)" )
		} 
		if ( positive.weight != 1 ) {
			cat( "   (positive documents rescaled by ", positive.weight, ")", sep="" )
		}		
		cat( "\n    itrs: ", iter, "/", maxIter, "\n" )
	} )
	
	cat( "\nBanned phrases: '", paste( x$banlist, collapse="', '" ), "'\n", sep="" )
	
	cat( "\nLabel count:" )
	print( table( x$labeling ) )
	cat( "\nFinal model:\n" )
	if ( simple ) {
		clean.mod = reformat.textreg.model( x )
		print( clean.mod, row.names=FALSE )
	} else {
		print( x$model, row.names=FALSE )
	}
	invisible( x )	
}




#' Is object a textreg.result object?
#'
#' @export
#' @aliases textreg.result
#' @param x the object to check.
#' @family textreg.result
is.textreg.result = function( x ) {
	inherits(x, "textreg.result")
}






