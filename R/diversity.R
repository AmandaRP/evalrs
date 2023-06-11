#' Compute diversity
#'
#' Compute the diversity of a collection of recommendation lists.  `Diversity'
#' here is defined as aggregate pairwise dissimilarity, where the aggregation
#' function and the measure of dissimilarity are specified by the user via
#' the parameters named `agg_fun ' and `method'.
#'
#' @param x data.frame that comprises a column of predictions (identified
#' by \code{pred_col}), a column of users (identified by \code{user_col})
#' and a column of items (identified by \code{item_col})
#' @param k numeric scalar that specifies the length of recommendation list
#' @param method character scalar that specifies the measure of dissimilarity
#' to employ
#' @param agg_fun  function that aggregates the pairwise dissimilarities
#' @param no_aggregate logical scalar that specifies to return the
#' un-aggregated pairwise dissimilarities instead of a single aggregate value
#' @param pred_col string that identifies \code{x}'s column of predictions
#' @param user_col string that identifies \code{x}'s column of users
#' @param item_col string that identifies \code{x}'s column of items
#' @param ... arguments to pass to dissimilarity function
#'
#' @return numeric scalar if \code{no_aggregate} is FALSE; otherwise numeric
#' vector of length \eqn{n}-choose-two, where \eqn{n} is the number of unique
#' values in \code{x[[user_col]]}.
#'
#' @seealso \code{evalrs:::spearman_dist}
#'
#' @export
#'
#' @importFrom parallelDist parDist
#' @importFrom parallel mclapply
#' @importFrom stats setNames
#' @importFrom utils combn
#'
#' @examples
#' # Generate data.
#' set.seed(1L)
#' nuser <- 10L                        # number of unique users
#' nitem <- 50L                        # number of unique items
#' u <- paste0("u", seq_len(nuser))    # set of user IDs
#' i <- paste0("i", seq_len(nitem))    # set of item IDs
#' x <- expand.grid(pred = NA_real_, item = i, user = u)
#' x$pred <- unlist(replicate(nuser, { # predictions vector
#'     y <- runif(nitem)               # random values in [0,1]...
#'     y / sum(y)                      # ...that sum to one
#' }, simplify = FALSE))
#' k <- 5L                          # length of each recommendation list
#' # Compute diversity of data multiple ways for comparison.
#' measures <- c("jaccard", "spearman")
#' names(measures) <- measures
#' d <- lapply(measures, function (m)
#'     diversity(x, k = k, method = m, no_aggregate = TRUE))
#' # Plot diversity values.
#' boxplot(d, xlab = "measure", ylab = "diversity",
#'     main = "Diversity by measure of dissimilarity")
diversity <- function (x, k, method = c("jaccard", "spearman"),
	agg_fun  = mean, no_aggregate = FALSE,
	pred_col = "pred", user_col = "user", item_col = "item", ...)
{
	if (!all(c(pred_col, user_col, item_col) %in% names(x))) {
		stop("`pred_col', `user_col' and `item_col' ",
			"not all present in `x'")
	}
	method <- match.arg(method)
	dots <- list(...)
	pu <- split(setNames(x[[pred_col]], x[[item_col]]), x[[user_col]])
	ru <- mclapply(pu, function (p) {
		r <- rank(-p, ties.method = "random")
		r[r <= k]
	})
	div <- if (method == "jaccard") {
		uniq.items <- sort(unique(x[[item_col]]))
		ui <- do.call(rbind, mclapply(ru, function (r)
			setNames(uniq.items %in% names(r), uniq.items)))
		args <- c(list(x = ui, method = "binary"), dots)
		as.numeric(do.call(parDist, args))
	} else {
		stopifnot(method %in% c("spearman"))
		if (length(ru) < 2L) {
			return(0L)
		}
		xy <- combn(ru, 2L, simplify = FALSE)
		args <- lapply(xy, function (xy)
			c(setNames(xy, c("x", "y")), dots))
		if (method == "spearman") {
			d <- spearman_dist
			npe <- getOption("mc.cores")
			if(is.null(npe)){
			  npe <- 1
			}
		} #else {
			#d <- kendall_dist
			#npe <- 1L # kendall_dist does its own concurrency
		#}
	}
	if (!no_aggregate) {
		div <- agg_fun (div)
	}
	div
}

#' Estimate diversity from random samples of users
#'
#' Calculating the diversity of a recommendation system by way of
#' \code{evarls::diversity} can be onerous, as it entails \eqn{n\choose2}
#' comparisons of recommendation lists, where \eqn{n} is the number of users
#' for which the system has recommendations.
#' The present function serves
#' to reduce the computational burden by estimating the result from random
#' samples of users of a specified size.
#'
#' @param x data.frame that comprises a column of predictions (identified
#' by \code{pred_col}), a column of users (identified by \code{user_col})
#' and a column of items (identified by \code{item_col})
#' @param nrep number of times to compute diversity of recommendation system
#' for the sample of users of the specified size
#' @param maxuser maximum number of users to include in random samples;
#' samples of size \code{min(maxuser, n)} are used, where \code{n} is the
#' number of unique users found in \code{x}
#' @param user_col string that identifies \code{x}'s column of users
#' @param ... arguments passed to \code{evalrs::diversity}, some of which (such
#' as \code{k}) are required
#'
#' @return mean of the diversity values calculated from subsets of the data
#'
#' @seealso \code{evalrs::diversity}
#'
#' @export
#'
#' @examples
#' # Generate data.
#' set.seed(1L)
#' nuser <- 1000L                      # number of unique users
#' nitem <- 5000L                      # number of unique items
#' u <- paste0("u", seq_len(nuser))    # set of user IDs
#' i <- paste0("i", seq_len(nitem))    # set of item IDs
#' x <- expand.grid(pred = NA_real_, item = i, user = u)
#' x$pred <- unlist(replicate(nuser, { # predictions vector
#'     y <- runif(nitem)               # random values in [0,1]...
#'     y / sum(y)                      # ...that sum to one
#' }, simplify = FALSE))
#' k <- 5L                          # length of each recommendation list
#' # Estimate diversity from the default number of random samples of users of
#' # the default size.
#' diversity_sampled(x, k = k, method = "spearman")
diversity_sampled <- function (x, nrep = 3L, maxuser = 100L,
	user_col = "user", ...)
{
	dots <- list(...)
	dots$user_col <- user_col
	# Special care is required when taking subsets of `x' if
	# `x'[[`user_col']] is a factor to ensure that levels of the factor
	# that are absent from a given subset are not retained during the
	# diversity calcuation for that subset; here's an easy way to do so.
	x[[user_col]] <- as.character(x[[user_col]])
	U <- unique(x[[user_col]])
	n <- min(length(U), maxuser)
	div <- replicate(nrep, {
		u <- sample(U, n)
		ok <- x[[user_col]] %in% u
		args <- c(dots, x = list(x[ok,]))
		do.call(evalrs::diversity, args)
	})
	mean(div)
}
