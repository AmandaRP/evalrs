#' Compute mean percentile-rank score.
#'
#' The mean percentile-rank score is defined here as a weighted mean of
#' percentile-ranks, which are calculated separately by user from the
#' \emph{negative} of the given predictions and are weighted by their
#' specified relevance.  Lower values are more desirable than higher values,
#' so that 0 is the best, 100 is the worst and 50 is the expected value for
#' predictions produced at random.
#'
#' @param x data-frame that has columns \code{pred_col}, \code{label_col}
#' and \code{user_col}
#' @param pred_col string that identifies column of \code{x} of predictions
#' @param label_col string that identifies column of \code{x} of relevance-
#' values
#' @param user_col string that identifies column of \code{x} of users
#'
#' @return numeric scalar in the interval \eqn{[0,100]}, where 0 is the best,
#' 100 is the worst and 50 is the expected value for predictions produced at
#' random
#'
#' @references
#' Hu Y. & Koren Y. & Volinsky C. (2008). Collaborative Filtering for Implicit
#' Feedback Datasets 8th IEEE International Conference on Data Mining, pp.
#' 263-272.
#'
#' @export
#'
#' @examples
#' # Generate data.
#' set.seed(1L)
#' nuser <- 10L                        # number of unique users
#' nitem <- 50L                        # number of unique items
#' u <- paste0("u", seq_len(nuser))    # set of user IDs
#' i <- paste0("i", seq_len(nitem))    # set of item IDs
#' x <- expand.grid(pred = NA_real_, label = NA_integer_, item = i, user = u)
#' x$pred <- unlist(replicate(nuser, { # predictions vector
#'     y <- runif(nitem)               # random values in [0,1]...
#'     y / sum(y)                      # ...that sum to one
#' }, simplify = FALSE))
#' x$label <- unlist(replicate(nuser, { # vector of binary relevance-values
#'     sample(c(1L, rep(0L, nitem - 1L)))
#' }, simplify = FALSE))
#' # Compute score.
#' mpr_score(x)
mpr_score <- function (x, pred_col = "pred", label_col = "label",
	user_col = "user")
{
	s <- split(-x[[pred_col]], x[[user_col]])
	pr. <- parallel::mclapply(s, pctl_rank)
	pr <- unsplit(pr., x[[user_col]])
	sum(pr * x[[label_col]]) / sum(x[[label_col]])
}
#' Compute percentile-ranks of the given sample.
#'
#' @param x vector
#'
#' @return vector of length \code{length(x)} of numbers in the interval
#' \eqn{[0,100]}
pctl_rank <- function (x)
{
	tf <- table(x)
	cf <- cumsum(tf)
	pr <- 100 * (cf - (0.5 * tf)) / length(x)
	as.numeric(pr[as.character(x)])
}
