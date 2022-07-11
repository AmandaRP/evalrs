#' Compute the mean reciprocal rank.
#'
#' The mean reciprocal rank is defined here as the arithmetic mean across
#' users of the reciprocal rank of their first relevant item.  Let \eqn{u}
#' be a user in the set of users \eqn{U}.  Let \eqn{I} be the set of items.
#' Let \eqn{r_u} be the reciprocal of the rank of the first relevant item as
#' per the ratings that \eqn{u} is predicted to assign to the items in \eqn{I}
#' (or \eqn{0} if there isn't any relevant item).  Then the mean reciprocal
#' rank is \eqn{1/|U|\sum_{u\in U}r_u.}
#'
#' Note that predictions are ranked so that higher values correspond to
#' lower ranks and thus to higher reciprocal ranks.  Therefore, higher mean
#' reciprocal ranks are more desirable than lower ones.
#'
#' @param x data.frame that has columns \code{pred_col}, \code{label_col}
#' and \code{user_col}
#' @param pred_col column of \code{x} of predicted ratings
#' @param label_col column of \code{x} of labels; non-zero values are assumed
#' to indicate relevance
#' @param user_col column of \code{x} of users
#'
#' @return numeric scalar in the interval \eqn{[1/|I|,1]}
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
#' mean_recip_rank(x)
mean_recip_rank <- function (x, pred_col = "pred", label_col = "label",
	user_col = "user")
{
	s <- split(x, x[[user_col]])
	rr <- unlist(parallel::mclapply(s, function (x) {
		r <- rank(-x[[pred_col]], ties.method = "random")
		i <- which(as.logical(x[[label_col]]))
		if (length(i) == 0L) 0L else 1 / r[i]
	}), use.names = FALSE)
	mean(rr)
}
