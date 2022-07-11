#' Compute novelty
#'
#' Compute the novelty of a collection of recommendation lists.  `Novelty'
#' here is defined as aggregate recommendation-list novelty, which in turn is
#' defined here for a given recommendation list as the aggregate proportion
#' of all users to whom the list's items are not recommended; the aggregation
#' function applied in both cases is specified by the user via the parameter
#' named `agg.fun'.  In other words, if \eqn{S} is the aforementioned
#' aggregation function then the recommendation-list novelty \eqn{s} of a
#' list \eqn{R} of \eqn{k} recommended items is computed as \eqn{s = S(1 -
#' p_1, 1 - p_2, \ldots, 1 - p_k),} where \eqn{p_i} for \eqn{i} in \eqn{1,
#' 2, \ldots, k} is the probability that item \eqn{i} of list \eqn{R}
#' is recommended to a randomly selected user, so that \eqn{p_i} is the
#' proportion of all users to whom item \eqn{i} is recommended; and so the
#' novelty of a collection \eqn{\mathcal{C}} of recommendation lists \eqn{R_1,
#' R_2, \ldots} is given by the aggregate \eqn{S(s_1, s_2, \ldots)} of the
#' corresponding recommendation-list novelty values \eqn{s_1, s_2, \ldots}.
#'
#' @param x data.frame that comprises a column of predictions (identified
#' by \code{pred_col}), a column of users (identified by \code{user_col})
#' and a column of items (identified by \code{item_col})
#' @param nrec numeric scalar that specifies the length of recommendation list
#' @param agg.fun function that aggregates the item non-recommendation
#' probabilities as well as (unless \code{no.aggregate} is \code{TRUE})
#' the recommendation-list novelty values
#' @param no.aggregate logical scalar that specifies to return the
#' recommendation-list novelty values instead of the result of applying
#' \code{agg.fun} thereto
#' @param pred_col scalar that identifies \code{x}'s column of predictions
#' @param user_col scalar that identifies \code{x}'s column of users
#' @param item_col scalar that identifies \code{x}'s column of items
#'
#' @return numeric scalar if \code{no.aggregate} is FALSE, otherwise
#' as many novelty values as there are unique users for whom there are
#' recommendations
#'
#' @export
#'
#' @examples
#'
#' # Quantify overall novelty of a recommendation system.
#' set.seed(1L)
#' nuser <- 5L                         # number of unique users
#' nitem <- 50L                        # number of unique items
#' k <- 5L                             # length of each recommendation list
#' u <- paste0("u", seq_len(nuser))    # set of user IDs
#' i <- paste0("i", seq_len(nitem))    # set of item IDs
#' x <- expand.grid(pred = NA_real_, item = i, user = u)
#' x$pred <- unlist(replicate(nuser, { # predictions vector
#'     y <- runif(nitem)               # random values in [0,1]...
#'     y / sum(y)                      # ...that sum to one
#' }, simplify = FALSE))
#' novelty(x, nrec = k)
#'
#' # More complex example: Plot grouped, un-aggregated novelty values
#' # on a single chart.
#' set.seed(1L)
#' ngroup <- 4L                            # number of groups
#' data <- replicate(ngroup, {
#'     nuser <- 5L                         # number of unique users
#'     nitem <- 50L                        # number of unique items
#'     k <- 5L                             # length of each rec. list
#'     u <- paste0("u", seq_len(nuser))    # set of user IDs
#'     i <- paste0("i", seq_len(nitem))    # set of item IDs
#'     x <- expand.grid(pred = NA_real_, item = i, user = u)
#'     x$pred <- unlist(replicate(nuser, { # predictions vector
#'         y <- runif(nitem)               # random values in [0,1]...
#'         y / sum(y)                      # ...that sum to one
#'     }, simplify = FALSE))
#'     novelty(x, nrec = k, no.aggregate = TRUE)
#' }, simplify = FALSE)
#' names(data) <- LETTERS[seq_along(data)] # group names
#' plot.new()
#' plot.window(xlim = grDevices::extendrange(seq_along(data)),
#'     ylim = grDevices::extendrange(unlist(data)))
#' title(main = "Novelty values by group", ylab = "novelty", xlab = "group")
#' invisible(lapply(seq_along(data), function (i)
#'     graphics::boxplot(data[[i]], at = i, add = TRUE)))
#' axis(1L, at = seq_along(data), labels = names(data))
novelty <- function (x, nrec, agg.fun = mean, no.aggregate = FALSE,
	pred_col = "pred", user_col = "user", item_col = "item")
{
	if (!all(c(pred_col, user_col, item_col) %in% names(x))) {
		stop("`pred_col', `user_col' and `item_col' ",
			"not all present in `x'")
	}
	preds.u <- split(x[[pred_col]], x[[user_col]])
	is.rec.u <- lapply(preds.u, function (p)
		rank(-p, ties.method = "random") <= nrec)
	is.rec <- unsplit(is.rec.u, x[[user_col]])
	x <- x[is.rec, , drop = TRUE]
	item.probs <- table(x[[item_col]]) / length(unique(x[[user_col]]))
	probs <- 1 - as.numeric(item.probs[as.character(x[[item_col]])])
	probs.u <- split(probs, x[[user_col]])
	nov <- vapply(probs.u, agg.fun, numeric(1L))
	if (!no.aggregate) {
		nov <- agg.fun(nov)
	}
	nov
}
