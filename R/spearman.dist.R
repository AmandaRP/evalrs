#' Compute Spearman's footrule distance with location parameter \eqn{L}
#'
#' Compute the distance between two rankings based Spearman's footrule metric
#' as proposed by Fagin et al. (2003) for top-k lists.  Spearman's footrule
#' metric with location parameter \eqn{L} yields the L1 distance between two
#' permutations after having modified them such that elements present in
#' exactly one of the two are assigned the value \eqn{L} in the other.
#'
#' If argument \code{normalize} equals \code{TRUE} then the Spearman's footrule
#' distance with location parameter \code{L} between arguments \code{x} and
#' \code{y} each of length \eqn{k} is mapped to the interval \eqn{[0,1]} by
#' dividing by \code{k * (2 * max(L, k + 1) - (k + 1))}.
#'
#' @param x named numeric vector of rankings of length \code{length(y)}
#' @param y named numeric vector of rankings of length \code{length(x)}
#' @param L numeric scalar to impute to missing elements
#' @param normalize logical scalar that indicates whether to map the result to
#' the interval [0,1]
#'
#' @return numeric scalar
#'
#' @references
#' Ronald Fagin, Ravi Kumar and D. Sivakumar. (2003). Comparing top k lists.
#' SIAM J. Discrete Mathematics 17, 1, 134–160.
#'
#' @export
#'
#' @examples
#' # Randomly rank k random letters.  Pretend that these k elements are the k
#' # of highest rank in some larger ranking.  Do it again.  Compare the two
#' # top-k lists via spearman.dist.
#' set.seed(1L)
#' k <- 10L
#' u <- LETTERS
#' x <- stats::setNames(sample(k), sample(u, k))
#' y <- stats::setNames(sample(k), sample(u, k))
#' spearman.dist(x, y)
spearman.dist <- function (x, y, L = 1L + length(x), normalize = TRUE)
{
	k <- length(x)
	if (!identical(length(y), k)) {
		stop("`x' and `y' differ in length")
	}
	if (k == 0) {
		return(0L)
	}
	Dx <- unique(names(x))
	if (!identical(length(Dx), k)) {
		stop("duplicate names in `x'")
	}
	Dy <- unique(names(y))
	if (!identical(length(Dy), k)) {
		stop("duplicate names in `y'")
	}
	S <- setdiff(Dx, Dy)
	T <- setdiff(Dy, Dx)
	x. <- c(x, stats::setNames(rep(L, length(T)), T))
	y. <- c(y, stats::setNames(rep(L, length(S)), S))
	D <- union(Dx, Dy)
	res <- sum(abs(x.[D] - y.[D]))
	if (normalize) {
		res <- res / (k * (2L * max(L, 1L + k) - (1L + k)))
	}
	res
}
