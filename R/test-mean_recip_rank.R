context("mean reciprocal rank (`MRR')")

set.seed(1L)

gen_dat <- function (nuser = 10L, nitem = 50L)
{
	u <- paste0("u", seq_len(nuser))    # set of user IDs
	i <- paste0("i", seq_len(nitem))    # set of item IDs
	x <- expand.grid(pred = NA_real_, label = NA_integer_,
		item = i, user = u)
	x$pred <- unlist(replicate(nuser, { # predictions vector
	    y <- runif(nitem)               # random values in [0,1]...
	    y / sum(y)                      # ...that sum to one
	}, simplify = FALSE))
	x$label <- unlist(replicate(nuser, { # vector of binary relevance-values
	    sample(c(1L, rep(0L, nitem - 1L)))
	}, simplify = FALSE))
	x
}

test_that("MRR equals 0 if no items are relevant", {
	x <- within(gen_dat(), label <- 0L)
	expect_equal(mean_recip_rank(x), 0L)
})
test_that("MRR equals 1 if sole relevant item has highest score", {
	x <- gen_dat()
	s <- split(x, x$user)
	x. <- lapply(s, function (x) {
		p <- x$label == 1L
		stopifnot(sum(p) == 1L)
		x$pred[p] <- 1L + max(x$pred)
		x
	})
	x <- unsplit(x., x$user)
	expect_equal(mean_recip_rank(x), 1L)
})
