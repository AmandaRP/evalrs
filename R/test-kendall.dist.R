context("Kendall distance with parameter p")

set.seed(1L)

rand_perm <- function (k = 10L, n = 3L * k, d = paste0("x", seq_len(n)))
     stats::setNames(sample(k), sample(d, k))
disjoint_perm <- function (x)
	stats::setNames(rand_perm(length(x)), paste0(names(x), "."))

# We use herein the word "list" to refer to the generic concept of a list,
# not to R's list type.
test_that("distance of zero-length lists equals zero", {
	x <- integer(0L)
	y <- integer(0L)
	expect_equal(kendall.dist(x, y), 0L)
})
test_that("distance of equal lists equals zero", {
	x <- rand_perm(1L)
	expect_equal(kendall.dist(x, x), 0L)
	x <- rand_perm(10L)
	y <- sample(x)
	expect_equal(kendall.dist(x, y), 0L)
})
test_that("distance of disjoint lists of length one equals one", {
	x <- rand_perm(1L)
	y <- disjoint_perm(x)
	expect_equal(kendall.dist(x, y), 1L)
})
test_that("distance of disjoint lists of length two equals (4 + 2p) / 6", {
	# Why?  If x and y are disjoint top-two lists then the union
	# of their domains comprises four elements of which there are
	# four-choose-two---that is, six---pairs of elements.  Exactly four
	# of these pairs, say {i,j}, is such that i is in the domain of x and
	# j is in the domain of y or the other way around; in both cases,
	# the score for this pair is one.  Exactly one of the other two
	# pairs, say {i',j'}, is such that i' and j' are in the domain of
	# x only or of y only, so the score for this pair is p.  Similarly,
	# the remaining pair {i*,j*} is such that i* and j* are in the domain
	# of y only (if i' and j' are in the domain of x) or of x only (if i'
	# and j' are in the domain of y), so the score for this pair too is p.
	# Therefore, the un-normalized total of the scores is (4)(1) + (2)(p).
	# This is normalized by the number of pairs of elements in the union
	# of the two lists' domains: 6.
	x <- rand_perm(2L)
	y <- disjoint_perm(x)
	p <- 0.25
	expect_equal(kendall.dist(x, y, p = p), (4 + 2 * p) / 6)
})
test_that("distance of reversed lists equals one", {
	rev_perm <- function (x) 1L + length(x) - x
	x <- rand_perm(2L)
	y <- rev_perm(x)
	expect_equal(kendall.dist(x, y), 1L)
	x <- rand_perm(20L)
	y <- rev_perm(x)
	expect_equal(kendall.dist(x, y), 1L)
})
