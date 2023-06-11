context("Spearman distance with location parameter L")

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
	expect_equal(spearman_dist(x, y), 0L)
})
test_that("distance of equal lists equals zero", {
	x <- rand_perm(1L)
	expect_equal(spearman_dist(x, x), 0L)
	x <- rand_perm(10L)
	y <- sample(x)
	expect_equal(spearman_dist(x, y), 0L)
})
test_that("raw distance of disjoint lists equals 2 * sum(|i - L|)", {
	for (k in sample(1e3L, 10L)) {
		x <- rand_perm(k)
		y <- disjoint_perm(x)
		L <- k + sample(k, 1L)
		expect_equal(spearman_dist(x, y, L = L, normalize = FALSE),
			2 * sum(abs(seq_len(k) - L)))
	}
})
test_that("raw distance of reversed lists equals 2 * sum(floor(i / 2))", {
	rev_perm <- function (x) 1L + length(x) - x
	for (k in sample(1e3L, 10L)) {
		x <- rand_perm(k)
		y <- rev_perm(x)
		expect_equal(spearman_dist(x, y, normalize = FALSE),
			2 * sum(floor(seq_len(k) / 2)))
	}
})
