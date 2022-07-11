test_that("mean_avg_precision() is 0 when all prediction scores are tied", {
  expect_equal(mean_avg_precision(data.frame(user=rep(1:5,2), item=1:20, label=rep(c(1,0),10), pred=0), k=2), 0)
})

test_that("mean_avg_precision() is 1 when predictions are equal to the labels", {
  expect_equal(mean_avg_precision(data.frame(user=rep(1:5,2), item=1:20, label=rep(c(1,0),10), pred=rep(c(1,0),10)), k=2), 1)
})

test_that("mean_avg_precision() is 1 when all labels are 1", {
  expect_equal(mean_avg_precision(data.frame(user=rep(1:5,2), item=1:20, label=1, pred=0.5), k=2), 1)
})
