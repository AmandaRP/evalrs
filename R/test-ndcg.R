test_that("ndcg() is 0 when all prediction scores are tied", {
  expect_equal(ndcg(data.frame(user=rep(1:5,2), item=1:20, label=rep(c(1,0),2), pred=0), k=2), 0)
})
