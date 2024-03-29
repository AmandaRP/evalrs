% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndcg.R
\name{ndcg}
\alias{ndcg}
\title{Compute Normalized Discounted Cummulative Gain (NDCG@k)}
\usage{
ndcg(test_pred, k, user_col = user, label_col = label, pred_col = pred)
}
\arguments{
\item{test_pred}{A data frame containing predictions for the test data set.}

\item{k}{An integer specifying NDCG at k.}

\item{user_col}{Name of the test_pred column containing the user id's. Default: user}

\item{label_col}{Name of the test_pred column containing binary truth labels indicating positive/like (1) or
negative/dislike (0) for each user-item combination. Default: label}

\item{pred_col}{Name of the test_pred column containing model prediction scores for each user-item combination. Default: pred}
}
\value{
NDCG@k, which is a decimal number in the range [0,1].
}
\description{
See wiki page description: https://en.wikipedia.org/wiki/Discounted_cumulative_gain#Normalized_DCG
}
\examples{
test_pred <- data.frame(pred = runif(20), user = rep(1:5,each=4), item = as.vector(sapply(rep(1,5), FUN=function(x){ rep(sample(1:4), x) } )), label = as.vector(sapply(rep(4,5), FUN=function(x){ sample(c(1,rep(0,3)), x, replace=FALSE) })))
ndcg(test_pred, k = 2)
test_pred <- data.frame(prediction = runif(20), user_id = rep(1:5,each=4), item_id = as.vector(sapply(rep(1,5), FUN=function(x){ rep(sample(1:4), x) } )), label = as.vector(sapply(rep(4,5), FUN=function(x){ sample(c(1,rep(0,3)), x, replace=FALSE) })))
ndcg(test_pred, k = 2, user_col=user_id, pred_col=prediction)
}
