#' Compute Normalized Discounted Cummulative Gain (NDCG@k)
#'
#' See wiki page description: https://en.wikipedia.org/wiki/Discounted_cumulative_gain#Normalized_DCG
#'
#' @param test_pred A data frame containing predictions for the test data set.
#' @param k An integer specifying NDCG at k.
#' @param user_col Name of the test_pred column containing the user id's. Default: user
#' @param label_col Name of the test_pred column containing binary truth labels indicating positive/like (1) or
#' negative/dislike (0) for each user-item combination. Default: label
#' @param pred_col Name of the test_pred column containing model prediction scores for each user-item combination. Default: pred
#'
#' @return NDCG@k, which is a decimal number in the range [0,1].
#' @export
#'
#' @import dplyr
#'
#' @examples
#' test_pred <- data.frame(pred = runif(20), user = rep(1:5,each=4), item = as.vector(sapply(rep(1,5), FUN=function(x){ rep(sample(1:4), x) } )), label = as.vector(sapply(rep(4,5), FUN=function(x){ sample(c(1,rep(0,3)), x, replace=FALSE) })))
#' ndcg(test_pred, k = 2)
#' test_pred <- data.frame(prediction = runif(20), user_id = rep(1:5,each=4), item_id = as.vector(sapply(rep(1,5), FUN=function(x){ rep(sample(1:4), x) } )), label = as.vector(sapply(rep(4,5), FUN=function(x){ sample(c(1,rep(0,3)), x, replace=FALSE) })))
#' ndcg(test_pred, k = 2, user_col=user_id, pred_col=prediction)
ndcg <- function(test_pred, k, user_col=user, label_col=label, pred_col=pred){

  idcg <- test_pred %>%
    group_by({{user_col}}) %>%
    slice_max(order_by = {{label_col}}, n = k, with_ties = FALSE) %>% # order by labels (highest at the top) to get "ideal" ordering
    mutate(rank = rank(desc({{label_col}}), ties.method = "random")) %>%
    mutate(idcg = (2^{{label_col}}-1)/log(rank+1, base = 2)) %>%
    summarize(idcg_user = sum(idcg)) %>%
    ungroup()

  test_pred %>%
    arrange({{label_col}}) %>% # put 0's at top (pessimistic view in the event that all scores are tied)
    group_by({{user_col}}) %>%
    slice_max(order_by = {{pred_col}}, n = k, with_ties = FALSE) %>%
    mutate(rank = rank(desc({{pred_col}}), ties.method = "random")) %>%
    mutate(dcg = (2^{{label_col}}-1)/log(rank+1, base = 2)) %>%
    summarize(dcg_user = sum(dcg)) %>%
    ungroup() %>%
    left_join(idcg) %>%
    mutate(ndcg_user = dcg_user/idcg_user) %>%
    summarize(ndcg = mean(ndcg_user)) %>%
    pull()
}

