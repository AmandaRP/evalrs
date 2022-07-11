#' Compute Hit Rate at k
#'
#' Hit rate at k (HR@k) is the mean number of users for which a known positive item
#' ranked in their top k recommended items. Equation:
#' \eqn{1/m \sum I(rank(positive item) >= k)} over \eqn{m} users, where I is the indicator function.
#'
#' @param test_pred A data frame containing predictions and truth labels for a test data set.
#' @param k An integer specifying HR at k.
#' @param user_col Name of the test_pred column containing the user id's. Default: user
#' @param label_col Name of the test_pred column containing binary truth labels indicating positive/like (1) or
#' negative/dislike (0) for each user-item combination. Default: label
#' @param pred_col Name of the test_pred column containing model prediction scores for each user-item combination. Default: pred
#'
#' @return HR@k:  a decimal number in the range [0,1].
#' @export
#'
#' @import dplyr
#'
#' @examples
#' test_pred <- data.frame(pred = runif(5), user = 1:5, item = 1:5, label = sample(0:1, 5, replace=TRUE))
#' hit_rate(test_pred, k = 2)
#' test_pred <- data.frame(prediction = runif(5), user_id = 1:5, item_id = 1:5, label = sample(0:1, 5, replace=TRUE))
#' hit_rate(test_pred, k = 2, user_col=user_id, pred_col=prediction)
hit_rate <- function(test_pred, k, user_col=user, label_col=label, pred_col=pred){
  test_pred %>%
    arrange({{label_col}}) %>% # put 0's at top
    group_by({{user_col}}) %>%
    slice_max(order_by = {{pred_col}}, n = k, with_ties = FALSE) %>% # with_ties = TRUE may return more rows than expected
    summarize(hits = max({{label_col}})) %>%
    summarize(hr = mean(hits)) %>%
    pull()
}


#' Compute Hit Rate at k
#'
#' This function is available only for backward compatibility. Superseded by hit_rate().
#'
#' @param test_pred A data frame containing predictions and truth labels for a test data set.
#' @param k An integer specifying HR at k.
#' @param user_col Name of the column containing the user id's. Default: user
#' @param label_col Name of the column containing binary truth labels indicating positive/like (1) or
#' negative/dislike (0) for each user-item combination. Default: label
#' @param pred_col Name of the column containing model prediction scores for each user-item combination. Default: pred
#'
#' @return HR@k:  a decimal number in the range [0,1].
#' @export
#'
#' @import dplyr
#'
#' @examples
#' test_pred <- data.frame(pred = runif(5), user = 1:5, item = 1:5, label = sample(0:1, 5, replace=TRUE))
#' compute_hr(test_pred, k = 2)
#' test_pred <- data.frame(prediction = runif(5), user_id = 1:5, item_id = 1:5, label = sample(0:1, 5, replace=TRUE))
#' compute_hr(test_pred, k = 2, user_col=user_id, pred_col=prediction)
compute_hr <- function(test_pred, k, user_col=user, label_col=label, pred_col=pred){
  hit_rate(test_pred, k, {{user_col}}, {{label_col}}, {{pred_col}})
}
