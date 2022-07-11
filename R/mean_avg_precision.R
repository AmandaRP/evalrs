#' Mean Average Precision (MAP@k)
#'
#' Compute the mean average precision for a top-k list of recommendations.
#'
#' Average precision at k (for each user) is defined as \deqn{AP@k = \frac{1}{m} \sum_{i=1}^k P(i) * rel(i)}
#' where \eqn{P(i)} is the precision at i. \eqn{rel(i) = 1} if the item is relevant (or liked by user)
#' and 0 if not relevant (not liked by user). \eqn{m = min(k, number of positives in full item space)}.
#'
#' Mean average precision at k is the mean of AP@k over all users.
#'
#' @seealso http://sdsawtelle.github.io/blog/output/mean-average-precision-MAP-for-recommender-systems.html
#'
#' @param test_pred A data frame containing predictions for the test data set.
#' @param k An integer specifying MAP at k.
#' @param user_col Name of the test_pred column containing the user id's. Default: user
#' @param label_col Name of the test_pred column containing binary truth labels indicating positive/like (1) or
#' negative/dislike (0) for each user-item combination. Default: label
#' @param pred_col Name of the test_pred column containing model prediction scores for each user-item combination. Default: pred
#'
#' @return MAP@k, which is a decimal number in the range [0,1].
#' @export
#'
#' @import dplyr
#'
#' @examples
#' test_pred <- data.frame(pred = runif(20), user = rep(1:5,each=4), item = as.vector(sapply(rep(1,5), FUN=function(x){ rep(sample(1:4), x) } )), label = as.vector(sapply(rep(4,5), FUN=function(x){ sample(c(1,rep(0,3)), x, replace=FALSE) })))
#' mean_avg_precision(test_pred, k = 2)
#' test_pred <- data.frame(prediction = runif(20), user_id = rep(1:5,each=4), item_id = as.vector(sapply(rep(1,5), FUN=function(x){ rep(sample(1:4), x) } )), label = as.vector(sapply(rep(4,5), FUN=function(x){ sample(c(1,rep(0,3)), x, replace=FALSE) })))
#' mean_avg_precision(test_pred, k = 2, user_col=user_id, pred_col=prediction)
mean_avg_precision <- function(test_pred, k, user_col=user, label_col=label, pred_col=pred){

  # Find the max number of postives in the top k:
  max_num_pos <- test_pred %>%
    group_by({{user_col}}) %>%
    summarize(denominator = sum({{label_col}})) %>%  # Find the number of positives for each user
    mutate(denominator = min(denominator, k))        # Need to take min to avoid penalizing for num pos > k

  #Compute map:
  test_pred %>%
    arrange({{label_col}}) %>% # put 0's at top (pessimistic view in the event that all scores are tied)
    group_by({{user_col}}) %>%
    slice_max(order_by = {{pred_col}}, n = k, with_ties = FALSE) %>%
    arrange({{user_col}}, desc({{pred_col}})) %>%   #arrange from largest prediction to smallest for each user
    mutate(precision_at_rank = cummean({{label_col}})) %>%
    mutate(p_at_r_label = precision_at_rank * {{label_col}}) %>%
    summarize(sum_precision = sum(p_at_r_label)) %>%
    inner_join(max_num_pos) %>%
    mutate(average_precision = sum_precision / denominator) %>%
    summarize(map = mean(average_precision)) %>%
    pull()
}
