
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evalrs <img src='man/figures/evalrs.png' align="right" height="200" />

Use the {evalrs} package to compute evaluation metrics for recommender
systems.

Both accuracy metrics (hit rate, normalized discounted cumulative gain,
mean average precision, mean reciprocal rank, mean precision rank) and
beyond-accuracy metrics (novelty and diversity\*) are included.

\*Note: Diversity is currently missing, but should be added back in
soon.

## Installation

You can install {evalrs} from this repo with:

``` r
install.packages("devtools")
devtools::install_github("AmandaRP/evalrs")
library(evalrs)
```

## More info

For help, type `?evalrs`.
