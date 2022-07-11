
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evalrs <img src='man/figures/evalrs.png' align="right" height="200" />

<!-- badges: start -->

<!-- badges: end -->

Use the {evalrs} package to compute evaluation metrics for recommender
systems.

## Installation

You can install {evalrs} from this repo with:

``` r
install.packages("devtools")
devtools::install_git("evalrs")
library(evalrs)
```

If the installation above is problematic, there are two alternative ways
to install the package:

1.  Pull this repo into RStudio and then run
    `devtools::load_all(reset=FALSE)` followed by `devtools::install()`,
    or
2.  Click the “download” button on this GitLab repo page. Save the file
    as a `.tar.gz` file and upload it to RStudio. In RStudio, select
    “Install Packages” from the “Tools” drop-down menu. Choose
    “Package archive file” as the “Install from” option then browse to
    find the `.tar.gz` file. Click “install”.

After completing one of the above steps, run `library(evalrs)` to load
the package.

## More info

For help, type `?evalrs`.
