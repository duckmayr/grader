
<!-- README.md is generated from README.Rmd. Please edit that file -->

# grader

<!-- badges: start -->
<!-- badges: end -->

`grader` provides helpful tools for grading, including functions to

- drop assignments, even fractional assignments
- curve scores
- and more

## Installation

You can install the development version of grader from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("duckmayr/grader")
```

## Example

Suppose you had the following set of grades:

``` r
set.seed(138)
grades = round(runif(n = 100, min = 0, max = 100))
knitr::kable(data.frame(
    "Low Score" = min(grades),
    "First Quartile" = quantile(grades, probs = 0.25),
    "Mean" = mean(grades), "Median" = median(grades),
    "Third Quartile" = quantile(grades, probs = 0.75),
    "High Score" = max(grades),
    check.names = FALSE
), row.names = FALSE)
```

| Low Score | First Quartile |  Mean | Median | Third Quartile | High Score |
|----------:|---------------:|------:|-------:|---------------:|-----------:|
|         2 |          27.75 | 50.26 |     53 |             75 |         99 |

``` r
plot(density(grades, from = 0, to = 100))
```

<img src="man/figures/README-example-grades-1.png" width="100%" />

But you wanted a more reasonable distribution of the grades… You can use
`grader::curve_grades()`. It provides two types of curve:

1.  A linear curve, matching two raw scores to two target scores and
    linearly adjusting the remaining grades; this is the default
    behavior, with defaults matching the mean raw score to 85 and the
    95th percentile score to 95.
2.  A “normal” curve, matching the empirical CDF of scores to a normal
    CDF with a specified target mean (default: 85) and specified target
    standard deviation (default: 10) truncated to $[0, 100]$.

Here’s an illustration of the default curving behavior (note the
x-axis!):

``` r
library(grader)
curved_grades = round(curve_grades(scores = grades))
knitr::kable(data.frame(
    "Low Score" = min(curved_grades),
    "First Quartile" = quantile(curved_grades, probs = 0.25),
    "Mean" = mean(curved_grades), "Median" = median(curved_grades),
    "Third Quartile" = quantile(curved_grades, probs = 0.75),
    "High Score" = max(curved_grades),
    check.names = FALSE
), row.names = FALSE)
```

| Low Score | First Quartile |  Mean | Median | Third Quartile | High Score |
|----------:|---------------:|------:|-------:|---------------:|-----------:|
|        74 |             80 | 84.99 |   85.5 |             91 |         96 |

``` r
plot(density(curved_grades, from = 0, to = 100), xlim = c(50, 100))
```

<img src="man/figures/README-example-linear-curve-1.png" width="100%" />

Or the normal curve behavior:

``` r
library(grader)
curved_grades = round(curve_grades(scores = grades, method = "normal"))
knitr::kable(data.frame(
    "Low Score" = min(curved_grades),
    "First Quartile" = quantile(curved_grades, probs = 0.25),
    "Mean" = mean(curved_grades), "Median" = median(curved_grades),
    "Third Quartile" = quantile(curved_grades, probs = 0.75),
    "High Score" = max(curved_grades),
    check.names = FALSE
), row.names = FALSE)
```

| Low Score | First Quartile |  Mean | Median | Third Quartile | High Score |
|----------:|---------------:|------:|-------:|---------------:|-----------:|
|        61 |             78 | 83.88 |     84 |             90 |         99 |

``` r
plot(density(curved_grades, from = 0, to = 100), xlim = c(50, 100))
```

<img src="man/figures/README-example-normal-curve-1.png" width="100%" />

You can also generate an average of scores after dropping a particular
number of assignments; suppose a student had assignment scores
$(0, 95, 0)$; we can see what the student’s average after dropping no
assignments, one, two, or even one and a half assignments (in the case
of fractional dropped assignments, a weighted average is used where the
marginal score receives the fractional weight):

``` r
grades = c(0, 95, 0)
knitr::kable(data.frame(
    Dropped = c(0, 1, 1.5, 2),
    Average = sapply(c(0, 1, 1.5, 2), function(x) avg_w_drops(grades, x))
))
```

| Dropped |  Average |
|--------:|---------:|
|     0.0 | 31.66667 |
|     1.0 | 47.50000 |
|     1.5 | 63.33333 |
|     2.0 | 95.00000 |
