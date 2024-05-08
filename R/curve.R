#' Curve grades
#'
#' Both a linear curving tool and a normal curving tool are provided
#' (see details)
#'
#' Two grade curving methods are implemented:
#'
#' 1. Linear curve: Takes two scores, `x0` and `x1`, and ensures those map onto
#'    two scores `y0` and `y1`, and linearly scales the remaining scores. For
#'    example, you may want to ensure the mean was a certain value like 85
#'    and the 95% percentile grade was actually 95 (the default behavior),
#'    or that the mean was 85 and the lowest score was 50, etc.
#' 2. Normal curve: Matches the empirical quantiles of your students' scores
#'    with the theoretical quantiles of a normal distribution with the given
#'    `target_mean` and `target_sd` truncated to \eqn{[0, 100]}.
#'
#' @param scores A numeric vector giving the raw scores; values should be in
#'     \eqn{[0, 1]}
#' @param method A character vector, one of "linear" or "normal" (see details);
#'     default: "linear"
#' @param x0,x1 If `method == "linear"`, two raw scores to serve as anchors;
#'     defaults are `mean(scores)` and `quantile(scores, probs = 0.95)`
#' @param y0,y1 If `method == "linear"`, what `x0` and `x1` should map onto;
#'     defaults are 85 and 95
#' @param target_mean If `method == "normal"`, the mean of the resulting
#'     normal distribution; default: 85
#' @param target_sd If `method == "normal"`, the standard deviation of the
#'     resulting normal distribution; default: 5
#'
#' @return A numeric vector giving the curved scores
#'
#' @export
curve_grades = function(
    scores, method = "linear",
    x0 = mean(scores), x1 = quantile(scores, probs = 0.95), y0 = 85, y1 = 95,
    target_mean = 85, target_sd = 5
) {
    if ( any(scores > 1) | any(scores < 0) ) {
        stop("scores should be between 0 and 1")
    }
    if ( method == "linear" ) {
        scores = y0 + ((y1-y0) / (x1-x0)) * (scores - x0)
    } else if ( method == "normal" ) {
        ## Use Taylor expansion approx to get params of truncated normal
        x = target_mean / target_sd
        z = dnorm(x) / pnorm(x)
        A = z * (1 + x^2 + z * x)
        B = z * (x + z)
        C = A * target_mean
        D = B * target_mean^2 - (1 - B) * target_sd^2
        s = sqrt((2*B*D + C^2 - 2*D + C*sqrt(C^2 - 4*D*(1-B))) / (2*(1-B)^2))
        m = (target_mean - A*s) / (1 - B)
        ## Use empirical quantiles to get theoretical quantiles
        p = rank(scores, ties.method = "max") / (length(scores) + 1)
        a = pnorm(0, m, s); b = pnorm(100, m, s)
        scores = qnorm(a + p * (b - a), m, s)
    } else {
        stop("method should be one of 'linear' or 'normal'")
    }
    return(scores)
}
