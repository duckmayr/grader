#' Average of grades after dropping lowest scores
#'
#' If the number `ndrop` of assignments to drop is not a whole number, a
#' weighted average is given, where the `length(scores) - ceiling(ndrop)`
#' highest scores are weighted normally, but the next highest score is given a
#' weight of `r` (and all lower scores receive a weight of 0).
#'
#' @param scores A numeric vector giving the raw scores; values should be in
#'     \eqn{[0, 1]} or \eqn{[0, 100]}
#' @param ndrop Number of assignments to drop
#'
#' @return The grades' average after dropping the relevant number of assignments
#'
#' @export
avg_w_drops = function(scores, ndrop) {
    using_hi_scale = all(in_interval(scores, 0, 100))
    using_lo_scale = all(in_interval(scores, 0, 1))
    if ( !(using_hi_scale | using_lo_scale) ) {
        stop("scores should be between 0 and 1 or between 0 and 100")
    }
    M = ifelse(any(scores > 1), 100, 1)
    N = length(scores)
    n = ceiling(ndrop)
    r = ndrop %% 1
    s = sort(scores, decreasing = TRUE)
    x = head(s, N-n)
    if ( r == 0 ) return(mean(x))
    w = c(rep(1, N-n), r)
    return( sum(w * c(x, s[N-n+1])) / sum(w) )
}
