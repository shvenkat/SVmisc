#' Recycles a vector (if needed) to a given length.
#' 
#' @param x vector to be recycled
#' @param n integer specifying the recycled length of x
#' @return x recycled to length n
#' @author Shiv Venkatasubrahmanyam
#' @export 
recycle <- function(x, n) {
    rep(x, ceiling(n/length(x)))[1:n]
}