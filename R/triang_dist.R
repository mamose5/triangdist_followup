
#' @title Triangular Distribution
#' @description Density, distribution, quantile and random generation for the triangular distribution.
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param min lower limit (a).
#' @param max upper limit (b).
#' @param mode mode (c).
#' @export
dtriang <- function(x, min = 0, max = 1, mode = 0.5) {
  if (any(min > max | mode < min | mode > max)) stop("Constraints: min <= mode <= max")
  n <- max(length(x), length(min), length(max), length(mode))
  x <- rep_len(x, n); min <- rep_len(min, n); max <- rep_len(max, n); mode <- rep_len(mode, n)
  res <- numeric(n)
  h <- 2 / (max - min)
  idx1 <- x >= min & x < mode
  res[idx1] <- h[idx1] * (x[idx1] - min[idx1]) / (mode[idx1] - min[idx1])
  idx2 <- x >= mode & x <= max
  res[idx2] <- h[idx2] * (max[idx2] - x[idx2]) / (max[idx2] - mode[idx2])
  return(res)
}

#' @rdname dtriang
#' @export
ptriang <- function(q, min = 0, max = 1, mode = 0.5) {
  if (any(min > max | mode < min | mode > max)) stop("Constraints: min <= mode <= max")
  n <- max(length(q), length(min), length(max), length(mode))
  q <- rep_len(q, n); min <- rep_len(min, n); max <- rep_len(max, n); mode <- rep_len(mode, n)
  res <- numeric(n)
  res[q >= max] <- 1
  idx1 <- q >= min & q < mode
  res[idx1] <- (q[idx1] - min[idx1])^2 / ((max[idx1] - min[idx1]) * (mode[idx1] - min[idx1]))
  idx2 <- q >= mode & q < max
  res[idx2] <- 1 - (max[idx2] - q[idx2])^2 / ((max[idx2] - min[idx2]) * (max[idx2] - mode[idx2]))
  return(res)
}

#' @rdname dtriang
#' @export
qtriang <- function(p, min = 0, max = 1, mode = 0.5) {
  if (any(p < 0 | p > 1)) stop("p must be in [0, 1]")
  if (any(min > max | mode < min | mode > max)) stop("Constraints: min <= mode <= max")
  n <- max(length(p), length(min), length(max), length(mode))
  p <- rep_len(p, n); min <- rep_len(min, n); max <- rep_len(max, n); mode <- rep_len(mode, n)
  res <- numeric(n)
  p_mode <- (mode - min) / (max - min)
  idx1 <- p < p_mode
  res[idx1] <- min[idx1] + sqrt(p[idx1] * (max[idx1] - min[idx1]) * (mode[idx1] - min[idx1]))
  idx2 <- p >= p_mode
  res[idx2] <- max[idx2] - sqrt((1 - p[idx2]) * (max[idx2] - min[idx2]) * (max[idx2] - mode[idx2]))
  return(res)
}

#' @rdname dtriang
#' @export
rtriang <- function(n, min = 0, max = 1, mode = 0.5) {
  return(qtriang(runif(n), min, max, mode))
}

