# Internal utility functions for artbin

#' Find noncentrality parameter for non-central chi-squared distribution
#'
#' Returns lambda such that pchisq(x, df, ncp = lambda) == p.
#' Mirrors Stata's npnchi2(df, x, p).
#'
#' @param df degrees of freedom
#' @param x the chi-squared value (critical value)
#' @param p the target probability (typically beta = 1 - power)
#' @return noncentrality parameter lambda
#' @noRd
.npnchi2 <- function(df, x, p) {
  f <- function(ncp) stats::pchisq(x, df, ncp = ncp) - p
  # Find an upper bound where f < 0 (pchisq decreases as ncp increases)
  upper <- max(100, 2 * df)
  while (f(upper) > 0) upper <- upper * 2
  stats::uniroot(f, c(0, upper), tol = 1e-10)$root
}


#' Continuity correction for 2-arm binary trial
#'
#' @param n uncorrected n0 (control arm size)
#' @param adiff |p1 - p0 - margin|
#' @param ratio allocation ratio ar10 = n1/n0 (default 1)
#' @param deflate logical; if TRUE deflate n (for power calc), else inflate (for SS)
#' @return corrected n0
#' @noRd
.cc <- function(n, adiff, ratio = 1, deflate = FALSE) {
  a <- (ratio + 1) / (adiff * ratio)
  if (deflate) {
    ((2 * n - a)^2) / (4 * n)
  } else {
    cf <- ((1 + sqrt(1 + 2 * a / n))^2) / 4
    n * cf
  }
}


#' Calculate beta (type II error) for distant unconditional k-group test
#'
#' Mirrors Stata's _pe2 subroutine.
#'
#' @param a0 (sum(S) - sbar) / s
#' @param q0 sum(MU^2 * AR) / s
#' @param a1 (sum(S^2 * W) + sbar^2) / s^2
#' @param q1 sum(MU^2 * S * AR) / s^2
#' @param k K = ngroups - 1 (degrees of freedom)
#' @param n total sample size (scalar)
#' @param a critical value qchisq(1 - alpha, k)
#' @return beta = P(type II error)
#' @noRd
.pe2 <- function(a0, q0, a1, q1, k, n, a) {
  b0 <- a0 + n * q0
  b1 <- a1 + 2 * n * q1
  l  <- b0^2 - k * b1
  f  <- sqrt(l * (l + k * b1))
  l  <- (l + f) / b1
  f  <- a * (k + l) / b0
  stats::pchisq(f, k, ncp = l)
}


#' Check all values are in (0, 1)
#' @noRd
.inrange01 <- function(...) {
  vals <- c(...)
  all(vals > 0 & vals < 1)
}
