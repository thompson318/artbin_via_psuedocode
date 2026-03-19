# Internal k-group binary trial calculation
# Translated from artbin.ado v2.1.2 (IW, 19feb2026) — k-groups section

#' Internal k-group sample size / power calculation
#'
#' Called by [artbin()] for multi-arm trials (>=2 arms) or when condit=TRUE.
#' Mirrors the k-groups section of artbin.ado.
#' Returns unrounded total sample size; rounding is done by [artbin()].
#'
#' @param pr vector of anticipated event probabilities (length >= 2)
#' @param ar_raw raw aratios vector (NULL = equal)
#' @param alpha significance level (already doubled if onesided, for 2-sided formula)
#' @param power desired power (target 1-beta)
#' @param n total sample size (0 = calculate SS, >0 = calculate power)
#' @param onesided logical (already incorporated into alpha for the k-group chi-sq tests)
#' @param local logical
#' @param condit logical; conditional test (Peto's OR approximation)
#' @param wald logical; Wald test (unconditional only)
#' @param trend logical; linear trend test
#' @param doses numeric vector of doses for trend test (NULL = 0, 1, ..., K)
#' @param convcrit convergence criterion for iterative bisection (default 1e-7)
#' @return named list: n (unrounded total), power, D (unrounded events), test_name
#' @noRd
.artbin_kgroup <- function(pr, ar_raw = NULL, alpha = 0.05, power = 0.8,
                            n = 0, onesided = FALSE, local = FALSE,
                            condit = FALSE, wald = FALSE,
                            trend = FALSE, doses = NULL,
                            convcrit = 1e-7) {

  ngroups <- length(pr)
  K       <- ngroups - 1   # degrees of freedom

  # --- Allocation ratios (normalized to sum to 1) ---
  if (is.null(ar_raw)) {
    AR <- rep(1 / ngroups, ngroups)
  } else {
    AR <- ar_raw / sum(ar_raw)
  }

  # --- Basic statistics ---
  PI    <- pr
  pibar <- sum(PI * AR)         # weighted mean probability
  s     <- pibar * (1 - pibar)  # variance of weighted mean
  MU    <- PI - pibar           # deviations from mean
  S_var <- PI * (1 - PI)        # arm-level variances

  sbar  <- sum(S_var * AR)      # weighted mean of arm variances

  # --- For onesided with trend/chi-sq K=1: alpha is already 2*alpha_onesided
  # (artbin.ado line 624: if onesided then alpha=2*alpha before k-group calc)
  # So we use alpha as-is here.

  ssize <- (n == 0)   # TRUE = calculate SS, FALSE = calculate power
  beta  <- 1 - power

  # ==========================================
  # CONDITIONAL TEST (Peto's OR approximation)
  # ==========================================
  if (condit) {
    v   <- pibar * (1 - pibar)
    LOR <- log(PI / (1 - PI)) - log(PI[1] / (1 - PI[1]))
    LOR[1] <- 0
    LOR <- LOR - sum(LOR * AR)  # center

    if (!trend) {
      # Chi-square conditional test
      q0      <- sum(LOR^2 * AR)
      crit_a  <- stats::qchisq(1 - alpha, K)

      if (ssize) {
        lambda <- .npnchi2(K, crit_a, beta)
        d_val  <- lambda
        l_val  <- sqrt(d_val * (d_val - 4 * q0 * v))
        d_val  <- (d_val + l_val) / (2 * q0 * (1 - pibar))
        n_out  <- d_val / pibar
        D_out  <- n_out * pibar
      } else {
        d_val  <- n * pibar
        l_val  <- d_val * (n - d_val) * q0 / (n - 1)
        beta   <- stats::pchisq(crit_a, K, ncp = l_val)
        power  <- 1 - beta
        n_out  <- n
        D_out  <- d_val
      }

    } else {
      # Trend test (conditional)
      DOSE <- .make_doses(doses, ngroups, AR)
      tr   <- sum(DOSE * LOR * AR)
      q0   <- sum(DOSE^2 * AR)
      crit_a <- stats::qnorm(1 - alpha / 2)

      if (ssize) {
        crit_a <- sqrt(q0) * (crit_a + stats::qnorm(power))
        l_val  <- (crit_a / tr)^2
        d_val  <- l_val
        l_val  <- sqrt(l_val * (l_val - 4 * v))
        d_val  <- (d_val + l_val) / (2 * (1 - pibar))
        n_out  <- d_val / pibar
        D_out  <- n_out * pibar
      } else {
        d_val  <- n * pibar
        l_val  <- d_val * (n - d_val) / (n - 1)
        crit_a <- abs(tr) * sqrt(l_val / q0) - crit_a
        beta   <- 1 - stats::pnorm(crit_a)
        power  <- 1 - beta
        n_out  <- n
        D_out  <- d_val
      }
    }

    return(list(n = n_out, power = power, D = D_out))
  }

  # ==========================================
  # UNCONDITIONAL TEST
  # ==========================================

  if (!trend) {
    # Chi-square or Wald test

    if (wald) {
      # Wald: variance-covariance matrix of differences
      VA <- matrix(0, nrow = K, ncol = K)
      for (k in seq_len(K)) {
        for (l in seq_len(K)) {
          kk <- k + 1; ll <- l + 1
          VA[k, l] <- S_var[kk] * ((k == l) / AR[kk] - 1) - S_var[ll] + sbar
        }
      }
      MU_sub <- MU[-1]  # MU for groups 2..K+1
      q0 <- as.numeric(t(MU_sub) %*% solve(VA) %*% MU_sub)
    } else {
      # Score / chi-square
      q0 <- sum(MU^2 * AR) / s
    }

    crit_a <- stats::qchisq(1 - alpha, K)

    if (local || wald) {
      # Simple NCP formula
      if (ssize) {
        lambda <- .npnchi2(K, crit_a, beta)
        n_out  <- lambda / q0
        D_out  <- n_out * pibar
      } else {
        beta   <- stats::pchisq(crit_a, K, ncp = n * q0)
        power  <- 1 - beta
        n_out  <- n
        D_out  <- n * pibar
      }

    } else {
      # Distant (default): iterative calculation using _pe2
      W  <- 1 - 2 * AR
      a0 <- (sum(S_var) - sbar) / s
      q1 <- sum(MU^2 * S_var * AR) / s^2
      a1 <- (sum(S_var^2 * W) + sbar^2) / s^2

      if (ssize) {
        # Starting point (local/wald approximation)
        n0     <- .npnchi2(K, crit_a, beta) / q0
        b0_val <- .pe2(a0, q0, a1, q1, K, n0, crit_a)

        if (abs(b0_val - beta) <= convcrit) {
          n_out <- n0
        } else {
          # Bisection — mirrors Stata's artbin.ado interval-bisection
          if (b0_val < beta) {
            nu <- n0; nl <- n0 / 2
          } else {
            nl <- n0; nu <- 2 * n0
          }

          repeat {
            n0    <- (nl + nu) / 2
            b_val <- .pe2(a0, q0, a1, q1, K, n0, crit_a)
            if (isTRUE(abs(b_val - beta) <= convcrit) || (nu - nl) <= convcrit) break
            if (isTRUE(b_val < beta)) nu <- n0 else nl <- n0
          }
          n_out <- n0
        }
        D_out <- n_out * pibar

      } else {
        # Power from given n
        beta  <- .pe2(a0, q0, a1, q1, K, n, crit_a)
        power <- 1 - beta
        n_out <- n
        D_out <- n * pibar
      }
    }

  } else {
    # ==========================
    # TREND TEST (unconditional)
    # ==========================
    DOSE <- .make_doses(doses, ngroups, AR)
    tr   <- sum(MU * DOSE * AR)
    q0   <- sum(DOSE^2 * AR) * s

    if (local) {
      q1 <- q0
    } else {
      q1 <- sum(DOSE^2 * S_var * AR)
    }

    if (!is.null(wald) && wald) {
      a_crit <- sqrt(q1) * stats::qnorm(1 - alpha / 2)
    } else {
      a_crit <- sqrt(q0) * stats::qnorm(1 - alpha / 2)
    }

    if (ssize) {
      a_crit <- a_crit + sqrt(q1) * stats::qnorm(power)
      n_out  <- (a_crit / tr)^2
      D_out  <- n_out * pibar
    } else {
      z_val  <- abs(tr) * sqrt(n) - a_crit
      beta   <- 1 - stats::pnorm(z_val / sqrt(q1))
      power  <- 1 - beta
      n_out  <- n
      D_out  <- n * pibar
    }
  }

  list(n = n_out, power = power, D = D_out)
}


#' Build and center dose vector for trend test
#' @param doses user-supplied doses (NULL = 0, 1, ..., K)
#' @param ngroups number of groups
#' @param AR normalized allocation ratios
#' @return centered dose vector (length ngroups)
#' @noRd
.make_doses <- function(doses, ngroups, AR) {
  if (is.null(doses)) {
    d <- seq(0, ngroups - 1)
  } else {
    d <- doses
  }
  d <- d - sum(d * AR)   # center by weighted mean
  d
}
