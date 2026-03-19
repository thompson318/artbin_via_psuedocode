# Internal 2-arm binary trial calculation
# Translated from art2bin.ado v1.01 (EMZ, 09jun2022)

#' Internal 2-arm sample size / power calculation
#'
#' Called by [artbin()] for 2-arm trials. Mirrors art2bin.ado.
#' Always returns unrounded sample sizes; rounding is done by [artbin()].
#'
#' @param p0 control arm event probability
#' @param p1 intervention arm event probability
#' @param margin NI/substantial-superiority margin (default 0)
#' @param n total sample size (for power calc; 0 = calculate SS)
#' @param n0 control arm size (for power calc; 0 = derive from n)
#' @param n1 intervention arm size (for power calc; 0 = derive from n)
#' @param ar allocation ratio; scalar ar10 = n1/n0, or length-2 vector c(r0, r1)
#' @param alpha significance level (two-sided; halved internally for onesided)
#' @param power desired power (for SS calc)
#' @param nvmethod null variance method: 1=sample, 2=fixed marginal, 3=constrained ML
#' @param onesided logical
#' @param ccorrect logical
#' @param local logical; use local alternative
#' @param wald logical; use Wald test
#' @return named list: n, n0, n1, power, alpha, allocr, Dart, ar10
#' @noRd
.art2bin <- function(p0, p1, margin = 0, n = 0, n0 = 0, n1 = 0,
                     ar = NULL, alpha = 0.05, power = 0.8,
                     nvmethod = 3, onesided = FALSE,
                     ccorrect = FALSE, local = FALSE, wald = FALSE) {

  # --- Allocation ratio ---
  if (is.null(ar) || identical(ar, 1) || identical(ar, c(1, 1))) {
    allocr <- "equal group sizes"
    ar10 <- 1
  } else if (length(ar) == 1) {
    allocr <- paste0("1:", ar)
    ar10 <- as.numeric(ar)
  } else if (length(ar) == 2) {
    allocr <- paste0(ar[1], ":", ar[2])
    ar10 <- ar[2] / ar[1]
  } else {
    stop("Invalid allocation ratio")
  }

  # --- Null variance method ---
  nvm <- as.integer(nvmethod)
  if (is.na(nvm) || nvm < 1 || nvm > 3) nvm <- 3L

  mrg <- margin

  # --- Null hypothesis event probabilities ---
  if (nvm == 1L) {
    p0null <- p0
    p1null <- p1
  } else if (nvm == 2L) {
    p0null <- (p0 + ar10 * p1 - ar10 * mrg) / (1 + ar10)
    p1null <- (p0 + ar10 * p1 + mrg) / (1 + ar10)
    if (!.inrange01(p0null, p1null)) {
      stop("Event probabilities and/or margin are incompatible with fixed marginal totals method")
    }
  } else {
    # nvm == 3: constrained maximum likelihood (Farrington & Manning 1990)
    a_c <- 1 + ar10
    b_c <- mrg * (ar10 + 2) - 1 - ar10 - p0 - ar10 * p1
    c_c <- (mrg - 1 - ar10 - 2 * p0) * mrg + p0 + ar10 * p1
    d_c <- p0 * mrg * (1 - mrg)
    v_c <- (b_c / (3 * a_c))^3 - (b_c * c_c) / (6 * a_c^2) + d_c / (2 * a_c)
    u_c <- sign(v_c) * sqrt(pmax(0, (b_c / (3 * a_c))^2 - c_c / (3 * a_c)))
    toosmall <- 1e-12
    cos_val  <- if (abs(v_c) <= toosmall && abs(u_c^3) <= toosmall) {
      0
    } else {
      v_c / u_c^3
    }
    cos_val  <- max(-1, min(1, cos_val))   # clamp for numerical safety
    w_c      <- (pi + acos(cos_val)) / 3
    p0null   <- 2 * u_c * cos(w_c) - b_c / (3 * a_c)
    p1null   <- p0null + mrg
    if (!.inrange01(p0null, p1null)) {
      stop("You have found a case that we have never encountered before! Please contact the artbin authors.")
    }
  }

  # --- Core quantities ---
  D_diff <- abs(p1 - p0 - mrg)

  za <- if (onesided) stats::qnorm(1 - alpha) else stats::qnorm(1 - alpha / 2)
  zb <- stats::qnorm(power)

  snull <- sqrt(p0null * (1 - p0null) + p1null * (1 - p1null) / ar10)
  salt  <- sqrt(p0 * (1 - p0) + p1 * (1 - p1) / ar10)

  # --- Determine if calculating SS or power ---
  # n=0 and n0=0 and n1=0 → SS; otherwise power
  ss <- (n == 0 && n0 == 0 && n1 == 0)

  if (!ss) {
    # Resolve n0/n1 from n if needed
    if (n > 0 && n0 == 0 && n1 == 0) {
      n0 <- n / (1 + ar10)
      n1 <- n - n0
    } else if (n > 0 && n0 == 0) {
      n0 <- n - n1
    } else if (n > 0 && n1 == 0) {
      n1 <- n - n0
    } else if (n == 0 && n1 == 0) {
      n1 <- n0 * ar10
    } else if (n == 0 && n0 == 0) {
      n0 <- n1 / ar10
    }
  }

  if (ss) {
    # --- Sample size calculation ---
    if (local) {
      m <- ((za * snull + zb * snull) / D_diff)^2
    } else if (wald) {
      m <- ((za * salt + zb * salt) / D_diff)^2
    } else {
      m <- ((za * snull + zb * salt) / D_diff)^2
    }

    if (ccorrect) {
      m <- .cc(m, D_diff, ar10, deflate = FALSE)
    }

    # Return unrounded (caller handles rounding)
    n0_out <- m
    n1_out <- ar10 * m
    n_out  <- n0_out + n1_out
    Power  <- power

  } else {
    # --- Power calculation ---
    n0_adj <- if (ccorrect) .cc(n0, D_diff, ar10, deflate = TRUE) else n0

    if (local) {
      Power <- stats::pnorm((D_diff * sqrt(n0_adj) - za * snull) / snull)
    } else if (wald) {
      Power <- stats::pnorm((D_diff * sqrt(n0_adj) - za * salt) / salt)
    } else {
      Power <- stats::pnorm((D_diff * sqrt(n0_adj) - za * snull) / salt)
    }

    n0_out <- n0
    n1_out <- n1
    n_out  <- n0_out + n1_out
  }

  Dart <- n0_out * p0 + n1_out * p1

  list(
    n     = n_out,
    n0    = n0_out,
    n1    = n1_out,
    power = Power,
    alpha = alpha,
    allocr = allocr,
    Dart  = Dart,
    ar10  = ar10
  )
}
