#' Sample Size and Power for Binary Outcome Clinical Trials
#'
#' Calculates the power or total sample size for various tests comparing K
#' anticipated probabilities for binary outcomes. Supports superiority,
#' non-inferiority, and substantial-superiority trial designs.
#'
#' @param pr numeric vector of length >= 2. Anticipated event probabilities.
#'   `pr[1]` is the control group probability; `pr[2]`, `pr[3]`, ... are the
#'   treatment group probabilities.
#' @param margin numeric or NULL. Non-inferiority or substantial-superiority
#'   margin. If NULL or 0, a superiority design is assumed.
#' @param alpha numeric. Significance level (default 0.05).
#' @param power numeric or NULL. Desired power. If NULL and `n` is NULL,
#'   defaults to 0.80. Supply either `power` or `n`, not both.
#' @param n integer or NULL. Total sample size. If supplied, power is calculated
#'   instead of sample size.
#' @param aratios numeric vector or NULL. Allocation ratio(s). For a 2-arm
#'   trial `c(1, 2)` means 1 control : 2 treatment. Default is equal allocation.
#' @param ltfu numeric or NULL. Proportion lost to follow-up (0 to <1). The
#'   total sample size will be inflated accordingly.
#' @param onesided logical. If TRUE, the significance level `alpha` is treated
#'   as one-sided (default FALSE).
#' @param favourable logical or NULL. If TRUE the outcome is favourable
#'   (higher probability is better); if FALSE it is unfavourable. If NULL
#'   (default) the program infers the direction.
#' @param condit logical. If TRUE applies a conditional test using Peto's
#'   odds-ratio approximation (default FALSE).
#' @param local logical. If TRUE calculates under local alternatives; valid
#'   only for small treatment effects (default FALSE).
#' @param trend logical. If TRUE applies a linear trend test for k-group trials
#'   (default FALSE).
#' @param doses numeric vector or NULL. Doses for the linear trend test.
#'   Default is 0, 1, ..., K-1.
#' @param nvmethod integer or NULL. Method for estimating null-hypothesis
#'   variance. 1 = sample estimates, 2 = fixed marginal totals,
#'   3 = constrained maximum likelihood (default). Set automatically to 1
#'   when `wald = TRUE`.
#' @param wald logical. If TRUE applies a Wald test instead of the score test
#'   (default FALSE).
#' @param ccorrect logical. If TRUE applies a continuity correction
#'   (default FALSE).
#' @param noround logical. If TRUE the per-group sample sizes are not rounded
#'   up to the nearest integer (default FALSE).
#' @param force logical. If TRUE overrides the program's inference of the
#'   favourable/unfavourable outcome type (default FALSE).
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{`n`}{Total sample size (calculated or designed).}
#'     \item{`n1`, `n2`, ...}{Sample size per group.}
#'     \item{`power`}{Power (calculated or designed).}
#'     \item{`D`}{Expected total number of events.}
#'     \item{`D1`, `D2`, ...}{Expected number of events per group.}
#'   }
#'
#' @references
#' Marley-Zagar E, White IR, Royston P, Babiker A, Barthel F (2023).
#' artbin: Extended Sample-Size Calculation for Randomized Trials with
#' Binary Outcomes. *Stata Journal*, 23(1), 93–123.
#'
#' Farrington CP, Manning G (1990). Test Statistics and Sample Size Formulae
#' for Comparative Binomial Trials with Null Hypothesis of Non-Zero Risk
#' Difference or Non-Unity Relative Risk. *Statistics in Medicine*, 9(12),
#' 1447–1454.
#'
#' Blackwelder WC (1982). "Proving the null hypothesis" in clinical trials.
#' *Controlled Clinical Trials*, 3(4), 345–353.
#'
#' @examples
#' # Superiority trial: Pocock 1983 (Anturan trial)
#' artbin(pr = c(0.1, 0.05), alpha = 0.05, power = 0.9, wald = TRUE)
#'
#' # Non-inferiority trial: Blackwelder 1982
#' artbin(pr = c(0.1, 0.1), margin = 0.2, alpha = 0.1, power = 0.9, wald = TRUE)
#'
#' # One-sided non-inferiority
#' artbin(pr = c(0.9, 0.9), margin = -0.05, onesided = TRUE)
#'
#' # Multi-arm superiority
#' artbin(pr = c(0.1, 0.2, 0.3, 0.4), alpha = 0.1, power = 0.9)
#'
#' # STREAM trial: NI with 1:2 allocation and 20% LTFU
#' artbin(pr = c(0.7, 0.75), margin = -0.1, power = 0.8,
#'        aratios = c(1, 2), wald = TRUE, ltfu = 0.2)
#'
#' @export
artbin <- function(pr,
                   margin    = NULL,
                   alpha     = 0.05,
                   power     = NULL,
                   n         = NULL,
                   aratios   = NULL,
                   ltfu      = NULL,
                   onesided  = FALSE,
                   favourable = NULL,
                   condit    = FALSE,
                   local     = FALSE,
                   trend     = FALSE,
                   doses     = NULL,
                   nvmethod  = NULL,
                   wald      = FALSE,
                   ccorrect  = FALSE,
                   noround   = FALSE,
                   force     = FALSE) {

  # ------------------------------------------------------------------ #
  # 1.  INPUT VALIDATION                                                 #
  # ------------------------------------------------------------------ #

  if (!is.numeric(pr) || length(pr) < 2)
    stop("pr must be a numeric vector of length >= 2")
  if (any(pr <= 0) || any(pr >= 1))
    stop("Event probabilities out of range (must be strictly between 0 and 1)")

  npr <- length(pr)

  if (!is.null(n) && !is.null(power))
    stop("You can't specify both n and power")
  if (is.null(n) && is.null(power))
    power <- 0.8

  niss <- !is.null(margin) && margin != 0   # NI or substantial-sup flag

  if (niss && npr > 2)
    stop("Can not have margin with >2 groups")

  if (npr == 2 && is.null(margin) && pr[1] == pr[2])
    stop("Event probabilities can not be equal with 2 groups")

  # Old syntax traps
  if (!is.null(margin) && margin == 0 && niss)
    stop("You are using the old syntax. For a superiority trial do not specify margin, or use margin = 0.")

  # Onesided / twosided
  sided <- if (onesided) "one" else "two"

  # ccorrect
  if (ccorrect && npr > 2)
    stop("Correction for continuity not allowed in comparison of > 2 groups")

  # Option combination checks
  if (local && wald)
    stop("Local and Wald not allowed together")
  if (condit && wald)
    stop("Conditional and Wald not allowed together")
  if (wald && !is.null(nvmethod) && nvmethod != 1)
    stop("Need nvm(1) if Wald specified")
  if (wald && is.null(nvmethod))
    nvmethod <- 1L
  if (is.null(nvmethod))
    nvmethod <- 3L
  nvmethod <- as.integer(nvmethod)
  if (nvmethod < 1 | nvmethod > 3) nvmethod <- 3L
  if (local && nvmethod != 3)
    stop("Need nvm(3) if local specified")
  if (niss && condit)
    stop("Can not select conditional option for non-inferiority/substantial-superiority trial")
  if (npr == 2 && trend)
    stop("Can not select trend option for a 2-arm trial")
  if (npr == 2 && !is.null(doses))
    stop("Can not select doses option for a 2-arm trial")

  # Conditional implies local
  if (condit && !local) {
    message("NOTE: As conditional has been selected local will be used.")
    local <- TRUE
  }

  if (npr == 2 && (is.null(margin) || margin == 0) && condit && ccorrect)
    stop("Sorry ccorrect is not currently available in the 2-arm superiority conditional case")
  if (onesided && npr > 2 && !trend && is.null(doses))
    stop("One-sided not allowed in comparison of > 2 groups unless trend/doses specified")

  # ------------------------------------------------------------------ #
  # 2.  LTFU / n HANDLING                                                #
  # ------------------------------------------------------------------ #
  if (!is.null(ltfu)) {
    if (ltfu < 0 || ltfu >= 1) stop("ltfu must be in [0, 1)")
    obsfrac <- 1 - ltfu
  } else {
    obsfrac <- 1
  }

  ntotal_given <- NULL   # the n that was given by user (with LTFU inflation)
  n_for_calc   <- 0      # n passed to art2bin/kgroup (observed)

  if (!is.null(n)) {
    noround <- TRUE       # when n is given, do not re-round
    ntotal_given <- n
    if (!is.null(ltfu)) {
      n_for_calc <- round(n * obsfrac)   # observed sample size (integer)
    } else {
      n_for_calc <- n
    }
  }

  ssize <- is.null(n)   # TRUE = calculate SS

  # ------------------------------------------------------------------ #
  # 3.  ALLOCATION RATIOS                                                #
  # ------------------------------------------------------------------ #
  if (is.null(aratios)) {
    allr <- rep(1, npr)
    nar  <- npr
  } else {
    allr <- aratios
    nar  <- length(allr)
    if (npr > 2 && nar < npr)
      stop("Please specify the same number of aratios() as pr() for >2 groups")
    if (nar == 1) {
      allr <- c(1, allr)   # e.g. aratios=2 means 1:2
      nar  <- 2
    }
  }

  # Rescale so allr[1] == 1
  if (allr[1] != 1) {
    allr <- allr / allr[1]
  }
  totalallr <- sum(allr)

  # ------------------------------------------------------------------ #
  # 4.  FAVOURABLE / UNFAVOURABLE INFERENCE (2-arm only)                 #
  # ------------------------------------------------------------------ #
  trialtype    <- "superiority"
  trialoutcome <- NA_character_
  H0 <- H1 <- NULL

  if (npr == 2) {
    if (is.null(margin)) margin <- 0
    threshold <- pr[1] + margin

    if (is.null(favourable)) {
      if (pr[2] < threshold) trialoutcome <- "unfavourable"
      else if (pr[2] > threshold) trialoutcome <- "favourable"
      else stop("p2 can not equal p1 + margin")
    } else {
      trialoutcome <- if (isTRUE(favourable)) "favourable" else "unfavourable"
    }

    # Consistency checks
    if (trialoutcome == "unfavourable" && threshold < pr[2]) {
      if (!force) stop("artbin thinks your outcome is favourable. Please check your command. If your command is correct then consider using force = TRUE.")
      else warning("artbin thinks your outcome should be favourable.")
    }
    if (trialoutcome == "favourable" && threshold > pr[2]) {
      if (!force) stop("artbin thinks your outcome is unfavourable. Please check your command. If your command is correct then consider using force = TRUE.")
      else warning("artbin thinks your outcome should be unfavourable.")
    }

    if ((trialoutcome == "unfavourable" && margin > 0) ||
        (trialoutcome == "favourable"   && margin < 0)) {
      trialtype <- "non-inferiority"
    } else if ((trialoutcome == "unfavourable" && margin < 0) ||
               (trialoutcome == "favourable"   && margin > 0)) {
      trialtype <- "substantial-superiority"
    }

    if (trialoutcome == "unfavourable") {
      H0 <- sprintf("H0: pi2 - pi1 >= %s", margin)
      H1 <- sprintf("H1: pi2 - pi1 < %s",  margin)
    } else {
      H0 <- sprintf("H0: pi2 - pi1 <= %s", margin)
      H1 <- sprintf("H1: pi2 - pi1 > %s",  margin)
    }

    # Catch misuse of superiority option
    if (!niss && trialtype == "non-inferiority")
      stop("Can not select non-inferiority trial and superiority option (margin = 0 or missing)")
  }

  # ------------------------------------------------------------------ #
  # 5.  CORE CALCULATION                                                  #
  # ------------------------------------------------------------------ #

  # Route: 2-arm without condit → art2bin; otherwise k-group
  use_art2bin <- (npr == 2 && !condit)

  if (use_art2bin) {
    # alpha for art2bin: two-sided value; onesided handled inside
    # For onesided the art2bin function uses qnorm(1-alpha) directly
    res <- .art2bin(
      p0       = pr[1],
      p1       = pr[2],
      margin   = if (is.null(margin)) 0 else margin,
      n        = if (ssize) 0 else n_for_calc,
      ar       = if (is.null(aratios)) NULL else aratios,
      alpha    = alpha,
      power    = if (!ssize) 0.8 else power,   # dummy power for power-calc path
      nvmethod = nvmethod,
      onesided = onesided,
      ccorrect = ccorrect,
      local    = local,
      wald     = wald
    )

    n_unrounded <- res$n
    power_result <- res$power

  } else {
    # K-group path
    # For onesided: artbin.ado doubles alpha before k-group calc
    alpha_kg <- if (onesided) 2 * alpha else alpha

    # Pass allocation ratios as original (before rescaling)
    ar_for_kg <- if (is.null(aratios)) NULL else aratios

    res <- .artbin_kgroup(
      pr       = pr,
      ar_raw   = ar_for_kg,
      alpha    = alpha_kg,
      power    = if (!ssize) 0.8 else power,
      n        = if (ssize) 0 else n_for_calc,
      onesided = onesided,
      local    = local,
      condit   = condit,
      wald     = wald,
      trend    = trend,
      doses    = doses
    )

    n_unrounded  <- res$n
    power_result <- res$power
  }

  # ------------------------------------------------------------------ #
  # 6.  ROUNDING & PER-ARM RESULTS                                       #
  # ------------------------------------------------------------------ #

  nbygroup <- n_unrounded / totalallr

  n_arms  <- numeric(npr)
  D_arms  <- numeric(npr)

  if (ssize) {
    for (a in seq_len(npr)) {
      if (noround) {
        n_arms[a] <- nbygroup * allr[a] / obsfrac
      } else {
        n_arms[a] <- ceiling(nbygroup * allr[a] / obsfrac)
      }
      D_arms[a] <- n_arms[a] * pr[a] * obsfrac
    }
    ntotal <- sum(n_arms)
  } else {
    ntotal <- ntotal_given
    for (a in seq_len(npr)) {
      n_arms[a] <- ntotal * allr[a] / totalallr
      D_arms[a] <- n_arms[a] * pr[a] * obsfrac
    }
  }

  D_total <- sum(D_arms)

  # ------------------------------------------------------------------ #
  # 7.  ASSEMBLE RETURN VALUE                                             #
  # ------------------------------------------------------------------ #
  out <- list(n = ntotal, power = power_result, D = D_total)

  for (a in seq_len(npr)) {
    out[[paste0("n", a)]] <- n_arms[a]
    out[[paste0("D", a)]] <- D_arms[a]
  }

  out
}
