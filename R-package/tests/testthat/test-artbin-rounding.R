# Tests for rounding behaviour
# Ported from artbin_test_rounding.do (IW, 5/12/2023)

# Helper: run both noround and rounded, check properties
check_rounding <- function(args_str, pr, ...) {
  args <- list(pr = pr, ...)
  res_nr <- do.call(artbin, c(args, list(noround = TRUE)))
  res_r  <- do.call(artbin, args)

  narms <- sum(!is.null(res_nr$n3), TRUE, TRUE)   # hacky: check n3 exists
  # Determine number of arms
  arm_names <- grep("^n[0-9]+$", names(res_nr), value = TRUE)
  narms <- length(arm_names)

  for (a in seq_len(narms)) {
    nr_a <- res_nr[[paste0("n", a)]]
    r_a  <- res_r[[paste0("n", a)]]
    # Rounded n == ceiling(unrounded n)
    expect_equal(r_a, ceiling(nr_a),
                 label = paste0("rounding arm ", a, ": ", args_str))
    # D ratio preserved
    d_nr <- res_nr[[paste0("D", a)]]
    d_r  <- res_r[[paste0("D", a)]]
    expect_equal(d_r / r_a, d_nr / nr_a, tolerance = 1e-9,
                 label = paste0("D ratio arm ", a, ": ", args_str))
  }
  # Overall totals are sums of per-arm
  n_total  <- sum(vapply(seq_len(narms), function(a) res_r[[paste0("n", a)]], numeric(1)))
  D_total  <- sum(vapply(seq_len(narms), function(a) res_r[[paste0("D", a)]], numeric(1)))
  expect_equal(res_r$n, n_total, label = paste0("total n: ", args_str))
  expect_equal(res_r$D, D_total, tolerance = 1e-12,
               label = paste0("total D: ", args_str))
}

test_that("Rounding: 2-arm NI, aratios(1,2)", {
  check_rounding("NI 1:2",
    pr = c(0.02, 0.02), margin = 0.02, aratios = c(1, 2))
})

test_that("Rounding: 2-arm superiority, aratios(1,2)", {
  check_rounding("sup 1:2",
    pr = c(0.02, 0.04), aratios = c(1, 2))
})

test_that("Rounding: 2-arm superiority, aratios(10,17)", {
  check_rounding("sup 10:17",
    pr = c(0.2, 0.3), aratios = c(10, 17))
})

test_that("Rounding: 3-arm superiority, aratios(3,2,1)", {
  check_rounding("3-arm 3:2:1",
    pr = c(0.02, 0.04, 0.06), aratios = c(3, 2, 1))
})

test_that("Rounding: 3-arm trend, aratios(3,2,1)", {
  check_rounding("3-arm trend 3:2:1",
    pr = c(0.02, 0.04, 0.06), aratios = c(3, 2, 1), trend = TRUE)
})
