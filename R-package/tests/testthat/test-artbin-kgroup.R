# Tests for multi-arm (k-group) superiority trials
# Ported from artbin_testing_7.do and artbin_examples.do (Stata)

test_that("4-arm superiority: pr(0.1, 0.2, 0.3, 0.4), alpha=10%, power=90% → n=176 (44/arm)", {
  # artbin, pr(0.1 0.2 0.3 0.4) alpha(0.1) power(0.9) → n=176, 44/arm
  res <- artbin(pr = c(0.1, 0.2, 0.3, 0.4), alpha = 0.1, power = 0.9)
  expect_equal(res$n, 176)
  expect_equal(res$n1, 44)
  expect_equal(res$n2, 44)
  expect_equal(res$n3, 44)
  expect_equal(res$n4, 44)
})

test_that("3-arm superiority (distant): pr(0.1, 0.2, 0.3)", {
  # artbin, pr(0.1 0.2 0.3) → should return a valid sample size
  res <- artbin(pr = c(0.1, 0.2, 0.3))
  expect_true(is.numeric(res$n) && res$n > 0)
  expect_equal(length(res$n1), 1)
})

test_that("3-arm superiority (local): pr(0.1, 0.2, 0.3) local → larger SS than distant", {
  res_d <- artbin(pr = c(0.1, 0.2, 0.3))
  res_l <- artbin(pr = c(0.1, 0.2, 0.3), local = TRUE)
  expect_true(res_l$n >= res_d$n)
})

test_that("3-arm power calculation matches SS", {
  res_ss <- artbin(pr = c(0.1, 0.2, 0.3), power = 0.8)
  res_pw <- artbin(pr = c(0.1, 0.2, 0.3), n = res_ss$n)
  expect_equal(round(res_pw$power, 1), 0.8)
})

test_that("3-arm with unequal aratios: pr(0.1, 0.2, 0.3), aratios(3, 2, 1)", {
  res <- artbin(pr = c(0.1, 0.2, 0.3), aratios = c(3, 2, 1))
  expect_true(is.numeric(res$n) && res$n > 0)
  # Arm sizes proportional to aratios
  expect_equal(res$n1 * 2, res$n2 * 3, tolerance = 1)
})

test_that("2-arm conditional: pr(0.1, 0.2), condit=TRUE → k-group path with NOTE", {
  expect_message(
    res <- artbin(pr = c(0.1, 0.2), condit = TRUE),
    "conditional"
  )
  expect_true(is.numeric(res$n) && res$n > 0)
})

test_that("Trend test: pr(0.1, 0.2, 0.3), trend=TRUE", {
  res <- artbin(pr = c(0.1, 0.2, 0.3), trend = TRUE)
  expect_true(is.numeric(res$n) && res$n > 0)
})

test_that("Trend test with doses: pr(0.1, 0.2, 0.3), doses=c(0, 1, 4)", {
  res <- artbin(pr = c(0.1, 0.2, 0.3), trend = TRUE, doses = c(0, 1, 4))
  expect_true(is.numeric(res$n) && res$n > 0)
})

test_that("Wald test k-group: pr(0.1, 0.2, 0.3), wald=TRUE", {
  # wald uses the simple NCP formula
  res_score <- artbin(pr = c(0.1, 0.2, 0.3))
  res_wald  <- artbin(pr = c(0.1, 0.2, 0.3), wald = TRUE, nvmethod = 1)
  # Both should give valid results
  expect_true(is.numeric(res_score$n) && res_score$n > 0)
  expect_true(is.numeric(res_wald$n)  && res_wald$n  > 0)
})

test_that("4-arm power back-calculation matches SS", {
  res_ss <- artbin(pr = c(0.1, 0.2, 0.3, 0.4), alpha = 0.1, power = 0.9)
  res_pw <- artbin(pr = c(0.1, 0.2, 0.3, 0.4), alpha = 0.1, n = res_ss$n)
  expect_equal(round(res_pw$power, 1), 0.9)
})

test_that("Conditional trend test: pr(0.1, 0.2, 0.3), condit=TRUE, trend=TRUE", {
  expect_message(
    res <- artbin(pr = c(0.1, 0.2, 0.3), condit = TRUE, trend = TRUE),
    "conditional"
  )
  expect_true(is.numeric(res$n) && res$n > 0)
})

test_that("3-arm noround: n is not integer", {
  res <- artbin(pr = c(0.1, 0.2, 0.3), noround = TRUE)
  expect_false(res$n1 == round(res$n1))
})
