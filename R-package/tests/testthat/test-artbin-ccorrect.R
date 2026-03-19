# Tests for continuity correction option
# Ported from artbin_testing_3.do (Stata)

test_that("ccorrect increases sample size vs no correction", {
  res_no  <- artbin(pr = c(0.1, 0.2), alpha = 0.05, power = 0.8)
  res_cc  <- artbin(pr = c(0.1, 0.2), alpha = 0.05, power = 0.8, ccorrect = TRUE)
  expect_true(res_cc$n >= res_no$n)
})

test_that("ccorrect works for NI trial", {
  res_cc <- artbin(pr = c(0.1, 0.1), margin = 0.1, alpha = 0.05, power = 0.8,
                   ccorrect = TRUE)
  expect_true(is.numeric(res_cc$n) && res_cc$n > 0)
})

test_that("ccorrect power calculation", {
  res_ss <- artbin(pr = c(0.1, 0.2), alpha = 0.05, power = 0.8, ccorrect = TRUE)
  res_pw <- artbin(pr = c(0.1, 0.2), alpha = 0.05, n = res_ss$n, ccorrect = TRUE)
  expect_equal(round(res_pw$power, 1), 0.8)
})

test_that("ccorrect with onesided works", {
  res <- artbin(pr = c(0.1, 0.2), alpha = 0.05, power = 0.8,
                ccorrect = TRUE, onesided = TRUE)
  expect_true(is.numeric(res$n) && res$n > 0)
})

test_that("ccorrect with local works", {
  res <- artbin(pr = c(0.1, 0.2), alpha = 0.05, power = 0.8,
                ccorrect = TRUE, local = TRUE)
  expect_true(is.numeric(res$n) && res$n > 0)
})
