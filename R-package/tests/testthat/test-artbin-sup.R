# Tests for 2-arm superiority trials
# Ported from artbin_testing_2.do (Stata)
# Reference values from Pocock 1983 and Sealed Envelope calculator

test_that("Superiority SS: Pocock 1983 — pr(0.05, 0.1), alpha=5%, power=90% → n=1156", {
  # artbin, pr(0.05 0.1) alpha(0.05) power(0.9) wald → n=1156, 578/arm
  res <- artbin(pr = c(0.05, 0.1), alpha = 0.05, power = 0.9, wald = TRUE)
  expect_equal(res$n, 1156)
  expect_equal(res$n1, 578)
  expect_equal(res$n2, 578)
})

test_that("Superiority SS: Sealed envelope — pr(0.1, 0.2), alpha=10%, power=80% → n=310", {
  # artbin, pr(0.1 0.2) alpha(0.1) power(0.8) wald → n=310, 155/arm
  res <- artbin(pr = c(0.1, 0.2), alpha = 0.1, power = 0.8, wald = TRUE)
  expect_equal(res$n, 310)
  expect_equal(res$n1, 155)
  expect_equal(res$n2, 155)
})

test_that("Superiority power: Pocock 1983 — n=1156 → power≈0.9", {
  res <- artbin(pr = c(0.05, 0.1), alpha = 0.05, n = 1156, wald = TRUE)
  expect_equal(round(res$power, 1), 0.9)
})

test_that("Superiority power: Sealed envelope — n=310 → power≈0.8", {
  res <- artbin(pr = c(0.1, 0.2), alpha = 0.1, n = 310, wald = TRUE)
  expect_equal(round(res$power, 1), 0.8)
})

# ---- Score test (default) matches Stata power command ----

test_that("Score test (nvm=3) gives 1164, matching Stata power command", {
  # artbin, pr(0.1 0.05) alpha(0.05) power(0.9) → 1164 (score, nvm=3)
  # Stata: power twoproportions 0.1 0.05, alpha(0.05) power(0.9) → same 1164
  res <- artbin(pr = c(0.1, 0.05), alpha = 0.05, power = 0.9)
  expect_equal(res$n, 1164)
})

test_that("Score (nvm=1) and Wald both give 1156 for Pocock case", {
  res_s <- artbin(pr = c(0.1, 0.05), alpha = 0.05, power = 0.9, nvmethod = 1)
  res_w <- artbin(pr = c(0.1, 0.05), alpha = 0.05, power = 0.9, wald = TRUE)
  expect_equal(res_s$n, 1156)
  expect_equal(res_w$n, 1156)
})

# ---- pr(0.1, 0.05) with wald = TRUE matches Pocock 1983 ----

test_that("Superiority: pr(0.1, 0.05) same as pr(0.05, 0.1) → n=1156", {
  res1 <- artbin(pr = c(0.1, 0.05), alpha = 0.05, power = 0.9, wald = TRUE)
  res2 <- artbin(pr = c(0.05, 0.1), alpha = 0.05, power = 0.9, wald = TRUE)
  expect_equal(res1$n, 1156)
  expect_equal(res2$n, 1156)
})

# ---- local alternative ----

test_that("Superiority local: gives larger SS than distant", {
  res_dist  <- artbin(pr = c(0.1, 0.2), alpha = 0.05, power = 0.8)
  res_local <- artbin(pr = c(0.1, 0.2), alpha = 0.05, power = 0.8, local = TRUE)
  # Local should give larger SS for typical superiority trial
  expect_true(res_local$n >= res_dist$n)
})

# ---- noround option ----

test_that("noround returns non-integer sample size", {
  res <- artbin(pr = c(0.1, 0.2), alpha = 0.05, power = 0.8, noround = TRUE)
  expect_false(res$n == round(res$n))
})

# ---- favourable / unfavourable options ----

test_that("favourable = TRUE gives same result as inferred for pr(0.9, 0.95)", {
  res_inf <- artbin(pr = c(0.9, 0.95), alpha = 0.05, power = 0.9, wald = TRUE)
  res_fav <- artbin(pr = c(0.9, 0.95), alpha = 0.05, power = 0.9, wald = TRUE,
                    favourable = TRUE)
  expect_equal(res_inf$n, res_fav$n)
})
