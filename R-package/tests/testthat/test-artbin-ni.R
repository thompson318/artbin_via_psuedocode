# Tests for non-inferiority and substantial-superiority trials
# Ported from artbin_testing_1.do (Stata)
# Reference: artbin_testing_1.do, artbin_testing_4.do

test_that("NI: Blackwelder 1982 — p=90%, d=20%, alpha=5%, beta=10%", {
  # artbin, pr(0.1 0.1) margin(0.2) alpha(0.1) power(0.9) wald → n=78, 39/arm
  res <- artbin(pr = c(0.1, 0.1), margin = 0.2, alpha = 0.1, power = 0.9, wald = TRUE)
  expect_equal(res$n, 78)
  expect_equal(res$n1, 39)
  expect_equal(res$n2, 39)
})

test_that("NI: Julious 2011 Table 4 — p=70%, d=5%, alpha=2.5%, beta=10%", {
  # artbin, pr(0.3 0.3) margin(0.05) alpha(0.05) power(0.9) wald → n=3532, 1766/arm
  res <- artbin(pr = c(0.3, 0.3), margin = 0.05, alpha = 0.05, power = 0.9, wald = TRUE)
  expect_equal(res$n, 3532)
  expect_equal(res$n1, 1766)
})

test_that("NI: Pocock 2003 — p=85%, d=15%, alpha=5%, beta=10%", {
  # artbin, pr(0.15 0.15) margin(0.15) alpha(0.05) power(0.9) wald → n=240, 120/arm
  res <- artbin(pr = c(0.15, 0.15), margin = 0.15, alpha = 0.05, power = 0.9, wald = TRUE)
  expect_equal(res$n, 240)
  expect_equal(res$n1, 120)
})

test_that("NI: Sealed envelope — p=80%, d=10%, alpha=10%, beta=20%", {
  # artbin, pr(0.2 0.2) margin(0.1) alpha(0.2) power(0.8) wald → n=290, 145/arm
  res <- artbin(pr = c(0.2, 0.2), margin = 0.1, alpha = 0.2, power = 0.8, wald = TRUE)
  expect_equal(res$n, 290)
  expect_equal(res$n1, 145)
})

test_that("NI: Julious 2011 Table 4 — p=90%, d=5%, alpha=2.5%, beta=10%", {
  # artbin, pr(0.1 0.1) margin(0.05) alpha(0.05) power(0.9) wald → n=1514, 757/arm
  res <- artbin(pr = c(0.1, 0.1), margin = 0.05, alpha = 0.05, power = 0.9, wald = TRUE)
  expect_equal(res$n, 1514)
  expect_equal(res$n1, 757)
})

test_that("NI: Julious 2011 Table 4 — p=75%, d=20%, alpha=2.5%, beta=10%", {
  # → n=198, 99/arm
  res <- artbin(pr = c(0.25, 0.25), margin = 0.2, alpha = 0.05, power = 0.9, wald = TRUE)
  expect_equal(res$n, 198)
  expect_equal(res$n1, 99)
})

test_that("NI: Julious 2011 Table 4 — p=80%, d=15%, alpha=2.5%, beta=10%", {
  # → n=300, 150/arm
  res <- artbin(pr = c(0.2, 0.2), margin = 0.15, alpha = 0.05, power = 0.9, wald = TRUE)
  expect_equal(res$n, 300)
  expect_equal(res$n1, 150)
})

test_that("NI: Julious 2011 Table 4 — p=85%, d=5%, alpha=2.5%, beta=10%", {
  # → n=2144, 1072/arm
  res <- artbin(pr = c(0.15, 0.15), margin = 0.05, alpha = 0.05, power = 0.9, wald = TRUE)
  expect_equal(res$n, 2144)
  expect_equal(res$n1, 1072)
})

# ---- Power calculations from NI tests ----

test_that("NI power: Blackwelder 1982 — n=78 → power≈0.9", {
  res <- artbin(pr = c(0.1, 0.1), margin = 0.2, alpha = 0.1, n = 78, wald = TRUE)
  expect_equal(round(res$power, 1), 0.9)
})

test_that("NI power: Julious 2011 Table 4 — p=70%, d=5%, n=3532 → power≈0.9", {
  res <- artbin(pr = c(0.3, 0.3), margin = 0.05, alpha = 0.05, n = 3532, wald = TRUE)
  expect_equal(round(res$power, 1), 0.9)
})

test_that("NI power: Pocock 2003 — p=85%, d=15%, n=240 → power≈0.9", {
  res <- artbin(pr = c(0.15, 0.15), margin = 0.15, alpha = 0.05, n = 240, wald = TRUE)
  expect_equal(round(res$power, 1), 0.9)
})

# ---- NI with allocation ratios ----

test_that("NI: one-sided, aratio(2) matches niss reference (≤1 off)", {
  # niss reference: 51; artbin, pr(0.3 0.1) margin(0.2) alpha(0.025) onesided power(0.9) aratio(2) wald → 51
  res <- artbin(pr = c(0.3, 0.1), margin = 0.2, alpha = 0.025, onesided = TRUE,
                power = 0.9, aratios = c(1, 2), wald = TRUE)
  expect_true(res$n %in% 51)
})

# ---- Substantial-superiority ----

test_that("Substantial-superiority: Palisade 2018 — 1:3 allocation → n=391", {
  # artbin, pr(.2 .5) margin(.15) aratio(1 3) → 391
  res <- artbin(pr = c(0.2, 0.5), margin = 0.15, aratios = c(1, 3))
  expect_equal(res$n, 391)
})

# ---- NI with nvmethod (score test, default nvm=3) ----

test_that("NI score test (nvm=3): pr(0.1, 0.1), margin=0.2, alpha=0.1, power=0.9", {
  # Should give slightly different result from wald
  res <- artbin(pr = c(0.1, 0.1), margin = 0.2, alpha = 0.1, power = 0.9)
  expect_true(is.numeric(res$n) && res$n > 0)
})

test_that("NI one-sided: pr(0.9, 0.9), margin=-0.05 → n=914 (457 per arm)", {
  # artbin, pr(0.9 0.9) margin(-0.05) onesided → n=914
  res <- artbin(pr = c(0.9, 0.9), margin = -0.05, onesided = TRUE)
  expect_equal(res$n, 914)
  expect_equal(res$n1, 457)
  expect_equal(res$n2, 457)
})
