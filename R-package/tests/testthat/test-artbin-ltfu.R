# Tests for loss-to-follow-up (ltfu) option
# Ported from artbin_test_ltfu.do (IW, 22/11/2023)

test_that("STREAM trial: pr(0.7,0.75) margin(-0.1) wald aratios(1,2) ltfu(0.2) → n=398", {
  # artbin, pr(0.7 0.75) margin(-0.1) power(0.8) ar(1 2) wald ltfu(0.2) → n=398
  res <- artbin(pr = c(0.7, 0.75), margin = -0.1, power = 0.8,
                aratios = c(1, 2), wald = TRUE, ltfu = 0.2)
  expect_equal(res$n, 398)
  expect_equal(res$n1, 133)
  expect_equal(res$n2, 265)
})

test_that("LTFU power->SS: ltfu(0.1) inflates total n by 1/0.9 relative to no-ltfu", {
  res_no  <- artbin(pr = c(0.02, 0.02), margin = 0.02, noround = TRUE)
  res_ltfu <- artbin(pr = c(0.02, 0.02), margin = 0.02, noround = TRUE, ltfu = 0.1)
  expect_equal(res_ltfu$n, res_no$n / 0.9, tolerance = 1e-6)
})

test_that("LTFU power->SS: expected events same with and without ltfu(0.1)", {
  res_no  <- artbin(pr = c(0.02, 0.02), margin = 0.02, noround = TRUE)
  res_ltfu <- artbin(pr = c(0.02, 0.02), margin = 0.02, noround = TRUE, ltfu = 0.1)
  expect_equal(res_ltfu$D, res_no$D, tolerance = 1e-6)
})

test_that("LTFU n->power: n=1000 ltfu(0.1) gives same power as n=900 no-ltfu", {
  res_ltfu <- artbin(pr = c(0.02, 0.02), margin = 0.02, n = 1000, noround = TRUE,
                     ltfu = 0.1)
  res_noltfu <- artbin(pr = c(0.02, 0.02), margin = 0.02, n = 900, noround = TRUE)
  expect_equal(res_ltfu$power, res_noltfu$power, tolerance = 1e-6)
})

test_that("LTFU round-trip: n->power->n recovers original n (2-arm NI, 1:2 alloc)", {
  norig <- 1000
  res_pw <- artbin(pr = c(0.02, 0.02), margin = 0.02, aratios = c(1, 2),
                   n = norig, ltfu = 0.1)
  res_ss <- artbin(pr = c(0.02, 0.02), margin = 0.02, aratios = c(1, 2),
                   power = res_pw$power, ltfu = 0.1, noround = TRUE)
  expect_equal(res_ss$n, norig, tolerance = 1e-6)
})

test_that("LTFU round-trip: n->power->n (2-arm superiority 1:2 alloc)", {
  norig <- 1000
  res_pw <- artbin(pr = c(0.02, 0.04), aratios = c(1, 2), n = norig, ltfu = 0.1)
  res_ss <- artbin(pr = c(0.02, 0.04), aratios = c(1, 2),
                   power = res_pw$power, ltfu = 0.1, noround = TRUE)
  expect_equal(res_ss$n, norig, tolerance = 1e-6)
})

test_that("LTFU round-trip: n->power->n (3-arm with unequal aratios)", {
  norig <- 1000
  res_pw <- artbin(pr = c(0.02, 0.04, 0.06), aratios = c(3, 2, 1),
                   n = norig, ltfu = 0.1)
  res_ss <- artbin(pr = c(0.02, 0.04, 0.06), aratios = c(3, 2, 1),
                   power = res_pw$power, ltfu = 0.1, noround = TRUE)
  expect_equal(res_ss$n, norig, tolerance = 1e-6)
})

test_that("LTFU works with non-integer ltfu*n", {
  # artbin, pr(.02 .02) margin(.02) ltfu(.05) n(1836) → n==1836
  res <- artbin(pr = c(0.02, 0.02), margin = 0.02, ltfu = 0.05, n = 1836)
  expect_equal(res$n, 1836)
})
