# Tests for error handling
# Ported from artbin_errortest_8.do (Stata)

test_that("Error: n and power both specified", {
  expect_error(artbin(pr = c(0.1, 0.2), n = 100, power = 0.8))
})

test_that("Error: pr must have >= 2 elements", {
  expect_error(artbin(pr = c(0.1)))
})

test_that("Error: event probabilities out of range (0)", {
  expect_error(artbin(pr = c(0, 0.2)))
})

test_that("Error: event probabilities out of range (1)", {
  expect_error(artbin(pr = c(0.1, 1.0)))
})

test_that("Error: event probabilities out of range (>1)", {
  expect_error(artbin(pr = c(0.1, 1.5)))
})

test_that("Error: equal probabilities with 2 groups and no margin", {
  expect_error(artbin(pr = c(0.2, 0.2)))
})

test_that("Error: margin with >2 groups", {
  expect_error(artbin(pr = c(0.1, 0.2, 0.3), margin = 0.05))
})

test_that("Error: local and wald together", {
  expect_error(artbin(pr = c(0.1, 0.2), local = TRUE, wald = TRUE))
})

test_that("Error: condit and wald together", {
  expect_error(artbin(pr = c(0.1, 0.2), condit = TRUE, wald = TRUE))
})

test_that("Error: wald with nvmethod=2", {
  expect_error(artbin(pr = c(0.1, 0.2), wald = TRUE, nvmethod = 2))
})

test_that("Error: wald with nvmethod=3", {
  expect_error(artbin(pr = c(0.1, 0.2), wald = TRUE, nvmethod = 3))
})

test_that("Error: local with nvmethod=1", {
  expect_error(artbin(pr = c(0.1, 0.2), local = TRUE, nvmethod = 1))
})

test_that("Error: local with nvmethod=2", {
  expect_error(artbin(pr = c(0.1, 0.2), local = TRUE, nvmethod = 2))
})

test_that("Error: NI trial with condit", {
  expect_error(artbin(pr = c(0.1, 0.1), margin = 0.1, condit = TRUE))
})

test_that("Error: trend for 2-arm trial", {
  expect_error(artbin(pr = c(0.1, 0.2), trend = TRUE))
})

test_that("Error: doses for 2-arm trial", {
  expect_error(artbin(pr = c(0.1, 0.2), doses = c(0, 1)))
})

test_that("Error: ccorrect for >2 groups", {
  expect_error(artbin(pr = c(0.1, 0.2, 0.3), ccorrect = TRUE))
})

test_that("Error: onesided for >2 groups without trend", {
  expect_error(artbin(pr = c(0.1, 0.2, 0.3), onesided = TRUE))
})

test_that("Error: p2 == p1 + margin", {
  expect_error(artbin(pr = c(0.1, 0.2), margin = 0.1))   # 0.1 + 0.1 = 0.2 = p2
})

test_that("Error: inconsistent favourable + outcome direction (no force)", {
  # pr(0.1, 0.2): favourable means p2>p1, so favourable=FALSE (unfavourable) is wrong
  expect_error(artbin(pr = c(0.1, 0.2), favourable = FALSE))
})

test_that("No error: inconsistent with force=TRUE gives warning", {
  expect_warning(
    artbin(pr = c(0.1, 0.2), favourable = FALSE, force = TRUE)
  )
})

test_that("Error: ltfu out of range", {
  expect_error(artbin(pr = c(0.1, 0.2), ltfu = 1.0))
  expect_error(artbin(pr = c(0.1, 0.2), ltfu = -0.1))
})

test_that("Error: >2 groups with too few aratios", {
  expect_error(artbin(pr = c(0.1, 0.2, 0.3), aratios = c(1, 2)))
})

test_that("No error: valid 2-arm NI onesided", {
  res <- artbin(pr = c(0.1, 0.1), margin = 0.05, onesided = TRUE, power = 0.8)
  expect_true(is.numeric(res$n) && res$n > 0)
})

test_that("No error: valid 2-arm superiority with all options", {
  res <- artbin(pr = c(0.1, 0.2), alpha = 0.05, power = 0.9,
                wald = TRUE, aratios = c(1, 2))
  expect_true(is.numeric(res$n) && res$n > 0)
})
