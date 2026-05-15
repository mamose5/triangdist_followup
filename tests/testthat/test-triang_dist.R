test_that("dtriang works", {
  expect_equal(dtriang(0.5, 0, 1, 0.5), 2)
  expect_equal(dtriang(-1, 0, 1, 0.5), 0)
  expect_equal(dtriang(2, 0, 1, 0.5), 0)
  expect_equal(dtriang(0, 0, 1, 0.5), 0)
  expect_equal(dtriang(1, 0, 1, 0.5), 0)
  expect_equal(dtriang(0.3, 0, 1, 0.5), 2 * 0.3 / 0.5)
  expect_equal(dtriang(0.7, 0, 1, 0.5), 2 * 0.3 / 0.5)
  expect_error(dtriang(0.5, 1, 0, 0.5))
  expect_error(dtriang(0.5, 0, 1, 2))
})

test_that("ptriang works", {
  expect_equal(ptriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(1, 0, 1, 0.5), 1)
  expect_equal(ptriang(0.5, 0, 1, 0.5), 0.5)
  expect_equal(ptriang(-1, 0, 1, 0.5), 0)
  expect_equal(ptriang(2, 0, 1, 0.5), 1)
  expect_equal(ptriang(0.3, 0, 1, 0.5), (0.3^2) / (1 * 0.5))
  expect_equal(ptriang(0.7, 0, 1, 0.5), 1 - (0.3^2) / (1 * 0.5))
})

test_that("qtriang works", {
  expect_equal(qtriang(0, 0, 1, 0.5), 0)
  expect_equal(qtriang(1, 0, 1, 0.5), 1)
  expect_equal(qtriang(0.5, 0, 1, 0.5), 0.5)
  expect_error(qtriang(-0.1, 0, 1, 0.5))
  expect_error(qtriang(1.1, 0, 1, 0.5))
  expect_equal(qtriang(0.25, 0, 1, 0.5), sqrt(0.25 * 1 * 0.5))
  expect_equal(qtriang(0.75, 0, 1, 0.5), 1 - sqrt(0.25 * 1 * 0.5))
})

test_that("rtriang works", {
  set.seed(123)
  samp <- rtriang(100, 0, 1, 0.5)
  expect_true(all(samp >= 0 & samp <= 1))
  expect_length(samp, 100)
})

test_that("vectorization works", {
  expect_equal(
    dtriang(c(0.2, 0.8), c(0,0), c(1,1), c(0.5,0.5)),
    c(dtriang(0.2,0,1,0.5), dtriang(0.8,0,1,0.5))
  )
  expect_equal(
    ptriang(c(0.2, 0.8), c(0,0), c(1,1), c(0.5,0.5)),
    c(ptriang(0.2,0,1,0.5), ptriang(0.8,0,1,0.5))
  )
  expect_equal(
    qtriang(c(0.25, 0.75), c(0,0), c(1,1), c(0.5,0.5)),
    c(qtriang(0.25,0,1,0.5), qtriang(0.75,0,1,0.5))
  )
})

test_that("mode = min edge cases", {
  expect_equal(dtriang(0, 0, 1, 0), 2)
  expect_equal(dtriang(0.5, 0, 1, 0), 1)
  expect_equal(dtriang(1, 0, 1, 0), 0)
  expect_equal(ptriang(0.5, 0, 1, 0), 0.75)
  expect_equal(qtriang(0.75, 0, 1, 0), 0.5)
})

test_that("mode = max edge cases", {
  expect_equal(dtriang(0, 0, 1, 1), 0)
  expect_equal(dtriang(0.5, 0, 1, 1), 1)
  expect_equal(dtriang(1, 0, 1, 1), 2)
  expect_equal(ptriang(0.5, 0, 1, 1), 0.25)
  expect_equal(qtriang(0.25, 0, 1, 1), 0.5)
})

test_that("error handling works", {
  expect_error(dtriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(dtriang(0.5, min = 0, max = 1, mode = 2))
  expect_error(ptriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(ptriang(0.5, min = 0, max = 1, mode = 2))
  expect_error(qtriang(0.5, min = 1, max = 0, mode = 0.5))
  expect_error(qtriang(0.5, min = 0, max = 1, mode = 2))
})
