## Test code for Normalization + transformation

x <- rnorm(1000)
y <- rnorm(1000)
z <- rnorm(1000)

test.data <- data.frame(x,y,z)

test_that("Check normalization, denormalization",{
  normal.data <- normalize_all(test.data)

  ## Check that all normalized values between 0 and 1
  expect_true(all(normal.data >= 0))
  expect_true(all(normal.data <= 1))

  ## Check Denormalization
  normal.data <- denormalize_all(normal.data)
  clean_up_norm()
  expect_equivalent(normal.data, test.data)
})

