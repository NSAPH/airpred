## Test code for Normalization + transformation

x <- rnorm(1000)
y <- rnorm(1000)
z <- rnorm(1000)
site <- rep_len(0.69, 1000)

test.data <- data.frame(x,y,z, site)
gen_config()
test_that("Check normalization, denormalization",{
  std.data <- standardize_all(test.data)

  expect_true(all(std.data$site == 0.69))

  ## Check Denormalization
  normal.data <- destandardize_all(std.data)
  clean_up_stats()
  clean_up_config()
  expect_equivalent(std.data, test.data)
})

