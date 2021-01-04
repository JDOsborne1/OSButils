test_that("index casting works", {
  expect_equal(util_IndexCaster(.002), -499)
  expect_equal(util_IndexCaster(200), 199)
  expect_equal(util_IndexCaster(1.2), 0.2)
  expect_equal(util_IndexCaster(1), 0)
})
