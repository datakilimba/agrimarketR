test_that("price_wide() function pulls at least 1 row", {
  expect_gt(nrow(price_wide()), 1)
})