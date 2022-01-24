test_that("getting local data when there is no internet connection works", {
  columns = ncol(get_data())
  expect_equal(columns, 141)
})
