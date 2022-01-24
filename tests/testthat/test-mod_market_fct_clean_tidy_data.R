test_that("Data can go through assertr cleaning process and produce failed rows", {
  suppressWarnings(
    clean_tidy_data()
  )
    expect_true(nrow(my_failed_rows) > 1)
})
