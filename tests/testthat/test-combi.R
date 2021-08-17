test_that("the df produced by combi is a dataframe of length 3", {
  expect_s3_class(tab, "data.frame")
  expect_length(tab, 3)
})
