test_that("the df produced by se_sp is a dataframe of length 5", {
  expect_s3_class(mks, "data.frame")
  expect_length(mks, 5)
})
