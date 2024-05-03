test_that("multiplication works", {
  v<- myquad(1:10)
  expect_length(v,10)
  expect_equal(2 * 2, 4)
})
