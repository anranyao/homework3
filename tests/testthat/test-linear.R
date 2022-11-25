test_that("linear works", {
  expect_equal(as.numeric(linear(cc,mydata)$F.value),as.numeric(summary(lm(cc,data=mydata))$fstatistic[1]))
})
