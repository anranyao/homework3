test_that("linear works", {
  cc<-Depression ~ conFatalism + Sex + R_E + factor(Age1)
  expect_equal(as.numeric(linear(cc,mydata)$F.value),as.numeric(summary(lm(cc,data=mydata))$fstatistic[1]))
})
