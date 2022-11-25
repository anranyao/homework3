# homework3: R package
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/anranyao/homework3/workflows/R-CMD-check/badge.svg)](https://github.com/anranyao/homework3/actions)
  <!-- badges: end -->
  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/anranyao/homework3/branch/main/graph/badge.svg)](https://app.codecov.io/gh/anranyao/homework3?branch=main)
  <!-- badges: end -->
## Description

The package includes a function and a dataset.

# linear: Fitting Simple Linear Regression Models

## Description

`linear` is used to fit simple linear regression models. And it can also returns 95% confidence intervals for one or more parameters in a fitted model. 


## Usage
```{r}
linear(formula.lm, data)
```

## Arguments

*formula.lm*
  
  an object of class `"formula"` (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.

## Details

Models for lm are specified symbolically. A typical model has the form `response ~ terms` where `response` is the (numeric) response vector and `terms` is a series of terms which specifies a linear predictor for `response`. 

A formula has an implied intercept term. To remove this use either `y ~ x - 1` or `y ~ 0 + x`.

## Reference

LM: Fitting linear models (1969) RDocumentation. R-core R-core@R-project.org. Available at: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm (Accessed: November 20, 2022). 
