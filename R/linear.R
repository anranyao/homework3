#'linear: Fitting Simple Linear Regression Models
#'
#'`linear` is used to fit simple linear regression models. And it can also returns 95% confidence intervals for one or more parameters in a fitted model.
#'
#'@param formula.lm an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#'@param data an optional data frame, list or environment (or object coercible by \code{\link{as.data.frame}} to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which linear is called.
#'
#'@return linear returns a list containing at least the following components:
#'\describe{
#'\item{residuals.descript}{The desciption of residuals(minimum, 1st quartile, median, 3rd quartile, maximum)}
#'\item{Coefficients}{a p x 4 matrix with columns for the estimated coefficient, its standard error, t-statistic and corresponding (two-sided) p-value. Aliased coefficients are omitted.}
#'\item{CI}{95% confidence intervals for parameters in the fitted model.}
#'\item{residual.std.error}{the square root of the estimated variance of the random error σ^2 = 1/(n-p) Sum(w[i] R[i]^2),where R[i] is the i-th residual, residuals[i].}
#'\item{residual.df}{the residual degrees of freedom.}
#'\item{R_squared}{R^2, the ‘fraction of variance explained by the model’,R^2 = 1 - Sum(R[i]^2) / Sum((y[i]- y*)^2),where y* is the mean of y[i] if there is an intercept and zero otherwise.}
#'\item{Adjusted.R_squared}{the above R^2 statistic ‘adjusted’, penalizing for higher p.}
#'\item{F.value}{}
#'\item{df}{degrees of freedom of F statistic.}
#'\item{pvalue.f}{P value of F statistic.}
#'\item{fittedvalues}{the fitted mean values.}
#'\item{residual}{the residuals, that is response minus fitted values.}
#'}
#'
#'@examples
#'attach(mydata) #using the data
#'cc<-Depression ~ conFatalism + Sex + R_E + factor(Age1)
#'results<-linear(cc,mydata)
#'coef_c<-results$Coefficients # the coefficients and its related values.
#'coef_c
#'results$F.value
#'## input"formula.lm" must be a formula.
#' \dontrun{
#' attach(mydata)
#'    linear(Sex,mydata)
#' }
#'
#'@export
#'
#'@details Models for linear are specified symbolically. A typical model has the form `response ~ terms` where `response` is the (numeric) response vector and `terms` is a series of terms which specifies a linear predictor for `response`.
#'
#'A formula has an implied intercept term. To remove this use either `y ~ x - 1` or `y ~ 0 + x`.
#'
#'@references LM: Fitting linear models (1969) RDocumentation. R-core R-core@R-project.org. Available at: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm (Accessed: November 20, 2022).


linear <- function(formula.lm, data) {
  if (class(formula.lm) != "formula")
    stop("'formula.lm' must be a formula")
  newdata <- na.omit(data[, all.vars(formula.lm)])
  response <- newdata[, 1]
  variables <- model.matrix(formula.lm, data = newdata)
  estimate <-
    solve(t(variables) %*% variables) %*% t(variables) %*% response
  fittedvalues <- variables %*% estimate
  residual <- response - fittedvalues
  rank.p <- length(estimate)
  num.observe <- nrow(residual)
  residual.df <- num.observe - rank.p
  ssy <-
    t(response) %*% (diag(num.observe) - matrix(rep(1, num.observe ^ 2), num.observe, num.observe) /
                       num.observe) %*% response
  sse <- t(residual) %*% residual
  ssr <- ssy - sse
  estimate.sigma_2 <- as.numeric(sse / residual.df)
  residual.std.error <- sqrt(estimate.sigma_2)
  estimate.std.error <-
    sqrt(diag(as.matrix(solve(
      t(variables) %*% variables
    )) * estimate.sigma_2))
  t.value <- estimate / estimate.std.error
  t.df <- pt(abs(t.value), df = residual.df)
  p.t <- 2 * (1 - t.df)
  CI <-
    as.numeric(estimate) + cbind(-1 * estimate.std.error * t.df, estimate.std.error *
                                   t.df)
  R_squared <- as.numeric(ssr / ssy)
  Adjusted.R_squared <-
    as.numeric(1 - (sse / residual.df) / (ssy / (num.observe - 1)))
  F.value <- as.numeric((ssr / (rank.p - 1)) / (sse / residual.df))
  p.f <- pf(F.value, rank.p - 1, residual.df, lower.tail = FALSE)
  degreefreedom <- c(rank.p - 1, residual.df)
  coefficient <-
    data.frame(Estimate = estimate, Std.Error = estimate.std.error, t.value, p.t)
  residual.descript <- fivenum(residual)
  return(
    list(
      residuals.descript = residual.descript,
      Coefficients = coefficient,
      CI = CI,
      residual.std.error = residual.std.error,
      residual.df = residual.df,
      R_squared = R_squared,
      Adjusted.R_squared = Adjusted.R_squared,
      F.value = F.value,
      df = degreefreedom,
      pvalue.f = p.f,
      fittedvalues = fittedvalues,
      residual = residual
    )
  )
}
