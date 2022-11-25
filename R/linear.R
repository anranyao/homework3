linear <- function(formula.lm, data) {
  if (class(formula.lm) != "formula")
    stop("'formula.lm' must be a formula")
  if (is.data.frame(data) != TRUE) {
    warning("'data' is changed to a data.frame")
    data = as.data.frame(data)
  }
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
      formula.lm,
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
