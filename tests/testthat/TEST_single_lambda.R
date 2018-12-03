# set.seed(1234)
# n <- 1e3
# x <- rnorm(n)
# y <- (rje::expit(2*x + rnorm(n)) > .5) + 0
# wgt <- rep(1, n)

# x <- c(x, 1)
# y <- c(y, (rje::expit(-10*1 + rnorm(1)) > .5) + 0)
# wgt <- c(wgt, 1e2)

# library(hal9001)
# hal_fit <- fit_hal_single_lambda(X = x, Y = y, weights = wgt, use_min = TRUE, yolo = F, fit_type = 'glmnet', family="binomial", lambda = 2e-2)
# yhat <- predict(hal_fit, new_data = x)

# plot(rje::expit(yhat) ~ x, ylim = c(0,1))
