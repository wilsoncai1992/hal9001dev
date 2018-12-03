# library(fixedHAL)
# library(SuperLearner)
# library(hal9001)
# source("./simulate_data.R")

# # n_sim = 1e2
# n_sim = 5e2
# # n_sim <- 5e3
# # n_sim <- 1e4
# a1 <- .5
# b1 <- 3
# a2 <- .1
# INFLATE_LAMBDA <- 1

# data_sim <- simulate_data(n_sim = n_sim, a1 = a1, a2 = a2, b1 = b1)
# true_Psi <- 1
# lambda_grid <- c(1e-8, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 3e-2, 5e-2, 7e-2, 1e-1)

# # lambda <- 1e-1
# lambda <- 1e-2
# # lambda <- 1e-8

# library(hal9001)
# W_train <- data_sim$W$W[data_sim$A == 1]
# Y_train <- data_sim$Y[data_sim$A == 1]
# hal_fit <- fit_hal_single_lambda(X = as.matrix(W_train), Y = Y_train, fit_type = "glmnet", n_folds = 3, use_min = TRUE, lambda = lambda, yolo = FALSE)
# beta_hat <- hal_fit$coefs[, 1]



# create_1d_prediction <- function(W_train, hal_fit) {
#   W_epsilon = 1e-1
#   W_range <- c(min(W_train) - W_epsilon, max(W_train) + W_epsilon)
#   W_grid <- seq(W_range[1], W_range[2], length.out = 1e3)
#   y_hat <- predict(hal_fit, new_data = data.frame(W = W_grid))
#   return(list(W_grid, y_hat))
# }

# plot_hal_fit <- function(W_train, hal_fit) {
#   prediction = create_1d_prediction(W_train, hal_fit)
#   W_grid <- prediction[[1]]
#   y_hat <- prediction[[2]]
#   plot(y_hat ~ W_grid, type = 'l', col = 'blue')
# }
# compute_sec_var_norm <- function(W_train, hal_fit) {
#   prediction = create_1d_prediction(W_train, hal_fit)
#   W_grid <- prediction[[1]]
#   y_hat <- prediction[[2]]
#   return(sum(abs(diff(y_hat))))
# }
# plot_hal_fit(W_train, hal_fit)
# points(Y_train ~ W_train, col = 'grey')
# # compute_sec_var_norm(W_train, hal_fit)
# sum(abs(beta_hat))

# # hal_fit_cvglmnet <- fit_hal(X = as.matrix(W_train), Y = Y_train, fit_type = "glmnet", n_folds = 3, yolo = FALSE)
# # beta_hat <- hal_fit_cvglmnet$coefs[, 1]
# # sum(abs(beta_hat))

# # fit constrained form
# hal_fit_constrained <- fit_hal_constraint_form(X = as.matrix(W_train), Y = Y_train, fit_type = "glmnet", n_folds = 3, yolo = FALSE, M = 25)
# # hal_fit_constrained <- fit_hal_constraint_form(X = as.matrix(W_train), Y = Y_train, fit_type = "glmnet", n_folds = 3, yolo = FALSE, M = 100)
# beta_hat <- hal_fit_constrained$coefs
# plot_hal_fit(W_train, hal_fit_constrained)
# sum(abs(beta_hat))

# hal_fit_constrained$l1_norms
