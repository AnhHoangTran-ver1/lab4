#' Ridge regression
#'
#' Fits a ridge regression model using the QR decomposition of an augmented
#' design matrix. All covariates are standardized before fitting 
#' 
#' @param formula A formula, e.g. mpg ~ wt + hp.
#' @param data A data frame containing the variables in the formula.
#' @param lambda A single non-negative number, the ridge penalty.
#'
#' @return An object of class "ridgereg"
#'
#' @importFrom stats model.frame model.matrix model.response terms .getXlevels
#' @export
ridgereg <- function(formula, data, lambda = 0) {
  stopifnot(inherits(formula, "formula"), is.data.frame(data),
            is.numeric(lambda), length(lambda) == 1, lambda >= 0)
  
  mf <- model.frame(formula, data)
  tt <- terms(mf)
  y <- model.response(mf)
  X <- model.matrix(tt, mf)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  p <- ncol(X)
  
  # standardize covariates
  x_mean <- colMeans(X)
  X_centered <- sweep(X, 2, x_mean)
  x_scale <- sqrt(colMeans(X_centered^2))
  X_std <- sweep(X_centered, 2, x_scale, "/")
  y_mean <- mean(y)
  
  # ridge via QR
  X_aug <- rbind(X_std, sqrt(lambda) * diag(p))
  y_aug <- c(y - y_mean, rep(0, p))
  beta <- qr.coef(qr(X_aug), y_aug)
  
  coefficients <- c(y_mean, beta)
  names(coefficients) <- c("(Intercept)", colnames(X))
  fitted <- drop(y_mean + X_std %*% beta)
  
  result <- list(
    coefficients = coefficients,
    fitted = fitted,
    lambda = lambda,
    x_mean = x_mean,
    x_scale = x_scale,
    terms = tt,
    xlevels = .getXlevels(tt, mf),
    call = match.call()
  )
  class(result) <- "ridgereg"
  result
}

# Methods 
# Print method
#' Print a ridgereg object
#'
#' @param x A "ridgereg" object.
#' @param ... Not used.
#' @return The object, invisibly.
#' @export
print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(format(x$coefficients, digits = 3), quote = FALSE, print.gap = 2)
  invisible(x)
}


# Predict method 
#' Predict from a ridgereg model
#'
#' @param object A "ridgereg" object.
#' @param newdata Optional data frame. If omitted, the fitted values are returned.
#' @param ... Not used.
#' 
#' @return A numeric vector of predicted values.
#' @importFrom stats predict delete.response
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted)
  
  tt <- delete.response(object$terms)
  mf <- model.frame(tt, newdata, xlev = object$xlevels)
  X <- model.matrix(tt, mf)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  
  # scale new data with the TRAINING means and scales
  X_std <- sweep(sweep(X, 2, object$x_mean), 2, object$x_scale, "/")
  drop(object$coefficients[1] + X_std %*% object$coefficients[-1])
}


# Coef method
#' Ridge regression coefficients
#'
#' @param object A "ridgereg" object.
#' @param ... Not used.
#' 
#' @return Named vector of coefficients (standardized covariate scale).
#' @importFrom stats coef
#' @export
coef.ridgereg <- function(object, ...) {
  object$coefficients
}


