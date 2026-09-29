#' Defining the linreg class
#' @param formula A formula, e.g. Petal.Length ~ Species.
#' @param data A data frame containing the variables in the formula.
#'
#' @return An object of class "linreg" (a list with the coefficients, fitted
#'   values, residuals, degrees of freedom, variances, t-values and p-values).
#'
#' @references \url{https://en.wikipedia.org/wiki/Ordinary_least_squares}
#'
#' @importFrom stats model.matrix pt
#' @export
linreg <- function(formula, data) {
  X <- model.matrix(formula, data) 
  y_name <- all.vars(formula)[1]
  y <- data[[y_name]]
  
  n <- nrow(X)
  p <- ncol(X)
  
  #calculations
  #coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
  coefficients <- drop(solve(t(X) %*% X) %*% t(X) %*% y)
  names(coefficients) <- colnames(X)
  fitted <- X %*% coefficients
  residuals <- y - fitted
  degrees_of_freedom <- n - p
  residual_variance <- sum(residuals^2) / degrees_of_freedom
  variance_of_coefficients <- residual_variance * solve(t(X) %*% X)
  t_values <- coefficients / sqrt(diag(variance_of_coefficients))
  p_values <- 2 * pt(abs(t_values), degrees_of_freedom, lower.tail = FALSE)
  
  result <- list(
    coefficients = coefficients,
    fitted = fitted,
    residuals = residuals,
    df = degrees_of_freedom,
    sigma2 = residual_variance,
    var_beta = variance_of_coefficients,
    t_values = t_values,
    p_values = p_values,
    call = match.call(),
    formula = formula,
    data_name = deparse(substitute(data))
  )
  class(result) <- "linreg"
  result
}

#data(iris)
#mod_object <- linreg(Petal.Length~Species, data = iris)

