#' print function
#' @param x An object of class "linreg".
#' @param ... Not used.
#' @return The object x, invisibly.
#' @export
print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print.default(format(x$coefficients, digits = 3), print.gap = 2, quote = FALSE)
  invisible(x)
}

#print.linreg(mod_object)



#' plot function 
#' @param x An object of class "linreg".
#' @param ... Not used.
#' @return The two plots, invisibly, as a list.
#' @references \url{https://en.wikipedia.org/wiki/Errors_and_residuals}
#' @importFrom stats median
#' @importFrom rlang .data
#' @export
plot.linreg <- function(x, ...) {
  d <- data.frame(
    fitted = x$fitted,
    resid = x$residuals,
    std_resid = sqrt(abs(x$residuals / sqrt(x$sigma2))),
    id = seq_along(x$residuals)
  )
  xlab <- paste0("Fitted values\nlinreg(", paste(deparse(x$formula), collapse = ""), ")")
  top3 <- d[order(abs(d$resid), decreasing = TRUE)[1:3], ]
  
  p1 <- ggplot2::ggplot(d, ggplot2::aes(x = .data$fitted, y = .data$resid)) +
    ggplot2::geom_point(shape = 1, size = 3) +
    ggplot2::stat_summary(fun = median, geom = "line", colour = "red") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = "grey") +
    ggplot2::geom_text(data = top3, ggplot2::aes(label = .data$id), vjust = -0.5) +
    ggplot2::labs(title = "Residuals vs Fitted", x = xlab, y = "Residuals") +
    ggplot2::theme_bw()
  
  p2 <- ggplot2::ggplot(d, ggplot2::aes(x = .data$fitted, y = .data$std_resid)) +
    ggplot2::geom_point(shape = 1, size = 3) +
    ggplot2::stat_summary(fun = median, geom = "line", colour = "red") +
    ggplot2::geom_text(data = top3, ggplot2::aes(label = .data$id), vjust = -0.5) +
    ggplot2::labs(title = "Scale-Location", x = xlab,
                  y = expression(sqrt("|Standardized residuals|"))) +
    ggplot2::theme_bw()
  
  print(p1)
  print(p2)
  invisible(list(p1, p2))
}

#plot.linreg(mod_object)



#' resid() should return the vector of residuals eˆ
#' @param object An object of class "linreg".
#' @param ... Not used.
#' @return A vector of residuals.
#' @export
residuals.linreg <- function(object, ...) {
  c(object$residuals)
}

#is.vector(residuals.linreg(mod_object))



#' coef() should return the coefficients as a named vector
#' @param object An object of class "linreg".
#' @param ... Not used.
#' @return A named vector of regression coefficients.
#' @export
coef.linreg <- function(object, ...) {
  c(object$coefficients)
}

#is.vector(coef.linreg(mod_object))



#' Predicted values
#'
#' Returns the predicted values \eqn{\hat{y}} of a fitted model.
#'
#' @param x An object of class "linreg".
#' @param ... Further arguments passed to methods (not used).
#' @return A vector of predicted values.
#' @export
pred <- function(x, ...) UseMethod("pred")

#' @rdname pred
#' @export
pred.linreg <- function(x, ...) {
  as.vector(x[["fitted"]])
}

#pred.linreg(mod_object)



#' summary
#' @param object An object of class "linreg".
#' @param ... Not used.
#' @return The object, invisibly.
#' @references \url{https://en.wikipedia.org/wiki/T-statistic}
#' @importFrom stats printCoefmat
#' @export
summary.linreg <- function(object, ...) {
  se <- sqrt(diag(object$var_beta))
  
  coef_table <- cbind(
    Estimate     = object$coefficients,
    `Std. Error` = se,
    `t value`    = object$t_values,
    `Pr(>|t|)`   = object$p_values
  )
  
  cat("Call:\n")
  print(object$call)
  cat("\nCoefficients:\n")
  printCoefmat(coef_table, digits = 4)
  cat("\nResidual standard error:", format(sqrt(object$sigma2), digits = 4),
      "on", object$df, "degrees of freedom\n")
  
  invisible(object)
}

#summary.linreg(mod_object)


