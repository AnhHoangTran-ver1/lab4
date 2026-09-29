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
}

#print.linreg(mod_object)



#' plot function 
#' @param x An object of class "linreg".
#' @param ... Not used.
#' @return The two plots, invisibly, as a list.
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
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    ggplot2::labs(title = "Residuals vs Fitted", x = xlab, y = "Residuals") +
    ggplot2::theme_bw()
  
  p2 <- ggplot2::ggplot(d, ggplot2::aes(x = .data$fitted, y = .data$std_resid)) +
    ggplot2::geom_point(shape = 1, size = 3) +
    ggplot2::stat_summary(fun = median, geom = "line", colour = "red") +
    ggplot2::geom_text(data = top3, ggplot2::aes(label = .data$id), vjust = -0.5) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    ggplot2::labs(title = "Scale-Location", x = xlab,
                  y = expression(sqrt("|Standardized residuals|"))) +
    ggplot2::theme_bw()
  
  print(p1)
  print(p2)
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
#' Generic function that returns the predicted values.
#'
#' @param x An object.
#' @param ... Further arguments passed to methods.
#' @return The predicted values.
#' @export
pred <- function(x, ...) UseMethod("pred")

#' Predicted values of a linreg object
#' @param x An object of class "linreg".
#' @param ... Not used.
#' @return A vector of predicted values.
#' @export
pred.linreg <- function(x, ...) {
  x$fitted
}

#pred.linreg(mod_object)



#' summary
#' @param object An object of class "linreg".
#' @param ... Not used.
#' @return The object, invisibly.
#' @importFrom stats printCoefmat
#' @export
summary.linreg <- function(object, ...) {
  se <- sqrt(diag(object$var_beta))
  
  coef_table <- cbind(
    Estimate     = object$coefficients,
    `Std. Error` = se,
    `t value`    = object$t_values,
    `p value`   = object$p_values
  )
  
  cat("Call:\n")
  print(object$call)
  cat("\nCoefficients:\n")
  printCoefmat(coef_table, digits = 4, print.gap = 3, has.Pvalue = TRUE)
  cat("\nResidual standard error:", format(sqrt(object$sigma2), digits = 4),
      "on", object$df, "degrees of freedom\n")
}

#summary.linreg(mod_object)


