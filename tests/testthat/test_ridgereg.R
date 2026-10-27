test_that("ridgereg coefficients match MASS::lm.ridge", {
  skip_if_not_installed("MASS")
  f <- mpg ~ wt + hp + disp
  for (l in c(0, 1, 10)) {
    fit <- ridgereg(f, data = mtcars, lambda = l)
    ref <- MASS::lm.ridge(f, data = mtcars, lambda = l)
    expect_equal(unname(coef(fit)[-1]), unname(ref$coef))
    expect_equal(unname(coef(fit)[1]), unname(ref$ym))
  }
})

test_that("lambda = 0 gives the same fitted values as lm()", {
  fit <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 0)
  ref <- lm(mpg ~ wt + hp, data = mtcars)
  expect_equal(unname(predict(fit)), unname(fitted(ref)))
})

test_that("function predict on newdata works and matches fitted values", {
  fit <- ridgereg(Petal.Length ~ Sepal.Width + Species, data = iris, lambda = 2)
  expect_equal(unname(predict(fit, newdata = iris)), unname(predict(fit)))
  expect_length(predict(fit, newdata = iris[1:5, ]), 5)
})

test_that("ridgereg rejects bad input", {
  expect_error(ridgereg(mpg ~ wt, data = mtcars, lambda = -1))
  expect_error(ridgereg(mpg ~ wt, data = "mtcars", lambda = 1))
  expect_error(ridgereg("mpg ~ wt", data = mtcars, lambda = 1))
})

test_that("ridgereg returns the right class", {
  fit <- ridgereg(mpg ~ wt, data = mtcars, lambda = 1)
  expect_s3_class(fit, "ridgereg")
  expect_output(print(fit), "Coefficients")
})