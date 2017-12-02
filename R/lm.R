#' Ordinary Least Squares
#'
#' @param data A data.frame containing the dependent and independent variables used in the regression.
#' @inheritParams stats::lm
#' @param robust Logical. If true robust standard errors is used.
#'
#' @return mod_lm returns an object of the class mod_lm, which is a list containing information about the fitted model.
#' @export
#'
#' @examples mod_lm(mtcars, mpg~cyl)
mod_lm <- function(data, formula, robust = FALSE) {
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)
  res <- lm_rcpp(X, y, robust)
  res$call <- match.call()
  res$coefficients <- as.vector(res$coefficients)
  res$formula <- formula
  names(res$coefficients) <- colnames(X)
  class(res) <- "mod_lm"
  res
}

#' @export
print.mod_lm <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients, digits = 5)
}
