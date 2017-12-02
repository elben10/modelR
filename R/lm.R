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

#' @export
summary.mod_lm <- function(object, ...) {
  coefficients_mat <- cbind(object$coefficients,
                            object$coefficients_se,
                            object$t_values,
                            object$prob_values)
  rownames(coefficients_mat) <- names(object$coefficients)
  colnames(coefficients_mat) <- c("Estimate", "StdErr", "t.value", "p.value")
  res <- list(call = object$call,
              coefficients = coefficients_mat,
              r_squared = object$r_squared,
              r_squared_adj = object$r_squared_adj,
              sigma = object$sigma,
              df = object$df,
              resid_summary = summary(as.vector(object$resid), digits = 5)[-4])

  class(res) <- "summary.mod_lm"
  res
}

#' @export
print.summary.mod_lm <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nResiduals:\n")
  print(x$resid_summary)
  cat("\n")

  printCoefmat(x$coefficients, P.values=TRUE, has.Pvalue=TRUE)
  digits <- max(3, getOption("digits") - 3)
  cat("\nResidual standard error: ", formatC(x$sigma, digits=digits), " on ",
      formatC(x$df), " degrees of freedom\n", sep="")
  cat("Multiple R-squared: ", formatC(x$r_squared, digits=digits),
      ",\tAdjusted R-squared: ",formatC(x$r_squared_adj, digits=digits),
      "\n", sep="")
  invisible(x)
}

#' @export
vcov.mod_lm <- function(object, ...) {
  res <- object$coefficients_var
  rownames(res) <- names(coef(object))
  colnames(res) <- names(coef(object))
  res
}

#' @export
predict.mod_lm <- function(object, newdata = NULL, ...) {
  if (is_null(newdata)) {
    y_fit <- as.vector(object$fitted)
  } else {
    if (!is_null(object$formula)) {
      X <- model.matrix(object$formula, newdata)
    } else {
      X <- newdata
    }
    y_fit <- as.vector(X %*% coef(object))
  }
  y_fit
}

#' @export
residuals.mod_lm <- function(object, newdata = NULL, ...) {
  if (is_null(newdata)) {
    resid <- as.vector(object$resid)
  } else {
    if (!is_null(object$formula)) {
      X <- model.matrix(object$formula, newdata)
    } else {
      X <- newdata
    }
    y_fit <- as.vector(X %*% coef(object))
    y <- model.response(model.frame(object$formula, data = newdata))
    resid <- as.vector(y - y_fit)
  }
  resid
}
