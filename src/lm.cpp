#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

arma::mat inv(arma::mat X) {
  return arma::solve(X, arma::eye(arma::size(X)));
}

// [[Rcpp::export]]
List lm_rcpp(const arma::mat & X, arma::vec y, bool robust) {
  int n = X.n_rows, k = X.n_cols;
  arma::mat coefficients_var;

  arma::vec coefficients = arma::solve(X.t() * X, X.t() * y);
  arma::vec resid = y - X * coefficients;
  arma::vec fitted = y - resid;

  double resid_var = arma::as_scalar(resid.t() * resid / (n - k));

  if (robust) {
    coefficients_var =  inv(X.t() * X) * X.t() * n / (n - k) * arma::diagmat(resid % resid) * X * inv(X.t() * X);
  } else {
    coefficients_var = resid_var * inv(X.t() * X);
  }

  arma::vec coefficients_se = sqrt(arma::diagvec(coefficients_var));
  double r_squared_notres = arma::as_scalar((resid.t() * resid) / ((y.t() - mean(y)) * (y - mean(y))));

  return List::create(Named("coefficients") = coefficients,
                      Named("resid_se") = sqrt(resid_var),
                      Named("coefficients_var") = coefficients_var,
                      Named("coefficients_se") = coefficients_se,
                      Named("t_values") = coefficients / coefficients_se,
                      Named("prob_values") = 2 * pt(-abs(as<NumericVector>(wrap(coefficients / coefficients_se))), n - k, true, false),
                      Named("obs") = n,
                      Named("regressors") = k,
                      Named("df") = n-k,
                      Named("r_squared") = 1 - r_squared_notres,
                      Named("r_squared_adj") = 1 - r_squared_notres * (n - 1) / (n - k),
                      Named("sigma") = sqrt((resid.t() * resid) / (n - k)),
                      Named("fitted") = fitted,
                      Named("resid") = resid);
}
