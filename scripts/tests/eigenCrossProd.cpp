#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]
using Eigen::MappedSparseMatrix;
using Eigen::SparseMatrix;

// [[Rcpp::export]]
SparseMatrix<double> eigenCrossProd(const MappedSparseMatrix<double> A) {
  return (A.adjoint() * A).pruned();
}
