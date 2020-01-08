#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]] 

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
sp_mat armaCrossProd(sp_mat A) {
  return (trimatl(A.t() * A));
}

// [[Rcpp::export]]
mat armaCrossProdThreeColumns(sp_mat crossProd){

  sp_mat::const_iterator it     = crossProd.begin();
  sp_mat::const_iterator it_end = crossProd.end();
  
  mat result = mat(crossProd.n_nonzero, 2, fill::zeros);
  
  long ind = 0;
  for(; it != it_end; ++it)
  {
      result(ind,0) = it.row();
      result(ind,1) = it.col();
      ind++;
  }  
  return result;
}



// [[Rcpp::export]]
fmat testFloatMat(){
  return fmat(1000, 1000,fill::ones);
}

// [[Rcpp::export]]
dmat testDoubleMat(){
  return dmat(1000, 1000,fill::ones);
}

// [[Rcpp::export]]
umat testUMat(){
  return umat(1000, 1000,fill::ones);
}

// [[Rcpp::export]]
imat testIMat(){
  return imat(1000, 1000,fill::ones);
}

// [[Rcpp::export]]
sp_mat testSMat() {
  return sp_mat(mat(1000, 1000,fill::ones));
}

// [[Rcpp::export]]
vec testVec() {
  return nonzeros(mat(1000, 1000,fill::ones));
}



