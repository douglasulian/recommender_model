#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
#include <stdlib.h>
#include <string.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
arma::vec getEigenValues(arma::mat M) {
  return arma::eig_sym(M);
}

// [[Rcpp::export]]
sp_mat getJaccardDistancesCSparse(
                                   NumericVector rowIndexes, 
                                   NumericVector colIndexes, 
                                   sp_mat usersArticlesMatrix,
                                   sp_mat usersArticlesAttenCoeffMatrix,
                                   NumericVector articlesPopularityIndexVector){
  int userAidx = 0;
  int userBidx = 0;
  int nrow = usersArticlesMatrix.n_rows;
  NumericVector usersArticlesTimeExpVector(colIndexes.size());
  int unionSum = 0;
  int intersectionSum = 0;
  sp_mat jaccard(usersArticlesMatrix.n_cols,usersArticlesMatrix.n_cols);
  for (int k = 0;k < colIndexes.size();k++){
    userAidx = rowIndexes(k)-1;
    userBidx = colIndexes(k)-1;
    unionSum = 0;
    intersectionSum = 0;
    double diff = 0.0;
    for (int j = 0; j < nrow; j++){
      if ((usersArticlesMatrix(j,userAidx)!=0)&&(usersArticlesMatrix(j,userBidx)!=0)){
        diff = usersArticlesAttenCoeffMatrix(j,userAidx) - usersArticlesAttenCoeffMatrix(j,userBidx);
        if (diff > 0){
          usersArticlesTimeExpVector(k) += (usersArticlesAttenCoeffMatrix(j,userAidx)/usersArticlesAttenCoeffMatrix(j,userBidx))*articlesPopularityIndexVector(j);
        }
        else{
          usersArticlesTimeExpVector(k) += (usersArticlesAttenCoeffMatrix(j,userBidx)/usersArticlesAttenCoeffMatrix(j,userAidx))*articlesPopularityIndexVector(j);
        }
      }
      if ((usersArticlesMatrix(j,userAidx)!=0)||(usersArticlesMatrix(j,userBidx)!=0)){
        unionSum+=1;
      }
    }
    if (usersArticlesTimeExpVector(k)/unionSum == 1){
      jaccard(userAidx,userBidx) = 2;
    }
    else{
      jaccard(userAidx,userBidx) = 1-usersArticlesTimeExpVector(k)/unionSum;  
    }
  }
  
  return jaccard;
}

// [[Rcpp::export]]
sp_mat getJaccardDistancesCSparseSingleCore(sp_mat crossProd,
                                            sp_mat usersArticlesMatrix,
                                            sp_mat usersArticlesAttenCoeffMatrix,
                                            NumericVector articlesPopularityIndexVector){
  
  
  sp_mat::const_iterator it     = crossProd.begin();
  sp_mat::const_iterator it_end = crossProd.end();
  sp_mat jaccard(usersArticlesMatrix.n_cols,usersArticlesMatrix.n_cols);
  
  //mat result = mat(crossProd.n_nonzero, 2, fill::zeros);
  
  long k = 0;
  for(; it != it_end; ++it)
  {
    long userAidx = 0;
    long userBidx = 0;
    //userAidx = rowIndexes(k)-1;
    userAidx = it.row();
    //userBidx = colIndexes(k)-1;
    userBidx = it.col();
    
    long nrow = usersArticlesMatrix.n_rows;
    NumericVector usersArticlesTimeExpVector(crossProd.n_nonzero);
    
    double unionSum = 0;
    double intersectionSum = 0;

    unionSum = 0;
    intersectionSum = 0;
    double diff = 0.0;
    for (int j = 0; j < nrow; j++){
      if ((usersArticlesMatrix(j,userAidx)!=0)&&(usersArticlesMatrix(j,userBidx)!=0)){
        diff = usersArticlesAttenCoeffMatrix(j,userAidx) - usersArticlesAttenCoeffMatrix(j,userBidx);
        if (diff > 0){
          usersArticlesTimeExpVector(k) += (usersArticlesAttenCoeffMatrix(j,userAidx)/usersArticlesAttenCoeffMatrix(j,userBidx))*articlesPopularityIndexVector(j);
        }
        else{
          usersArticlesTimeExpVector(k) += (usersArticlesAttenCoeffMatrix(j,userBidx)/usersArticlesAttenCoeffMatrix(j,userAidx))*articlesPopularityIndexVector(j);
        }
      }
      if ((usersArticlesMatrix(j,userAidx)!=0)||(usersArticlesMatrix(j,userBidx)!=0)){
        unionSum+=1;
      }
    }
    if (usersArticlesTimeExpVector(k)/unionSum == 1){
    //if (usersArticlesTimeExpVector(k)/(*it) == 1){
      jaccard(userAidx,userBidx) = 2;
    }
    else{
      jaccard(userAidx,userBidx) = 1-usersArticlesTimeExpVector(k)/unionSum;  
      //jaccard(userAidx,userBidx) = 1-usersArticlesTimeExpVector(k)/(*it);  
    }
    k++;
  }  
  
  return jaccard;
}