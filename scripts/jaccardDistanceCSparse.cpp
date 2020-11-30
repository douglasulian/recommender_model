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
sp_mat armaCrossProd(sp_mat A) {
  return (trimatl(A.t() * A));
}

// [[Rcpp::export]]
NumericMatrix getJaccardDistancesC(NumericMatrix jaccard, 
                                   NumericVector rowIndexes, 
                                   NumericVector colIndexes, 
                                   NumericMatrix usersArticlesMatrix,
                                   NumericMatrix usersArticlesAttenCoeffMatrix,
                                   NumericVector articlesPopularityIndexVector){
  int userAidx = 0;
  int userBidx = 0;
  int nrow = usersArticlesMatrix.nrow();
  NumericVector usersArticlesTimeExpVector(colIndexes.size());
  int unionSum = 0;
  int intersectionSum = 0;
  
  for (int k = 0;k < colIndexes.size();k++){
    userAidx = rowIndexes(k)-1;
    userBidx = colIndexes(k)-1;
    unionSum = 0;
    intersectionSum = 0;
    double diff = 0.0;
    for (int j = 0; j < nrow; j++){
      if ((usersArticlesMatrix(j,userAidx)!=0)&&(usersArticlesMatrix(j,userBidx)!=0)){
        diff = usersArticlesAttenCoeffMatrix(j,userAidx) - usersArticlesAttenCoeffMatrix(j,userBidx);
        if (diff < 0){
          usersArticlesTimeExpVector(k) += (usersArticlesAttenCoeffMatrix(j,userAidx)/usersArticlesAttenCoeffMatrix(j,userBidx))*articlesPopularityIndexVector(j);
          Rcout << "usersArticlesAttenCoeffMatrix(j,userAidx " << usersArticlesAttenCoeffMatrix(j,userAidx) << std::endl;
          Rcout << "usersArticlesAttenCoeffMatrix(j,userBidx) " << usersArticlesAttenCoeffMatrix(j,userBidx) << std::endl;
          Rcout << "articlesPopularityIndexVector(j) " << articlesPopularityIndexVector(j) << std::endl;
        }
        else{
          usersArticlesTimeExpVector(k) += (usersArticlesAttenCoeffMatrix(j,userBidx)/usersArticlesAttenCoeffMatrix(j,userAidx))*articlesPopularityIndexVector(j);
        }
      }
      if ((usersArticlesMatrix(j,userAidx)!=0)||(usersArticlesMatrix(j,userBidx)!=0)){
        unionSum+=1;
      }
    }
    jaccard(userAidx,userBidx) = 1-usersArticlesTimeExpVector(k)/unionSum;
    Rcout << "userAidx " << userAidx << std::endl;
    Rcout << "userBidx " << userBidx << std::endl;
    Rcout << "usersArticlesTimeExpVector(k) " << usersArticlesTimeExpVector(k) << std::endl;
    Rcout << "unionSum " << unionSum << std::endl;
  }
  
  return jaccard;
}
// [[Rcpp::export]]
sp_mat getJaccardDistancesCSparse(sp_mat jaccard,
                                  const NumericVector rowIndexes, 
                                  const NumericVector colIndexes, 
                                  const sp_mat usersArticlesMatrix,
                                  const sp_mat usersArticlesAttenCoeffMatrix,
                                  const NumericVector articlesPopularityIndexVector){
  int times = 0;
  int userAidx = 0;
  int userBidx = 0;
  int nrow = usersArticlesMatrix.n_rows;
  NumericVector usersArticlesTimeExpVector(colIndexes.size());
  int unionSum = 0;
  int intersectionSum = 0;
  
  for (int k = 0;k < colIndexes.size();k++){
    userAidx = rowIndexes(k)-1;
    userBidx = colIndexes(k)-1;
    unionSum = 0;
    intersectionSum = 0;
    double diff = 0.0;
    for (int j = 0; j < nrow; j++){
      times++;
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
  Rcout << "Times is " << times << std::endl;
  return jaccard;
}

// [[Rcpp::export]]
sp_mat getJaccardDistancesCSparseSingleCore(const sp_mat crossProd,
                                            const sp_mat usersArticlesMatrix,
                                            const sp_mat usersArticlesAttenCoeffMatrix,
                                            const NumericVector articlesPopularityIndexVector){
  int times = 0;
  
  sp_mat jaccard(usersArticlesMatrix.n_cols,usersArticlesMatrix.n_cols);
  
  //mat result = mat(crossProd.n_nonzero, 2, fill::zeros);
  
  long k = 0;
  
  for(arma::sp_mat::const_iterator it = crossProd.begin(); it != crossProd.end(); ++it)
  {
    long userAidx = 0;
    long userBidx = 0;
    userAidx = it.row();
    userBidx = it.col();
    if (userAidx != userBidx){
      long nrow = usersArticlesMatrix.n_rows;
      NumericVector usersArticlesTimeExpVector(crossProd.n_nonzero);
      
      double unionSum = 0;
      double intersectionSum = 0;
  
      unionSum = 0;
      intersectionSum = 0;
      double diff = 0.0;
      for (int j = 0; j < nrow; j++){
        times++;
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
  }  
  Rcout << "Times is " << times << std::endl;
  return jaccard;
}