#include <Rcpp.h>
#include <stdlib.h>
#include <string.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
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
    jaccard(userAidx,userBidx) = 1-usersArticlesTimeExpVector(k)/unionSum;
  }
  
  return jaccard;
}