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
                                   NumericMatrix usersArticlesAtenCoeffMatrix,
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
        diff = usersArticlesAtenCoeffMatrix(j,userAidx) - usersArticlesAtenCoeffMatrix(j,userBidx);
        if (diff > 0){
          usersArticlesTimeExpVector(k) += (usersArticlesAtenCoeffMatrix(j,userAidx)/usersArticlesAtenCoeffMatrix(j,userBidx))*articlesPopularityIndexVector(j);
        }
        else{
          usersArticlesTimeExpVector(k) += (usersArticlesAtenCoeffMatrix(j,userBidx)/usersArticlesAtenCoeffMatrix(j,userAidx))*articlesPopularityIndexVector(j);
        }
      }
      if ((usersArticlesMatrix(j,userAidx)!=0)||(usersArticlesMatrix(j,userBidx)!=0)){
        unionSum+=1;
      }
    }
    jaccard(userAidx,userBidx) = 1-usersArticlesTimeExpVector(k)/unionSum;
    // aux =     which(articlesUserUserTimeDiffTable[   ,'userA'] == auxUserAEmail)
    // aux = aux[which(articlesUserUserTimeDiffTable[aux,'userB'] == auxUserBEmail)]
    // userUserArticlesTimeDiffVector_Vioj = articlesUserUserTimeDiffTable[aux,'diff']
    
    // userAArticlesTimeExpVector_Wik = rep(0,sum(intersection))
    // userAArticlesTimeExpVector_Wik[userUserArticlesTimeDiffVector_Vioj > 0] =   usersArticlesAtenCoeffMatrix[intersection,userAidx][userUserArticlesTimeDiffVector_Vioj > 0]
    // userAArticlesTimeExpVector_Wik[userUserArticlesTimeDiffVector_Vioj < 0] = 1/usersArticlesAtenCoeffMatrix[intersection,userAidx][userUserArticlesTimeDiffVector_Vioj < 0]
    
    // userBArticlesTimeExpVector_Wjk = rep(0,sum(intersection))
    // userBArticlesTimeExpVector_Wjk[userUserArticlesTimeDiffVector_Vioj > 0] = 1/usersArticlesAtenCoeffMatrix [intersection,userBidx][userUserArticlesTimeDiffVector_Vioj > 0]
    // userBArticlesTimeExpVector_Wjk[userUserArticlesTimeDiffVector_Vioj < 0] =   usersArticlesAtenCoeffMatrix [intersection,userBidx][userUserArticlesTimeDiffVector_Vioj < 0]
    // jaccard[userAidx,userBidx] = 1-sum(userAArticlesTimeExpVector_Wik*userBArticlesTimeExpVector_Wjk*articlesPopularityIndexVector[intersection,]$popularityIndex)/sum(union)
  }
  
  return jaccard;
}