#include <Rcpp.h>
#include <stdlib.h>
#include <string.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
int fun5c(NumericMatrix usersArticlesMatrix, NumericVector rowIndexes, NumericVector colIndexes){
  int total = 0;
  int userAidx = 0;
  int userBidx = 0;
  int nrow = usersArticlesMatrix.nrow();
  int intersection = 0;
  int unionSum = 0;
  // printf("Nrow: %d \n",nrow);
  // printf("Ncol: %d \n",usersArticlesMatrix.ncol());
  for (int k = 0; k < colIndexes.size();k++){
    userAidx = rowIndexes[k];
    userBidx = colIndexes[k];
    unionSum = 0;
    intersection = 0;
    for (int j = 0; j < nrow; j++){
      // if (k == 3 && j < 10){
      //   printf("j:%d %d-%d:%f-%f \n",j,userAidx,userBidx,usersArticlesMatrix(j,userAidx-1),usersArticlesMatrix(j,userBidx-1));
      // }
      if ((usersArticlesMatrix(j,userAidx-1)!=0)&&(usersArticlesMatrix(j,userBidx-1)!=0)){
        intersection+=1;
      }
      if ((usersArticlesMatrix(j,userAidx-1)!=0)||(usersArticlesMatrix(j,userBidx-1)!=0)){
        unionSum+=1;
      }
      //intersection = ((usersArticlesMatrix[,userAidx]!=0)&(usersArticlesMatrix[,userBidx]!=0))  
    }
    if (intersection > 0){
      //printf("%d-%d:%d \n",userAidx,userBidx,unionSum);
      total += unionSum;
    }
  }
  return total;
}
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
int sumMatrix(NumericMatrix matrix){
  int total = 0;
  
  for (int i = 0; i < matrix.nrow();i++)
    for (int j = 0; j < matrix.ncol();j++)  
      total+=matrix(i,j);

  return total;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//