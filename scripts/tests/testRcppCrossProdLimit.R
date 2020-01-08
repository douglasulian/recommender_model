setwd("/home/douglas_ulian/recommender_model/scripts/tests")
##### Libraries ####
list.of.packages = c('Matrix','inline','Rcpp','bit64','RcppArmadillo')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(inline)
library(Matrix)
library(Rcpp)
library(bit64)

### Create the data for the test
nRows = 67988
nColumns = 87575
#Rows = 100
#nColumns = 200

spMat = sparseMatrix(x = 0L, i = nRows, j = nColumns)

spMat[1,] = 1
spMat[,1] = 1

sum(spMat)
class(spMat)
print(object.size(spMat),units = "auto")

#### Test Eigen Cxxfunction
cppCode = paste0('using Eigen::MappedSparseMatrix;',
                       'using Eigen::SparseMatrix;',
                       'const MappedSparseMatrix<double> A(as<MappedSparseMatrix<double> >(x));',
                       'const SparseMatrix<double> At(A.adjoint());',
                       'return wrap((At * A).pruned());')

eigenCrossProdCxxfunction = cxxfunction(signature(x = "dgCMatrix"),cppCode, "RcppEigen")

resultCxxfunction = eigenCrossProdCxxfunction(spMat)
sum(tril(resultCxxfunction))
print(object.size(tril(resultCxxfunction)),units = "auto")


#### Test Eigen with Sourcepp
sourceCpp("eigenCrossProd.cpp")
resultSourceCppEigen = eigenCrossProd(spMat)
sum(tril(resultSourceCppEigen))
print(object.size(tril(resultSourceCppEigen)),units = "auto")


#### Test Arma with Sourcepp
sourceCpp("../armaCrossProd.cpp")
resultSourceCppArma = armaCrossProd(spMat)
sum(resultSourceCppArma)
print(object.size(resultSourceCppArma),units = "auto")

#### Test with R
resultR = t(spMat) %*% spMat
sum(tril(resultR))
print(object.size(tril(resultR)),units = "auto")


sum(tril(resultCxxfunction))
print(object.size(tril(resultCxxfunction)),units = "auto")
sum(tril(resultSourceCppEigen))
print(object.size(tril(resultSourceCppEigen)),units = "auto")
sum(resultSourceCppArma)
print(object.size(resultSourceCppArma),units = "auto")
sum(tril(resultR))
print(object.size(tril(resultR)),units = "auto")
