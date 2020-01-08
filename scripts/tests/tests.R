library(microbenchmark)

library(RcppEigen)
library(inline)
sparseProdCpp = paste0('using Eigen::MappedSparseMatrix;',
                       'using Eigen::SparseMatrix;',
                       'const MappedSparseMatrix<double> A(as<MappedSparseMatrix<double> >(x));',
                       'const SparseMatrix<double> At(A.adjoint());',
                       'const SparseMatrix<double> R((At * A).pruned());',
                       'return wrap(R);')

rcppEigenSparseProd = cxxfunction(signature(x = "dgCMatrix"),sparseProdCpp, "RcppEigen")

usersArticles = Matrix(nrow = 1000,ncol = 1000,data = sample(x = c(0,0,0,0,0,0,0,1), size = 1000*1000, replace = TRUE), sparse = TRUE)
sum(t(usersArticles) %*% usersArticles)
sum(rcppEigenSparseProd(x = usersArticles))

set.seed(21)

d = microbenchmark(rcppEigenSparseProd(x = usersArticles),
                   t(usersArticles)%*%usersArticles,
                   Matrix::crossprod(usersArticles),
                   times = 100, 
                   setup=set.seed(21))

sum(rcppEigenSparseProd(x = usersArticles))
sum(t(usersArticles)%*%usersArticles)
sum(Matrix::crossprod(usersArticles))


sourceCpp("scripts/testRcppEigenLimit.cpp")




testeCXXLimit = paste0("// [[Rcpp::depends(RcppEigen)]]    ",
                       '#include <RcppEigen.h>',
                       '#include <Rcpp.h>',
                       'const Map<Eigen::MatrixXi>       M(as<Eigen::Map<Eigen::MatrixXi> >(m));',
                      " return wrap(1); ")


testFunction = cxxfunction(signature(m = "Matrix"),testeCXXLimit)

N <- 87575
A <- matrix(1L, ncol = N, nrow = N)
A <- matrix(1L, ncol = 10, nrow = 10)
get_length_rcpp(A)
get_length_eigen(A)
testFunction(m = A)