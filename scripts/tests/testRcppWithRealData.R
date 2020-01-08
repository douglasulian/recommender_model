setwd("/home/douglas_ulian/recommender_model")
##### Libraries ####
list.of.packages = c('Matrix','Rcpp')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(Matrix)
library(Rcpp)

sourceCpp("scripts/armaCrossProd.cpp", showOutput = TRUE)
sourceCpp("scripts/jaccardDistanceCSparse.cpp", showOutput = TRUE)

#load("trainningSparseDataPrivate.RData")
load("trainningSparseData2000.RData")
usersArticlesMatrix = trainningSparseData$usersArticlesMatrix
usersArticlesAttenCoeffMatrix = trainningSparseData$usersArticlesAttenCoeffMatrix
articlesPopularityIndexVector = trainningSparseData$articlesPopularityIndexVector

rm(trainningSparseData)
gc()

sum(usersArticlesMatrix)
class(usersArticlesMatrix)

print(object.size(usersArticlesMatrix),units = "auto")



#load("crossProdResult.RData")
sum(armaCP > 0)

usersUsersIndexesMatrix = armaCP
usersUsersIndexesMatrix = tril(x = usersUsersIndexesMatrix, k = -1)
usersUsersIndexesVector = as.data.frame(summary(usersUsersIndexesMatrix))
usersUsersIndexesVector$row = usersUsersIndexesVector$i
usersUsersIndexesVector$col = usersUsersIndexesVector$j
usersUsersIndexesVector$x = NULL
usersUsersIndexesVector$i = NULL
usersUsersIndexesVector$j = NULL


noCores = 1
size    = ncol(usersArticlesMatrix)
indexes = getIndexes(size,1)

##### ORIGINAL
jaccardOriginal = getJaccardDistancesC(jaccard = matrix(data = 1, nrow = 10, ncol = 10,dimnames = list(usersArticlesMatrixRowNames,usersArticlesMatrixRowNames)),
                                       rowIndexes = as.numeric(indexes[[1]][,1]),
                                       colIndexes = as.numeric(indexes[[1]][,2]),
                                       usersArticlesMatrix = usersArticlesMatrix,
                                       usersArticlesAttenCoeffMatrix = usersArticlesTimeDiffMatrix,
                                       articlesPopularityIndexVector = as.numeric(articlesPopularityIndexVector$pop))
sum(jaccardOriginal)

##### SPARSE 1
jaccardSparse1 = getJaccardDistancesCSparse(rowIndexes = usersUsersIndexesVector$row,
                                            colIndexes = usersUsersIndexesVector$col,
                                            usersArticlesMatrix = usersArticlesMatrix,
                                            usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix,
                                            articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
jaccardSparse1[tril(jaccardSparse1 == 0,k = -1)] = 1
jaccardSparse1[jaccardSparse1==2] = 0
sum(jaccardSparse1)

##### SPARSE 2
armaCP = armaCrossProd(usersArticlesMatrix)
jaccardSparse2 = getJaccardDistancesCSparseSingleCore(crossProd = armaCP,
                                                      usersArticlesMatrix = usersArticlesMatrix,
                                                      usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix,
                                                      articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
jaccardSparse2[tril(jaccardSparse2 == 0,k = -1)] = 1
jaccardSparse2[jaccardSparse2==2] = 0
sum(jaccardSparse2)







# fmat = testFloatMat()
# dmat = testDoubleMat()
# umat = testUMat()
# imat = testIMat()
# smat = testSMat()
# vec = testVec()
# fvec = testFVec()
# 
# print(object.size(fmat),units = "auto")
# print(object.size(dmat),units = "auto")
# print(object.size(umat),units = "auto")
# print(object.size(imat),units = "auto")
# print(object.size(smat),units = "auto")
# print(object.size(vec),units = "auto")
# print(object.size(fvec),units = "auto")
# 
# class(fvec)
# 
# 
# rvec = rep(1,1000*1000)
# class(rvec)
# print(object.size(rvec),units = "auto")





















