setwd("/home/douglas_ulian/recommender_model")
##### Libraries ####
list.of.packages = c('Matrix','Rcpp','profvis')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(Matrix)
library(Rcpp)
library(profvis)

sourceCpp("scripts/armaCrossProd.cpp", showOutput = TRUE)
sourceCpp("scripts/jaccardDistanceCSparse.cpp", showOutput = TRUE)
#sourceCpp("scripts/jaccardDistanceC.cpp", showOutput = TRUE)


#load("trainningSparseDataPrivate.RData")
load("trainningSparseData200.RData")
usersArticlesMatrix = trainningSparseData$usersArticlesMatrix
usersArticlesAttenCoeffMatrix = trainningSparseData$usersArticlesAttenCoeffMatrix
articlesPopularityIndexVector = trainningSparseData$articlesPopularityIndexVector
usersArticlesTimeDiffMatrix   = trainningSparseData$usersArticlesTimeDiffMatrix

rm(trainningSparseData)
gc()

sum(usersArticlesMatrix)
class(usersArticlesMatrix)

print(object.size(usersArticlesMatrix),units = "auto")

#load("crossProdResult.RData")

##### SPARSE 2

testJOTime  = function(armaCP, usersArticlesMatrix, usersArticlesDenseMatrix, usersArticlesAttenCoeffDenseMatrix, articlesPopularityIndexVector){
  jOTime = system.time({
    usersUsersIndexesMatrix = armaCP
    usersUsersIndexesMatrix = tril(x = usersUsersIndexesMatrix, k = -1)
    usersUsersIndexesVector = as.data.frame(summary(usersUsersIndexesMatrix))

    jaccard = matrix(data = 1,nrow = ncol(usersArticlesMatrix), ncol = ncol(usersArticlesMatrix),dimnames = list(colnames(usersArticlesMatrix),colnames(usersArticlesMatrix)))
    jaccard = base::lower.tri(jaccard,diag = FALSE)
    sum(jaccard)
    jaccardOriginal = getJaccardDistancesC(jaccard = jaccard,
                                           rowIndexes = usersUsersIndexesVector$i,
                                           colIndexes = usersUsersIndexesVector$j,
                                           usersArticlesMatrix = usersArticlesDenseMatrix,
                                           usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffDenseMatrix,
                                           articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
  })
  return(list("time" = jOTime,"result" = jaccardOriginal))
}
testJS1Time = function(armaCP, usersArticlesMatrix, usersArticlesAttenCoeffMatrix, articlesPopularityIndexVector){
  
  jS1Time = system.time({
    usersUsersIndexesMatrix = armaCP
    usersUsersIndexesMatrix = tril(x = usersUsersIndexesMatrix, k = -1)
    usersUsersIndexesVector = as.data.frame(summary(usersUsersIndexesMatrix))
    
    jaccard = sparseMatrix(x = 0, i = ncol(usersArticlesMatrix), j = ncol(usersArticlesMatrix))
    jaccardSparse1 = getJaccardDistancesCSparse(jaccard = jaccard,
                                                rowIndexes = usersUsersIndexesVector$i,
                                                colIndexes = usersUsersIndexesVector$j,
                                                usersArticlesMatrix = usersArticlesMatrix,
                                                usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix,
                                                articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
    jaccardSparse1[tril(jaccardSparse1 == 0,k = -1)] = 1
    jaccardSparse1[jaccardSparse1==2] = 0
  })
  return(list("time" = jS1Time,"result" = jaccardSparse1))
} 
testJS2Time = function(        usersArticlesMatrix, usersArticlesAttenCoeffMatrix, articlesPopularityIndexVector){
  jS2Time = system.time({
    armaCP = armaCrossProd(usersArticlesMatrix)
    jaccardSparse2 = getJaccardDistancesCSparseSingleCore(crossProd = armaCP,
                                                          usersArticlesMatrix = usersArticlesMatrix,
                                                          usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix,
                                                          articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
    jaccardSparse2[tril(jaccardSparse2 == 0,k = -1)] = 1
    jaccardSparse2[jaccardSparse2==2] = 0
    jaccardSparse2 = tril(jaccardSparse2,k=-1)
  })
  return(list("time" = jS2Time,"result" = jaccardSparse2))
}


armaCP = armaCrossProd(usersArticlesMatrix)
usersArticlesDenseMatrix = as.matrix(usersArticlesMatrix)
usersArticlesAttenCoeffDenseMatrix = as.matrix(usersArticlesAttenCoeffMatrix)

resutlTestJOTime = testJOTime(armaCP = armaCP, 
                              usersArticlesMatrix = usersArticlesMatrix, 
                              usersArticlesDenseMatrix = usersArticlesDenseMatrix, 
                              usersArticlesAttenCoeffDenseMatrix = usersArticlesAttenCoeffDenseMatrix, 
                              articlesPopularityIndexVector = articlesPopularityIndexVector)


resutlTestJS1Time = testJS1Time(armaCP = armaCP,
                                usersArticlesMatrix = usersArticlesMatrix, 
                                usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix, 
                                articlesPopularityIndexVector = articlesPopularityIndexVector)

resutlTestJS2Time = testJS2Time(usersArticlesMatrix = usersArticlesMatrix, 
                                usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix, 
                                articlesPopularityIndexVector = articlesPopularityIndexVector)

sum(resutlTestJOTime$result)
sum(resutlTestJS1Time$result)
sum(resutlTestJS2Time$result)

resutlTestJOTime$time
resutlTestJS1Time$time
resutlTestJS2Time$time

# prof1 = profvis(interval = 0.005,{resutlTestJOTime = testJOTime(armaCP = armaCP, 
#                                                                 usersArticlesMatrix = usersArticlesMatrix, 
#                                                                 usersArticlesDenseMatrix = usersArticlesDenseMatrix, 
#                                                                 usersArticlesAttenCoeffDenseMatrix = usersArticlesAttenCoeffDenseMatrix, 
#                                                                 articlesPopularityIndexVector = articlesPopularityIndexVector)})
# 
# prof2 = profvis(interval = 0.005,{resutlTestJS1Time = testJS1Time(armaCP = armaCP,
#                                               usersArticlesMatrix = usersArticlesMatrix, 
#                                               usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix, 
#                                               articlesPopularityIndexVector = articlesPopularityIndexVector)})
# 
# prof3 = profvis(interval = 0.005,{resutlTestJS2Time = testJS2Time(usersArticlesMatrix = usersArticlesMatrix, 
#                                                                   usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix, 
#                                                                   articlesPopularityIndexVector = articlesPopularityIndexVector)})









