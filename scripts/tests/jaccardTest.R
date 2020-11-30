setwd("/home/douglas_ulian/recommender_model")
##### Libraries ####
list.of.packages = c('Matrix','Rcpp','tibble','dplyr')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(Matrix)
library(Rcpp)
library(tibble)
library(dplyr)

#sourceCpp("scripts/armaCrossProd.cpp", showOutput = TRUE)
sourceCpp("scripts/jaccardDistanceCSparse.cpp", showOutput = TRUE)

getIndexes = function(sampleSize,ncores){
  #ncores = ncores*4
  z <- sequence(sampleSize)
  indexes = cbind(
    row = unlist(lapply(2:sampleSize, function(x) x:sampleSize), use.names = FALSE),
    col = rep(z[-length(z)], times = rev(tail(z, -1)) - 1))
  len = nrow(indexes)
  perCore = round(len/ncores)
  from = 1
  to = 0
  indexesList = list(rep(0,ncores))
  for (i in  1:ncores) {
    to = perCore*i
    if (i == ncores) to = len
    indexesList[[i]] = indexes[from:to,]
    from = to + 1
  }
  return(indexesList)
}
usersArticles = tribble(~User,~a1,~a2,~a3,~a4,~a5,~a6,~a7,~a8,
                        "A",0,1,0,1,1,0,0,0,
                        "B",1,0,1,0,1,1,0,0,
                        "C",0,0,0,0,0,0,1,0,
                        "D",1,1,0,0,0,0,0,0,
                        "E",1,0,1,0,0,1,0,0,
                        "F",0,0,0,0,0,0,0,1,
                        "G",1,1,0,0,0,0,0,0,
                        "H",0,0,0,1,0,0,0,0,
                        "I",0,0,0,1,0,1,0,0,
                        "J",0,0,0,1,0,0,0,0)

usersArticlesMatrixRowNames = usersArticles$User

usersArticles$User = NULL

usersArticlesMatrix = as.matrix(usersArticles)

rownames(usersArticlesMatrix) =usersArticlesMatrixRowNames

usersArticlesMatrix = usersArticlesMatrix[1:10,1:8]

usersArticlesTimeDiffMatrix = matrix(data = 1L,nrow = 10,ncol = 8)
#usersArticlesTimeDiffMatrix = usersArticlesMatrix
#usersArticlesTimeDiffMatrix[usersArticlesTimeDiffMatrix == 1] = seq(from = 0.1,to = 1,by = 1/(sum(usersArticlesTimeDiffMatrix == 1)-1))
articlesPopularityIndexVector = tibble(article = rep(0,8)) %>% 
                                  add_column(count = rep(1,ncol(usersArticlesMatrix))) %>%
                                    add_column(pop = rep(1,ncol(usersArticlesMatrix)))

#articlesPopularityIndexVector[2,3] = 1/(log1p(1))

usersArticlesMatrix = t(usersArticlesMatrix)
usersArticlesTimeDiffMatrix = t(usersArticlesTimeDiffMatrix)

usersArticlesTimeDiffMatrix[2,4] = exp(-0.1974*0.5)
usersArticlesTimeDiffMatrix[2,7] = 1
usersArticlesTimeDiffMatrix[1,4] = exp(-0.1974*0.7)
usersArticlesTimeDiffMatrix[1,7] = 1

#noCores = 1
size    = nrow(usersArticlesMatrix)
indexes = getIndexes(size,1)

##### ORIGINAL
jaccard = getJaccardDistancesC(jaccard = matrix(data = 1, nrow = 10, ncol = 10,dimnames = list(usersArticlesMatrixRowNames,usersArticlesMatrixRowNames)),
                                rowIndexes = as.numeric(indexes[[1]][,1]),
                                colIndexes = as.numeric(indexes[[1]][,2]),
                                usersArticlesMatrix = usersArticlesMatrix,
                                usersArticlesAttenCoeffMatrix = usersArticlesTimeDiffMatrix,
                                articlesPopularityIndexVector = as.matrix(articlesPopularityIndexVector[,3]))
                                


jaccard = tril(jaccard,k = -1)
sum(jaccard)

jaccard[rownames(jaccard)=="G",colnames(jaccard)=="D"]

##### SPARSE SINGLE THREAD
armaCP = armaCrossProd(as(usersArticlesMatrix, "sparseMatrix"))
jaccardSparseSingle = getJaccardDistancesCSparseSingleCore(crossProd = armaCP,
                                                usersArticlesMatrix = as(usersArticlesMatrix, "sparseMatrix"),
                                                usersArticlesAttenCoeffMatrix = as(usersArticlesTimeDiffMatrix, "sparseMatrix"),
                                                articlesPopularityIndexVector = as.matrix(articlesPopularityIndexVector[,3]))
jaccardSparseSingle[tril(jaccardSparseSingle == 0,k = -1)] = 1
jaccardSparseSingle[jaccardSparseSingle==2] = 0
jaccardSparseSingle = tril(jaccardSparseSingle,k=-1)

sum(jaccard)
sum(jaccardSparseSingle)

object.size(jaccard)
object.size(jaccardSparseSingle)



