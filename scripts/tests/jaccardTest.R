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

#debugSource(fileName = 'scripts/distances.R')
#debugSource(fileName = 'scripts/util.R')

usersArticles = tribble(~User,~a1,~a2,~a3,~a4,~a5,~a6,~a7,~a8,
                        "A",0,1,0,1,1,0,0,0,
                        "B",1,0,1,0,1,1,0,0,
                        "C",0,0,0,0,0,0,1,0,
                        "D",0,1,0,0,0,0,0,0,
                        "E",1,0,1,0,0,1,0,0,
                        "F",0,0,0,0,0,0,0,1,
                        "G",0,1,0,0,0,0,0,0,
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

usersArticlesMatrix = t(usersArticlesMatrix)
usersArticlesTimeDiffMatrix = t(usersArticlesTimeDiffMatrix)

#noCores = 1
size    = ncol(usersArticlesMatrix)
indexes = getIndexes(size,1)

##### ORIGINAL
jaccard = getJaccardDistancesC(jaccard = matrix(data = 1, nrow = 10, ncol = 10,dimnames = list(usersArticlesMatrixRowNames,usersArticlesMatrixRowNames)),
                                rowIndexes = as.numeric(indexes[[1]][,1]),
                                colIndexes = as.numeric(indexes[[1]][,2]),
                                usersArticlesMatrix = usersArticlesMatrix,
                                usersArticlesAttenCoeffMatrix = usersArticlesTimeDiffMatrix,
                                articlesPopularityIndexVector = as.numeric(articlesPopularityIndexVector$pop))
                                


jaccard = tril(jaccard,k = -1)
sum(jaccard)

##### SPARSE MULTI THREAD
# jaccardSparse = getJaccardDistancesCSparse(rowIndexes = as.numeric(indexes[[1]][,1]),
#                                            colIndexes = as.numeric(indexes[[1]][,2]),
#                                            usersArticlesMatrix = as(usersArticlesMatrix, "sparseMatrix"),
#                                            usersArticlesAttenCoeffMatrix = as(usersArticlesTimeDiffMatrix, "sparseMatrix"),
#                                            articlesPopularityIndexVector = as.matrix(articlesPopularityIndexVector[,3]))
# jaccardSparse[tril(jaccardSparse == 0,k = -1)] = 1
# jaccardSparse[jaccardSparse==2] = 0
# 
# sum(jaccardSparse)

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
sum(jaccardSparse)
sum(jaccardSparseSingle)

object.size(jaccard)
object.size(jaccardSparse)
object.size(jaccardSparseSingle)
sum(jaccardSparse > 0) - sum(jaccardSparse == 1)
