# options(warn = -1) 
list.of.packages = c('RPostgreSQL','dplyr','tidyr','doSNOW','microbenchmark')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(microbenchmark)
library(compiler)
library(Rcpp)
library(dplyr)
library(tidyr)
library(tibble)
library(foreach)
library(parallel)
library(doParallel)
library(ggplot2)

debugSource("scripts/distances.R")
debugSource("scripts/dataBase.R")
sourceCpp("scripts/jaccardDistanceC.cpp")

plotsDir = 'document//plots//'

getJaccardDistanceNaive             = function(usersArticlesMatrix, articlesPopularityIndexVector, alphaIndex, articlesUserUserTimeDiffTable){
  jaccardPopTime<-matrix(data = 0,nrow = ncol(usersArticlesMatrix),ncol = ncol(usersArticlesMatrix),dimnames = list(colnames(usersArticlesMatrix),colnames(usersArticlesMatrix)))

  for (userAidx in 1:ncol(usersArticlesMatrix)){
    for (userBidx in 1:ncol(usersArticlesMatrix)){
      # print(paste0(userAidx,'-',userBidx))
      for (articleIdx in 1:nrow(usersArticlesMatrix)){
        if (userAidx != userBidx && ((usersArticlesMatrix[articleIdx,userAidx]!=0) & (usersArticlesMatrix[articleIdx,userBidx]!=0))){
          forgIndex = exp(alphaIndex*filter(articlesUserUserTimeDiffTable,userA==colnames(usersArticlesMatrix)[userAidx],userB==colnames(usersArticlesMatrix)[userBidx],articleId==rownames(usersArticlesMatrix)[articleIdx])$diff)
          jaccardPopTime[userAidx,userBidx] <- jaccardPopTime[userAidx,userBidx] + articlesPopularityIndexVector[articleIdx,3] * forgIndex
        }
      }
      if (sum((usersArticlesMatrix[,userAidx]!=0)&(usersArticlesMatrix[,userBidx]!=0)) > 0)
        jaccardPopTime[userAidx,userBidx] = jaccardPopTime[userAidx,userBidx]/sum((usersArticlesMatrix[,userAidx]!=0)|(usersArticlesMatrix[,userBidx]!=0))
      else
        jaccardPopTime[userAidx,userBidx] = 0
    }
  }
  jaccardPopTime[upper.tri(jaccardPopTime,diag = TRUE)] = 0
  result = jaccardPopTime
  result[upper.tri(result,diag = TRUE)] = 0
  result[lower.tri(result,diag = FALSE)] = 1
  result = result - jaccardPopTime
  return (result)
}
getJaccardDistanceBasicImprovements = function(usersArticlesMatrix, articlesPopularityIndexVector, alphaIndex, articlesUserUserTimeDiffTable){
  
  usersArticlesMatrixOrderByArticles <- usersArticlesMatrix[sort(rownames(usersArticlesMatrix),decreasing = TRUE),]
  
  jaccard<-matrix(data = 0,nrow = ncol(usersArticlesMatrixOrderByArticles),ncol = ncol(usersArticlesMatrixOrderByArticles),dimnames = list(colnames(usersArticlesMatrixOrderByArticles),colnames(usersArticlesMatrixOrderByArticles)))
  jaccardPopTime<-matrix(data = 0,nrow = ncol(usersArticlesMatrixOrderByArticles),ncol = ncol(usersArticlesMatrixOrderByArticles),dimnames = list(colnames(usersArticlesMatrixOrderByArticles),colnames(usersArticlesMatrixOrderByArticles)))
  
  count <- nrow(usersArticlesMatrixOrderByArticles)
  
  for (userBidx in 1:(ncol(usersArticlesMatrixOrderByArticles)-1)){
    
    userAidxStart <- userBidx+1
    
    for (userAidx in userAidxStart:ncol(usersArticlesMatrixOrderByArticles)){
      
      jaccard[userAidx,userBidx] <- sum((usersArticlesMatrixOrderByArticles[,userAidx]!=0)&(usersArticlesMatrixOrderByArticles[,userBidx]!=0))/count
      
      l <- articlesPopularityIndexVector[((usersArticlesMatrixOrderByArticles[,userAidx]!=0)&(usersArticlesMatrixOrderByArticles[,userBidx]!=0)),]
      
      if(nrow(l)>0){
        for (articleIdx in 1:nrow(l)){
          
          auxArticleId <- l[articleIdx,]$articleId
          auxUserAEmail <- colnames(usersArticlesMatrixOrderByArticles)[userAidx]
          auxUserBEmail <- colnames(usersArticlesMatrixOrderByArticles)[userBidx]
          auxArticlePopularity <- filter(articlesPopularityIndexVector,articleId==auxArticleId)$popularityIndex
          
          auxUserUserArticleTimeDiff <- filter(articlesUserUserTimeDiffTable,userA==auxUserAEmail,userB==auxUserBEmail,articleId==auxArticleId)$diff
          
          jaccardPopTime[userAidx,userBidx] <- jaccardPopTime[userAidx,userBidx] + (auxArticlePopularity * exp(alphaIndex*auxUserUserArticleTimeDiff))
        }
        jaccardPopTime[userAidx,userBidx] = 1-(jaccardPopTime[userAidx,userBidx]/sum((usersArticlesMatrixOrderByArticles[,userAidx]!=0)|(usersArticlesMatrixOrderByArticles[,userBidx]!=0)))
      }
      else{
        jaccardPopTime[userAidx,userBidx] = 1
      }
    }
  }
  return (jaccardPopTime)
}
getJaccardDistanceParallel          = function(usersArticlesMatrix, articlesPopularityIndexVector, alphaIndex, articlesUserUserTimeDiffTable){
  size = ncol(usersArticlesMatrix)  
  
  noCores = detectCores(all.tests = FALSE, logical = TRUE) - 1
  # cl <- makePSOCKcluster(noCores, outfile = 'cluster.txt')
  doParallel::registerDoParallel(cores = noCores)
  # doSNOW::registerDoSNOW(cl)
  
  ia = trunc(size/3)
  ib = ia*2
  ic = size
  iA = trunc(size/3)
  iB = ia*2
  iC = size
  
  indexesY1 = seq(from = 1, to = iB)
  indexesX1 = seq(from = 1, to = ia)
  indexesY2 = seq(from = iB+1, to = iC)
  indexesX2 = c(seq(from = 1, to = ia),seq(from = ib+1, to = ic))
  indexesY3 = seq(from = iA+1, to = iC)
  indexesX3 = seq(from = ia+1, to = ib)
  indexes = list(list(indexesY1,indexesX1),list(indexesY2,indexesX2),list(indexesY3,indexesX3))
  
  emails = colnames(usersArticlesMatrix)
  usersArticlesMatrixOrderByArticles = usersArticlesMatrix[sort(rownames(usersArticlesMatrix),decreasing = TRUE),]
  
  parallelResults = foreach(index = indexes,.export=ls(), .packages='dplyr') %dopar% {
    jaccard = matrix(data = 0,nrow = size, ncol = size,dimnames = list(emails,emails))
    indexesY = index[[1]]
    indexesX = index[[2]]
    for (j in 1:length(indexesY)){
      for (k in 1:length(indexesX)){
        userAidx = indexesY[j]
        userBidx = indexesX[k]
        if (userAidx > userBidx){
          intersection = ((usersArticlesMatrixOrderByArticles[,userAidx]!=0)&(usersArticlesMatrixOrderByArticles[,userBidx]!=0))
          union        = ((usersArticlesMatrixOrderByArticles[,userAidx]!=0)|(usersArticlesMatrixOrderByArticles[,userBidx]!=0))
          intersectionList = articlesPopularityIndexVector[intersection,]
          auxUserAEmail = emails[userAidx]
          auxUserBEmail = emails[userBidx]
          
          if(nrow(intersectionList)>0){
            for (articleIdx in 1:nrow(intersectionList)){
              auxArticleId = intersectionList[articleIdx,]$articleId
              auxArticlePopularity = filter(articlesPopularityIndexVector,articleId==auxArticleId)$popularityIndex
              auxUserUserArticleTimeDiff = filter(articlesUserUserTimeDiffTable,userA==auxUserAEmail,userB==auxUserBEmail,articleId==auxArticleId)$diff
              jaccard[userAidx,userBidx] = jaccard[userAidx,userBidx] + (auxArticlePopularity * exp(alphaIndex*auxUserUserArticleTimeDiff))
            }
            jaccard[userAidx,userBidx] = 1-(jaccard[userAidx,userBidx]/sum(union))
          }
          else{
            jaccard[userAidx,userBidx] = 1
          }
        }
      }
    }
    jaccard
  }
  result = Reduce('+',parallelResults)
  # stopCluster(cl)
  stopImplicitCluster()
  
  return(result)
}
getJaccardDistanceParallelPreExp    = function(usersArticlesMatrix, articlesPopularityIndexVector, alphaIndex, articlesUserUserTimeDiffTable, usersArticlesTimeDiffMatrix){
  usersArticlesAttenCoeffMatrix = t(mapply(FUN = getAttenCoeff, as.tibble(t(as.matrix(usersArticlesTimeDiffMatrix))),alpha = alphaIndex))
  emails = colnames(usersArticlesMatrix)
  size = ncol(usersArticlesMatrix)  
  
  noCores = detectCores(all.tests = FALSE, logical = TRUE) - 1
  
  indexes = getIndexes(size,noCores)
  
  doParallel::registerDoParallel(cores = noCores)
  # cl <- makePSOCKcluster(noCores, outfile = 'cluster.txt')
  # doSNOW::registerDoSNOW(cl)
  
  parallelResults = foreach(index = indexes, .export=ls(envir=globalenv()), .packages='dplyr') %dopar% {
    jaccard = matrix(data = 0,nrow = size, ncol = size,dimnames = list(emails,emails))
    rowIndexes = index[,1]
    colIndexes = index[,2]
    for (k in 1:length(colIndexes)){
      userAidx = rowIndexes[k]
      userBidx = colIndexes[k]

      intersection    = ((usersArticlesMatrix[,userAidx] != 0) & (usersArticlesMatrix[,userBidx] != 0))
      union           = ((usersArticlesMatrix[,userAidx] != 0) | (usersArticlesMatrix[,userBidx] != 0))
      if (sum(intersection) > 0) {
        
        auxUserAEmail = emails[userAidx]
        auxUserBEmail = emails[userBidx]
        
        aux = which(articlesUserUserTimeDiffTable[,'userA'] == auxUserAEmail)
        aux = aux[which(articlesUserUserTimeDiffTable[aux,'userB'] == auxUserBEmail)]
        userUserArticlesTimeDiffVector_Vioj = articlesUserUserTimeDiffTable[aux,'diff']
        
        userAArticlesTimeExpVector = c(rep(0,sum(intersection)))
        userAArticlesTimeExpVector[userUserArticlesTimeDiffVector_Vioj >  0] = (usersArticlesAttenCoeffMatrix[intersection,userAidx][userUserArticlesTimeDiffVector_Vioj >  0]/usersArticlesAttenCoeffMatrix[intersection,userBidx][userUserArticlesTimeDiffVector_Vioj >  0])*articlesPopularityIndexVector[intersection,]$popularityIndex[userUserArticlesTimeDiffVector_Vioj > 0]
        userAArticlesTimeExpVector[userUserArticlesTimeDiffVector_Vioj <= 0] = (usersArticlesAttenCoeffMatrix[intersection,userBidx][userUserArticlesTimeDiffVector_Vioj <= 0]/usersArticlesAttenCoeffMatrix[intersection,userAidx][userUserArticlesTimeDiffVector_Vioj <= 0])*articlesPopularityIndexVector[intersection,]$popularityIndex[userUserArticlesTimeDiffVector_Vioj <= 0]

        jaccard[userAidx,userBidx] = 1 - (sum(userAArticlesTimeExpVector)/sum(union))
        
      }
      else{
        jaccard[userAidx,userBidx] = 1
      }
    }
    jaccard
  }
  result = Reduce('+',parallelResults)
  # stopCluster(cl)
  doParallel::stopImplicitCluster()
  return(result)
}
getJaccardDistanceParallelC         = function(usersArticlesMatrix, articlesPopularityIndexVector, alphaIndex, usersArticlesTimeDiffMatrix){
  usersArticlesAttenCoeffMatrix = t(mapply(FUN = getAttenCoeff, as.tibble(t(as.matrix(usersArticlesTimeDiffMatrix))),alpha = alphaIndex))
  emails = colnames(usersArticlesMatrix)
  size   = ncol(usersArticlesMatrix)  
  noCores = detectCores(all.tests = FALSE, logical = TRUE) - 1
  indexes = getIndexes(size,noCores)
  
  # cl <- makePSOCKcluster(noCores, outfile = 'cluster.txt')
  # clusterEvalQ(cl, {library(Rcpp) sourceCpp("scripts/jaccardDistanceC.cpp") })
  # doSNOW::registerDoSNOW(cl)
  doParallel::registerDoParallel(cores = noCores)
  
  parallelResults = foreach(index = indexes, .export = ls()) %dopar% {
    jaccard = matrix(data = 0,nrow = size, ncol = size,dimnames = list(emails,emails))
    jaccard2 = getJaccardDistancesC(jaccard = jaccard,
                                   rowIndexes = as.integer(index[,1]),
                                   colIndexes = as.integer(index[,2]),
                                   usersArticlesMatrix = as.matrix(usersArticlesMatrix),
                                   usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix,
                                   articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
    jaccard2
  }
  
  result = Reduce('+',parallelResults)
  # stopCluster(cl)
  stopImplicitCluster()
  gc()
  return(result)
}

getArticlesUserUserTimeDiffTable = function(con){
articlesUserUserTimeDiffTable = dbGetQuery(con, 'select a.id as "articleId",
                                             userA.email as "userA",
                                             userB.email as "userB",
                                             avg(extract(epoch from eventsUserA.data_evento)) - avg(extract(epoch from eventsUserB.data_evento)) as diff
                                             from users userA
                                             inner join events eventsUserA on userA.email = eventsUserA.email
                                             inner join articles a on eventsUserA.article_id = a.id
                                             inner join events eventsUserB on eventsUserB.article_id = a.id
                                             inner join users userB on userB.email = eventsUserB.email
                                             where a.push_time is null 
                                               and userA.email != userB.email
                                             group by a.id,userA.email,userB.email
                                             order by userA.email,userB.email,a.id desc')
  
  return(articlesUserUserTimeDiffTable)
}
getArticlesUserUserTimeDiffTableAbs = function(con){
  articlesUserUserTimeDiffTable = dbGetQuery(con, 'select a.id as "articleId",
                                             userA.email as "userA",
                                             userB.email as "userB",
                                             abs(avg(extract(epoch from eventsUserA.data_evento)) - avg(extract(epoch from eventsUserB.data_evento))) as diff
                                             from users userA
                                             inner join events eventsUserA on userA.email = eventsUserA.email
                                             inner join articles a on eventsUserA.article_id = a.id
                                             inner join events eventsUserB on eventsUserB.article_id = a.id
                                             inner join users userB on userB.email = eventsUserB.email
                                             where a.push_time is null 
                                             and userA.email != userB.email
                                             group by a.id,userA.email,userB.email
                                             order by userA.email,userB.email,a.id desc')
  
  return(articlesUserUserTimeDiffTable)
}

naive = function(){
  getJaccardDistanceNaive(usersArticlesMatrix = usersArticlesMatrix, 
                          articlesPopularityIndexVector = articlesPopularityIndexVector, 
                          alphaIndex = alphaIndex, 
                          articlesUserUserTimeDiffTable = articlesUserUserTimeDiffTableAbs)
}
basic = function(){
  getJaccardDistanceBasicImprovements(usersArticlesMatrix = usersArticlesMatrix, 
                                      articlesPopularityIndexVector = articlesPopularityIndexVector, 
                                      alphaIndex = alphaIndex, 
                                      articlesUserUserTimeDiffTable = articlesUserUserTimeDiffTableAbs) 
}

basicParallel = function(){
  getJaccardDistanceParallel(usersArticlesMatrix = usersArticlesMatrix, 
                             articlesPopularityIndexVector = articlesPopularityIndexVector, 
                             alphaIndex = alphaIndex, 
                             articlesUserUserTimeDiffTable = articlesUserUserTimeDiffTableAbs)
}

preExpParallel = function(){
  getJaccardDistanceParallelPreExp(usersArticlesMatrix = usersArticlesMatrix, 
                                   articlesPopularityIndexVector = articlesPopularityIndexVector, 
                                   alphaIndex = alphaIndex, 
                                   articlesUserUserTimeDiffTable = articlesUserUserTimeDiffTable, 
                                   usersArticlesTimeDiffMatrix = usersArticlesTimeDiffMatrix)
}

preExpParallelCmpFun = cmpfun(preExpParallel)

cParallel = function(){
  getJaccardDistanceParallelC(usersArticlesMatrix = usersArticlesMatrix, 
                              articlesPopularityIndexVector = articlesPopularityIndexVector, 
                              alphaIndex = alphaIndex, 
                              usersArticlesTimeDiffMatrix = usersArticlesTimeDiffMatrix)  
}

clearConnections()
con = getDataBaseConnection(schema = 'trainning200',dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")

trainningData = getTrainningData(con)

usersArticlesMatrix           = trainningData$usersArticlesMatrix
usersArticlesTimeDiffMatrix   = trainningData$usersArticlesTimeDiffMatrix
articlesPopularityIndexVector = trainningData$articlesPopularityIndexVector
articlesPopularityIndexVector = articlesPopularityIndexVector[which(articlesPopularityIndexVector[,1] %in% rownames(usersArticlesMatrix)),]

maxTime = getEventsMaxTime(con)
articlesUserUserTimeDiffTable = getArticlesUserUserTimeDiffTable(con)
articlesUserUserTimeDiffTable[,4] = articlesUserUserTimeDiffTable[,4]/60/60/24

articlesUserUserTimeDiffTableAbs = getArticlesUserUserTimeDiffTableAbs(con)
articlesUserUserTimeDiffTableAbs[,4] = articlesUserUserTimeDiffTableAbs[,4]/60/60/24

alphaIndex = 0.1

# bench = microbenchmark(naive(), basic(), basicParallel(),preExpParallel(),cParallel(),times = 10)
test = list()
for (i in 1:10) {
  
  # naiveTime          = system.time(naive())
  basicTime                = system.time(basic())
  basicParallelTime        = system.time(basicParallel())
  preExpParallelTime       = system.time(preExpParallel())
  preExpParallelCmpFunTime = system.time(preExpParallelCmpFun())
  cParallelTime            = system.time(cParallel())
  
  test$run                  = c(test$run,rep(i,5))
  # test$userTime             = c(test$userTime            ,naiveTime[1])
  # test$systemTime           = c(test$systemTime          ,naiveTime[2])
  # test$elapsedTime          = c(test$elapsedTime         ,naiveTime[3])
  # test$func                 = c(test$func                ,'Naive')
  test$userTime             = c(test$userTime            ,basicTime[1])
  test$systemTime           = c(test$systemTime          ,basicTime[2])
  test$elapsedTime          = c(test$elapsedTime         ,basicTime[3])
  test$func                 = c(test$func                ,'Basic')
  test$userTime             = c(test$userTime            ,basicParallelTime[1])
  test$systemTime           = c(test$systemTime          ,basicParallelTime[2])
  test$elapsedTime          = c(test$elapsedTime         ,basicParallelTime[3])
  test$func                 = c(test$func                ,'Parallel')
  test$userTime             = c(test$userTime            ,preExpParallelTime[1])
  test$systemTime           = c(test$systemTime          ,preExpParallelTime[2])
  test$elapsedTime          = c(test$elapsedTime         ,preExpParallelTime[3])
  test$func                 = c(test$func                ,'Parallel Pre. Exp.')
  test$userTime             = c(test$userTime            ,preExpParallelCmpFunTime[1])
  test$systemTime           = c(test$systemTime          ,preExpParallelCmpFunTime[2])
  test$elapsedTime          = c(test$elapsedTime         ,preExpParallelCmpFunTime[3])
  test$func                 = c(test$func                ,'Pre-compiled Parallel Pre. Exp.')
  test$userTime             = c(test$userTime            ,cParallelTime[1])
  test$systemTime           = c(test$systemTime          ,cParallelTime[2])
  test$elapsedTime          = c(test$elapsedTime         ,cParallelTime[3])
  test$func                 = c(test$func                ,'Parallel C')
}



gg = ggplot2::ggplot(data = as.tibble(test), mapping = aes(x = run,y = elapsedTime,colour = func)) +
   geom_line() +
   ggtitle('Weighted Jaccard Distance Implementations Performance') +
   labs(x = 'Run', y = 'Time (seconds)', colour = 'Implementation') +
   scale_x_continuous(breaks = seq(min(test$run), max(test$run), by = 1)) +
   theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(plotsDir,"weighted-jaccard-test-plot.png"),
       plot = gg, 
       width = 10, 
       height = 8, 
       dpi = 600)

print(gg)


# jaccardTest = list()
# 
# jaccardTest$naive$time = system.time({
#   jaccardTest$naive$result          = getJaccardDistanceNaive(usersArticlesMatrix = usersArticlesMatrix, articlesPopularityIndexVector = articlesPopularityIndexVector, alphaIndex = alphaIndex, articlesUserUserTimeDiffTable = articlesUserUserTimeDiffTableAbs)
# })
#  
# jaccardTest$basic$time = system.time({
#   jaccardTest$basic$result          = getJaccardDistanceBasicImprovements(usersArticlesMatrix = usersArticlesMatrix, articlesPopularityIndexVector = articlesPopularityIndexVector, alphaIndex = alphaIndex, articlesUserUserTimeDiffTable = articlesUserUserTimeDiffTableAbs)
# })
# 
# jaccardTest$parallel$time = system.time({
#   jaccardTest$parallel$result       = getJaccardDistanceParallel(usersArticlesMatrix = usersArticlesMatrix, articlesPopularityIndexVector = articlesPopularityIndexVector, alphaIndex = alphaIndex, articlesUserUserTimeDiffTable = articlesUserUserTimeDiffTableAbs)
# })
# 
# jaccardTest$parallelPreExp$time = system.time({
#   jaccardTest$parallelPreExp$result = getJaccardDistanceParallelPreExp(usersArticlesMatrix = usersArticlesMatrix, articlesPopularityIndexVector = articlesPopularityIndexVector, alphaIndex = alphaIndex, articlesUserUserTimeDiffTable = articlesUserUserTimeDiffTable, usersArticlesTimeDiffMatrix = usersArticlesTimeDiffMatrix)
# })
# 
# jaccardTest$parallelC$time = system.time({
#   jaccardTest$parallelC$result = getJaccardDistanceParallelC(usersArticlesMatrix = usersArticlesMatrix, articlesPopularityIndexVector = articlesPopularityIndexVector, alphaIndex = alphaIndex, usersArticlesTimeDiffMatrix = usersArticlesTimeDiffMatrix)
# })
# 


