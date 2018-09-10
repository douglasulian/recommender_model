
# Calculates Jaccard Index over columns
jaccardDistance = function(usersArticlesMatrixOrder,articlesPopularityIndexVector,articlesUserUserTimeDiffTable,alphaIndex){
  print('jaccardDistance')  
  usersArticlesMatrixOrderByArticles = usersArticlesMatrix[sort(rownames(usersArticlesMatrix),decreasing = TRUE),]
  
  print(ncol(usersArticlesMatrixOrderByArticles))
  
  jaccard<-matrix(data = 0,nrow = ncol(usersArticlesMatrixOrderByArticles),ncol = ncol(usersArticlesMatrixOrderByArticles),dimnames = list(colnames(usersArticlesMatrixOrderByArticles),colnames(usersArticlesMatrixOrderByArticles)))
  jaccardPopTime<-matrix(data = 0,nrow = ncol(usersArticlesMatrixOrderByArticles),ncol = ncol(usersArticlesMatrixOrderByArticles),dimnames = list(colnames(usersArticlesMatrixOrderByArticles),colnames(usersArticlesMatrixOrderByArticles)))
  
  count = nrow(usersArticlesMatrixOrderByArticles)
  
  for (userAidx in 1:ncol(usersArticlesMatrixOrderByArticles)){
    
    for (userBidx in 1:ncol(usersArticlesMatrixOrderByArticles)){
      
      jaccard[userAidx,userBidx] = sum((usersArticlesMatrixOrderByArticles[,userAidx]!=0)&(usersArticlesMatrixOrderByArticles[,userBidx]!=0))/count
      
      for (articleIdx in 1:nrow(usersArticlesMatrixOrderByArticles)){
        
        if (userAidx != userBidx && ((usersArticlesMatrixOrderByArticles[articleIdx,userAidx]!=0) & (usersArticlesMatrixOrderByArticles[articleIdx,userBidx]!=0))){
          
          auxArticleId = rownames(usersArticlesMatrixOrderByArticles)[articleIdx]
          auxUserAEmail = colnames(usersArticlesMatrixOrderByArticles)[userAidx]
          auxUserBEmail = colnames(usersArticlesMatrixOrderByArticles)[userBidx]
          auxArticlePopularity = articlesPopularityIndexVector[articleIdx,3]
          auxUserUserArticleTimeDiff = filter(articlesUserUserTimeDiffTable,userA==auxUserAEmail,userB==auxUserBEmail,articleId==auxArticleId)$diff
          
          jaccardPopTime[userAidx,userBidx] = jaccardPopTime[userAidx,userBidx] + (auxArticlePopularity * exp(-alphaIndex*auxUserUserArticleTimeDiff))
          
        }
      }
    }
  }
  return (list(jaccard=jaccard,jaccardPopTime=jaccardPopTime))
}

# Calculates Jaccard Index over columns just for upper triangle
jaccardDistanceTriag = function(usersArticlesMatrix,articlesPopularityIndexVector,articlesUserUserTimeDiffTable,alphaIndex){
  print('jaccardDistanceTriag')  
  
  usersArticlesMatrixOrderByArticles = usersArticlesMatrix[sort(rownames(usersArticlesMatrix),decreasing = TRUE),]
  
  jaccard<-matrix(data = 0,nrow = ncol(usersArticlesMatrixOrderByArticles),ncol = ncol(usersArticlesMatrixOrderByArticles),dimnames = list(colnames(usersArticlesMatrixOrderByArticles),colnames(usersArticlesMatrixOrderByArticles)))
  jaccardPopTime<-matrix(data = 0,nrow = ncol(usersArticlesMatrixOrderByArticles),ncol = ncol(usersArticlesMatrixOrderByArticles),dimnames = list(colnames(usersArticlesMatrixOrderByArticles),colnames(usersArticlesMatrixOrderByArticles)))
  
  count = nrow(usersArticlesMatrixOrderByArticles)
  
  for (userAidx in 1:(ncol(usersArticlesMatrixOrderByArticles)-1)){
    
    userBidxStart = userAidx+1
    
    for (userBidx in userBidxStart:ncol(usersArticlesMatrixOrderByArticles)){
      
      jaccard[userAidx,userBidx] = sum((usersArticlesMatrixOrderByArticles[,userAidx]!=0)&(usersArticlesMatrixOrderByArticles[,userBidx]!=0))/count
      
      for (articleIdx in 1:nrow(usersArticlesMatrixOrderByArticles)){
        
        if (userAidx != userBidx && ((usersArticlesMatrixOrderByArticles[articleIdx,userAidx]!=0) & (usersArticlesMatrixOrderByArticles[articleIdx,userBidx]!=0))){
          auxArticleId = rownames(usersArticlesMatrixOrderByArticles)[articleIdx]
          auxUserAEmail = colnames(usersArticlesMatrixOrderByArticles)[userAidx]
          auxUserBEmail = colnames(usersArticlesMatrixOrderByArticles)[userBidx]
          auxArticlePopularity = articlesPopularityIndexVector[articleIdx,3]
          auxUserUserArticleTimeDiff = filter(articlesUserUserTimeDiffTable,userA==auxUserAEmail,userB==auxUserBEmail,articleId==auxArticleId)$diff
          jaccardPopTime[userAidx,userBidx] = jaccardPopTime[userAidx,userBidx] + (auxArticlePopularity * exp(-alphaIndex*auxUserUserArticleTimeDiff))
        }
      }
    }
  }
  return (list(jaccard=jaccard,jaccardPopTime=jaccardPopTime))
}

# Calculates Jaccard Index using vector index matching
jaccardDistanceVectorIdx = function(usersArticlesMatrix,articlesPopularityIndexVector,articlesUserUserTimeDiffTable,alphaIndex){
  print('jaccardDistanceVectorIdx')  
  usersArticlesMatrixOrderByArticles = usersArticlesMatrix[sort(rownames(usersArticlesMatrix),decreasing = TRUE),]
  
  jaccard<-matrix(data = 0,nrow = ncol(usersArticlesMatrixOrderByArticles),ncol = ncol(usersArticlesMatrixOrderByArticles),dimnames = list(colnames(usersArticlesMatrixOrderByArticles),colnames(usersArticlesMatrixOrderByArticles)))
  jaccardPopTime<-matrix(data = 0,nrow = ncol(usersArticlesMatrixOrderByArticles),ncol = ncol(usersArticlesMatrixOrderByArticles),dimnames = list(colnames(usersArticlesMatrixOrderByArticles),colnames(usersArticlesMatrixOrderByArticles)))
  
  count = nrow(usersArticlesMatrixOrderByArticles)
  
  for (userAidx in 1:(ncol(usersArticlesMatrixOrderByArticles)-1)){
    
    userBidxStart = userAidx+1
    
    for (userBidx in userBidxStart:ncol(usersArticlesMatrixOrderByArticles)){
      intersection <- ((usersArticlesMatrixOrderByArticles[,userAidx]!=0)&(usersArticlesMatrixOrderByArticles[,userBidx]!=0))
      jaccard[userAidx,userBidx] = sum(intersection)/count
      intersectionList = articlesPopularityIndexVector[intersection,]
      emails <- colnames(usersArticlesMatrixOrderByArticles)
      auxUserAEmail = emails[userAidx]
      auxUserBEmail = emails[userBidx]
      
      if(nrow(intersectionList)>0){
        for (articleIdx in 1:nrow(intersectionList)){
          auxArticleId = intersectionList[articleIdx,]$articleId
          auxArticlePopularity = filter(articlesPopularityIndexVector,articleId==auxArticleId)$popularityIndex
          auxUserUserArticleTimeDiff = filter(articlesUserUserTimeDiffTable,userA==auxUserAEmail,userB==auxUserBEmail,articleId==auxArticleId)$diff
          jaccardPopTime[userAidx,userBidx] = jaccardPopTime[userAidx,userBidx] + (auxArticlePopularity * exp(-alphaIndex*auxUserUserArticleTimeDiff))
        }
      }
    }
  }
  return (list(jaccard=jaccard,jaccardPopTime=jaccardPopTime))
}

getUsersArticlesTimeDiffMatrix<-function(con){
  # Defines the date anchor against wich all the other dates will be aged
  maxTimesamp = dbGetQuery(con, "select max(extract(epoch from e.data_evento)) from events e")
  
  # Extracts users versus articles table, with avverage age for each article  
  usersArticlesTimeDiff = dbGetQuery(con, paste('select u.email ',
                                                ',e.article_id as "articleId" ',
                                                ',(',
                                                maxTimesamp,
                                                '- avg(extract(epoch from e.data_evento)))/60/60/24/365 as timeDiff ',
                                                'from users u ',
                                                'inner join events e on u.email = e.email',
                                                'group by u.email,e.article_id'))
  
  # Converts table into matrix
  usersArticlesTimeDiffMatrix = spread(usersArticlesTimeDiff,key = email,value = timediff, fill = 0)
  
  # Sets rownames on the DF and removes id column
  rownames(usersArticlesTimeDiffMatrix) = usersArticlesTimeDiffMatrix[,1]
  usersArticlesTimeDiffMatrix$articleId = NULL
  
  return(usersArticlesTimeDiffMatrix)  
}
jaccardDistanceVectorIdxParallel <- function(usersArticlesMatrix,articlesPopularityIndexVector,articlesUserUserTimeDiffTable,alphaIndex, parallel = TRUE){
  noCores = detectCores(all.tests = FALSE, logical = TRUE) - 1
  size = ncol(usersArticlesMatrix)  
  indexes = getIndexes(size,noCores)
  
  emails = colnames(usersArticlesMatrix)
  usersArticlesMatrixOrderByArticles = usersArticlesMatrix[sort(rownames(usersArticlesMatrix),decreasing = TRUE),]
  jaccard = matrix(data = 0,nrow = size, ncol = size,dimnames = list(emails,emails))
  if (parallel){
    cl<-makeCluster(noCores,type="FORK",outfile = 'jaccardDistanceVectorIdxParallel.txt')
    registerDoParallel(cl)
    
    parallelResults = foreach(index = indexes) %dopar% {
      indexesRow = index[,1]
      indexesCol = index[,2]
      for (k in 1:length(indexesRow)){
        userAidx = indexesRow[k]
        userBidx = indexesCol[k]
        if (userAidx > userBidx){
          intersection = ((usersArticlesMatrixOrderByArticles[,userAidx]!=0)&(usersArticlesMatrixOrderByArticles[,userBidx]!=0))
          union = ((usersArticlesMatrixOrderByArticles[,userAidx]!=0)|(usersArticlesMatrixOrderByArticles[,userBidx]!=0))
          intersectionList = articlesPopularityIndexVector[intersection,]
          auxUserAEmail = emails[userAidx]
          auxUserBEmail = emails[userBidx]
          if(nrow(intersectionList)>0){
            for (articleIdx in 1:nrow(intersectionList)){
              auxArticleId = intersectionList[articleIdx,]$articleId
              auxArticlePopularity = filter(articlesPopularityIndexVector,articleId==auxArticleId)$popularityIndex
              auxUserUserArticleTimeDiff = abs(filter(articlesUserUserTimeDiffTable,userA==auxUserAEmail,userB==auxUserBEmail,articleId==auxArticleId)$diff)
              jaccard[userAidx,userBidx] = jaccard[userAidx,userBidx] + (auxArticlePopularity * exp(-alphaIndex*auxUserUserArticleTimeDiff))
            }
            jaccard[userAidx,userBidx] = 1-jaccard[userAidx,userBidx]/sum(union)
          }
        }
      }
      jaccard
    }
    result = Reduce('+',parallelResults)
    result2 = matrix(data = rep(0,size**2),nrow = size)
    result2[lower.tri(result2)] = 1
    result = result2 - result
    stopCluster(cl = cl)
  }
  else{
    # write(x = (paste0('userAidx.userBidx',
    #                   ' coef',
    #                   ' pop', 
    #                   ' timeDiff', 
    #                   ' article')
    # ),file = 'threefour.txt',append = TRUE)
    
    parallelResults = foreach(index = indexes) %do% {
      indexesRow = index[[1]]
      indexesCol = index[[2]]
      for (j in 1:length(indexesCol)){
        for (k in 1:length(indexesRow)){
          userAidx = indexesRow[k]
          userBidx = indexesCol[j]
          if (userAidx > userBidx){
            intersection = ((usersArticlesMatrixOrderByArticles[,userAidx]!=0)&(usersArticlesMatrixOrderByArticles[,userBidx]!=0))
            union = ((usersArticlesMatrixOrderByArticles[,userAidx]!=0)|(usersArticlesMatrixOrderByArticles[,userBidx]!=0))
            intersectionList = articlesPopularityIndexVector[intersection,]
            auxUserAEmail = emails[userAidx]
            auxUserBEmail = emails[userBidx]
            if(nrow(intersectionList)>0){
              for (articleIdx in 1:nrow(intersectionList)){
                auxArticleId = intersectionList[articleIdx,]$articleId
                auxArticlePopularity = filter(articlesPopularityIndexVector,articleId==auxArticleId)$popularityIndex
                auxUserUserArticleTimeDiff = abs(filter(articlesUserUserTimeDiffTable,userA==auxUserAEmail,userB==auxUserBEmail,articleId==auxArticleId)$diff)
                jaccard[userAidx,userBidx] = jaccard[userAidx,userBidx] + (auxArticlePopularity * exp(-alphaIndex*auxUserUserArticleTimeDiff))
                # write(x = (paste0(userAidx,'.',userBidx,
                #                   ' ',(auxArticlePopularity * exp(-alphaIndex*auxUserUserArticleTimeDiff)),
                #                   ' ',auxArticlePopularity, 
                #                   ' ',auxUserUserArticleTimeDiff, 
                #                   ' ',auxArticleId)
                #            ),file = 'threefour.txt',append = TRUE)
              }
              jaccard[userAidx,userBidx] = 1-jaccard[userAidx,userBidx]/sum(union)
            }
          }
        }
      }
      jaccard
    }
    result = parallelResults[[3]]
  }
  return(result)
  
}
jaccardDistance <- function(usersArticlesMatrix,
                            usersArticlesTimeDiffMatrix,
                            articlesPopularityIndexVector,
                            articlesUserUserTimeDiffTable,
                            alphaIndex){
  
  usersArticlesAtenCoeffMatrix = t(mapply(FUN = getAtenCoeff, as.tibble(t(as.matrix(usersArticlesTimeDiffMatrix))), mi = articlesPopularityIndexVector$popularity,alpha = alphaIndex))
  emails = colnames(usersArticlesMatrix)
  size = ncol(usersArticlesMatrix)  
  
  noCores = detectCores(all.tests = FALSE, logical = TRUE) - 1
  jaccard = matrix(data = 0,nrow = size, ncol = size,dimnames = list(emails,emails))
  indexes = getIndexes(size,noCores)
  
  cl<-makeCluster(noCores,type="FORK",outfile = 'jaccardDistanceVectorIdxParallelPreExp.txt')
  registerDoParallel(cl)
  
  parallelResults = foreach(index = indexes) %dopar% {
    indexesRow = index[,1]
    indexesCol = index[,2]
    for (k in 1:length(indexesRow)){
      userAidx = indexesRow[k]
      userBidx = indexesCol[k]
      intersection    = ((usersArticlesMatrix[,userAidx]!=0)&(usersArticlesMatrix[,userBidx]!=0))
      if(sum(intersection) > 0) {
        union         = ((usersArticlesMatrix[,userAidx]!=0)|(usersArticlesMatrix[,userBidx]!=0))
        auxUserAEmail = emails[userAidx]
        auxUserBEmail = emails[userBidx]
        
        articleIds = rownames(usersArticlesMatrix[intersection,])
        
        aux = which(articlesUserUserTimeDiffTable[,'userA'] == auxUserAEmail)
        aux = aux[which(articlesUserUserTimeDiffTable[aux,'userB'] == auxUserBEmail)]
        userUserArticlesTimeDiffVector_Vioj = articlesUserUserTimeDiffTable[aux,'diff']
        
        userAArticlesTimeExpVector_Wik = rep(0,sum(intersection))
        userAArticlesTimeExpVector_Wik[userUserArticlesTimeDiffVector_Vioj > 0] =   usersArticlesAtenCoeffMatrix[intersection,userAidx][userUserArticlesTimeDiffVector_Vioj > 0]
        userAArticlesTimeExpVector_Wik[userUserArticlesTimeDiffVector_Vioj < 0] = 1/usersArticlesAtenCoeffMatrix[intersection,userAidx][userUserArticlesTimeDiffVector_Vioj < 0]
        
        userBArticlesTimeExpVector_Wjk = rep(0,sum(intersection))
        userBArticlesTimeExpVector_Wjk[userUserArticlesTimeDiffVector_Vioj > 0] = 1/usersArticlesAtenCoeffMatrix [intersection,userBidx][userUserArticlesTimeDiffVector_Vioj > 0]
        userBArticlesTimeExpVector_Wjk[userUserArticlesTimeDiffVector_Vioj < 0] =   usersArticlesAtenCoeffMatrix [intersection,userBidx][userUserArticlesTimeDiffVector_Vioj < 0]
        jaccard[userAidx,userBidx] = 1-sum(userAArticlesTimeExpVector_Wik*userBArticlesTimeExpVector_Wjk*articlesPopularityIndexVector[intersection,]$popularityIndex)/sum(union)
      }
    }
    jaccard
  }
  result = Reduce('+',parallelResults)
  result2 = matrix(data = rep(0,size**2),nrow = size)
  result2[lower.tri(result2)] = 1
  result = result2 - result
  
  stopCluster(cl = cl)
  return(result)
}


# Extracts the users x articles matrix from the database
getUsersArticlesMatrix = function(con){
  # Extracts users versus articles table, with count of occurences  
  usersArticles = dbGetQuery(con, 'select e.article_id as "articleId",u.email, count(1)
                             from users u 
                             inner join events e on u.email = e.email
                             group by u.email,e.article_id
                             order by u.email')
  
  # Converts table into matrix
  usersArticlesMatrix = spread(usersArticles,key = email,value = count, fill = 0)
  # Sets rownames on the DF and removes id column
  rownames(usersArticlesMatrix) = usersArticlesMatrix[,1]
  usersArticlesMatrix$articleId = NULL
  
  return (usersArticlesMatrix)
}

# Creates the user x user distance matrix based on cosine similarity over content info
cosineDistance = function(usersTagsMatrix){
  # Calculates cosine similarity between users based on tags (content similarity)
  usersUsersContentSimilarityMatrix = cosine(t(usersTagsMatrix))
  
  # Calculates cosine distance (1-cosine similarity)
  # usersUsersContentDissimilarityMatrix = apply(usersUsersContentSimilarityMatrix,FUN = function(x){return (1-x)},MARGIN = c(1,2))
  aux = matrix(data = rep(1,nrow(usersTagsMatrix)**2),nrow = nrow(usersTagsMatrix))
  aux[upper.tri(aux,diag = TRUE)] = 0
  usersUsersContentDissimilarityMatrix = aux - (usersUsersContentSimilarityMatrix * aux)
  
  return(usersUsersContentDissimilarityMatrix)
}


jaccardDistance <- function(usersArticlesMatrix,
                            usersArticlesTimeDiffMatrix,
                            articlesPopularityIndexVector,
                            articlesUserUserTimeDiffTable,
                            alphaIndex){
  
  usersArticlesAtenCoeffMatrix = t(mapply(FUN = getAtenCoeff, as.tibble(t(as.matrix(usersArticlesTimeDiffMatrix))),alpha = alphaIndex))
  emails = colnames(usersArticlesMatrix)
  size = ncol(usersArticlesMatrix)  
  
  noCores = detectCores(all.tests = FALSE, logical = TRUE) - 1
  jaccard = matrix(data = 0,nrow = size, ncol = size,dimnames = list(emails,emails))
  indexes = getIndexes(size,noCores)
  
  cl<-makeCluster(noCores,type="FORK",outfile = 'jaccardDistance.txt')
  registerDoParallel(cl)
  
  parallelResults = foreach(index = indexes) %dopar% {
    jaccard = getJaccardDistances(jaccard = jaccard,
                                  rowIndexes = index[,1],
                                  colIndexes = index[,2],
                                  usersArticlesMatrix = usersArticlesMatrix,
                                  emails = emails,
                                  articlesUserUserTimeDiffTable = articlesUserUserTimeDiffTable,
                                  usersArticlesAtenCoeffMatrix = usersArticlesAtenCoeffMatrix,
                                  articlesPopularityIndexVector = articlesPopularityIndexVector)
  }
  result = Reduce('+',parallelResults)
  result[result==0] = 1
  
  stopCluster(cl = cl)
  return(result)
}


getJaccardDistances = function(jaccard,rowIndexes,colIndexes,usersArticlesMatrix,emails,articlesUserUserTimeDiffTable,usersArticlesAtenCoeffMatrix,articlesPopularityIndexVector){
  for (k in 1:length(colIndexes)){
    userAidx = rowIndexes[k]
    userBidx = colIndexes[k]
    intersection    = ((usersArticlesMatrix[,userAidx]!=0)&(usersArticlesMatrix[,userBidx]!=0))
    if(sum(intersection) > 0) {
      union         = ((usersArticlesMatrix[,userAidx]!=0)|(usersArticlesMatrix[,userBidx]!=0))
      auxUserAEmail = emails[userAidx]
      auxUserBEmail = emails[userBidx]
      
      aux = which(articlesUserUserTimeDiffTable[,'userA'] == auxUserAEmail)
      aux = aux[which(articlesUserUserTimeDiffTable[aux,'userB'] == auxUserBEmail)]
      userUserArticlesTimeDiffVector_Vioj = articlesUserUserTimeDiffTable[aux,'diff']
      
      userAArticlesTimeExpVector_Wik = rep(0,sum(intersection))
      userAArticlesTimeExpVector_Wik[userUserArticlesTimeDiffVector_Vioj > 0] =   usersArticlesAtenCoeffMatrix[intersection,userAidx][userUserArticlesTimeDiffVector_Vioj > 0]
      userAArticlesTimeExpVector_Wik[userUserArticlesTimeDiffVector_Vioj < 0] = 1/usersArticlesAtenCoeffMatrix[intersection,userAidx][userUserArticlesTimeDiffVector_Vioj < 0]
      
      userBArticlesTimeExpVector_Wjk = rep(0,sum(intersection))
      userBArticlesTimeExpVector_Wjk[userUserArticlesTimeDiffVector_Vioj > 0] = 1/usersArticlesAtenCoeffMatrix [intersection,userBidx][userUserArticlesTimeDiffVector_Vioj > 0]
      userBArticlesTimeExpVector_Wjk[userUserArticlesTimeDiffVector_Vioj < 0] =   usersArticlesAtenCoeffMatrix [intersection,userBidx][userUserArticlesTimeDiffVector_Vioj < 0]
      jaccard[userAidx,userBidx] = 1-sum(userAArticlesTimeExpVector_Wik*userBArticlesTimeExpVector_Wjk*articlesPopularityIndexVector[intersection,]$popularityIndex)/sum(union)
    }
  }
  return(jaccard)
}


main = function(){
  # Defines forgetting curve intensity (agging for articles time access)
  forgCurveLambdaIndex          = 0.05
  userUserTimeDiffAlphaIndex    = 0.05
  contentBehaviourPropBetaIndex = 0.5 # 0 = full content, 1 = full behaviour
  noClustersK                   = 7
  tagsBelongClusterCutGamaIndex = 0.9   # Used only when toNArticles is not null
  topNArticles                  = 20  # If null, then gamaIndex is used to select tags
  
  normByCol                     = TRUE
  
  if (!exists('calcDistances')) {
    i = 0
    results = iniResults()
    clearConnections()
    calcDistances = TRUE 
    lastForgCurveLambdaIndex          = -1
    lastUserUserTimeDiffAlphaIndex    = -1
    lastContentBehaviourPropBetaIndex = -1
    # trainningConnection = getDataBaseConnection(schema = 'trainning20'  ,dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
    # testingConnection   = getDataBaseConnection(schema = 'testing20'    ,dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
    trainningConnection = getDataBaseConnection(schema = 'trainning200' ,dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
    testingConnection   = getDataBaseConnection(schema = 'testing200'   ,dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
    # trainningConnection = getDataBaseConnection(schema = 'trainning2000',dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
    # testingConnection   = getDataBaseConnection(schema = 'testing2000'  ,dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
    # trainningConnection = getDataBaseConnection(schema = 'trainning'    ,dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
    # testingConnection   = getDataBaseConnection(schema = 'testing'      ,dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
    # fullDataBase        = getDataBaseConnection(schema = 'public'       ,dbUser = "cl-us-gzh",dbHost = "10.238.4.109",dbName = "cl-us-gzh",dbPass = "cl-us-gzh")
    
  } else {
    calcDistances = FALSE
  } 
  
  if (lastForgCurveLambdaIndex != forgCurveLambdaIndex | lastUserUserTimeDiffAlphaIndex != userUserTimeDiffAlphaIndex) {
    distances                      = getDistances(con = trainningConnection, lambdaIndex = forgCurveLambdaIndex, alphaIndex = userUserTimeDiffAlphaIndex, betaIndex = contentBehaviourPropBetaIndex)
    lastForgCurveLambdaIndex       = forgCurveLambdaIndex
    lastUserUserTimeDiffAlphaIndex = userUserTimeDiffAlphaIndex
    
    plotAttenuationCurve(distances$parameters$lambdaIndex , 100 ,titleText = 'Forgetting Curve (time attenuation factor over content)')
    plotAttenuationCurve(distances$parameters$alphaIndex  , 100 ,titleText = 'User x User Time Difference Curve (time attenuation factor over behaviour)')
    plotPopularityIndex( distances$data$articlesPopularityIndexVector,titleText = 'Popularity Index (popularity penalty factor over behaviour)')
    
    # prePlot(distances)
  } else if (lastContentBehaviourPropBetaIndex != contentBehaviourPropBetaIndex) {
    distances = setMixedDistanceIndex(distances,contentBehaviourPropBetaIndex)
    lastContentBehaviourPropBetaIndex = contentBehaviourPropBetaIndex
  }
  
  distance    = distances$mixedDistance
  # seeds       = getSeeds(emails = colnames(distances$data$usersArticlesMatrix),k = noClustersK)
  
  # Performs both Kmedoid and Ward.d clustering
  clusters    = list(cluster = getKMedoidCluster(     distance = distance, k = noClustersK)
                     ,cluster = getHierarchicalCluster(distance = distance, k = noClustersK, method = 'ward.D'))
  clustersTab = getClustersTab(clusters = clusters, contentData = distances$data$usersTagsMatrix)
  
  # Evaluates the solution with KMedoid clustering
  solution    = evaluateSolution(con         = testingConnection, 
                                 usersTagsM  = distances$data$usersTagsMatrix, 
                                 cluster     = clusters[[1]]$cluster, 
                                 alphaIndex  = userUserTimeDiffAlphaIndex,
                                 betaIndex   = contentBehaviourPropBetaIndex,
                                 lambdaIndex = forgCurveLambdaIndex,
                                 gamaIndex   = tagsBelongClusterCutGamaIndex,
                                 k           = noClustersK,
                                 n           = topNArticles,
                                 normByCol   = normByCol) 
  
  record = list(solution    = solution,
                clusters    = clusters,
                clustersTab = clustersTab,
                distances   = distances)
  
  i = i + 1
  results[[length(results) + 1]] = record
}

clustersTest = function(){
  for (clusterIndex in 1:ncol(usersClustersMTR)) {
    writeLog('usersCluster              = rownames(usersClustersMTR[usersClustersMTR[,clusterIndex] == 1,,drop = FALSE])')
    # Retrieves the list of users that belong to the current cluster
    usersCluster              = rownames(usersClustersMTR[usersClustersMTR[,clusterIndex] == 1,,drop = FALSE])
    writeLog('articlesUsersClusterSuggM = articlesUsersSuggM[,colnames(articlesUsersSuggM) %in% usersCluster, drop = FALSE]')
    # Filters the suggestion to those for the current cluster's users (where
    # article's weight is larger than zero)
    articlesUsersClusterSuggM = articlesUsersSuggM[,colnames(articlesUsersSuggM) %in% usersCluster, drop = FALSE]
    articlesUsersClusterSuggM = articlesUsersClusterSuggM[articlesUsersClusterSuggM[,1] > 0,, drop = FALSE]
    writeLog('articlesUsersTestSampleM  = getArticlesUsersSuggTestM(usersArticlesTestMTS = usersArticlesTestMTS,articlesUsersSuggM = articlesUsersClusterSuggM)')
    # Creates the test sample (recommendation + not recommendation) for
    # the articles suggested to the current cluster
    articlesUsersTestSampleM  = getArticlesUsersSuggTestM(usersArticlesTestMTS = usersArticlesTestMTS,articlesUsersSuggM = articlesUsersClusterSuggM)
    articlesSample            = rownames(articlesUsersTestSampleM)
    writeLog('usersIntersect            = getIntersect(vectorA = colnames(articlesUsersClusterSuggM), vectorB = colnames(articlesUsersTestSampleM))')
    # Restricts the users to those present in the suggestion matrix
    # and those present at the test dataset
    usersIntersect            = getIntersect(vectorA = colnames(articlesUsersClusterSuggM), vectorB = colnames(articlesUsersTestSampleM))
    writeLog('    if (length(usersIntersect) > 0 & length(articlesSample) > 0) {')
    if (length(usersIntersect) > 0 & length(articlesSample) > 0) {
      totalUsers                = totalUsers + length(usersIntersect)
      totalArticles             = totalArticles + length(articlesSample)

      articlesUsersClusterSuggM = equalize(m = articlesUsersClusterSuggM,toEqualize = articlesSample ,byRow = TRUE)
      articlesUsersClusterSuggM = equalize(m = articlesUsersClusterSuggM,toEqualize = usersIntersect ,byRow = FALSE)
      articlesUsersTestSampleM  = equalize(m = articlesUsersTestSampleM ,toEqualize = articlesSample ,byRow = TRUE)
      articlesUsersTestSampleM  = equalize(m = articlesUsersTestSampleM ,toEqualize = usersIntersect ,byRow = FALSE)

      positiveHits = positiveHits + sum((articlesUsersClusterSuggM != 0) * (articlesUsersTestSampleM != 0))
      typeOneError = typeOneError + sum((articlesUsersClusterSuggM != 0) * (articlesUsersTestSampleM == 0))
    }
  }
  
}