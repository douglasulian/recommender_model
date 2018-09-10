##### Libraries ####
list.of.packages = c('foreach','doParallel','parallel','lsa','qlcMatrix','Matrix')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

# Loads parallel computation libraries
library(foreach)
library(doParallel)
library(parallel)
library(lsa)
library(qlcMatrix)
library(Matrix)
library(Rcpp)

##### Code ####
debugSource("scripts/util.R")
sourceCpp("scripts/jaccardDistanceC.cpp")

getDistances = function(trainningData,lambdaIndex, alphaIndex, betaIndex){
  parameters = list(alphaIndex = alphaIndex,
                    betaIndex = betaIndex,
                    lambdaIndex = lambdaIndex)
  
  n = ncol(trainningData$usersArticlesMatrix)
  auxMatrix = matrix(data = rep(1,n*n),nrow = n)
  auxMatrix[upper.tri(x = auxMatrix,diag = TRUE)] = 0
  writeLog('behaviourDistanceTime = system.time({')
  behaviourDistanceTime = system.time({
    
    # calculates behaviour distance
    behaviourDistance = getJaccardDistance(usersArticlesMatrix           = trainningData$usersArticlesMatrix,
                                           usersArticlesTimeDiffMatrix   = trainningData$usersArticlesTimeDiffMatrix,
                                           articlesPopularityIndexVector = trainningData$articlesPopularityIndexVector,
                                           alphaIndex = alphaIndex)
    behaviourDistance = behaviourDistance * auxMatrix
    rm(list = c('auxMatrix'))
  })
  writeLog('contentDistanceTime = system.time({')
  gc()
  
  contentDistanceTime = system.time({

    # Applies forgetting curve over content (tags)
    usersTagsMatrix = getUsersTagsMatrix(usersArticlesMatrix          = trainningData$usersArticlesMatrix,
                                         articlesTagsMatrix           = trainningData$articlesTagsMatrix,
                                         usersArticlesTimeDiffMatrix  = trainningData$usersArticlesTimeDiffMatrix,
                                         lambdaIndex                  = lambdaIndex)
    # Calculates content distance
    contentDistance = getCosineDistance(usersTagsMatrix = usersTagsMatrix)
  })
  writeLog('mixedDistanceTime = system.time({')
  gc()
  
  mixedDistanceTime = system.time({

    mixedDistance = betaIndex*behaviourDistance + contentDistance * (1 - betaIndex)
  })
  writeLog('return(list(contentDistance = contentDistance')
  gc()
  
  return(list(contentDistance       = contentDistance, 
              behaviourDistance     = behaviourDistance,
              mixedDistance         = mixedDistance,
              contentDistanceTime   = contentDistanceTime, 
              behaviourDistanceTime = behaviourDistanceTime,
              mixedDistanceTime     = mixedDistanceTime,
              parameters            = parameters, 
              usersTagsMatrix       = usersTagsMatrix))
}

getJaccardDistance <- function(usersArticlesMatrix,
                               usersArticlesTimeDiffMatrix,
                               articlesPopularityIndexVector,
                               alphaIndex){
  
  usersArticlesAttenCoeffMatrix = t(mapply(FUN = getAttenCoeff, as.tibble(t(as.matrix(usersArticlesTimeDiffMatrix))),alpha = alphaIndex))
  emails = colnames(usersArticlesMatrix)
  size   = ncol(usersArticlesMatrix)  
  
  # noCores = detectCores(all.tests = FALSE, logical = TRUE) - 2
  noCores = getNoCores()
  
  jaccard = matrix(data = 0,nrow = size, ncol = size,dimnames = list(emails,emails))
  indexes = getIndexes(size,noCores)
  
  doParallel::registerDoParallel(cores = noCores,outfile = 'jaccardDistanceForkOutput.txt')
  
  parallelResults = foreach(index = indexes) %dopar% {
    
    jaccard = getJaccardDistancesC(jaccard = jaccard,
                                   rowIndexes = as.integer(index[,1]),
                                   colIndexes = as.integer(index[,2]),
                                   usersArticlesMatrix = as.matrix(usersArticlesMatrix),
                                   usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix,
                                   articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
  
  }
  
  result = Reduce('+',parallelResults)
  result[result == 0] = 1
  doParallel::stopImplicitCluster()
  rm(list = c('parallelResults','jaccard','usersArticlesAttenCoeffMatrix'))
  gc()
  return(result)
}

# Calculates cosine distance using sparse matrices
getCosineDistance = function(usersTagsMatrix){
  
  sparseUsersTagsMatrix = Matrix(data = t(usersTagsMatrix))
  
  # Calculates cosine similarity between users based on tags (content similarity)
  sparseDist = cosSparse(sparseUsersTagsMatrix)
  
  usersUsersContentSimilarityMatrix = as.matrix(sparseDist)
  rm(list = c('sparseUsersTagsMatrix','sparseDist'))
  usersUsersContentSimilarityMatrix[is.infinite(usersUsersContentSimilarityMatrix)] = 0
  # Calculates cosine distance (1-cosine similarity)
  aux = matrix(data = rep(1,nrow(usersTagsMatrix)**2),nrow = nrow(usersTagsMatrix))
  aux[upper.tri(aux,diag = TRUE)] = 0
  usersUsersContentDissimilarityMatrix = aux - (usersUsersContentSimilarityMatrix * aux)
  rm(list = c('aux'))
  gc()
  return(usersUsersContentDissimilarityMatrix)
}

# Creates the users x tags matrix with forgetting curve attenuation
getUsersTagsMatrix = function(usersArticlesMatrix,articlesTagsMatrix,usersArticlesTimeDiffMatrix,lambdaIndex){
  usersArticlesTimeDiffMatrix = usersArticlesTimeDiffMatrix/365
  # Applies forgetting curve attenuation
  usersArticlesForgCurvedMatrix = usersArticlesMatrix*sapply(usersArticlesTimeDiffMatrix,getForgettingIndex,lambdaIndex = lambdaIndex)
  
  usersArticlesForgCurvedMatrix = t(usersArticlesForgCurvedMatrix)
  # Multiplies User x Articles Matrix by Articles x Tags Matrix, producing User x Tags Matrix
  # usersTagsMatrix = as.matrix(usersArticlesForgCurvedMatrix) %*% as.matrix(articlesTagsMatrix)
  usersTagsMatrix = matrixMult(m1 = as.matrix(usersArticlesForgCurvedMatrix), m2 = as.matrix(articlesTagsMatrix))
  rm(list = c('usersArticlesTimeDiffMatrix','usersArticlesForgCurvedMatrix'))
  return(usersTagsMatrix)
}

# Function responsible for time attenuation over article access time
getForgettingIndex = function(timeDifference,lambdaIndex){
  return(exp(-lambdaIndex*timeDifference))
}

# Set 
setMixedDistanceIndex = function(distances, newBetaIndex){
  mixedDistanceTime = system.time({
    mixedDistance = newBetaIndex*distances$behaviourDistance + distances$contentDistance * (1 - newBetaIndex)
  })
  distances$mixedDistance = mixedDistance
  distances$mixedDistanceTime = mixedDistanceTime
  distances$betaIndex = newBetaIndex
  return(distances)
}

# Generates a list of indexes for a lower triangle of a simetric matrix, withoug its diagonal
# Splits the indexes list ~equaly over a list of size 'ncores'
getIndexes = function(sampleSize,ncores){
  ncores = ncores*4
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

# Retrieves seeds pre-defined if available or else random.
getSeeds = function(emails,k){
  seeds = list(
    c("criscrisvital2@hotmail.com","felipe_.94@hotmail.com","gustavo.bertotti@gmail.com","tiagopatias@msn.com","zipssoutinho@gmail.com")  
    ,c('anair.rech@gmail.com','mariomoreiracolorado@gmail.com','canqueri@terra.com.br','jusouza03@hotmail.com','carloshumberto13@gmail.com')
    ,c('1000tom.magno@gmail.com','deyvidcolvero88@gmail.com','aporriadodo38@terra.com.br','drrandon@terra.com.br','vivianesilvabrito@gmail.com')
  )
  selectedSeeds = list()
  for (i in 1:length(seeds)) {  
    if (length(which(seeds[[i]] %in% emails)) > 0)
      selectedSeeds = seeds[[i]][which(seeds[[i]] %in% emails)]
  }
  if (length(selectedSeeds) == 0) {
    return(sample(x = emails[which(selectedSeeds != emails)],size = k,replace = FALSE))
  }
  else{
    return(c(unlist(selectedSeeds),sample(x = emails[which(selectedSeeds != emails)],size = k,replace = FALSE))[1:k])
  }
}

# Calculates attenuation index for forgetting curve
getAttenCoeff = function(alpha, ti){
  # return(exp(-alpha*ti)/sqrt(log1p(mi)))
  return(exp(-alpha*ti))
}
