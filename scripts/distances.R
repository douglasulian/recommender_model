##### Libraries ####
list.of.packages = c('foreach','doParallel','parallel','lsa','qlcMatrix','Matrix','Rcpp','spam','spam64','RcppArmadillo','RcppEigen')
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
library(RcppArmadillo)
library(RcppEigen)
library(inline)
library(spam)
library(spam64)

##### Code ####
#debugSource("scripts/util.R")
sourceCpp("scripts/jaccardDistanceC.cpp")
sourceCpp("scripts/jaccardDistanceCSparse.cpp")

sparseProdCpp = paste0('using Eigen::MappedSparseMatrix;',
                       'using Eigen::SparseMatrix;',
                       'const MappedSparseMatrix<double> A(as<MappedSparseMatrix<double> >(x));',
                       'const SparseMatrix<double> At(A.adjoint());',
                       'const SparseMatrix<double> R((At * A).pruned());',
                       'return wrap(R);')

rcppEigenSparseProd = cxxfunction(signature(x = "dgCMatrix"),sparseProdCpp, "RcppEigen")

getDistances = function(trainningData,lambdaIndex, alphaIndex, betaIndex){
  parameters = list(alphaIndex = alphaIndex,
                    betaIndex = betaIndex,
                    lambdaIndex = lambdaIndex)
  
  n = ncol(trainningData$usersArticlesMatrix)
  auxMatrix = matrix(data = rep(1,n*n),nrow = n)
  auxMatrix[upper.tri(x = auxMatrix,diag = TRUE)] = 0
  writeLog(         'behaviourDistanceTime                  = system.time({')
  behaviourDistanceTime = system.time({
    
    # calculates behaviour distance
    behaviourDistance = getJaccardDistance(usersArticlesMatrix           = trainningData$usersArticlesMatrix,
                                           usersArticlesTimeDiffMatrix   = trainningData$usersArticlesTimeDiffMatrix,
                                           articlesPopularityIndexVector = trainningData$articlesPopularityIndexVector,
                                           alphaIndex = alphaIndex)
  writeLog(paste0(  'behaviourDistance                      = ',as.character(sum(behaviourDistance))))
    behaviourDistance = behaviourDistance * auxMatrix
    rm(list = c('auxMatrix'))
  })
  writeLog(paste0(  'behaviourDistance                      = ',as.character(sum(behaviourDistance))))
  writeLog(         'contentDistanceTime                    = system.time({')
  gc()
  
  contentDistanceTime = system.time({

    # Applies forgetting curve over content (tags)
    usersTagsMatrix = getUsersTagsMatrix(usersArticlesMatrix          = trainningData$usersArticlesMatrix,
                                         articlesTagsMatrix           = trainningData$articlesTagsMatrix,
                                         usersArticlesTimeDiffMatrix  = trainningData$usersArticlesTimeDiffMatrix,
                                         lambdaIndex                  = lambdaIndex)
    writeLog(paste0('sum(usersTagsMatrix)                   = ',sum(usersTagsMatrix)))
    
    # Calculates content distance
    contentDistance = getCosineDistance(usersTagsMatrix = usersTagsMatrix)
  })
  writeLog(paste0(  'contentDistance                        = ',as.character(sum(contentDistance))))
  writeLog(         'mixedDistanceTime                      = system.time({')
  gc()
  
  mixedDistanceTime = system.time({

    mixedDistance = betaIndex*behaviourDistance + contentDistance * (1 - betaIndex)
  })
  writeLog(         'return(list(contentDistance            = contentDistance')
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
getDistancesSparse = function(trainningData,lambdaIndex, alphaIndex, betaIndex){
  parameters = list(alphaIndex  = alphaIndex,
                    betaIndex   = betaIndex,
                    lambdaIndex = lambdaIndex)
  
  #aux = ncol(trainningData$usersArticlesMatrix)
  #auxMatrix = matrix(data = rep(1,aux*aux),nrow = aux)
  #auxMatrix[upper.tri(x = auxMatrix,diag = TRUE)] = 0
  writeLog(         'behaviourDistanceTime                  = system.time({')
  behaviourDistanceTime = system.time({
    
    # calculates behaviour distance
    behaviourDistance = getJaccardDistanceSparse(usersArticlesMatrix           = trainningData$usersArticlesMatrix,
                                                 usersArticlesAttenCoeffMatrix = trainningData$usersArticlesAttenCoeffMatrix,
                                                 articlesPopularityIndexVector = trainningData$articlesPopularityIndexVector,
                                                 alphaIndex = alphaIndex)
    writeLog(paste0('behaviourDistance                      = ',as.character(sum(behaviourDistance))))
    #behaviourDistance = behaviourDistance * auxMatrix
    behaviourDistance = tril(x=behaviourDistance,k = 0)
    
  })
  writeLog(paste0(  'behaviourDistance                      = ',as.character(sum(behaviourDistance))))
  writeLog(         'contentDistanceTime                    = system.time({')
  gc()
  
  contentDistanceTime = system.time({
    
    # Applies forgetting curve over content (tags)
    usersTagsMatrix = getUsersTagsMatrix(usersArticlesMatrix          = trainningData$usersArticlesMatrix,
                                         articlesTagsMatrix           = trainningData$articlesTagsMatrix,
                                         usersArticlesTimeDiffMatrix  = trainningData$usersArticlesTimeDiffMatrix,
                                         lambdaIndex                  = lambdaIndex)
    writeLog(paste0('sum(usersTagsMatrix)                   = ',sum(usersTagsMatrix)))
    # Calculates content distance
    contentDistance = getCosineDistance(usersTagsMatrix = usersTagsMatrix)
  })
  writeLog(paste0(  'contentDistance                        = ',as.character(sum(contentDistance))))
  writeLog(         'mixedDistanceTime                      = system.time({')
  gc()
  
  mixedDistanceTime = system.time({
    
    mixedDistance = betaIndex*behaviourDistance + contentDistance * (1 - betaIndex)
  })
  writeLog(         'return(list(contentDistance            = contentDistance')
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
  #writeLog(paste0('sum(usersArticlesAttenCoeffMatrix)     = ',sum(usersArticlesAttenCoeffMatrix)))
  #attenVector = as.data.frame(summary(usersArticlesTimeDiffMatrix))
  #articlesPopularityIndexVector[,3] = 1
  
  emails = colnames(usersArticlesMatrix)
  size   = ncol(usersArticlesMatrix)  
  
  # noCores = detectCores(all.tests = FALSE, logical = TRUE) - 2
  noCores = getNoCores()

  jaccard = matrix(data = 0,nrow = size, ncol = size,dimnames = list(emails,emails))
  indexes = getIndexes(size,noCores)
  
  writeLog(paste0('sum(jaccard)                           = ',sum(jaccard)))
  writeLog(paste0('sum(usersArticlesMatrix)               = ',sum(usersArticlesMatrix)))
  writeLog(paste0('sum(usersArticlesAttenCoeffMatrix)     = ',sum(usersArticlesAttenCoeffMatrix)))
  writeLog(paste0('sum(articlesPopularityIndexVector[,3]) = ',sum(articlesPopularityIndexVector[,3])))
  writeLog(paste0('rownames(usersArticlesMatrix)[2]       = ',rownames(usersArticlesMatrix)[2]))
  writeLog(paste0('rownames(usersArticlesMatrix)[nrow-2]  = ',rownames(usersArticlesMatrix)[nrow(usersArticlesMatrix)-2]))
  writeLog(paste0('colnames(usersArticlesMatrix)[2]       = ',colnames(usersArticlesMatrix)[2]))
  writeLog(paste0('colnames(usersArticlesMatrix)[nrow-2]  = ',colnames(usersArticlesMatrix)[ncol(usersArticlesMatrix)-2]))
  
  doParallel::registerDoParallel(cores = noCores)
  
  #parallelResults = foreach(index = indexes) %dopar% {
  parallelResults = foreach(index = indexes,.init = jaccard, .combine = "+") %dopar% {
    
    jaccard = getJaccardDistancesC(jaccard = jaccard,
                                   rowIndexes = as.integer(index[,1]),
                                   colIndexes = as.integer(index[,2]),
                                   usersArticlesMatrix = as.matrix(usersArticlesMatrix),
                                   usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix,
                                   articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
  
  }
  #result = Reduce('+',parallelResults)
  result = parallelResults
  writeLog(paste0('sum(result)                            = ',sum(result)))
  writeLog(paste0('ncol(result)                           = ',ncol(result)))
  writeLog(paste0('nrow(result)                           = ',nrow(result)))
  writeLog(paste0('result[1,2]                            = ',result[1,2] ))
  
  #result[result == 0] = 1
  doParallel::stopImplicitCluster()
  rm(list = c('parallelResults','jaccard','usersArticlesAttenCoeffMatrix'))
  gc()
  return(result)
}
getJaccardDistanceSparse <- function(usersArticlesMatrix,
                                     usersArticlesAttenCoeffMatrix,
                                     articlesPopularityIndexVector,
                                     alphaIndex){
  
  emails = colnames(usersArticlesMatrix)
  size   = ncol(usersArticlesMatrix)  
  
  # noCores = detectCores(all.tests = FALSE, logical = TRUE) - 2
  noCores = getNoCores()

  jaccard = sparseMatrix(x = 0L,i = size, j = size,dimnames = list(emails,emails))
  
  #usersUsersIndexesMatrix = t(usersArticlesMatrix) %*% usersArticlesMatrix
  usersUsersIndexesMatrix = crossprod.spam(usersArticlesMatrix)
  usersUsersIndexesMatrix = tril(x = usersUsersIndexesMatrix, k = -1)
  usersUsersIndexesVector = as.data.frame(summary(usersUsersIndexesMatrix))
  usersUsersIndexesVector$row = usersUsersIndexesVector$i
  usersUsersIndexesVector$col = usersUsersIndexesVector$j
  usersUsersIndexesVector$x = NULL
  usersUsersIndexesVector$i = NULL
  usersUsersIndexesVector$j = NULL
  
  indexes = getIndexesFromVector(indexesVector = usersUsersIndexesVector,ncores = noCores)
  
  writeLog(paste0('sum(jaccard)                           = ',sum(jaccard)))
  writeLog(paste0('sum(usersArticlesMatrix)               = ',sum(usersArticlesMatrix)))
  writeLog(paste0('sum(usersArticlesAttenCoeffMatrix)     = ',sum(usersArticlesAttenCoeffMatrix)))
  writeLog(paste0('sum(articlesPopularityIndexVector[,3]) = ',sum(articlesPopularityIndexVector[,3])))
  writeLog(paste0('rownames(usersArticlesMatrix)[2]       = ',rownames(usersArticlesMatrix)[2]))
  writeLog(paste0('rownames(usersArticlesMatrix)[nrow-2]  = ',rownames(usersArticlesMatrix)[nrow(usersArticlesMatrix)-2]))
  writeLog(paste0('colnames(usersArticlesMatrix)[2]       = ',colnames(usersArticlesMatrix)[2]))
  writeLog(paste0('colnames(usersArticlesMatrix)[nrow-2]  = ',colnames(usersArticlesMatrix)[ncol(usersArticlesMatrix)-2]))
  
  gc()
  
  doParallel::registerDoParallel(cores = noCores)
  
  parallelResults = foreach(index = indexes,.init = jaccard, .combine = "+") %dopar% {
    
    getJaccardDistancesCSparse(rowIndexes = as.integer(index[,1]),
                               colIndexes = as.integer(index[,2]),
                               usersArticlesMatrix = usersArticlesMatrix,
                               usersArticlesAttenCoeffMatrix = usersArticlesAttenCoeffMatrix,
                               articlesPopularityIndexVector = articlesPopularityIndexVector[,3])
    
  }
  parallelResults[tril(parallelResults == 0,k = -1)] = 1
  parallelResults[parallelResults==2] = 0
  
  result = parallelResults
  
  writeLog(paste0('sum(result)                            = ',sum(result)))
  writeLog(paste0('ncol(result)                           = ',ncol(result)))
  writeLog(paste0('nrow(result)                           = ',nrow(result)))
  writeLog(paste0('result[1,2]                            = ',result[1,2] ))
 
  doParallel::stopImplicitCluster()
  rm(list = c('parallelResults','jaccard','usersArticlesAttenCoeffMatrix','usersUsersIndexesVector'))
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
getUsersTagsSparseMatrix = function(usersArticlesMatrix,articlesTagsMatrix,usersArticlesTimeDiffMatrix,lambdaIndex){
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
getIndexesFromVector = function(indexesVector,ncores){
  len = nrow(indexesVector)
  perCore = round(len/ncores)
  from = 1
  to = 0
  indexesList = list(rep(0,ncores))
  for (i in  1:ncores) {
    to = perCore*i
    if (i == ncores) to = len
    indexesList[[i]] = indexesVector[from:to,]
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
