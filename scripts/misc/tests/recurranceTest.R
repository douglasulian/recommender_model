test3 = function(trainningConnection,
                 testingConnection,
                 userUserTimeDiffAlphaIndex,
                 contentBehaviourPropBetaIndex,
                 forgCurveLambdaIndex,
                 noClustersK,
                 clusterMethod,
                 tagsBelongClusterCutGamaIndex,
                 topNArticles,
                 normByCol,
                 lastResults,
                 lastDistances
){
  
  recalcPrimaryDistances = FALSE
  recalcMixedDistance    = FALSE
  recalcClusters         = FALSE
  
  if (length(lastResults) == 0){
    recalcPrimaryDistances = TRUE
    recalcMixedDistance    = TRUE
    recalcClusters         = TRUE
  } 
  else {
    lastSolutionParameters  = lastResults[[length(lastResults)]]$solution$parameters
    lastDistancesParameters = lastDistances$parameters
    lastClustersParameters  = lastResults[[length(lastResults)]]$cluster$parameters
    
    if (lastDistancesParameters$lambdaIndex != forgCurveLambdaIndex |
        lastDistancesParameters$alphaIndex  != userUserTimeDiffAlphaIndex) {
      recalcPrimaryDistances = TRUE
      recalcClusters         = TRUE
    }
    
    if (lastDistancesParameters$betaIndex != contentBehaviourPropBetaIndex){
      recalcMixedDistance    = TRUE
      recalcClusters         = TRUE
    }
    
    if (lastClustersParameters$method != clusterMethod |
        lastClustersParameters$k      != noClustersK ){
      recalcClusters         = TRUE
    }
    
  }
  if (recalcPrimaryDistances){
    print('recalc primary distances')    
    distances = list(parameters = list(lambdaIndex = forgCurveLambdaIndex, alphaIndex = userUserTimeDiffAlphaIndex, betaIndex = contentBehaviourPropBetaIndex))
    
  } else if (recalcMixedDistance) {
    print('recalc mixed distance')
    distances = list(parameters = list(lambdaIndex = forgCurveLambdaIndex, alphaIndex = userUserTimeDiffAlphaIndex, betaIndex = contentBehaviourPropBetaIndex))
  }
  else
  {
    distances = lastDistances
  }
  if (recalcClusters){
    print('recalc recalc cluster')
    cluster = list(parameters = list(k = noClustersK, method = clusterMethod))
  }
  else
  {
    cluster = lastResults[[length(lastResults)]]$cluster
  }
  
  # Evaluates the solution with KMedoid clustering
  solution    = list(parameters = list(
    alphaIndex  = userUserTimeDiffAlphaIndex,
    betaIndex   = contentBehaviourPropBetaIndex,
    lambdaIndex = forgCurveLambdaIndex,
    gamaIndex   = tagsBelongClusterCutGamaIndex,
    k           = noClustersK,
    n           = topNArticles,
    normByCol   = normByCol) )
  
  record = list(cluster    = cluster,
                solution    = solution)
  
  lastResults[[length(lastResults) + 1]] = record
  
  return(list(lastResults = lastResults,
              distances = distances))
  
}
testResults3 = list()

testResults3 = test2(trainningConnection = NULL,
                     testingConnection = NULL,
                     userUserTimeDiffAlphaIndex = 2,
                     contentBehaviourPropBetaIndex = 2,
                     forgCurveLambdaIndex = 2,
                     noClustersK = 2,
                     clusterMethod = '1  ',
                     tagsBelongClusterCutGamaIndex = 1,
                     topNArticles = 2,
                     normByCol = 1,
                     lastResults = testResults3$lastResults,
                     lastDistances = testResults3$distances)
