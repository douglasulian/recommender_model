


TAG.MTH = 'index'
TAG.IDX = runif(n = 1,min = 0,max = 1)
ART.MTH = 'index'
ART.IDX = runif(n = 1,min = 0,max = 1)
UxU.IDX = runif(n = 1,min = 0,max = 1)
FGC.IDX = runif(n = 1,min = 0,max = 1)
MXD.IDX = runif(n = 1,min = 0,max = 1)
NBR.CLS = round(runif(n = 1,min = 0,max = 10000))
Dist    = 'NEW'
Clus    = 'CACHE'
Sol     = 'NEW'
Res     = runif(n = 1,min = 0,max = 1)

teste()


teste = function(){
  printHeader()
  for (i in 1:10) {
    printValue(parameter = 'clusterMethod'          , value = 'ward.D')
    printValue(parameter = 'tagsMethod'             , value = TAG.MTH)
    printValue(parameter = 'tagsCutGamaIndex'       , value = TAG.IDX)
    printValue(parameter = 'articlesMethod'         , value = ART.MTH)
    printValue(parameter = 'articlesCutZetaIndex'   , value = ART.IDX)
    printValue(parameter = 'usersTimeDiffAlphaIndex', value = UxU.IDX)
    printValue(parameter = 'forgCurveLambdaIndex'   , value = FGC.IDX)
    printValue(parameter = 'mixedDistanceBetaIndex' , value = MXD.IDX)
    printValue(parameter = 'noClustersK'            , value = NBR.CLS)
    printValue(parameter = 'distances'              , value = Dist   )
    printValue(parameter = 'clusters'               , value = Clus   )
    printValue(parameter = 'solution'               , value = Sol    )
    printValue(parameter = 'result'                 , value = Res    )
    
  }
}

printNice = function(value,size,precision = 0){
  if (is.numeric(value)) {
    value = round(x = value, digits = precision)
  
    strValue = as.character(value)
    if (precision > 0 & regexpr(pattern = '\\.',text = strValue)[1] < precision) {
      if (regexpr(pattern = '\\.',text = strValue)[1] == -1) dot = '.'
      else dot = ''
      zeros = c(rep('0',precision))
      strValue = paste(c(strValue,dot,zeros),collapse = '')  
      strValue = substr(x = strValue, start = 1,stop = regexpr(pattern = '\\.',text = strValue)[1] + precision)
    }
  
    spaces = c(rep(' ',size))
    strValue = paste(c(spaces,strValue),collapse = '')
    strValue = substr(x = strValue, start = nchar(strValue) - size + 1,stop = nchar(strValue))
  }
  else{
    if (nchar(value) > size)
      strValue = substr(x = value,start = 1,stop = size)
    else if (nchar(value) < size) {
      f <- pmax((size - nchar(value)) / 2, 0)
      strValue = sprintf("%-*s%s%*s", f, "", value, ceiling(f), "")
    }
  }
  return(strValue)
}

printHeader = function(enabled = TRUE){
  if (enabled) {
    cat(paste0('| CLS.MTH | TAG.MTH | TAG.IDX | ART.MTH | ART.IDX | UxU.IDX | FGC.IDX | MXD.IDX | NBR.CLS | Distan. | Cluster | Solut.  | Result  |','\n'))
  }
}

printValue = function(value,parameter,enabled = TRUE){
  if (enabled) {
    if      (parameter == 'clusterMethod'          ) cat(paste0('| ', printNice(value = value, size = 7), ' | '))
    else if (parameter == 'tagsMethod'             ) cat(paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'tagsCutGamaIndex'       ) cat(paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'articlesMethod'         ) cat(paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'articlesCutZetaIndex'   ) cat(paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'usersTimeDiffAlphaIndex') cat(paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'forgCurveLambdaIndex'   ) cat(paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'mixedDistanceBetaIndex' ) cat(paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'noClustersK'            ) cat(paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'distances'              ) cat(paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'clusters'               ) cat(paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'solution'               ) cat(paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'result'                 ) cat(paste0(      printNice(value = value, size = 7, precision = 3), ' | ','\n'))
  }
}
# 1234567890
#      0.219

