list.of.packages = c('mailR','tibble')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(mailR)
library(tibble)

# Write matrices to files
writeCsv = function(usersArticlesMatrix,articlesTagsMatrix,usersArticlesForgCurvedMatrix,usersTagsMatrix,usersUsersContentDissimilarityMatrix){
  write.csv2(x = usersArticlesMatrix,file = 'A_userArticlesMatrix.csv',row.names = TRUE)
  write.csv2(x = articlesTagsMatrix,file = 'B_articlesTagsMatrix.csv',row.names = TRUE)
  write.csv2(x = usersArticlesForgCurvedMatrix,file = 'AL_usersArticlesForgCurvedMatrix.csv',row.names = TRUE)
  write.csv2(x = usersTagsMatrix,file = 'ALB_usersTagsMatrix.csv',row.names = TRUE)
  write.csv2(x = usersUsersContentDissimilarityMatrix,file = 'CD_usersUsersContentDissimilarityMatrix.csv',row.names = TRUE)
}
is.compile <- function(func)
{
  # this function lets us know if a function has been byte-coded or not
  #If you have a better idea for how to do this - please let me know...
  if (class(eval(parse(text = func))) != "function") stop("You need to enter a function")
  last_2_lines <- tail(capture.output(eval(parse(text = func))),2)
  any(grepl("bytecode:", last_2_lines)) # returns TRUE if it finds the text "bytecode:" in any of the last two lines of the function's print
}
cmpfunDouglas = function(func){
  return(cmpfun(eval(parse(text = func))))
}

iniResults = function(){
  results = list(a = list(distances = NA,seeds = NA,clusters = NA,solutions = list()))
  return(results)
}

# Applies normalization over a matrix, either by col or by row
getNormMatrix = function(m,byCol = FALSE){
  if (byCol) {
    return(apply(X = m,MARGIN = 2,FUN = function(x){
      mx = max(x)
      if (mx == 0)
        mx = 1
      return(x/mx)} ))
  }
  else{
    return(m/max(m))  
  }
}

# Returns just the elements of m that are in the top n values, preserving m's order
getTopNMatrix = function(m,n){
  if (n > nrow(m)) {
    return(m)
  }
  else{
    n = round(n)
    return(apply(X = m,MARGIN = 2,FUN = function(x,n){
      ord = order(x,decreasing = TRUE)
      x[ord[(n + 1):length(x)]] = 0
      return(x)
    },n = n))
  }
}

# Tests if both matrices have compatible dimensions
testForMatrixMult = function(m1,m2){
  if (colnames(m1) != rownames(m2))
    stop(call. = TRUE)
}

# Multiplies two matrices
matrixMult = function(m1, m2){
  m1 = m1[ , order(colnames(m1))]
  m2 = m2[order(rownames(m2)), ]
  
  testForMatrixMult(m1 = m1, m2 = m2)
  
  return(m1 %*% m2)
}

# Returns a new vector containing the intersection between vector A and vector B ordered
getIntersect = function(vectorA,vectorB){
  vectorIntersect = intersect(vectorA,vectorB)
  vectorIntersect = vectorIntersect[order(vectorIntersect,decreasing = TRUE)]
  return(vectorIntersect)
}

# Filters matrix m returning just the rows that belong to "toEqualize"
equalize = function(m,toEqualize,byRow = TRUE){
  if (!byRow)
    m = t(m)
  m = m[which(rownames(m) %in% toEqualize),,drop = FALSE]
  notIn = toEqualize[which(!(toEqualize %in% rownames(m)))]
  if (length(notIn) > 0) {
    toAppend = matrix(data = 0,nrow = length(notIn),ncol = ncol(m),dimnames = list(notIn,colnames(m)))
    m = rbind(m,toAppend)
  }
  m = m[order(rownames(m),decreasing = TRUE),,drop = FALSE] 
  if (!byRow)
    m = t(m)
  return(m)
}

# Prints the value right aligned with precision set when it's a number, centered otherwise

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
    strValue = as.character(value)
    if (nchar(strValue) > size)
      strValue = substr(x = strValue,start = 1,stop = size)
    else if (nchar(strValue) < size) {
      f <- pmax((size - nchar(strValue)) / 2, 0)
      strValue = sprintf("%-*s%s%*s", f, "", strValue, ceiling(f), "")
    }
  }
  return(strValue)
}

printHeader = function(enabled = TRUE){
  if (enabled) {
    cat(file = "console.txt",append = TRUE,paste0('| CLS.MTH | TAG.MTH | TAG.IDX | ART.MTH | ART.IDX | UxU.IDX | FGC.IDX | MXD.IDX | Distan. | Cluster | NBR.CLS | Solut.  | Result  | TotTime |','\n'))
  }
}

printValue = function(value,parameter,enabled = TRUE){
  if (enabled) {
    if      (parameter == 'clusterMethod'          ) cat(file = "console.txt",append = TRUE,paste0('| ', printNice(value = value, size = 7), ' | '))
    else if (parameter == 'tagsMethod'             ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'tagsCutGamaIndex'       ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'articlesMethod'         ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'articlesCutZetaIndex'   ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'usersTimeDiffAlphaIndex') cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'forgCurveLambdaIndex'   ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'mixedDistanceBetaIndex' ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'noClustersK'            ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'distances'              ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'clusters'               ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'solution'               ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7), ' | '))
    else if (parameter == 'result'                 ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7, precision = 3), ' | '))
    else if (parameter == 'totalTime'              ) cat(file = "console.txt",append = TRUE, paste0(      printNice(value = value, size = 7, precision = 1), ' | ','\n'))
  }
}

printCache = function(){
  printValue(parameter = 'clusterMethod'          , value = clusterMethod)
  printValue(parameter = 'tagsMethod'             , value = tagsMethod)
  printValue(parameter = 'tagsCutGamaIndex'       , value = tagsCutGamaIndex)
  printValue(parameter = 'articlesMethod'         , value = articlesMethod)
  printValue(parameter = 'articlesCutZetaIndex'   , value = articlesCutZetaIndex)
  printValue(parameter = 'usersTimeDiffAlphaIndex', value = usersTimeDiffAlphaIndex)
  printValue(parameter = 'forgCurveLambdaIndex'   , value = forgCurveLambdaIndex)
  printValue(parameter = 'mixedDistanceBetaIndex' , value = mixedDistanceBetaIndex)
  
}

writeLog = function(message){
  toWrite = paste0(Sys.time(),' ',message)
  write(x = toWrite,file = 'log.txt',append = TRUE)
}

sendEmail = function(subject, body){
  result = tryCatch({
    currentSpace = as.numeric(system(command = "df /home  | awk '{ print $4 }'", intern = TRUE)[2])/1024/1024
    spaceMessage = paste0('Current space is: ', printNice(currentSpace,6,2), ' GB\n')
    
    r = send.mail(from    = "<douglas.ulian@gruporbs.com.br>",
                  to      = "<douglas.ulian@gruporbs.com.br>",
                  subject = subject,
                  body    = paste0(spaceMessage,body),
                  smtp    = list(host.name = "192.168.50.128",
                                port = 25),
                  authenticate = FALSE,
                  send = TRUE)
  }, warning = function(w) {
    whiteLog(w)
    2
  }, error = function(e) {
    whiteLog(e)
    1
  }, finally = {
    0
  })
  return(result)
}

printCache = function(x, y, clusterMethod, tagsMethod, articlesMethod){
  for (i in 1:nrow(x)) {
    printValue(parameter = 'clusterMethod'          , value = clusterMethod)
    printValue(parameter = 'tagsMethod'             , value = tagsMethod)
    printValue(parameter = 'tagsCutGamaIndex'       , value = x[i,1])
    printValue(parameter = 'articlesMethod'         , value = articlesMethod)
    printValue(parameter = 'articlesCutZetaIndex'   , value = x[i,2])
    printValue(parameter = 'usersTimeDiffAlphaIndex', value = x[i,4])
    printValue(parameter = 'forgCurveLambdaIndex'   , value = x[i,3])
    printValue(parameter = 'mixedDistanceBetaIndex' , value = x[i,5])
    printValue(parameter = 'distances'              , value = 'CACHE'   )
    printValue(parameter = 'clusters'               , value = 'CACHE'   )
    printValue(parameter = 'noClustersK'            , value = x[i,6])
    printValue(parameter = 'solution'               , value = 'CACHE'   )
    printValue(parameter = 'result'                 , value = -y[i])
    printValue(parameter = 'totalTime'              , value = 0)
  }
}

enoughSpace = function(){
  if (file.exists('controls//compact.txt'))
    return(FALSE)
  # space = system(command = "df /home  | awk '{ print $4 }'", intern = TRUE)[2]
  if ((as.numeric(system(command = "df /home  | awk '{ print $4 }'", intern = TRUE)[2])/1024/1024) < 30)
    return(FALSE)
  else
    return(TRUE)
}

getNoCores = function(){
  if (file.exists('controls//no_cores.txt')) {
    noCores = as.numeric(readLines('controls//no_cores.txt')[1])
  }
  else{
    noCores = detectCores(all.tests = FALSE, logical = TRUE) - 2
  }
  return(noCores)
}

getAttenCoeff = function(alpha, ti){
  # return(exp(-alpha*ti)/sqrt(log1p(mi)))
  return(exp(-alpha*ti))
}
