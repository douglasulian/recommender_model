library(microbenchmark)
library(compiler)
library(Rcpp)
sourceCpp("test.cpp")

rowIndexes = sample(x = seq(1:10000),size = 5000,replace = TRUE)
colIndexes = sample(x = seq(1:10000),size = 5000,replace = TRUE)
usersArticlesMatrix = matrix(data = sample(x = c(rep(0,10),1),size = 10000000,replace = TRUE),nrow = 1000,ncol = 10000)
fun1 = function(){
  total = 0
  for (k in 1:length(colIndexes)){
    userAidx = rowIndexes[k]
    userBidx = colIndexes[k]
    intersection    = ((usersArticlesMatrix[,userAidx]!=0)&(usersArticlesMatrix[,userBidx]!=0))
    if(sum(intersection) > 0) {
      total = total + sum((usersArticlesMatrix[,userAidx]!=0)|(usersArticlesMatrix[,userBidx]!=0))
    }
  }
  return(total)
}
fun2 = function(){
  intersectionMatrixA = usersArticlesMatrix[,rowIndexes] != 0 # indices diferentes de zero em A
  intersectionMatrixB = usersArticlesMatrix[,colIndexes] != 0 # indices diferentes de zero em B
  intersectionMatrix = intersectionMatrixA * intersectionMatrixB # matrix com combinações de indices maiores que zero, na celula, a quantidade de diferentes de zero
  teste = apply(X = intersectionMatrix > 0,MARGIN = 2,FUN = sum)
  intersectionIndexesMatrix = which(teste > 0) # indices combinados mairoes que zero da metade inferior
  total2 = 0
  for (k in 1:length(intersectionIndexesMatrix)){ # para a lista de indices
    # total2 = total2 + sum((usersArticlesMatrix[,rowIndexes[intersectionIndexesMatrix[k]]]!=0)|(usersArticlesMatrix[,colIndexes[intersectionIndexesMatrix[k]]]!=0))
    total2 = sumFunc(total = total2,usersArticlesMatrix = usersArticlesMatrix,rowIndexes = rowIndexes,colIndexes = colIndexes,k = intersectionIndexesMatrix[k])
  }
  return(total2)
}

sumFunc = function(total,usersArticlesMatrix,rowIndexes,colIndexes,k){
  return (total + sum((usersArticlesMatrix[,rowIndexes[k]]!=0)|(usersArticlesMatrix[,colIndexes[k]]!=0)))
}

fun3 = function(){
  intersectionMatrixA = usersArticlesMatrix[,rowIndexes] != 0 # indices diferentes de zero em A
  intersectionMatrixB = usersArticlesMatrix[,colIndexes] != 0 # indices diferentes de zero em B
  intersectionMatrix = intersectionMatrixA * intersectionMatrixB # matrix com combinações de indices maiores que zero, na celula, a quantidade de diferentes de zero
  teste = apply(X = intersectionMatrix > 0,MARGIN = 2,FUN = sum)
  intersectionIndexesMatrix = which(teste > 0) # indices combinados mairoes que zero da metade inferior
  total2 = 0
  res = vapply(X = intersectionIndexesMatrix,
         FUN = sumFunc,
         total = total2,
         usersArticlesMatrix = usersArticlesMatrix,
         rowIndexes = rowIndexes,
         colIndexes = colIndexes,
         FUN.VALUE = double(1))
  return(sum(res))
}
fun4 = cmpfun(fun1)
fun5 = function(){
  return (fun5c(usersArticlesMatrix = usersArticlesMatrix,rowIndexes = rowIndexes,colIndexes = colIndexes))
}


system.time({fun1()})
system.time({fun2()})
system.time({fun3()})
system.time({fun4()})
system.time({fun5()})
