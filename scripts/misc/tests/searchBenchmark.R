library(dplyr)
library(microbenchmark)

size = 100
data = as.data.frame(matrix(data = sample(x = seq(1:size),replace = TRUE,size = size**2),ncol = 2,dimnames = list(paste0('R',seq(1:((size**2)/2))),paste0('C',seq(1:2)))))
firstCond = sample(x = seq(1:size),size = size,replace = TRUE)
secondCond = sample(x = seq(1:size),size = size,replace = TRUE)

f1 = function(){
  for (i in 1:size){
    subset(data, C1 == firstCond[i] & C2 == secondCond[i])
  }
}

f2 = function(){
  for (i in 1:size){
    filter(data,C1 == firstCond[i], C2 == secondCond[i])
  }
}

f3 = function(){
  for (i in 1:size){
    aux = which(data[,'C1'] == firstCond[i])
    aux = aux[which(data[aux,'C2'] == secondCond[i])]
    data[aux, ]
  }
}

f4 = function(){
  for (i in 1:size){
    data[data[, "C1"] == firstCond[i] & data[,"C2"]== secondCond[i],] 
  }
}

microbenchmark(f1(),f2(),f3(),f4())