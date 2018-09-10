library(dplyr)
library(microbenchmark)

size = 10000
data = as.data.frame(matrix(data = sample(x = seq(1:size),replace = TRUE,size = size**2),ncol = 2,dimnames = list(paste0('R',seq(1:((size**2)/2))),paste0('C',seq(1:2)))))
firstCond = sample(x = seq(1:size),size = size,replace = TRUE)

f1 = function(){
  data[data$C1 %in% firstCond,]
}

f2 = function(){
  subset(data, C1 %in% firstCond)
}

microbenchmark(f1(),f2())

