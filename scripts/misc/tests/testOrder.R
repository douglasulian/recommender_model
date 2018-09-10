m = matrix(data = sample(x = rep(0:10),size = 50,replace = TRUE),nrow = 10,ncol = 5)

m2 = apply(X = m,MARGIN = 2,FUN = function(x) return(x/max(x)))

rownames(m2) = rep(1:10)

apply(X = m2,MARGIN = 2,FUN = function(x,n){
  ord = order(x,decreasing = TRUE)
  x[ord[(n+1):length(x)]] = 0
  return(x)
},n = 5)



x = m2[,1]

ord = order(x,decreasing = TRUE)
x[ord[(length(x)-n+1):length(x)]] = 0
x[9]
sort(x,decreasing = TRUE)
x[ord <= length(x)-3] = 0
