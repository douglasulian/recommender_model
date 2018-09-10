funciona = function(tui,tvi,m,u,v,vuov,alpha){
  # tui = 2
  # tvi = 1
  # m = 5
  # u = 1
  # v = 2
  # vuov = 1
  # alpha = 0.05
  # 
  wui =  u*exp(-alpha*tui*vuov)/sqrt(log(1+m))
  wvi =  v*exp(-alpha*tvi*-vuov)/sqrt(log(1+m))
  r1 = wui * wvi
  r2 = (u*v)*(1/log(1+m))*exp(-alpha*abs(tui-tvi))
  # print(r1)
  # print(r2)
  return(list(r1=r1,r2=r2))
  
  # wui =  u*exp(tui*vuov)
  # wvi =  v*exp(tvi*-vuov)
  # r1 = wui * wvi
  # r2 = (u*v)*exp(abs(tui-tvi))
  # r1
  # r2
}

u  = sample(x = c(0,1),replace = TRUE,size = 10)
v  = sample(x = c(0,1),replace = TRUE,size = 10)
m  = c(1,2,3,4,5,6,7,8,9,10)
alpha = 0.05

tu = c(rep(0,10))
tu = vapply(X = u,FUN = function(x) return(x*sample(x = c(1,0,5,12,4,10),replace = TRUE,size = 1)),FUN.VALUE = double(1))
tv = c(rep(0,10))
tv = vapply(X = v,FUN = function(x) return(x*sample(x = c(1,0,5,12,4,10),replace = TRUE,size = 1)),FUN.VALUE = double(1))

union = (u==1)|(v==1)

diff = tu<tv
vuov = rep(1,10)
vuov[diff] = -1 

total = 0
for(i in 1:10){
  tui = tu[i]
  tvi = tv[i]
  total = total + (u[i]*v[i])*(1/log(1+m[i]))*exp(-alpha*abs(tui-tvi))
}
sim = total/sum(union)

total2 = 0
for(i in 1:10){
  tui = tu[i]
  tvi = tv[i]
  if (u[i] == v[i] & v[i] == 1) {
    # wui =  u[i]*exp(-alpha*tui*vuov[i])/sqrt(log(1+m[i]))
    # wvi =  v[i]*exp(-alpha*tvi*-vuov[i])/sqrt(log(1+m[i]))
    if (vuov[i] == 1)
      wui =  u[i]*exp(-alpha*tui)/sqrt(log(1+m[i]))
    else
      wui =  u[i]*1/exp(-alpha*tui)/sqrt(log(1+m[i]))
    
    if (vuov[i] == 1)
      wvi =  v[i]*1/exp(-alpha*tvi)/sqrt(log(1+m[i]))
    else
      wvi =  v[i]*exp(-alpha*tvi)/sqrt(log(1+m[i]))
    
    total2 = total2 + (wui * wvi)
  }
}
sim2 = total2/sum(union)

sim
sim2
# 
# i = rep(sample(x = c(1,0),size = 10,replace = TRUE))
# v1 = rep(2,10)
# v2 = rep(4,10)
# d = sample(x = c(1,-1),size = 10,replace = TRUE)
# v = rep(0,10)
# d = d*i
# v[d==-1] = v1[d==-1]
# v[d==1] = v2[d==1]
# v
# d
# i
