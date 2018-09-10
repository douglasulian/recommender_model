## Introductory comments:
##
## To run this script, you need to install R, the language for statistical computing.
## https://cran.r-project.org/
## For ease of use, you may consider RStudio IDE, but this is not required.
## https://www.rstudio.com/
## For a tutorial/introduction to R, see e.g.:
## https://cran.r-project.org/doc/manuals/r-release/R-intro.html

## Preparation:
##
## You may need to install some of the packages that this example
## for the PPSN 2016 tutorial is using. If they are available,
## the library("...") command used in the following will
## load the successfully. Otherwise you may receive an error.
## To install the required packages, uncomment the following lines:
# install.packages("SPOT") 
library("SPOT") #load required package: SPOT

## Initialize random number generator seed. Reproducibility.
set.seed(1)

## Main part
## This example should demonstrate the use of surrogate-modeling
## in optimization. To that end, we first need to define an
## optimization problem to be solved by such methods.
## Clearly, we lack the time to consider a real-world, expensive
## optimization problem. Hence, use the following simple, 
## one-dimensional test function, from the book 
## A. I. J. Forrester, A. Sobester, A. J. Keane; 
## "Engineering Design via Surrogate Modeling"; 
## Wiley (2008)

objectFun <- function(x){
  (6*x-2)^2 * sin(12*x-4)
}

## Plot the function:
par(mar=c(4,4,0.5,4),mgp=c(2,1,0))
curve(objectFun(x),0,1)

## Now, let us assume objectFun is expensive.
## First, we start with making some initial
## design of experiment, which in this case
## is simply a regular grid:
x <- seq(from=0, by=0.3,to=1)

## Evaluate with objective:
y <- sapply(x,objectFun)

## Add to plot:
points(x,y)

## Build a model (here: Kriging, with the SPOT package. 
## But plenty of alternatives available)
# fit <- forrBuilder(as.matrix(x),as.matrix(y),
#                    control=list(uselambda=FALSE #do not use nugget effect (regularization)
#                    ))
fit <- buildKriging(x = as.matrix(x),y = as.matrix(y),
                   control=list(uselambda=FALSE, target=c("y","s","ei") #do not use nugget effect (regularization)
                   ))

## Evaluate prediction based on model fit
xtest <- seq(from=0, by=0.001,to=1)
pred <- predict(fit,as.matrix(xtest))
ypred <- pred$y
spred <- pred$s

## Plot the prediction of the model:
lines(xtest,ypred,lty=2)

## Plot suggested candidate solution
points(xtest[which.min(ypred)],ypred[which.min(ypred)],col="black",pch=20)

## Calculate expected improvement (EI)
# ei <- 10^(-spotInfillExpImp(ypred,spred,min(y)))
ei <- 10^(-expectedImprovement(mean = ypred,sd = spred,min = min(y)))

par(new = T)
plot(xtest,ei,lty=3, type="l", axes=F, xlab=NA, ylab=NA, 
     ylim=rev(range(ei)))
axis(side = 4); mtext(side = 4, line = 2, 'EI')
## but note: EI is on a different scale


## Plot suggested candidate solution, based on EI
points(xtest[which.max(ei)],ei[which.max(ei)],col="red",pch=20)
newx <- xtest[which.max(ei)]

## Add data
x <- c(x,newx)
y <- c(y,objectFun(newx))

## Now repeat the same as often as necessary:
repeatThis <- expression({
  curve(objectFun(x),0,1)
  points(x,y)
  fit <- buildKriging(x = as.matrix(x),y = as.matrix(y),
                      control=list(uselambda=FALSE, target=c("y","s","ei") #do not use nugget effect (regularization)
                      ))
  xtest <- seq(from=0, by=0.001,to=1)
  pred <- predict(fit,as.matrix(xtest),predictAll=T)
  ypred <- pred$y
  spred <- pred$s
  lines(xtest,ypred,lty=2)
  points(xtest[which.min(ypred)],ypred[which.min(ypred)],col="black",pch=20)  
  ei <- 10^(-expectedImprovement(mean = ypred,sd = spred,min = min(y)))
  par(new = T)
  plot(xtest,ei,lty=3, type="l", axes=F, xlab=NA, ylab=NA, 
       ylim=rev(range(ei)))
  axis(side = 4); mtext(side = 4, line = 2, 'EI')
  points(xtest[which.max(ei)],ei[which.max(ei)],col="red",pch=20)
  newx <- xtest[which.max(ei)]
  x <- c(x,newx)
  y <- c(y,objectFun(newx)) 
}) 

eval(repeatThis)
eval(repeatThis) 
eval(repeatThis) 
eval(repeatThis) 
eval(repeatThis) 
eval(repeatThis) 
eval(repeatThis) 
eval(repeatThis) 
eval(repeatThis) 


spotResult = spot(fun = function(x){(6*x - 2)^2 * sin(12*x - 4)},lower = c(0),upper = c(1),control = list(plots = TRUE))
spotResult$xbest
spotResult$count
spotResult$msg
spotResult$modelFit


## Observation: 
## EI looks noisy, strange. 
## Predicted mean has bad accuracy. 
## Why? 
## COMMON PRACTICAL PROBLEM 
## If repeated to often -> Numerical issues: 
## Due to close spacing of candidates -> Problem for Kriging model
## Potential remedy: use regularization with nugget and reinterpolation.
## Note: Other interpretation of such an issue may be convergence of
## the optimization process. But this is not necessarily correct.

## repeat as often as necessary (but now with regularization):
# repeatThis <- expression({
#   curve(objectFun(x),0,1)
#   points(x,y)
#   fit <- buildKriging(x = as.matrix(x),y = as.matrix(y),
#                      control=list(
#                        useLambda=TRUE, # Use nugget (parameter lambda)
#                        reinterpolate=TRUE, # Reinterpolation, to fix uncertainty estimates, etc.
#                        target=c("y","s","ei") 
#                      ))
#   xtest <- seq(from=0, by=0.001,to=1)
#   pred <- predict(fit,as.matrix(xtest),predictAll=T)
#   ypred <- pred$y
#   spred <- pred$s
#   lines(xtest,ypred,lty=2)
#   points(xtest[which.min(ypred)],ypred[which.min(ypred)],col="black",pch=20)  
#   ei <- 10^(-expectedImprovement(mean = ypred,sd = spred,min = min(y)))
#   par(new = T)
#   plot(xtest,ei,lty=3, type="l", axes=F, xlab=NA, ylab=NA, 
#        ylim=rev(range(ei)))
#   axis(side = 4); mtext(side = 4, line = 2, 'EI')
#   points(xtest[which.max(ei)],ei[which.max(ei)],col="red",pch=20)
#   newx <- xtest[which.max(ei)]
#   x <- c(x,newx)
#   y <- c(y,objectFun(newx))
# })
# eval(repeatThis)
# eval(repeatThis)