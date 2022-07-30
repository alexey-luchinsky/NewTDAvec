## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(TDAvec)

## -----------------------------------------------------------------------------
# sample 100 points uniformly from unit circle and add Gaussian noise
N <- 100
set.seed(123)
X <- TDA::circleUnif(N) + rnorm(2*N,mean = 0,sd = 0.2)
plot(X, asp=1)

## -----------------------------------------------------------------------------
D <- TDA::ripsDiag(X,maxdimension = 1,maxscale = 2)$diagram

## -----------------------------------------------------------------------------
# switch from birth-death to birth-persistence coordinates
D[,3] <- D[,3] - D[,2] 
colnames(D)[3] <- "Persistence"
head(D)

## -----------------------------------------------------------------------------
D0 = D[D[,1]==0,]
D1 = D[D[,1]==1,]
plot(D0[,2], D0[,3], pch=1, xlim=c(0,0.5), col="red", xlab="Birth", ylab="Persistense")
points(D1[,2], D1[,3], pch=2, col="blue")
abline(a=0, b=1)
legend("topright", legend = c("H=0", "H=1"), pch = c(1,2), col=c("red", "blue"))

## -----------------------------------------------------------------------------
ySeqH0 <- unique(quantile(D[D[,1]==0,3],probs = seq(0,1,by=0.1)))
T0 <- computeVPB(D,homDim = 0,xSeq=NA,ySeqH0,tau = 0.3)
T0

## -----------------------------------------------------------------------------
xSeqH1 <- unique(quantile(D[D[,1]==1,2],probs = seq(0,1,by=0.2)))
ySeqH1 <- unique(quantile(D[D[,1]==1,3],probs = seq(0,1,by=0.2)))
list(
  VPB=computeVPB(D,homDim = 1,xSeqH1,ySeqH1,tau = 0.3)
)

