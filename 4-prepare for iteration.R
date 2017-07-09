#get the initial value of beta
datamatrix=cbind(DBframe$index,DBframe$PE,DBframe$turnover,DBframe$csv)
colnames(datamatrix)=c("index","PE","turnover","csv")


Y=DBframe$Y

result=lm(formula = Y ~ datamatrix)
summary(result)

#test heteroscedasticity
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(result)
library("lmtest")
restest=bptest(result)

#get the initial beta
beta0=result$coefficients

#estimate alpha in ARCH
res = result$resid
res2=res^2
res=as.matrix(res2)

m=length(res)
U=res[4:m]
v1=matrix(res[3:(m-1)],nrow=m-3)
v2=matrix(res[2:(m-2)],nrow=m-3)
v3=matrix(res[1:(m-3)],nrow=m-3)

V=cbind(v1,v2,v3)

lm2=lm(U~V)
summary(lm2)

alphat=lm2$coefficients

#calculate sigma
v4=matrix(rep(1,m-3),nrow=m-3)

Vt=cbind(v4,v1,v2,v3)

sigma=Vt%*%alphat

#calculate omega
et=result$resid
et=as.vector(et)


et2=matrix(0,nrow=3896)
mode(et2)
et2[which(et<0)]=1
et2[which(et==0)]=1
et2[which(et>0)]=0
et2=as.vector(et2)


thetavec=rep(theta,3896)
thetavec=as.vector(thetavec)

omega=abs(thetavec-et2)
is.vector(omega)
omega=omega[4:3896]


#calculate the new beta
Xt=cbind(1,datamatrix[4:3896,])
j=1
xxsummatrix=matrix(0,nrow=5,ncol = 5)
xysummatrix=matrix(0,nrow = 5,ncol = 1)
ynew=Y[4:3896]
for (j in 1:3893) {
  xxsummatrix=xxsummatrix+omega[j]/sigma[j]*crossprod(t(Xt[j,]),Xt[j,])
  xysummatrix=xysummatrix+omega[j]/sigma[j]*crossprod(t(Xt[j,]),ynew[j])
  }

solve(xxsummatrix)
beta1=crossprod(t(solve(xxsummatrix)),xysummatrix)



