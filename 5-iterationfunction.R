
  
itefun=function(beta){

#calculate residual
Xt=cbind(1,datamatrix)
n=nrow(Xt)
et=Y-crossprod(t(Xt),beta)
ets=et^2
U=ets[4:n]
v1=matrix(ets[3:(n-1)],nrow=n-3)
v2=matrix(ets[2:(n-2)],nrow=n-3)
v3=matrix(ets[1:(n-3)],nrow=n-3)
V=cbind(v1,v2,v3)

lm2=lm(U~V)
summary(lm2)

alphat=lm2$coefficients

#calculate sigma
v4=matrix(rep(1,n-3),nrow=n-3)

Vt=cbind(v4,v1,v2,v3)

sigma=Vt%*%alphat

#calculate omega
et2=matrix(0,nrow = n)
et2[which(et<0)]=1
et2[which(et==0)]=1
et2[which(et>0)]=0
et2=as.vector(et2)

thetavec=rep(theta,n)
thetavec=as.vector(thetavec)

omega=abs(thetavec-et2)
omega=omega[4:n]

j=1
xxsummatrix=matrix(0,nrow=5,ncol = 5)
xysummatrix=matrix(0,nrow = 5,ncol = 1)
ynew=Y[4:n]
for (j in 1:(n-3)) {
  xxsummatrix=xxsummatrix+omega[j]/sigma[j]*crossprod(t(Xt[j,]),Xt[j,])
  xysummatrix=xysummatrix+omega[j]/sigma[j]*crossprod(t(Xt[j,]),ynew[j])
}

beta=crossprod(solve(xxsummatrix),xysummatrix)
return(beta)
}



