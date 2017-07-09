#calculate VaR
alpha=0.05
q=quantile(close_diff,probs=alpha)

xmean=apply(Xt,2,mean)

#calculatevtheta
vtheta=xmean%*%betafinal

#calculate ES
ES=-(1+theta/((1-2*theta)*alpha))*vtheta+theta/((1-2*theta)*alpha)*mean(Y)

#the final results
datafinal=data.frame(alpha,theta,q,ES)
colnames(datafinal)=c("alpha","theta","VaR","ES")
View(datafinal)