setwd("/Users/peimingqi/Desktop/spl")
data=read.csv("DB.csv")
close=data$Adj.Close
close_ln=log(close,base=exp(1))
close_diff=diff(close_ln)

#the plot/curve of the kernel density distribution
plot(density(close_diff))

#estimate the approximate function of kernel distribution
kernel_density<-density(close_diff)
kernel_fun<-approxfun(kernel_density$x,kernel_density$y)

#
myfun=function(a){
  b=kernel_fun(a)*a
  return(b)
}

#calculate theta(alpha=0.05)
alpha=0.05

q=quantile(close_diff,probs=alpha)

inte1=integrate(myfun,-0.2,q)[[1]]
inte2=integrate(myfun,q,0.2)[[1]]

theta=(alpha*q-inte1)/(2*inte2-(1-2*alpha)*q)
theta
