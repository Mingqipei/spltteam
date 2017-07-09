#calculate the max diff of betas as the condition to end the iteration
difference1=beta1-beta0
difference1=abs(difference1)
maxdifference1=max(difference1)

#establish a matrix to put all the obtained betas
betamatrix=cbind(beta0,beta1)

#give the initial values in the loop
beta=beta1
maxdiff=1000

#the iteration loop
while(maxdiff>0.0001){
  k=beta
  beta=itefun(beta)
  difference=abs(beta-k)
  maxdiff=max(difference)
  betamatrix=cbind(betamatrix,beta)
}

betafinal=beta 

View(betafinal)
View(betamatrix)

