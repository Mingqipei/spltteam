setwd("/Users/peimingqi/Desktop/spl")
data=read.csv("DB.csv")
close=data$Adj.Close

#calculate the return
close_ln=log(close,base=exp(1))
close_diff=diff(close_ln)

#change the data format
data$Date<-as.Date(data$Date)
data_date=data$Date
View(data_date)

#draw the plot of the return data
return=data.frame(data$Date[2:3897],close_diff)
colnames(return)=c("date","diff")
View(return)
plot(return,type='l',xlab="date",ylab="return",col="dark red")

