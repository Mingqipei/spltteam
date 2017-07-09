setwd("/Users/peimingqi/Desktop/spl")
DB=read.csv("DB.csv")

#date
DB$Date<-as.Date(data$Date)
dateframe=data.frame(DB$Date[2:3897])

#index return
index=DB$GSPC
index_ln=log(index,base=exp(1))
index_diff=diff(index_ln)
indexframe=data.frame(index_diff)

#P/EPS
PE=DB$Close/DB$EPS
PEframe=data.frame(PE[2:3897])

#turnover rate
turnover=DB$Volume/DB$shares
turnoverframe=data.frame(turnover[2:3897])

#circulated stock value
csv=DB$shares*10000*DB$Close
ln_csv=log(csv,exp(1))
csvframe=data.frame(ln_csv[2:3897])

#Y
close=DB$Adj.Close
close_ln=log(close,base=exp(1))
close_diff=diff(close_ln)
Yframe=data.frame(close_diff)

#total data frame
DBframe=data.frame(dateframe,indexframe,PEframe,turnoverframe,csvframe,Yframe)
colnames(DBframe)=c("date","index","PE","turnover","csv","Y")
View(DBframe)

