################################################################
# This script is an adapted version of the original provided by Francois. 
# It can be run by using the data stored at the Turing OneDrive shared folder. 
################################################################

# Clean memory
rm(list = ls())
.rs.restartR() # Restarting R makes sure that the memory is truly empty (there is a memory leakage in some R functions)
# Load libraries
library(data.table); library(forecast); library(seasonal); library(readxl); library(readr); library(stringr)

# Set path for output directory (for plots and other outputs)
outputdir<-paste0("output/exploration_count_data/")
if(!dir.exists(outputdir)){dir.create(outputdir, recursive=T)}


################################################################
# Load data
tm<-"month"; reg<-"regional"; dtype<-"cnt"; digit<-2
load(paste0("Payment_data/Mar_data/edgelist_transactions/aggregate_cnt_by_",tm,"_and_2_sic_",reg,"_level.RData"))
dfcnt<-df # count data
load(paste0("Payment_data/Mar_data/edgelist_transactions/aggregate_amt_by_",tm,"_and_2_sic_",reg,"_level.RData"))
dfamt<-df # amount data

# df is the workfile for the next steps
df<-dfcnt

## Note: The following lines are the original by Francois. The function hist_ccdf_loglog is missing. A manual patch is provided below. 
#par(mar=c(4,4,1,1))
#par(las=1)
#hist_ccdf_loglog(df$feb_cnt_22/2,mycol=4,myylab="P(X>x)",myxlab="x")
#hist_ccdf_loglog(df$feb_cnt_16/2,add=T,mycol=2)
#hist_ccdf_loglog(df$jul_cnt_21/2,add=T,mycol=3)
#abline(a=0.1,b=-1.3,lty=2,col="darkgrey",lwd=2)

# Write outputs to a pdf document. 
pdf(paste0(outputdir, "plots_exploring_transaction_volumes_francois_script.pdf"), onefile = T)
p <- ppoints(1000)
X<-df$feb_cnt_16
X2<-df$feb_cnt_22
X3<-df$jul_cnt_21
par(mfrow=c(1,2))
#plot(quantile(X,p=p),p,type="l",ylab="P(X < x)",xlab="x",main="CDF")
#lines(quantile(X2,p=p),p, col="red")
#lines(quantile(X3,p=p),p, col="blue")
plot(quantile(X,p=p),1-p,type="l",ylab="P(X > x)",xlab="x",main="CCDF")
lines(quantile(X2,p=p),1-p, col="red")
lines(quantile(X3,p=p),1-p, col="blue")
plot(log(quantile(X,p=p)),log(1-p),
     ylab="log[P(X > x)]",xlab="log(x)",main="CCDF: log-log")
points(log(quantile(X2,p=p)),log(1-p),lty=1,col="red",lwd=1)
points(log(quantile(X3,p=p)),log(1-p),lty=1,col="blue",lwd=1)

# Change plot settings back to one pdf per page
par(mfrow=c(1,1))

# CCDF of ALL counts
# All counts
xx <- as.numeric(as.matrix(df[,!(colnames(df) %in% c("from","to"))]))
#hist_ccdf_loglog(xx/2,mycol=1,myylab="P(X>x)",myxlab="x")
plot(log(quantile(xx,p=p)),log(1-p),
     ylab="log[P(X > x)]",xlab="log(x)",main="CCDF: log-log")
# Number of non NA, non zero counts
length(xx)/1000^2
xx <- xx[!is.na(xx)]
xx <- xx[xx>0]
length(xx)/1000^2
# Number of transactions
sum(xx)/1000^2

months<-str_remove_all(colnames(df)[!(colnames(df) %in% c("from", "to"))], paste0("_",dtype))
################################################################
# Time series of monthly counts
################################################################
mymat <- as.matrix(df[,!(colnames(df) %in% c("from","to"))])
par(mar=c(4,4.2,1,1))
par(las=1)
ntran <- (colSums(mymat,na.rm=T)/1000^2)
plot(ntran,type="o",cex=0.5,
     ylab="# of transactions per month, million",cex.main=0.8,
     xaxt="n", xlab="", ylim=c(floor(min(ntran)), ceiling(max(ntran))))
axis(2,at=c(c(2,4,6,8)*10^6),labels=paste0(c(2,4,6,8)," x 10^6"))
monthslabels <- paste(months)
SEQ <- seq(from=1, to=length(monthslabels), by=6)
axis(1,at=SEQ,monthslabels[SEQ])

################################################################
# Time series of monthly values
################################################################
df<-dfamt
tokeep2 <- grep("amt", colnames(df))
colnames(df)[tokeep2]
mymat <- as.matrix(df[,tokeep2])
valtran <- (colSums(mymat,na.rm=T)/1000^3)
plot(valtran,type="o",cex=0.5,
     ylab="value of transactions per month, billion",
     xaxt="n", xlab="", ylim=c(0,max(valtran))
     )
axis(2,at=c(c(2,4,6,8)*10^6),labels=paste0(c(2,4,6,8)," x 10^6"))
monthslabels <- paste(substr(months,1,3), substr(months,9,10),sep="_")
SEQ <- seq(from=1, to=length(monthslabels), by=6)
axis(1,at=SEQ,monthslabels[SEQ])

# try to remove public admin
torm <- which( substr(df$from,1,2)=="84" |
       substr(df$to  ,1,2)=="84" ) 
mymat <- as.matrix(df[-torm,tokeep2])
valtran_restricted <- (colSums(mymat,na.rm=T)/1000^3)
lines(valtran_restricted,col=2,type="o",cex=0.5)
abline(v=which(monthslabels=="mar_20"),lty=2,col="darkgrey")
text(which(monthslabels=="mar_20")+7,65,"March 2020",col="darkgrey")
legend("topleft", legend=c("All" ,"After removing transactions 
in and out of Public Admin"),col=1:2,lwd=1,pch=1,cex=0.8)


################################################################
# Time series of average value of a transaction
################################################################
av_val <- valtran/ntran
plot(av_val,type="o",cex=0.5,
     ylab="average value of a transaction, thousands",
     xaxt="n", xlab=""
)
axis(2,at=c(c(2,4,6,8)*10^6),labels=paste0(c(2,4,6,8)," x 10^6"))
monthslabels <- paste(substr(months,1,3), substr(months,9,10),sep="_")
SEQ <- seq(from=1, to=length(monthslabels), by=6)
axis(1,at=SEQ,monthslabels[SEQ])
abline(v=which(monthslabels=="mar_20"),lty=2)
text(which(monthslabels=="mar_20")+7,25,"March 2020")


# "This spreadsheet contains the chained volume indices of 
# gross value added estimates of Monthly GDP and its main sectors: 
# Production, Services, Construction and Agriculture. 
# These figures are seasonally adjusted, 
# with a reference year of 2019 (2019 = 100)."
#df1 <- read_excel("/Users/francois/Downloads/monthlygdpto4dp.xlsx",sheet="Data_table")
df1 <- read_excel("public_data/macro_data/mgdp4dp.xlsx",sheet="Data_table")

coln <- c("date","GDP","A","B-E","F","G-T")
df1 <- as.data.frame(df1[-c(1:3),])
colnames(df1) <- coln
head(df1)

monthslabels[1]
monthslabels[length(monthslabels)]
w0 <- which(df1$date=="2015JAN")
w1 <- which(df1$date=="2022JUN")
gdp <- as.numeric(df1[w0:w1,"GDP"])

library(zoo)
basis <- 60
ma <- 2
par(mar=c(3,3,1,1))
plot(rollmean(valtran/valtran[basis],ma,fill=NA), type="o",cex=0.4,col=1, ylim=c(0.7,1.5), xaxt="n", xlab="",ylab="")
lines(rollmean(ntran/ntran[basis],ma,fill=NA),type="o",cex=0.4,col=2)
lines(gdp/gdp[basis],type="l",cex=0.4,col=4,lwd=2)
legend("topleft",col=c(1,2,4),
       legend=c("Total Value (mov. av.)","Number of transactions (mov. av.)","ONS monthly GDP (deseason.)"),
       lwd=2,pch=c(1,1,NA), title="Index of...")
legend("topright",legend=paste0("Number of months for Moving Average: ", ma), bty="n",cex=0.7)
axis(1,at=SEQ,monthslabels[SEQ])


### THIS DATASET JUST HAS THE SAME GDP, BUT HAS MORE DETAILS
# df2 <- as.data.frame(read_excel("/Users/francois/Downloads/mgdp.xlsx"))
# wcol <- which(df2[1,]=="ECY2")
# df2[,wcol]
# w0 <- which(df2[,1]=="2015 JAN")
# w1 <- which(df2[,1]=="2022 JUN")
# gdp2 <- as.numeric(df2[w0:w1,wcol])
# lines(gdp2/gdp2[basis],type="o",cex=0.4,col=5,lwd=2)
# 

# https://www.ons.gov.uk/economy/grossdomesticproductgdp/adhocs/14197indicativemonthlynonseasonallyadjustedgdp
# These figures are non-seasonally adjusted and have not been quality assured in-depth. Where quality adjustments are applied as part of the GDP compilation process, these are only applied to the seasonally adjusted series. Therefore, we would not consider the non-seasonally adjusted data to be of a quality to publish but may be suitable for internal analysis purposes in the context of the implied seasonal factors.
df3 <- as.data.frame(read_excel("public_data/macro_data/indicativemonthlygdpnsapubl.xlsx", sheet="Monthly Index"))
w0 <- which(df3[,1]=="2015JAN")
# Stops in nov 2021
w1 <- which(df3[,1]=="2021NOV")
wcol <- which(df3[4,]=="Total GVA")
gdp3 <- as.numeric(df3[w0:w1,wcol])
gdp3 <- c(gdp3, rep(NA,length(monthslabels)-length(gdp3)))
lines(gdp3/gdp3[basis],col="green",lwd=2)

gr_gdp <- diff(log(gdp3))
gr_ntran <- diff(log(ntran))

plot(gr_gdp,type="l")
points(gr_ntran,type="l",col=2)

gr_gdp_norm <- (gr_gdp - mean(gr_gdp,na.rm=T)) / sd(gr_gdp,na.rm=T)
gr_ntran_norm <- (gr_ntran - mean(gr_ntran,na.rm=T)) / sd(gr_ntran,na.rm=T)

par(mar=c(4,4,1,1))
plot(gr_gdp_norm,type="l",xaxt="n",
     ylab="tstat(growth rate)",xlab="",lwd=2)
points(gr_ntran_norm,type="l",col=2,lwd=2)
axis(1,at=SEQ,monthslabels[SEQ])
legend("bottomleft",col=1:2,legend=c("ONS not deseasonalized monthly GDP", "Payment # transactions"),lwd=2)

plot(gr_gdp_norm ~ gr_ntran_norm,type="o",
     ylab="tstat(growth rate GDP)",xlab="tstat(growth rate # Transactions)")
summary(lm(gr_gdp_norm ~ gr_ntran_norm+0))
theo <- 0.75429*gr_ntran_norm
lines(theo ~ gr_ntran_norm,col=2,lwd=2)
torm <- which(gr_gdp_norm < (-4))
summary(lm(gr_gdp_norm[-torm] ~ gr_ntran_norm[-torm]+0))
theo <- 0.6133*gr_ntran_norm
lines(theo ~ gr_ntran_norm,col=4,lty=1,lwd=2)
legend("bottomright",legend=c("OLS, all","OLS, remove 1 outlier","unit line"),col=c(2,4,"darkgrey"),lty=c(1,1),lwd=c(2,2,5) )
abline(a=0,b=1,col="darkgrey",lwd=5)

# Is the coef statistically different from 1?
coe <- summary(lm(gr_gdp_norm ~ gr_ntran_norm+0))
coe$coefficients[1]+2*coe$coefficients[2]



plot(gr_gdp_norm ~ gr_ntran_norm,type="p",
     ylab="tstat(growth rate GDP)",xlab="tstat(growth rate # Transactions)")
summary(lm(gr_gdp_norm ~ gr_ntran_norm+0))
theo <- 0.75429*gr_ntran_norm
lines(theo ~ gr_ntran_norm,col=2,lwd=2)
torm <- which(gr_gdp_norm < (-4))
summary(lm(gr_gdp_norm[-torm] ~ gr_ntran_norm[-torm]+0))
theo <- 0.6133*gr_ntran_norm
lines(theo ~ gr_ntran_norm,col=4,lty=1,lwd=2)
legend("bottomright",legend=c("OLS, all","OLS, remove 1 outlier","unit line"),col=c(2,4,"darkgrey"),lty=c(1,1),lwd=c(2,2,5) )
abline(a=0,b=1,col="darkgrey",lwd=5)

# Is the coef statistically different from 1?
coe <- summary(lm(gr_gdp_norm ~ gr_ntran_norm+0))
coe$coefficients[1]+2*coe$coefficients[2]



##################################################
## Quarterly GDP
##################################################
gdp_xl <- read_excel("public_data/macro_data/gdpolowlevelaggregatesq22022.xlsx", sheet="1")

# Data we have is for these months:
monthslabels
# Number of months is nm
length(ntran)==length(months)

gdp_xl[5,1]
gdp_xl[5,2]

# Don't get Q2 2022
quarters <- as.character(as.matrix(gdp_xl[110:138,1]))
qgdp     <- as.numeric(  as.matrix(gdp_xl[110:138,2]))
par(mfrow=c(1,2))
plot(qgdp,type="o")
plot(diff(log(qgdp)),type="o")
nq <- length(qgdp)

SEQ1 <- seq(from=1,to=length(ntran),by=3)
SEQ2 <- SEQ1 - 1
tt=1
ntran_quarterly <- rep(NA,nq)
for(tt in 1:nq){
  ntran_quarterly[tt] <- sum(ntran[SEQ1[tt]:SEQ2[tt+1]])
}

basis <- 10
plot(qgdp/qgdp[basis],type="o",ylim=c(0.4,1.4))
lines(ntran_quarterly/ntran_quarterly[basis])

dlnqgdp <- diff(log(qgdp))
dlnntranq <- diff(log(ntran_quarterly))
plot(dlnqgdp,type="o",ylim=c(-0.3,0.2),cex=0.3)
lines(dlnntranq,cex=0.3,type="o",col=2)

dlnqgdp_n <- (dlnqgdp-mean(dlnqgdp))/sd(dlnqgdp)
dlnntranq_n <- (dlnntranq-mean(dlnntranq))/sd(dlnntranq)

plot(dlnqgdp_n,type="o",ylim=c(-4,3),cex=0.3)
lines(dlnntranq_n,cex=0.3,type="o",col=2)

# install.packages("seasonal")
library("seasonal")
ntran_ts <- ts(data=ntran,deltat=1/12,start=2015)
SEAS <- seas(ntran_ts)
names(SEAS)
SEAS$data[["final"]]
ntran_sa <- as.data.frame(SEAS$data)$final
plot(ntran,type="l")
lines(ntran_sa,col=2)



ntran_quarterly_sa <- rep(NA,nq)
for(tt in 1:nq){
  ntran_quarterly_sa[tt] <- sum(ntran_sa[SEQ1[tt]:SEQ2[tt+1]])
}
basis <- 10
plot(qgdp/qgdp[basis],type="o",ylim=c(0.4,1.4))
lines(ntran_quarterly/ntran_quarterly[basis],col=2)
lines(ntran_quarterly_sa/ntran_quarterly_sa[basis],col=3)

dlnntranq_sa <- diff(log(ntran_quarterly_sa))
plot(dlnqgdp,type="o",ylim=c(-0.3,0.2),cex=0.3)
lines(dlnntranq,cex=0.3,type="o",col=2)
lines(dlnntranq_sa,cex=0.3,type="o",col=3)

dlnntranq_sa_n <- (dlnntranq_sa-mean(dlnntranq_sa))/sd(dlnntranq_sa)
plot(dlnqgdp_n,type="o",ylim=c(-3,2),cex=0.3)
lines(dlnntranq_n,cex=0.3,type="o",col=2)
lines(dlnntranq_sa_n,cex=0.3,type="o",col=3)

plot(dlnntranq_n ~ dlnqgdp_n)
points(dlnntranq_sa_n ~ dlnqgdp_n,col=2,pch=2)
abline(a=0,b=1)

### 
basis=1
qgdp_i <- qgdp/qgdp[basis]
ntran_i <- ntran_quarterly/ntran_quarterly[basis]
ntran_sa_i <- ntran_quarterly_sa/ntran_quarterly_sa[basis]
plot(qgdp_i,type="o",ylim=c(0.4,1.4))
lines(ntran_i,col=2)
lines(ntran_sa_i,col=3)

library(forecast)
AR <- Arima(log(qgdp_i),order=c(1,1,0))
ARX <- Arima(log(qgdp_i), xreg=log(ntran_i),order=c(1,1,0))
ARX_sa <- Arima(log(qgdp_i), xreg=log(ntran_sa_i),order=c(1,1,0))
ARX_sa$sigma2/AR$sigma2
ARX_sa$sigma2/ARX$sigma2

plot(density(AR$residuals))
lines(density(ARX$residuals),col=2)
lines(density(ARX_sa$residuals),col=3)


# Predict 2020 Q2
t0 <- 1
t1 <- 24
t2 <- 29
quarters[21]
OO <- c(0,1,1)
DD <- T
AR     <- Arima(log(qgdp_i[t0:t1]), order=OO, include.drift = DD)
ARX    <- Arima(log(qgdp_i[t0:t1]), order=OO, include.drift = DD, xreg=log(ntran_i   [t0:t1]))
ARX_sa <- Arima(log(qgdp_i[t0:t1]), order=OO, include.drift = DD, xreg=log(ntran_sa_i[t0:t1]))

plot(qgdp_i,xlim=c(t0,t2))
AR_pred     <- exp( forecast(AR,     h=t2-t1                               )$mean)
ARX_pred    <- exp( forecast(ARX,    h=t2-t1, xreg = log(ntran_i   [t1:t2]))$mean)
ARX_sa_pred <- exp( forecast(ARX_sa, h=t2-t1, xreg = log(ntran_sa_i[t1:t2]))$mean)
lines(c(qgdp_i[t0:t1],AR_pred))
lines(c(qgdp_i[t0:t1],ARX_pred),col=2)
lines(c(qgdp_i[t0:t1],ARX_sa_pred),col=3)

dev.off()


