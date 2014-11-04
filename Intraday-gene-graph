library(TTR)
library(zoo)

old.par<-par()

Time_lag = 3


nbr_datapoint=102
set.seed(100)

bids<-rnorm(nbr_datapoint,mean = 0, sd = .08)
bids<-(cumsum(bids)+100)
bids_lag <-rollmean(bids, k = Time_lag, na.pad = TRUE)
bids_lag_exp <-EMA(bids, width = Time_lag)

ask<-abs(rnorm(nbr_datapoint,mean = 0.7, sd = .02))
ask<-bids-ask
ask_lag <-SMA(ask, k = Time_lag)
ask_lag_exp <-EMA(ask, width = Time_lag)

Mkt_trades<-pmax(ask+.09,bids-abs(((bids-ask)/2)-rnorm(nbr_datapoint,mean = 0.03, sd = .2)))
N  <- 70
inds<- sample(1:nbr_datapoint,N, replace=FALSE)# the number of random values to replace
Mkt_trades[inds] <- NA                                   # use the random values as indicies to vec, for which to replace
Mkt_trades_approx <- zoo(Mkt_trades)
Mkt_trades_approx <- na.approx(Mkt_trades_approx)
Mkt_lag <-rollmeanr(Mkt_trades_approx , k = Time_lag, fill = NA)
Mkt_lag_exp <-EMA(Mkt_trades_approx , width = Time_lag)

BK_trades<-pmax(ask+.09,bids-abs(((bids-ask)/2)-rnorm(nbr_datapoint,mean = 0.03, sd = .2)))
N  <- 80
inds<- sample(1:nbr_datapoint,N, replace=FALSE)# the number of random values to replace
BK_trades[inds] <- 0    

layout(matrix(c(1,2), ncol=1, byrow=TRUE),heights=c(3.9, 1))
par(mai=c(1,1,1,1))
par(mar=c(0,4,3,2))
  
plot(ask, type="p",  pch=24,col="red",ylim=c(99,100.5),xlab = "",ylab = "")
lines(ask_lag , type="l", col="red")
lines(ask_lag_exp , type="l", lty=2, col="red")
lines(bids, type="p", pch=25, col="green3")
lines(bids_lag_exp,type="l",lty=2,col="green3")
lines(bids_lag,type="l", col="green3")
lines(Mkt_trades, type="p", pch=18, lty=2, col="black")
points(BK_trades, type="p", pch=4, lty=2, col="black")
lines(Mkt_lag , type="l", col="black")
lines(Mkt_lag_exp , type="l",lty=2,col="black")
# Create a title with a red, bold/italic font
title(main="5 min Intraday", font=2,cex = 1.5)
mtext("Time", 1, line=2, cex = 1.1 ,font=2)
mtext("Price", 2, line=2, cex = 1.1, font=2)
#legend("center", ncol=5,c("ask","ask_avg" ,"ask_exp_avg","bids","bids_avg","bids_exp_avg","Mkt_trades","Bk_trades",
#                          "Mkt_avg","Mkt_exp_avg"), col=c("red","red","red","green3","green3","green3","black",
#                                                          "black","black","black"), pch=c(24,NA,NA,25,NA,NA,18,4,NA,NA), lty=c(0,1,2,0,1,2,0,0,1,2),cex=.6)

# Create a legend at (1, g_range[2]) that is slightly smaller 
# (cex) and uses the same line colors and points used by 
# the actual plots 
plot.new()
#par(mar=c(0,0,0,0))
par(mai=c(0,0,0,0))
legend("center", ncol=5,,cex=1,c("ask","ask_avg" ,"ask_exp_avg","bids","bids_avg","bids_exp_avg","Mkt_trades","Bk_trades",
                        "Mkt_avg","Mkt_exp_avg"), col=c("red","red","red","green3","green3","green3","black",
                        "black","black","black"), pch=c(24,NA,NA,25,NA,NA,18,4,NA,NA), lty=c(0,1,2,0,1,2,0,0,1,2))
par(old.par)
