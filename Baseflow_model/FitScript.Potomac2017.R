library(EGRET)
library(dataRetrieval)
library(lubridate)
source("~/Dropbox/DickSmithBaseflow/BFM/runBaseReg.R") # this is the function that does base flow model estimation
load("~/Dropbox/DickSmithBaseflow/BFM/PotomacCB.TN.RData") # this file must contain eList, modelEstimation must already have been run before the workspace is saved
eList$INFO # this command allows us to see the contents of the metadata
savePath <- "~/Dropbox/DickSmithBaseflow/BFM/"   # needs to be specified, the place where the workspace gets saved
saveResults(savePath,eList) #this command saves a version of the workspace into a file called PotomacCB.TN.RData, we will save again later on
# the next four lines should be modified to test window options
windowY <- 10
windowQb <- 1
windowQp <- 50
windowS <- 0.5
#
cat("\nsummary(eList$Sample)\n")
print(summary(eList$Sample)) # this helps us see what is in the Sample data set
# now we will read in the discharge data
Qdf <- read.csv("~/Dropbox/DickSmithBaseflow/BFM/flowsep.ChBridge.csv")
cat("\nsummary(Qdf)\n")
print(summary(Qdf)) # just checking on the contents
# some reformatting of the dates
Date <- as.Date(Qdf[,2],format("%m/%d/%Y"))
cat("\nsummary(Date)\n")
print(summary(Date))
Q <- Qdf[,3]*0.02831647 # changing units to cms
Qb <- Qdf[,4]*0.02831647
Qp <- Qdf[,5]
newDaily <- data.frame(Date,Q,Qb,Qp)
Sample <- eList$Sample # pulling the Sample data frame out of eList for convenience
cat("\nsummary(Sample$Date)\n")
print(summary(Sample$Date))
# we need to trim both data sets to a common period, in this case 1984-10-01 through 2013-07-31
startDate <- eList$Daily$Date[1]
nDays <- length(eList$Daily$Date)
endDate <- eList$Daily$Date[nDays]
DecLow <- eList$Daily$DecYear[1]
DecHigh <- eList$Daily$DecYear[nDays]
# these next two lines are not really necessary, but if you haven't trimmed the csv file they will take care of it
newDaily <- subset(newDaily,Date>=startDate&Date<=endDate)
Sample <- subset(Sample,Date>=startDate&Date<=endDate)
newSample <- merge(Sample,newDaily,by="Date")
cat("\nsummary(newSample)\n")
print(summary(newSample))
# there are now two versions of Q in the newSample data frame, the original one called Q.x the new one called Q.y
# I want to run a check to make sure they match up
# here we try to see what the difference looks like
ratio <- newSample$Q.x/newSample$Q.y
cat("\nsummary(ratio)\n")
print(summary(ratio))
# the values of ratio should all be equal to 1 or very, very close to it.
# assuming this is all ok, we can clean things up a bit.
rm(ratio)
# also need to add Julian and DecYear colunms to newDaily
newDaily$Julian <- as.numeric(julian(as.Date(newDaily$Date),origin=as.Date("1850-01-01")))
newDaily$DecYear <- decimal_date(newDaily$Date)
# some clean up on newSample
newSample$Q <- newSample$Q.x
fractionCensored <- 1 - (sum(newSample$Uncen)/length(newSample$Uncen))
cat("\nfraction of data set censored",fractionCensored," if greater than about 0.05 we should not proceed")
# if there is a tiny amount of censoring but we will ignore it and call the concentration the
# average of the lower and upper bound each day
newSample$Conc <- (newSample$ConcLow + newSample$ConcHigh) / 2
newSample$LogQb <- log(newSample$Qb)
numSamples <- length(newSample$LogQb)
estPtYear <- newSample$DecYear
estPtLQb <- newSample$LogQb
estPtQp <- newSample$Qp
# This resaves the workspace with the additional information that was created by this script
saveResults(savePath,eList)
# This first time through, result1 is going to be the baseflow model's estimates for each sampled day
# in the output there are three columns: yHat which is the expected value of ln(concentration), SE which is the standard error of those estimates, and ConcHat which is the expected value of concentration (includes the bias adjustment)
result1 <- runBaseReg(estPtYear,estPtLQb,estPtQp,DecLow,DecHigh,Sample=newSample,windowY=windowY,windowQb=windowQb,windowQp=windowQp,windowS=windowS)
colnames(result1) <- c("yHat","SE","ConcHat")
# now the second time through we will set Qp = 100 on all days (as if all of the flow is base flow)
# we aren't going to use result2 in this version of the script
# estPtQp <- rep(100,numSamples)
# result2 <- runBaseReg(estPtYear,estPtLQb,estPtQp,DecLow,DecHigh,Sample=newSample,windowY=windowY,windowQb=windowQb,windowQp=windowQp,windowS=windowS)
# colnames(result2) <- c("yHat","SE","ConcHat")
# the third run through will actually be the WRTDS model using the total discharge (no regard for base flow) 
#these results are there because modelEstimation has already been run
#if the original eList that was read in, didn't already have modelEstimation completed then the following command would need to be included in the script at this point
#eList <- modelEstimation(eList)
result3 <- eList$Sample[,15:17]
summary(result1)
# summary(result2)
summary(result3)
# next we compute the residuals for the base flow model
newSample$resid <- log(newSample$Conc) - result1[,1]
eList$Sample$resid <- log(eList$Sample$ConcAve) - result3[,1]
mseBaseMod <- sum(newSample$resid^2) / numSamples
rmseBaseMod <- mseBaseMod^0.5
mseWRTDS <- sum(eList$Sample$resid^2) / numSamples
rmseWRTDS <- mseWRTDS^0.5
title <- paste("WRTDS model rmse = ",format(rmseWRTDS,digits=3),"\nBaseFlow Model rmse = ", format(rmseBaseMod,digits=3),"\nhalf windows are (Y, Qb, Qp, Qs)  ",windowY," ",windowQb," ",windowQp," ",windowS,sep="")
allResids <- c(eList$Sample$resid,newSample$resid)
modIndex <- c(rep(1,numSamples),rep(2,numSamples))
fluxBiasMulti(eList)
par(mar=c(4,4,6,4))
boxplot(allResids~modIndex,names=c("WRTDS","Base Flow Model") ,main=title,ylab="Residuals",las=1,cex.main=1.2,cex.lab=1.2,cex.axis=1.2)
abline(h=0,col="red")
text <- paste(eList$INFO$shortName,"\n",eList$INFO$paramShortName,"\nBase Flow Model Residuals versus Discharge")
plot(eList$Sample$Q,newSample$resid,log="x",xlab="Discharge in cms",ylab="residual",main=text,las=1,cex.main=1.2,cex.axis=1.3,cex.lab=1.3)
abline(h=0,col="red")
text <- paste(eList$INFO$shortName,"\n",eList$INFO$paramShortName,"\nBase Flow Model Residuals versus Predicted")
plot(newSample$yHat,newSample$resid,xlab="Predicted ln(concentration in mg/L)",ylab="residual",main=text,las=1,cex.main=1.2,cex.axis=1.3,cex.lab=1.3)
abline(h=0,col="red")
text <- paste(eList$INFO$shortName,"\n",eList$INFO$paramShortName,"\nBase Flow Model Residuals Boxplot by Month")
boxplot(newSample$resid~newSample$Month,xlab="Month",ylab="residual",main=text,las=1,cex.main=1.2,cex.axis=1.3,cex.lab=1.3,varwidth=TRUE)
abline(h=0,col="red")