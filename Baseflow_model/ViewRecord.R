windowY <- as.numeric(readline("enter windowY, suggestion is 10  "))
windowQp <- as.numeric(readline("enter window Qp, suggestion is 40  "))
windowQb <- as.numeric(readline("enter window Qb, suggestion is 2  "))
startDate <- as.character(readline("enter starting Date, no quotes  "))
endDate <- as.character(readline("enter ending Date, no quotes  "))
startDate <- as.Date(startDate)
endDate <- as.Date(endDate)
startJulian <- as.numeric(julian(startDate,origin=as.Date("1850-01-01")))
endJulian <- as.numeric(julian(endDate,origin=as.Date("1850-01-01")))
shortNewDaily <- subset(newDaily,Julian>=startJulian&Julian<=endJulian)
shortDaily <- subset(eList$Daily,Julian>=startJulian&Julian<=endJulian)
shortSample <- subset(eList$Sample,Julian>=startJulian&Julian<=endJulian)
newSample$Q <- newSample$Q.x
newSample$LogQb <- log(newSample$Qb)
newSample$Conc <- newSample$ConcAve
estPtYear <- shortNewDaily$DecYear
nPts <- length(estPtYear)
estPtLQb <- log(shortNewDaily$Qb)
estPtQp <- shortNewDaily$Qp
results <- runBaseReg(estPtYear,estPtLQb,estPtQp,DecLow,DecHigh,Sample=newSample,windowY = windowY,
                      windowQb = windowQb, windowQp = windowQp, windowS = 0.5, minNumObs = 100)
modelConc <- results[,3]
WRTDSConc <- shortDaily$ConcDay
maxConc <- 1.1 *max(modelConc,WRTDSConc,shortSample$ConcAve)
minDecYear <- shortNewDaily$DecYear[1] - 0.1
maxDecYear <- shortNewDaily$DecYear[nPts] + 0.1
title <- paste(eList$INFO$shortName,"\n",eList$INFO$paramShortName,
               "\nBlack Line is baseflow model, Red is WRTDS, dots are data")
plot(shortNewDaily$DecYear,modelConc,xlim=c(minDecYear,maxDecYear),xaxs="i",ylim=c(0,maxConc),
     yaxs="i",xlab="Decimal Year",ylab = "Concentration in mg/L",main = title,
     las=1,cex.main=1.2,cex.axis=1.2,cex.lab=1.2,lwd=2,type="l")
par(new=TRUE)
plot(shortDaily$DecYear,WRTDSConc,xlim=c(minDecYear,maxDecYear),xaxs="i",ylim=c(0,maxConc),
     yaxs="i",xlab="",ylab = "",main = "",
     las=1,cex.main=1.2,cex.axis=1.2,cex.lab=1.2,lwd=2,col="red",type="l")
par(new=TRUE)
plot(shortSample$DecYear,shortSample$ConcAve,xlim=c(minDecYear,maxDecYear),xaxs="i",ylim=c(0,maxConc),
     yaxs="i",xlab="",ylab = "",main = "",
     las=1,cex.main=1.2,cex.axis=1.2,cex.lab=1.2,cex=0.8)
# plot of baseflow separation
maxQ <- 1.1 * max(shortNewDaily$Q)
minQ <- 0.9 * min(shortNewDaily$Qb)
title <- paste(eList$INFO$shortName,"\nBlack Line is total discharge\nRed is baseflow discharge")
plot(shortNewDaily$DecYear,shortNewDaily$Q,xlim=c(minDecYear,maxDecYear),xaxs="i",
     ylim=c(minQ,maxQ),yaxs="i",log="y",xlab="Decimal Year",ylab="Discharge in cms",main=title,
     las=1,cex.main=1.2,cex.axis=1.2,cex.lab=1.2,lwd=2,type="l",col="black")
par(new=TRUE)
plot(shortNewDaily$DecYear,shortNewDaily$Qb,xlim=c(minDecYear,maxDecYear),xaxs="i",
     ylim=c(minQ,maxQ),yaxs="i",log="y",xlab="",ylab="",main=,
     las=1,cex.main=1.2,cex.axis=1.2,cex.lab=1.2,lwd=2,type="l",col="red")