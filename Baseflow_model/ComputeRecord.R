source("~/Dropbox/BFM/seasonSummary.R")
windowY <- as.numeric(readline("enter windowY, suggestion is 10  "))
windowQp <- as.numeric(readline("enter window Qp, suggestion is 40  "))
windowQb <- as.numeric(readline("enter window Qb, suggestion is 2  "))
# the next few lines are there just to be sure that the two Daily data frames have
# identical start and end dates
j1Daily <- eList$Daily$Julian[1]
n <- length(eList$Daily$Julian)
jnDaily <- eList$Daily$Julian[n]
j1newDaily <- newDaily$Julian[1]
n <- length(newDaily$Julian)
jnnewDaily <- newDaily$Julian[n]
j1 <- max(j1Daily,j1newDaily)
jn <- min(jnDaily,jnnewDaily)
localDaily <- subset(eList$Daily,Julian>=j1&Julian<=jn)
localNewSample <- subset(newSample,Julian>=j1&Julian<=jn)
localNewDaily <- subset(newDaily,Julian>=j1&Julian<=jn)
# end of trimming of data frames
nDays <- length(localNewDaily$Date)
DecLow <- localNewDaily$DecYear[1]
DecHigh <- localNewDaily$DecYear[nDays]
# the following 3 lines are just some housekeeping, may not be necessary, but safer
localNewSample$Q <- localNewSample$Q.x
localNewSample$LogQb <- log(localNewSample$Qb)
localNewSample$Conc <- localNewSample$ConcAve
estPtYear <- localNewDaily$DecYear
estPtLQb <- log(localNewDaily$Qb)
estPtQp <- localNewDaily$Qp
results <- runBaseReg(estPtYear,estPtLQb,estPtQp,DecLow,DecHigh,Sample=localNewSample,windowY = windowY,
                      windowQb = windowQb, windowQp = windowQp, windowS = 0.5, minNumObs = 100)
BFMConc <- results[,3]
BFMFlux <- BFMConc * localNewDaily$Q * 86.4
# next we rerun the estimation with every day at 100 percent baseflow
estPtQp <- rep(100,nDays)
results <- runBaseReg(estPtYear,estPtLQb,estPtQp,DecLow,DecHigh,Sample=localNewSample,windowY = windowY,
                      windowQb = windowQb, windowQp = windowQp, windowS = 0.5, minNumObs = 100)
BFMbfConc <- results[,3]
BFMbfFlux <- BFMbfConc * localNewDaily$Qb * 86.4
# here we load up the WRTDS estimates of concentration and flux
WRTDSConc <- localDaily$ConcDay
WRTDSFlux <- localDaily$FluxDay
Date <- localDaily$Date
Month <- localDaily$Month
Day <- localDaily$Day
Julian <- localDaily$Julian
DecYear <- localDaily$DecYear
MonthSeq <- localDaily$MonthSeq
Q <- localDaily$Q
allDaily <- data.frame(Date,Month,Day,Julian,DecYear,MonthSeq,Q,BFMConc,BFMFlux,BFMbfConc,
                       BFMbfFlux,WRTDSConc,WRTDSFlux)
# now seasonal summaries
allResult <- seasonSummary(allDaily)
fileNamecsv <- paste(savePath,eList$INFO$staAbbrev,".",eList$INFO$constitAbbrev,".allResult.csv",sep="")
write.csv(allResult,file=fileNamecsv)
fileNameRData <- paste(savePath,eList$INFO$staAbbrev,".",eList$INFO$constitAbbrev,".allResult.RData",sep="")
save(list=c("eList$INFO","allResult"),file=fileNameRData)