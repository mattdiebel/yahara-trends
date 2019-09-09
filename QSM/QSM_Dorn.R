setwd("C:/LWRD/yahara-trends/QSM/")

library(EGRET)
library(dataRetrieval)
library(lubridate)
library(EcoHydRology)
source("runQSepReg.R")

INFO = readNWISInfo("05427930", "00665", interactive=FALSE)
INFO$shortName = "Dorn Creek"
INFO$staAbbrev = "DCM"
INFO$paramShortName = "Total Phosphorus"
INFO$constitAbbrev = "TP"

Sample = readNWISSample("05427930", "00665")
Sample = Sample[Sample$Date>=as.Date("2013-10-01"),]
Sample = Sample[Sample$Date<=as.Date("2019-09-04"),]
plot(ConcAve~Date, Sample, log="y")

opd = aggregate(Sample[,c("Date","ConcAve")], by=list(Sample$Date), mean)
opd = data.frame(date=opd$Date, remark="", result=opd$ConcAve)
write.csv(opd, file="Dorn_samples.csv", row.names = FALSE)
Sample = readUserSample(filePath="C:/LWRD/yahara-trends/QSM/", fileName="Dorn_samples.csv")

Daily = readNWISDaily("05427948", "00060", "2013-10-01", "2019-09-04")
eList = mergeReport(INFO, Daily, Sample)
eList = modelEstimation(eList, windowY=7, windowQ=1, windowS=0.25)

Sample = eList$Sample
Sample$y = log(Sample$ConcAve)
Sample$residual = Sample$y - Sample$yHat
RMSE = round((mean(Sample$residual^2))^0.5,2)
lm = lm(y~yHat, Sample)
R2 = round(summary(lm)$r.squared,2)
plot_title = paste0("Dorn Creek WRTDS","\nR-squared = ", R2, ", RMSE = ", RMSE)
lims = c(min(Sample$ConcHat,Sample$ConcAve),max(Sample$ConcHat,Sample$ConcAve))
png("Dorn_WRTDS_fit_obs.png", res=300, height=1800, width=1800)
plot(ConcAve~ConcHat, Sample, xlim=lims, ylim=lims, log="xy", main=plot_title)
abline(0,1)
dev.off()

Qs = BaseflowSeparation(Daily$Q)
Daily$Qb = Qs$bt
Daily$LogQb = log(Daily$Qb)
Daily$Qq = Qs$qft +0.01
Daily$LogQq = log(Daily$Qq)
plot(Q~Date, Daily, type="l", log="y", col="orange")
lines(Qb~Date, Daily, col="blue")

Sample = eList$Sample
Sample = merge(Sample, Daily[,c("Date","Qb","LogQb","Qq","LogQq")], all.x=TRUE)

estPtYear = Sample$DecYear
estPtLQb = Sample$LogQb
estPtLQq = Sample$LogQq
DecLow = Daily$DecYear[1]
DecHigh = Daily$DecYear[length(Daily$Date)]
windowY = 7
windowQb = 0.3
windowQq = 1
windowS = 0.25
minNumObs = 100

QSM = runQSepReg(estPtYear, estPtLQb, estPtLQq, DecLow, DecHigh, localSample=Sample, windowY=windowY, windowQb=windowQb, windowQq=windowQq, windowS=windowS, minNumObs=minNumObs)
colnames(QSM) = c("yHat_QSM","SE_QSM","ConcHat_QSM")

Sample = cbind(Sample, QSM)
Sample$y = log(Sample$ConcAve)
Sample$residual = Sample$y - Sample$yHat_QSM
Sample_QSM = Sample
RMSE = round((mean(Sample$residual^2))^0.5,2)
lm = lm(y~yHat_QSM, Sample)
R2 = round(summary(lm)$r.squared,2)
plot_title = paste0("R-squared = ", R2, ", RMSE = ", RMSE)
lims = c(min(Sample$ConcHat_QSM,Sample$ConcAve),max(Sample$ConcHat_QSM,Sample$ConcAve))
plot_title = paste0("Dorn Creek QSM","\nR-squared = ", R2, ", RMSE = ", RMSE)
png("Dorn_QSM_fit_obs.png", res=300, height=1800, width=1800)
plot(ConcAve~ConcHat_QSM, Sample, xlim=lims, ylim=lims, log="xy", main=plot_title)
abline(0,1)
dev.off()

# Estimate yHat for Daily
estPtYear = Daily$DecYear
estPtLQb = Daily$LogQb
estPtLQq = Daily$LogQq

QSM_daily = runQSepReg(estPtYear, estPtLQb, estPtLQq, DecLow, DecHigh, localSample=Sample, windowY=windowY, windowQb=windowQb, windowQq=windowQq, windowS=windowS, minNumObs=minNumObs)
colnames(QSM_daily) = c("yHat","SE","ConcHat")

Daily = cbind(eList$Daily, QSM_daily)


# Plot QSM and WRTDS models
library(plotly)

p1 = plot_ly(type="scatter", mode="lines")

p1 = add_trace(p1,
               data=Daily,
               x=~Date,
               y=~ConcDay,
               name="WRTDS",
               line=list(width=1, color="#e3550e"),
               hoverlabel=list(bgcolor="white"),
               hoverinfo="text", 
               text=paste0("WRTDS", "<br>", 
                           format(Daily$Date, "%B %d, %Y"), "<br>", 
                           "TP: ", signif(Daily$ConcDay,3), " mg/L"))

p1 = add_trace(p1,
               data=Daily,
               x=~Date,
               y=~ConcHat,
               name="QSM",
               line=list(width=1, color="blue"),
               hoverlabel=list(bgcolor="white"),
               hoverinfo="text", 
               text=paste0("QSM", "<br>", 
                           format(Daily$Date, "%B %d, %Y"), "<br>", 
                           "TP: ", signif(Daily$ConcDay,3), " mg/L"))

p1 = add_trace(p1,
               data=Sample,
               x=~Date,
               y=~ConcAve,
               mode="markers",
               name="Measured",
               marker=list(
                 color="black",
                 line=list(
                   color="black",
                   width=1
                 )),
               hoverlabel=list(bgcolor="black"),
               hoverinfo="text",
               text=paste0("Measured", "<br>",
                           format(Sample$Date, "%B %d, %Y"), "<br>",
                           "TP: ", signif(Sample$ConcAve,3), " mg/L"))

p1 = layout(p1,
            yaxis=list(title="TP (mg/L)", type="log"),
            xaxis=list(title="", rangeslider=list(type="date", thickness=0.05)))

p1


