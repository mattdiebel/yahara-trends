setwd("C:/LWRD/yahara-trends/QSM/")

library(EGRET)
library(dataRetrieval)
library(lubridate)
source("runQSepReg.R")

load("Yahara_TP.RData")
eList = modelEstimation(eList, windowY=3, windowQ=1, windowS=0.25)

Sample = eList$Sample
Sample$y = log(Sample$ConcAve)
Sample$residual = Sample$y - Sample$yHat
RMSE = round((mean(Sample$residual^2))^0.5,2)
lm = lm(y~yHat, Sample)
R2 = round(summary(lm)$r.squared,2)
plot_title = paste0("Yahara Windsor WRTDS", "\nR-squared = ", R2, ", RMSE = ", RMSE)
lims = c(min(Sample$ConcHat,Sample$ConcAve),max(Sample$ConcHat,Sample$ConcAve))
png("Yahara_WRTDS_fit_obs.png", res=300, height=1800, width=1800)
plot(ConcAve~ConcHat, Sample, xlim=lims, ylim=lims, log="xy", main=plot_title)
abline(0,1)
dev.off()

WY = tableResults(eList, fluxUnit=13)
colnames(WY) = c("WY","Q","Conc","FNConc","Flux","FNFlux")

Sample = eList$Sample
names(Sample)[names(Sample) == "bt"] = "Qb"
Sample$LogQb = log(Sample$Qb)
names(Sample)[names(Sample) == "qft"] = "Qq"
Sample$Qq = Sample$Qq + 0.01
Sample$LogQq = log(Sample$Qq)

Daily = eList$Daily
names(Daily)[names(Daily) == "bt"] = "Qb"
Daily$LogQb = log(Daily$Qb)
names(Daily)[names(Daily) == "qft"] = "Qq"
Daily$Qq = Daily$Qq + 0.01
Daily$LogQq = log(Daily$Qq)

estPtYear = Sample$DecYear
estPtLQb = Sample$LogQb
estPtLQq = Sample$LogQq
DecLow = eList$Daily$DecYear[1]
DecHigh = eList$Daily$DecYear[length(eList$Daily$Date)]
windowY = 3
windowQb = 0.2
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
plot_title = paste0("Yahara Windsor QSM", "\nR-squared = ", R2, ", RMSE = ", RMSE)
lims = c(min(Sample$ConcHat_QSM,Sample$ConcAve),max(Sample$ConcHat_QSM,Sample$ConcAve))
png("Yahara_QSM_fit_obs.png", res=300, height=1800, width=1800)
plot(ConcAve~ConcHat_QSM, Sample, xlim=lims, ylim=lims, log="xy", main=plot_title)
abline(0,1)
dev.off()

# Estimate Daily yHat
estPtYear = Daily$DecYear
estPtLQb = Daily$LogQb
estPtLQq = Daily$LogQq

QSM_daily = runQSepReg(estPtYear, estPtLQb, estPtLQq, DecLow, DecHigh, localSample=Sample, windowY=windowY, windowQb=windowQb, windowQq=windowQq, windowS=windowS, minNumObs=minNumObs)
colnames(QSM_daily) = c("yHat","SE","ConcHat")

Daily = cbind(Daily, QSM_daily)

# Estimate Daily FNConc and FNFlux
wy = unique(Daily$waterYear)
nyears = length(wy)
ndays = nrow(Daily)
Creps = Freps = matrix(nrow=ndays, ncol=nyears)
for (y in 1:nyears) {
  
  print(wy[y])
  flush.console()
  
  Q = Daily[Daily$waterYear==wy[y],c("Day","Q")]
  repQ = merge(Daily[,c("Date","Day")], Q, all.x=TRUE)
  repQ = repQ[order(repQ$Date),]
  repQ$Q = approx(repQ$Date[!(is.na(repQ$Q))], repQ$Q[!(is.na(repQ$Q))], repQ$Date)$y
  
  LQb = Daily[Daily$waterYear==wy[y],c("Day","LogQb")]
  repLQb = merge(Daily[,c("Date","Day")], LQb, all.x=TRUE)
  repLQb = repLQb[order(repLQb$Date),]
  repLQb$LogQb = approx(repLQb$Date[!(is.na(repLQb$LogQb))], repLQb$LogQb[!(is.na(repLQb$LogQb))], repLQb$Date)$y
  estPtLQb = repLQb$LogQb
  
  LQq = Daily[Daily$waterYear==wy[y],c("Day","LogQq")]
  repLQq = merge(Daily[,c("Date","Day")], LQq, all.x=TRUE)
  repLQq = repLQq[order(repLQq$Date),]
  repLQq$LogQq = approx(repLQq$Date[!(is.na(repLQq$LogQq))], repLQq$LogQq[!(is.na(repLQq$LogQq))], repLQq$Date)$y
  estPtLQq = repLQq$LogQq

  QSM_daily = runQSepReg(estPtYear, estPtLQb, estPtLQq, DecLow, DecHigh, localSample=Sample, windowY=windowY, windowQb=windowQb, windowQq=windowQq, windowS=windowS, minNumObs=minNumObs)
  colnames(QSM_daily) = c("yHat","SE","ConcHat")
  Creps[,y] = QSM_daily[,3]
  Freps[,y] = QSM_daily[,3] * repQ$Q * 86.4
}

FNConc_QSM = rowMeans(Creps)
FNFlux_QSM = rowMeans(Freps)
Daily = cbind(Daily, FNConc_QSM, FNFlux_QSM)
eList$Daily = Daily
save(eList, file="Yahara_TP.RData")
WY_QSM_conc = aggregate(Daily[,c("waterYear","FNConc_QSM")], by=list(Daily$waterYear), FUN=mean)
WY_QSM_flux = aggregate(Daily$FNFlux_QSM, by=list(Daily$waterYear), FUN=sum)
colnames(WY_QSM_flux) = c("waterYear","FNFlux_QSM")

png("Yahara_FNConc_annual_trend.png", res=300, height=1800, width=1800)
plot(FNConc~WY, WY, type="l", xlab="", main="Yahara at Windsor")
lines(FNConc_QSM~waterYear, WY_QSM_conc, lty=2)
legend("topright", lty=c(1,2), legend=c("WRTDS","QSM"), bty="n")
dev.off()

png("Yahara_FNFlux_annual_trend.png", res=300, height=1800, width=1800)
plot(FNFlux~WY, WY, type="l", xlab="", main="Yahara at Windsor")
lines(FNFlux_QSM~waterYear, WY_QSM_flux, lty=2)
legend("topright", lty=c(1,2), legend=c("WRTDS","QSM"), bty="n")
dev.off()


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
               y=~FNConc,
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

