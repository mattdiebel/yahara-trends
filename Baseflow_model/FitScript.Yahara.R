setwd("C:/LWRD/yahara-trends/Baseflow_model/")

library(EGRET)
library(dataRetrieval)
library(lubridate)
source("runBaseRegMWD.R")

load("Yahara_TP.RData")
Sample = eList$Sample
names(Sample)[names(Sample) == "bt"] = "Qb"
Sample$LogQb = log(Sample$Qb)
Sample$Qp = Sample$Qb/Sample$Q
Daily = eList$Daily
names(Daily)[names(Daily) == "bt"] = "Qb"
Daily$LogQb = log(Daily$Qb)
Daily$Qp = Daily$Qb/Daily$Q


estPtYear = Sample$DecYear
estPtLQb = Sample$LogQb
estPtQp = Sample$Qp
DecLow = eList$Daily$DecYear[1]
DecHigh = eList$Daily$DecYear[length(eList$Daily$Date)]
windowY = 3
windowQb = 0.20
windowQp = 30
windowS = 0.25
minNumObs = 100


BFM = runBaseReg(estPtYear, estPtLQb, estPtQp, DecLow, DecHigh, Sample=Sample, windowY=windowY, windowQb=windowQb, windowQp=windowQp, windowS=windowS, minNumObs=minNumObs)
colnames(BFM) = c("yHat","SE","ConcHat")

Sample = cbind(Sample, BFM)
Sample$y = log(Sample$ConcAve)
Sample$residual = Sample$y - Sample$yHat
Sample_BFM = Sample
RMSE = round((mean(Sample$residual^2))^0.5,2)
lm = lm(y~yHat, Sample)
R2 = round(summary(lm)$r.squared,2)
plot_title = paste0("R-squared = ", R2, ", RMSE = ", RMSE)
lims = c(min(Sample$ConcHat,Sample$ConcAve),max(Sample$ConcHat,Sample$ConcAve))
plot(ConcAve~ConcHat, Sample, xlim=lims, ylim=lims, log="xy", main=plot_title)
abline(0,1)

# Estimate yHat for Daily
estPtYear = Daily$DecYear
estPtLQb = Daily$LogQb
estPtQp = Daily$Qp

BFM_daily = runBaseReg(estPtYear, estPtLQb, estPtQp, DecLow, DecHigh, Sample=Sample, windowY=windowY, windowQb=windowQb, windowQp=windowQp, windowS=windowS, minNumObs=minNumObs)
colnames(BFM_daily) = c("yHat_BFM","SE_BFM","ConcHat_BFM")

Daily = cbind(Daily, BFM_daily)

# Plot BFM and WRTDS models
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
               y=~ConcHat_BFM,
               name="BFM",
               line=list(width=1, color="blue"),
               hoverlabel=list(bgcolor="white"),
               hoverinfo="text", 
               text=paste0("BFM", "<br>", 
                           format(Daily$Date, "%B %d, %Y"), "<br>", 
                           "TP: ", signif(Daily$ConcHat_BFM,3), " mg/L"))

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

library(ggplot2)

png("residual_Q_month_BFM.png", res=400, height=8000, width=1000)
ggplot(data=Sample_BFM, aes(Q, residual)) +
  stat_density_2d(aes(fill=..level..), geom = "polygon") + 
  scale_fill_continuous(low="#d3dfed", high="#053975") +
  facet_wrap(~ Month, ncol=1) +
  scale_x_log10() +
  ylim(c(-1,1)) +
  theme_light() +
  theme(legend.position = "none")
dev.off()

png("residual_Q_month_QSM.png", res=400, height=8000, width=1000)
ggplot(data=Sample_QSM, aes(Q, residual)) +
  stat_density_2d(aes(fill=..level..), geom = "polygon") + 
  scale_fill_continuous(low="#d3dfed", high="#053975") +
  facet_wrap(~ Month, ncol=1) +
  scale_x_log10() +
  ylim(c(-1,1)) +
  theme_light() +
  theme(legend.position = "none")
dev.off()
