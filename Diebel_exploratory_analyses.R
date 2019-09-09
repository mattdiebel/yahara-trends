library(remake)
library(EGRET)
library(plotly)
library(EcoHydRology)
library(hexbin)
library(gridExtra)

tp_mod <- make('tp_wy_out', remake_file = '30_analyze_data_series.yml')

tp_mod_Qc = tp_mod

Daily = tp_mod_Qc$Daily
Qs = BaseflowSeparation(Daily$Q)
# Qs$Q = Qs$qft + Qs$bt
Qs$qc = Qs$qft + 0.2
# hist(log(Qs$qc), breaks=100)
hist(log(Qs$qc), breaks=c(seq(-5,5,by=0.1)))
# hist(log(Qs$bt), breaks=c(seq(-10,5,by=0.1)), add=TRUE, col="gray")

Daily = cbind(Daily,Qs)
Daily$Q = Daily$qc
Daily$LogQ = log(Daily$Q)
Sample = tp_mod$Sample
Sample = merge(Sample,Daily[,c("Date","bt","qft")])
INFO = tp_mod$INFO
tp_mod_Qc = mergeReport(INFO,Daily,Sample)

# Plotly for inspecting composite flow

p1 = plot_ly(type="scatter", mode="lines")

# p1 = add_trace(p1,
#                data=Daily,
#                x=~Date,
#                y=~qc,
#                name="Composite flow",
#                line=list(width=1, color="orange"))
# 
p1 = add_trace(p1,
               data=Daily,
               x=~Date,
               y=~Q,
               name="Stormflow",
               line=list(width=1, color="blue"))
# 
# p1 = add_trace(p1,
#                data=Daily,
#                x=~Date,
#                y=~bt,
#                name="Baseflow",
#                line=list(width=1, color="blue"))

p1 = layout(p1,
            yaxis=list(title="Q (cms)", type="log"),
            xaxis=list(title="", rangeslider=list(type="date", thickness=0.05)))

p1


parameters = expand.grid(windowQ=c(0.5,1,2), windowS=c(0.25,0.5,1), windowY=c(3,5,7))
parameters$filename = paste0("Q_",parameters$windowQ,"_S_",parameters$windowS,"_Y_",parameters$windowY)
parameters$name = paste0("windowQ = ",parameters$windowQ,", windowS = ",parameters$windowS,", windowY = ",parameters$windowY)

for (p in 1:nrow(parameters)) {
  
  tp_mod_Qc <- modelEstimation(tp_mod_Qc,
                            windowQ = parameters$windowQ[p],
                            windowS = parameters$windowS[p],
                            windowY = parameters$windowY[p])
  
  filename = paste0("C:/LWRD/yahara-trends/figures/",parameters$filename[p],".png")
  
  Daily = tp_mod$Daily
  Sample = tp_mod$Sample
  WY = tableResults(tp_mod)
  colnames(WY) = c("WY","Q","Conc","FN_Conc","Flux","FN_Flux")
  WY$Flux = WY$Flux*1000
  WY$FN_Flux = WY$FN_Flux*1000
  Sample$y = log(Sample$ConcAve)
  lm = lm(y~yHat, Sample)
  r2 = paste0("R-squared = ", round(summary(lm)$r.squared,3))
  Sample$residual = Sample$y - Sample$yHat
  lims = c(min(Sample$yHat, Sample$y), max(Sample$yHat, Sample$y))
  hexcol = c("#ebf1ff","#002270")
  
  p1 = ggplot(Sample, aes(yHat, y)) +
    geom_hex(bins=50) +
    scale_fill_gradientn(colours=hexcol) +
    xlim(lims) +
    ylim(lims) +
    theme_light() +
    theme(legend.position = "none") +
    labs(title = r2)
  
  p2 = ggplot(Sample, aes(Date, residual)) +
    geom_point() +
    ylim(c(-3,3)) +
    theme_light()
    
  p3 = ggplot(Sample, aes(Q, residual)) +
    geom_hex(bins=50) +
    ylim(c(-3,3)) +
    scale_x_log10() +
    scale_fill_gradientn(colours=hexcol) +
    theme_light() +
    theme(legend.position = "none")
  
  p4 = ggplot(WY) +
    geom_point(aes(WY, Conc)) +
    geom_line(aes(WY, FN_Conc)) +
    theme_light()
  
  p5 = ggplot(WY) +
    geom_point(aes(WY, Flux)) +
    geom_line(aes(WY, FN_Flux)) +
    theme_light()
  
  p6 = ggplot() +
    geom_hex(data=Sample, aes(Q, ConcAve, alpha=..count..), bins=50, fill="#002270") +
    geom_hex(data=Daily, aes(Q, ConcDay, size=..count..), bins=50, color="#eb6734", fill=NA) +
    scale_size(range = c(0,1)) +
    scale_x_log10() +
    scale_y_log10() +
    theme_light() +
    theme(legend.position = "none")
  
  png(filename, res=400, height=4000, width=3000)
  grid.arrange(p1, p3, p6, p2, p4, p5, nrow=3)
  dev.off()
  
}

plotConcTimeSmooth(tp_mod, q1=0.2, q2=0.4, q3=1, centerDate="12-01", yearStart=1989, yearEnd=2018, logScale=TRUE)
plotConcQSmooth(tp_mod, date1="1990-01-15", date2=NA, date3="2018-01-15", qLow=10, qHigh=350, qUnit=1, legendLeft=12,legendTop=1.1)
plotContours(tp_mod, 1989, 2018, 0.2, 5, contourLevels=seq(0,1.5,0.1), flowDuration = FALSE)
plotDiffContours(tp_mod, year0=1990, year1=2018, qBottom=10, qTop=500, qUnit=1, maxDiff=1, flowDuration = FALSE)


# Daily Streamflow Trend Analysis https://owi.usgs.gov/blog/Quantile-Kendall/

source("C:/LWRD/yahara-trends/quantile_kendall.R")
plotFlowTrend(tp_mod, istat=8)
plotQuantileKendall(tp_mod)
plotQuantileKendall(tp_mod, paStart=1, paLong=3)


# Plot C-Q relationship by month and year
Daily = tp_mod$Daily
Sample = tp_mod$Sample
breaks = 10^(-10:10)
minor_breaks = rep(1:9, 21)*(10^rep(-10:10, each=9))

png("C:/LWRD/yahara-trends/figures/Conc-Q.png", res=400, height=4000, width=3000)
ggplot(Sample, aes(x=Q, y=ConcAve, color=waterYear)) +
  scale_color_viridis_c() +
  geom_point() +
  facet_wrap(~Month, ncol=3) +
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks) +
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
  theme_light()
dev.off()

png("C:/LWRD/yahara-trends/figures/Conc-Qc.png", res=400, height=4000, width=3000)
ggplot(Sample, aes(x=Q, y=ConcAve, color=waterYear)) +
  scale_color_viridis_c() +
  geom_point() +
  facet_wrap(~Month, ncol=3) +
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks) +
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
  theme_light()
dev.off()

png("C:/LWRD/yahara-trends/figures/Conc_bt.png", res=400, height=4000, width=3000)
ggplot(Sample[Sample$qft<0.2,], aes(x=bt, y=ConcAve, color=waterYear)) +
  scale_color_viridis_c() +
  geom_point() +
  facet_wrap(~Month, ncol=3) +
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
  theme_light()
dev.off()

png("C:/LWRD/yahara-trends/figures/Conc_resi_bt.png", res=400, height=4000, width=3000)
ggplot(Sample[Sample$qft<0.2,], aes(x=bt, y=residual, color=waterYear)) +
  scale_color_viridis_c() +
  geom_point() +
  facet_wrap(~Month, ncol=3) +
  theme_light()
dev.off()

# Plotly for inspecting daily time series
Daily = tp_mod$Daily
Sample = tp_mod$Sample
Daily_Qc = tp_mod_Qc$Daily
Sample_Qc = tp_mod_Qc$Sample

p1 = plot_ly(type="scatter", mode="lines")

p1 = add_trace(p1,
     data=Daily,
     x=~Date,
     y=~ConcDay,
     name="Original Q",
     line=list(width=1, color="#e3550e"),
     hoverlabel=list(bgcolor="white"),
     hoverinfo="text", 
     text=paste0("Original Q", "<br>", 
                 format(Daily$Date, "%B %d, %Y"), "<br>", 
                 "TP: ", signif(Daily$ConcDay,3), " mg/L"))

p1 = add_trace(p1,
     data=Daily_Qc,
     x=~Date,
     y=~ConcDay,
     name="Composite Q",
     line=list(width=1, color="blue"),
     hoverlabel=list(bgcolor="white"),
     hoverinfo="text", 
     text=paste0("Composite Q", "<br>", 
                 format(Daily_Qc$Date, "%B %d, %Y"), "<br>", 
                 "TP: ", signif(Daily_Qc$ConcDay,3), " mg/L"))

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
     # yaxis=list(title="TP (mg/L)"),
     xaxis=list(title="", rangeslider=list(type="date", thickness=0.05)))

p1

# GAM
library(mgcv)
source("D:/Backup_DNR/Miscellaneous/R_functions/mod.vis.gam.R")
gam1 = gam(log(ConcAve) ~ te(LogQ, Day, DecYear), data=Sample)
mod.vis.gam(gam1, view=c("DecYear","LogQ"), color="bluegreen", plot.type="contour", main="")
points(LogQ~DecYear, Sample, cex=log(Sample$ConcAve*100)/2)


