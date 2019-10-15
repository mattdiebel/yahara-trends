plotFitObs <- function(eList) {
  par(mfrow = c(1,2))
  Sample <- eList$Sample
  Sample$y <- log(Sample$ConcAve)
  lims <- c(min(Sample[,c("ConcHat","ConcHat_QSM","ConcAve")]),
           max(Sample[,c("ConcHat","ConcHat_QSM","ConcAve")]))
  Sample$residual <- Sample$y - Sample$yHat
  RMSE <- round((mean(Sample$residual^2))^0.5,2)
  lm <- lm(y~yHat, Sample)
  R2 <- round(summary(lm)$r.squared,2)
  plot_title <- paste0(INFO$shortName, " WRTDS", "\nR-squared  =  ", R2, ", RMSE  =  ", RMSE)
  plot(ConcAve~ConcHat, Sample, xlim = lims, ylim = lims, log = "xy", xlab = "Estimated Concentration (mg/L)", ylab = "Observed Concentration (mg/L)", main = plot_title)
  abline(0,1)
  Sample$residual <- Sample$y - Sample$yHat_QSM
  RMSE <- round((mean(Sample$residual^2))^0.5,2)
  lm <- lm(y~yHat_QSM, Sample)
  R2 <- round(summary(lm)$r.squared,2)
  plot_title <- paste0(INFO$shortName, " QSM", "\nR-squared  =  ", R2, ", RMSE  =  ", RMSE)
  plot(ConcAve~ConcHat_QSM, Sample, xlim = lims, ylim = lims, log = "xy", xlab = "Estimated Concentration (mg/L)", ylab = "Observed Concentration (mg/L)", main = plot_title)
  abline(0,1)
  par(mfrow = c(1,1))
}