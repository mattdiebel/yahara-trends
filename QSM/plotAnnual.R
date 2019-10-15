plotAnnual <- function(eList) {
  Daily <- eList$Daily
  WY_conc <- aggregate(Daily[,c("ConcDay","ConcDay_QSM")], by = list(Daily$waterYear), mean)
  colnames(WY_conc) = c("waterYear","Conc","Conc_QSM")
  WY_flux <- aggregate(Daily[,c("FluxDay","FluxDay_QSM")], by = list(Daily$waterYear), sum)
  colnames(WY_flux) <- c("waterYear","Flux","Flux_QSM")
  WY <- merge(WY_conc, WY_flux)
  conc_lims = c(min(WY$Conc,WY$Conc_QSM), max(WY$Conc,WY$Conc_QSM))
  flux_lims = c(min(WY$Flux,WY$Flux_QSM), max(WY$Flux,WY$Flux_QSM))
  par(mfrow = c(1,2))
  plot(Conc_QSM~Conc, WY, xlab = "WRTDS", ylab = "QSM", xlim = conc_lims, ylim = conc_lims, main = "Concentration")
  abline(0,1)
  plot(Flux_QSM~Flux, WY, xlab = "WRTDS", ylab = "QSM", xlim = flux_lims, ylim = flux_lims, main = "Flux")
  abline(0,1)
  par(mfrow = c(1,1))
}