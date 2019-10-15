buildQSM <- function(eList) {
  Daily <- eList$Daily
  Qs <- BaseflowSeparation(Daily$Q)
  Daily$Qb <- Qs$bt
  Daily$LogQb <- log(Daily$Qb)
  Daily$Qq <- Qs$qft + 0.01
  Daily$LogQq <- log(Daily$Qq)
  Sample <- eList$Sample
  Sample <- Sample[!is.na(Sample$Q),]
  Sample <- merge(Sample, Daily[,c("Date","Qb","LogQb","Qq","LogQq")], all.x = TRUE)
  eList$Sample <- Sample
  cat("\nEstimating QSM for Sample...\n")
  QSM_Sample <- runQSepReg(estPtYear = Sample$DecYear, estPtLQb = Sample$LogQb, estPtLQq = Sample$LogQq, DecLow = Daily$DecYear[1], DecHigh = Daily$DecYear[length(Daily$Date)], localSample = Sample, windowY = windowY, windowQb = windowQb, windowQq = windowQq, windowS = windowS, minNumObs = minNumObs)
  colnames(QSM_Sample) <- c("yHat_QSM","SE_QSM","ConcHat_QSM")
  Sample <- cbind(Sample, QSM_Sample)
  eList$Sample <- Sample
  cat("\nEstimating QSM for Daily...\n")
  QSM_Daily <- runQSepReg(estPtYear = Daily$DecYear, estPtLQb = Daily$LogQb, estPtLQq = Daily$LogQq, DecLow = Daily$DecYear[1], DecHigh = Daily$DecYear[length(Daily$Date)], localSample = Sample, windowY = windowY, windowQb = windowQb, windowQq = windowQq, windowS = windowS, minNumObs = minNumObs)
  colnames(QSM_Daily) <- c("yHat_QSM","SE_QSM","ConcDay_QSM")
  Daily <- cbind(Daily, QSM_Daily) 
  Daily$FluxDay_QSM = Daily$ConcDay_QSM * Daily$Q * 86.4
  eList$Daily <- Daily
	return(eList)
}