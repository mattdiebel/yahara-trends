FNBF <- function(eList) {
  Sample <- eList$Sample
  Daily <- eList$Daily
  wy <- unique(Daily$waterYear)
  nyears <- length(wy)
  ndays <- nrow(Daily)
  Creps <- matrix(nrow = ndays, ncol = nyears)
  Freps <- Creps
  
  for (y in 1:nyears) {
    
    cat("WY", wy[y],"flow\n")
    
    Q <- Daily[Daily$waterYear==wy[y],c("Day","Q")]
    repQ <- merge(Daily[,c("Date","Day")], Q, all.x=TRUE)
    repQ <- repQ[order(repQ$Date),]
    repQ$Q <- approx(repQ$Date[!(is.na(repQ$Q))], repQ$Q[!(is.na(repQ$Q))], repQ$Date)$y
    
    LQb <- Daily[Daily$waterYear==wy[y],c("Day","LogQb")]
    repLQb <- merge(Daily[,c("Date","Day")], LQb, all.x=TRUE)
    repLQb <- repLQb[order(repLQb$Date),]
    repLQb$LogQb <- approx(repLQb$Date[!(is.na(repLQb$LogQb))], repLQb$LogQb[!(is.na(repLQb$LogQb))], repLQb$Date)$y
    
    LQq <- Daily[Daily$waterYear==wy[y],c("Day","LogQq")]
    repLQq <- merge(Daily[,c("Date","Day")], LQq, all.x=TRUE)
    repLQq <- repLQq[order(repLQq$Date),]
    repLQq$LogQq <- approx(repLQq$Date[!(is.na(repLQq$LogQq))], repLQq$LogQq[!(is.na(repLQq$LogQq))], repLQq$Date)$y
    
    QSM_Daily <- runQSepReg(estPtYear = Daily$DecYear, estPtLQb = repLQb$LogQb, estPtLQq = repLQq$LogQq, DecLow = Daily$DecYear[1], DecHigh = Daily$DecYear[length(Daily$Date)], localSample = Sample, windowY = windowY, windowQb = windowQb, windowQq = windowQq, windowS = windowS, minNumObs = minNumObs)
    Creps[,y] <- QSM_Daily[,3]
    Freps[,y] <- QSM_Daily[,3] * repQ$Q * 86.4
  }
  
  FNConc_QSM <- rowMeans(Creps)
  FNFlux_QSM <- rowMeans(Freps)
  Daily <- cbind(Daily, FNConc_QSM, FNFlux_QSM)
  eList$Daily <- Daily
  
  WY_vars <- expand.grid(model = c("WRTDS","QSM"),
                         FN = c("Annual","Flow-Normalized"),
                         measure = c("Concentration","Flux"))
  
  WY_vars$variable <- c("ConcDay","ConcDay_QSM","FNConc","FNConc_QSM","FluxDay","FluxDay_QSM","FNFlux","FNFlux_QSM")
  
  WY_vars$FUN[WY_vars$measure=="Concentration"] <- "mean"
  WY_vars$FUN[WY_vars$measure=="Flux"] <- "sum"
  
  WY <- data.frame()
  
  for (v in 1:nrow(WY_vars)) {
    
    f <- formula(paste(WY_vars$variable[v],"waterYear", sep="~"))
    temp <- aggregate(f, data = Daily, FUN = WY_vars$FUN[v])
    colnames(temp)[2] <- "value"
    temp <- cbind(temp, WY_vars[v,], row.names = NULL)
    WY <- rbind(WY, temp)
  }
  
  eList$WY <- WY
  
  return(eList)
}

