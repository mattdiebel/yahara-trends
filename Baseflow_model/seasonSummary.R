seasonSummary <- function (localDaily){
   for(season in 1:4) {
		paLong <- 3
		paStart <- 1 + ((season-1) * 3)
		cat("\nseason paStart",season, paStart)
    numDays <- length(localDaily$MonthSeq)
    firstMonthSeq <- localDaily$MonthSeq[1]
    lastMonthSeq <- localDaily$MonthSeq[numDays]
    Starts <- seq(paStart, lastMonthSeq, 12)
    Ends <- Starts + paLong - 1
    StartEndSeq <- data.frame(Starts, Ends)
    StartEndSeq <- StartEndSeq[(StartEndSeq$Starts >= firstMonthSeq) & 
        (StartEndSeq$Ends <= lastMonthSeq), ]
    firstMonth <- StartEndSeq[1, 1]
    numYears <- length(StartEndSeq$Starts)
    DecYear <- rep(NA, numYears)
    Q <- rep(NA, numYears)
    BFMConc <- rep(NA, numYears)
    BFMFlux <- rep(NA, numYears)
    BFMbfConc <- rep(NA, numYears)
    BFMbfFlux <- rep(NA, numYears)
    WRTDSConc <- rep(NA, numYears)
    WRTDSFlux <- rep(NA, numYears)
    for (i in 1:numYears) {
        startMonth <- (i - 1) * 12 + firstMonth
        stopMonth <- startMonth + paLong - 1
        DailyYear <- localDaily[which(localDaily$MonthSeq %in% 
            startMonth:stopMonth), ]
        counter <- ifelse(is.na(DailyYear$BFMConc), 0, 1)
        if (length(counter) > 0) {
            good <- (sum(counter) > 25)
        }
        else {
            good <- FALSE
        }
        DecYear[i] <- mean(DailyYear$DecYear)
        
        if (good) {
        	Q[i] <- mean(DailyYear$Q,na.rm = TRUE)
            BFMConc[i] <- mean(DailyYear$BFMConc, na.rm = TRUE)
            BFMFlux[i] <- sum(DailyYear$BFMFlux, na.rm = TRUE)
            BFMbfConc[i] <- mean(DailyYear$BFMbfConc, na.rm = TRUE)
            BFMbfFlux[i] <- sum(DailyYear$BFMbfFlux, na.rm = TRUE)
            WRTDSConc[i] <- mean(DailyYear$WRTDSConc, na.rm = TRUE)
            WRTDSFlux[i] <- sum(DailyYear$WRTDSFlux, na.rm = TRUE)
        }
    }
    seasonID <- rep(season,numYears)
    seasonResult <- data.frame(DecYear,seasonID,Q,BFMConc,BFMFlux,BFMbfConc,BFMbfFlux,WRTDSConc,WRTDSFlux)
    allResult <- if(season==1) seasonResult else rbind(seasonResult,allResult)
    }
    allResult <- allResult[order(allResult$DecYear),]
    return(allResult)
    }
